(* The cash forecast: a day-by-day run of each cash account from today's balance, applying the
   recurring series, the household contribution plan and usual spending. Pure and deterministic. *)
let ( +: ) = Int64.add
let ( -: ) = Int64.sub

type one_off = { date : Date.t; amount : int64; description : string }
type scenario = { leave_out : string list; one_off : one_off list; follow_budgets : bool }

let no_scenario = { leave_out = []; one_off = []; follow_budgets = false }

type goal = { goal_name : string; earmarked : int64; monthly : int64 option; target : int64 }
type member_contribution = { member_name : string; due_now : int64; per_cycle : int64 }
type contribution = Outgoing of { due_now : int64; per_cycle : int64 } | Incoming of member_contribution list

type input = {
  today : Date.t;
  setting : Pay_cycle_setting.t;
  horizon : int;
  book : Analytics.book;
  owners : Owner.t list;
  accounts : Account_info.t list;
  imported_through : (Account_id.t * Date.t) list;
  transfers : Transfer.t list;
  series : Series.t list;
  budgets : (Account_id.t * int64) list;
  goals : goal list;
  contribution : contribution option;
}

type kind = Bill | Income | Transfer | Contribution | One_off

let kind_to_string = function
  | Bill -> "bill"
  | Income -> "income"
  | Transfer -> "transfer"
  | Contribution -> "contribution"
  | One_off -> "one_off"

type item = { item_id : string; date : Date.t; kind : kind; name : string; amount : int64; account : Account_id.t; series : Series.t option }
type point = { date : Date.t; cash : int64; earmarked : int64 }
type outlook = { account : Account_info.t; start : int64; lowest : Date.t * int64; overdraw : Date.t option }

type assumption =
  | Usual_spending of { per_cycle : int64; first : Date.t; last : Date.t; days : int }
  | Stopped of Series.t
  | Stale_import of Date.t
  | Other_currency of Account_info.t list
  | Short_history of int
  | No_income_detected

type t = {
  today : Date.t;
  imported_through : Date.t option;
  horizon_end : Date.t;
  history : (Date.t * int64) list;
  baseline : point list;
  scenario : point list option;
  lowest : Date.t * int64;
  accounts : outlook list;
  items : item list;
  assumptions : assumption list;
}

let before a b = Date.compare a b < 0
let rec days_from first last = if before last first then [] else first :: days_from (Date.add_days first 1) last

let cash_accounts (input : input) =
  List.filter_map
    (fun (info : Account_info.t) ->
      match input.book.inside info.account.id with
      | Some info when info.account.kind = Asset && not (Analytics.is_investment info) -> Some info
      | _ -> None)
    input.accounts

(* What is left once the pending card reservations go through. *)
let start_of (info : Account_info.t) =
  let b = info.account.balances in
  b.debits_posted -: b.credits_posted -: b.credits_pending

let find_account (input : input) id = List.find_opt (fun (info : Account_info.t) -> Account_id.equal info.account.id id) input.accounts
let imported_for (input : input) id = List.find_map (fun (a, d) -> if Account_id.equal a id then Some d else None) input.imported_through

(* The payment after one on [date]. Salary comes on payday, so a monthly income that lands on a
   cycle start comes next on the following one. *)
let following (input : input) (s : Series.t) date =
  let cycle = Cycle.cycle_of input.setting date in
  if s.kind = Income && s.cadence = Monthly && Date.equal cycle.first date then (Cycle.next input.setting cycle).first
  else Analytics.next_after s.cadence date

let rec dates_until (input : input) s ~last date = if before last date then [] else date :: dates_until input s ~last (following input s date)

(* [None] for a series more than one interval overdue. A payment due since the account's last
   import may just not be imported yet, so it is due today; one the import already covers was
   skipped this time. An account without imports gets five days, as the Overview's shortfall
   check does (Analytics.shortfalls). *)
let series_dates (input : input) ~last (s : Series.t) =
  let next = s.next_expected in
  if before (following input s next) input.today then None
  else if not (before next input.today) then Some (dates_until input s ~last next)
  else
    let rest = dates_until input s ~last (following input s next) in
    match imported_for input s.account with
    | Some through when before through next -> Some (input.today :: rest)
    | Some _ -> Some rest
    | None when not (before next (Date.add_days input.today (-5))) -> Some (input.today :: rest)
    | None -> Some rest

let cycles_until (input : input) last =
  let rec go (cycle : Cycle.t) = if before last cycle.first then [] else cycle :: go (Cycle.next input.setting cycle) in
  go (Cycle.cycle_of input.setting input.today)

(* The planned contribution replaces the transfers it was detected as: to the household's
   accounts when it is ours to pay, from the members' when it is the household's to receive. *)
let household_series (input : input) (s : Series.t) =
  s.kind = Transfer
  && Option.is_none (input.book.inside s.counter)
  &&
  match find_account input s.counter, input.contribution with
  | Some info, Some (Outgoing _) -> (match info.account.owner with Owner.Household _ -> true | User _ -> false)
  | Some info, Some (Incoming _) -> (match info.account.owner with Owner.User _ -> true | Household _ -> false)
  | _ -> false

(* The account salary lands in, or the first cash account: where a payment with no history of
   its own is assumed to go. *)
let main_account (input : input) cash =
  let incomes = List.filter (fun (s : Series.t) -> s.kind = Income) input.series in
  match List.sort (fun (a : Series.t) (b : Series.t) -> Int64.compare b.amount a.amount) incomes, cash with
  | s :: _, _ -> Some s.account
  | [], (info : Account_info.t) :: _ -> Some info.account.id
  | [], [] -> None

let contribution_items (input : input) ~last ~account ~(usual : Series.t option) =
  let usual_day (cycle : Cycle.t) =
    match usual with
    | Some s -> List.find_opt (Cycle.contains cycle) (dates_until input s ~last:cycle.last s.next_expected)
    | None -> Some cycle.first
  in
  let each ~item_id ~name ~sign ~due_now ~per_cycle =
    List.concat
      (List.mapi
         (fun i (cycle : Cycle.t) ->
           if i = 0 then
             if Int64.compare due_now 0L <= 0 then []
             else
               let date = match usual_day cycle with Some d when not (before d input.today) -> d | _ -> input.today in
               [ { item_id; date; kind = Contribution; name; amount = sign due_now; account; series = None } ]
           else
             match usual_day cycle with
             | Some date when not (before last date) -> [ { item_id; date; kind = Contribution; name; amount = sign per_cycle; account; series = None } ]
             | _ -> [])
         (cycles_until input last))
  in
  match input.contribution with
  | None -> []
  | Some (Outgoing { due_now; per_cycle }) -> each ~item_id:"contribution" ~name:"household" ~sign:Int64.neg ~due_now ~per_cycle
  | Some (Incoming members) ->
      List.concat_map
        (fun m -> each ~item_id:("contribution:" ^ m.member_name) ~name:m.member_name ~sign:Fun.id ~due_now:m.due_now ~per_cycle:m.per_cycle)
        members

(* Usual spending: what each cash account paid for in the last three complete cycles, less the
   bills the series already cover, as (total, over days) parts that are spread evenly. *)
type usual = { parts : (Account_id.t * int64 * int) list; per_cycle : int64; first : Date.t; last : Date.t; days : int; short : bool }

let usual_spending (input : input) cash scenario =
  let is_cash id = List.exists (fun (info : Account_info.t) -> Account_id.equal info.account.id id) cash in
  let current = Cycle.cycle_of input.setting input.today in
  let c1 = Cycle.previous input.setting current in
  let c3 = Cycle.previous input.setting (Cycle.previous input.setting c1) in
  let posted = List.filter (fun (t : Transfer.t) -> Analytics.posted t && t.code <> Reclassify) input.transfers in
  let data_start =
    List.fold_left
      (fun earliest (t : Transfer.t) ->
        if is_cash t.debit_account_id || is_cash t.credit_account_id then
          Some (match earliest with Some d -> Date.min d t.value_date | None -> t.value_date)
        else earliest)
      None posted
  in
  let first = match data_start with Some d -> Date.max d c3.first | None -> current.first in
  let last = c1.last in
  let days = Date.days_between ~first ~last + 1 in
  let bills = List.filter_map (fun (s : Series.t) -> if s.kind = Bill then Some s.id else None) input.series in
  let expense id = match input.book.inside id with Some info when info.account.kind = Expense -> Some info.account.id | _ -> None in
  let spent = Hashtbl.create 16 in
  let add a k v = Hashtbl.replace spent (a, k) (v +: Option.value (Hashtbl.find_opt spent (a, k)) ~default:0L) in
  if days > 0 then
    List.iter
      (fun (t : Transfer.t) ->
        if (not (before t.value_date first)) && not (before last t.value_date) && not (List.mem ("bill:" ^ Analytics.merchant_key t) bills) then
          match expense t.debit_account_id, expense t.credit_account_id with
          | Some k, None when is_cash t.credit_account_id -> add t.credit_account_id k (Analytics.amount t)
          | None, Some k when is_cash t.debit_account_id -> add t.debit_account_id k (Int64.neg (Analytics.amount t))
          | _ -> ())
      posted;
  let entries = List.sort compare (Hashtbl.fold (fun (a, k) v acc -> (Account_id.to_string a, Account_id.to_string k, a, k, v) :: acc) spent []) in
  let budgeted k = if scenario.follow_budgets then List.find_map (fun (c, b) -> if Account_id.equal c k then Some b else None) input.budgets else None in
  let total = List.fold_left (fun sum (_, _, _, _, v) -> sum +: v) 0L entries in
  let cycle_days = Cycle.day_count current in
  (* Less than a cycle of history is spread over a whole cycle: a few days that happen to hold the
     rent aren't the daily rate. *)
  let spread = max days cycle_days in
  let history_parts =
    List.map
      (fun (info : Account_info.t) ->
        let a = info.account.id in
        let own = List.fold_left (fun sum (_, _, a', k, v) -> if Account_id.equal a a' && Option.is_none (budgeted k) then sum +: v else sum) 0L entries in
        (a, own, spread))
      cash
  in
  let budget_parts =
    if not scenario.follow_budgets then []
    else
      List.concat_map
        (fun (k, budget) ->
          let shares = List.filter_map (fun (_, _, a, k', v) -> if Account_id.equal k k' then Some (a, v) else None) entries in
          let category_total = List.fold_left (fun sum (_, v) -> sum +: v) 0L shares in
          if Int64.compare category_total 0L > 0 then List.map (fun (a, v) -> (a, Int64.div (Int64.mul budget v) category_total, cycle_days)) shares
          else match main_account input cash with Some a -> [ (a, budget, cycle_days) ] | None -> [])
        input.budgets
  in
  { parts = List.filter (fun (_, _, d) -> d > 0) (history_parts @ budget_parts);
    per_cycle = (if days > 0 then Int64.div (Int64.mul total (Int64.of_int cycle_days)) (Int64.of_int spread) else 0L);
    first; last; days = max 0 days; short = days <= 0 || before c3.first first }

(* Spending on the [n]th day after today, in whole minor units that add up exactly. *)
let spent_on usual account n =
  List.fold_left
    (fun sum (a, total, days) ->
      if not (Account_id.equal a account) then sum
      else
        let upto n = Int64.div (Int64.mul total (Int64.of_int n)) (Int64.of_int days) in
        sum +: (upto n -: upto (n - 1)))
    0L usual.parts

let earmarks (input : input) ~last =
  let paydays = List.filter_map (fun (c : Cycle.t) -> if before input.today c.first && not (before last c.first) then Some c.first else None) (cycles_until input last) in
  let saved = Array.of_list (List.map (fun (g : goal) -> g.earmarked) input.goals) in
  let schedule =
    List.map
      (fun payday ->
        List.iteri
          (fun i (g : goal) -> match g.monthly with Some m -> saved.(i) <- saved.(i) +: Int64.max 0L (Int64.min m (g.target -: saved.(i))) | None -> ())
          input.goals;
        (payday, Array.fold_left ( +: ) 0L saved))
      paydays
  in
  let initial = List.fold_left (fun sum (g : goal) -> sum +: g.earmarked) 0L input.goals in
  fun date -> List.fold_left (fun current (payday, total) -> if before date payday then current else total) initial schedule

type run = { run_items : item list; points : point list; outlooks : outlook list; stopped : Series.t list; usual : usual }

let run (input : input) scenario =
  let cash = cash_accounts input in
  let is_cash id = List.exists (fun (info : Account_info.t) -> Account_id.equal info.account.id id) cash in
  let last = Analytics.add_months input.today input.horizon in
  let series = List.filter (fun (s : Series.t) -> is_cash s.account && not (List.mem s.id scenario.leave_out)) input.series in
  let household = List.filter (household_series input) series in
  let series = List.filter (fun s -> not (household_series input s)) series in
  let dated = List.map (fun s -> (s, series_dates input ~last s)) series in
  let stopped = List.filter_map (fun (s, dates) -> if Option.is_none dates then Some s else None) dated in
  let kind_of (s : Series.t) = match s.kind with Bill -> Bill | Income -> Income | Transfer -> Transfer in
  let series_items =
    List.concat_map
      (fun ((s : Series.t), dates) ->
        List.map (fun date -> { item_id = s.id; date; kind = kind_of s; name = s.name; amount = s.amount; account = s.account; series = Some s })
          (Option.value dates ~default:[]))
      dated
  in
  let main = main_account input cash in
  let usual_household = match household with s :: _ -> Some s | [] -> None in
  let contribution =
    match Option.fold ~none:main ~some:(fun (s : Series.t) -> Some s.account) usual_household with
    | Some account -> contribution_items input ~last ~account ~usual:usual_household
    | None -> []
  in
  let one_offs =
    match main with
    | Some account ->
        List.mapi
          (fun i (o : one_off) -> { item_id = Printf.sprintf "one_off:%d" i; date = o.date; kind = One_off; name = o.description; amount = o.amount; account; series = None })
          scenario.one_off
    | None -> []
  in
  let items =
    List.sort
      (fun (a : item) (b : item) -> match Date.compare a.date b.date with 0 -> String.compare a.item_id b.item_id | c -> c)
      (series_items @ contribution @ one_offs)
  in
  let deltas =
    List.concat_map
      (fun (item : item) ->
        (item.account, item.date, item.amount)
        :: (match item.series with Some s when is_cash s.counter -> [ (s.counter, item.date, Int64.neg item.amount) ] | _ -> []))
      items
  in
  let usual = usual_spending input cash scenario in
  let days = days_from input.today last in
  let lines =
    List.map
      (fun (info : Account_info.t) ->
        let id = info.account.id in
        let balance = ref (start_of info) in
        ( info,
          List.map
            (fun day ->
              List.iter (fun (a, d, v) -> if Account_id.equal a id && Date.equal d day then balance := !balance +: v) deltas;
              let n = Date.days_between ~first:input.today ~last:day in
              if n > 0 then balance := !balance -: spent_on usual id n;
              (day, !balance))
            days ))
      cash
  in
  let lowest line = List.fold_left (fun (ld, lv) (d, v) -> if Int64.compare v lv < 0 then (d, v) else (ld, lv)) (List.hd line) line in
  let outlooks =
    List.map
      (fun ((info : Account_info.t), line) ->
        { account = info; start = start_of info; lowest = lowest line;
          overdraw = List.find_map (fun (d, v) -> if Int64.compare v 0L < 0 then Some d else None) line })
      lines
  in
  let earmarked = earmarks input ~last in
  let points =
    List.mapi
      (fun i day -> { date = day; cash = List.fold_left (fun sum (_, line) -> sum +: snd (List.nth line i)) 0L lines; earmarked = earmarked day })
      days
  in
  { run_items = items; points; outlooks; stopped; usual }

(* Cash at the end of each day before today, worked back from today's posted balances. *)
let history (input : input) ~first =
  let cash = cash_accounts input in
  let is_cash id = List.exists (fun (info : Account_info.t) -> Account_id.equal info.account.id id) cash in
  let now = List.fold_left (fun sum (info : Account_info.t) -> sum +: Account.posted_balance info.account) 0L cash in
  let changes =
    List.filter_map
      (fun (t : Transfer.t) ->
        if not (Analytics.posted t) then None
        else
          let v = (if is_cash t.debit_account_id then Analytics.amount t else 0L) -: if is_cash t.credit_account_id then Analytics.amount t else 0L in
          if Int64.equal v 0L then None else Some (t.value_date, v))
      input.transfers
  in
  List.map
    (fun day -> (day, List.fold_left (fun cash (d, v) -> if before day d then cash -: v else cash) now changes))
    (days_from first (Date.add_days input.today (-1)))

let project (input : input) scenario =
  let baseline = run input no_scenario in
  let cash = cash_accounts input in
  let imported_through =
    List.fold_left
      (fun through (info : Account_info.t) ->
        match imported_for input info.account.id, through with
        | Some d, Some t -> Some (Date.min d t)
        | Some d, None -> Some d
        | None, through -> through)
      None cash
  in
  let other_currency =
    List.filter
      (fun (info : Account_info.t) ->
        info.account.kind = Asset && (not (Analytics.is_investment info)) && List.exists (Owner.equal info.account.owner) input.owners
        && not (Currency.equal info.account.currency input.book.currency))
      input.accounts
  in
  let usual = baseline.usual in
  let incoming = match input.contribution with Some (Incoming _) -> true | _ -> false in
  let assumptions =
    List.concat
      [ (match imported_through with Some d when before d (Date.add_days input.today (-7)) -> [ Stale_import d ] | _ -> []);
        (if usual.days > 0 then [ Usual_spending { per_cycle = usual.per_cycle; first = usual.first; last = usual.last; days = usual.days } ] else []);
        (if usual.short then [ Short_history usual.days ] else []);
        (if incoming || List.exists (fun (s : Series.t) -> s.kind = Income) input.series then [] else [ No_income_detected ]);
        (if other_currency = [] then [] else [ Other_currency other_currency ]);
        List.map (fun s -> Stopped s) baseline.stopped ]
  in
  let current = Cycle.cycle_of input.setting input.today in
  let rec back cycle n = if n <= 0 then cycle else back (Cycle.previous input.setting cycle) (n - 1) in
  let lowest = List.fold_left (fun (ld, lv) p -> if Int64.compare p.cash lv < 0 then (p.date, p.cash) else (ld, lv)) ((List.hd baseline.points).date, (List.hd baseline.points).cash) baseline.points in
  { today = input.today; imported_through; horizon_end = Analytics.add_months input.today input.horizon;
    history = history input ~first:(back current (min input.horizon 3)).first; baseline = baseline.points;
    scenario = (if scenario = no_scenario then None else Some (run input scenario).points); lowest; accounts = baseline.outlooks;
    items = baseline.run_items; assumptions }
