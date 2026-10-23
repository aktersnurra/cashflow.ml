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
  | Range_from of { forecasts : int; since : Date.t }
  | No_range_yet
  | Seasonal of { month : int; year : int; percent : int }

type t = {
  today : Date.t;
  imported_through : Date.t option;
  horizon_end : Date.t;
  history : (Date.t * int64) list;
  baseline : point list;
  scenario : point list option;
  lowest : Date.t * int64;
  range : (Date.t * int64 * int64) list;
  chance_positive : int option;
  accounts : outlook list;
  items : item list;
  assumptions : assumption list;
}

let before a b = Date.compare a b < 0
let rec days_from first last = if before last first then [] else first :: days_from (Date.add_days first 1) last

let cash_accounts (input : input) =
  List.filter_map
    (fun (info : Account_info.t) ->
      (* The book says which accounts are in scope; the balances are the input's, which differ from
         the book's when the backtest starts in the past. *)
      match input.book.inside info.account.id with
      | Some _ when info.account.kind = Asset && not (Analytics.is_investment info) -> Some info
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

(* What one transfer spent from a cash account, net of refunds, unless a bill series covers it:
   (cash account, category, amount). *)
let spending (input : input) ~is_cash ~bills (t : Transfer.t) =
  let expense id = match input.book.inside id with Some info when info.account.kind = Expense -> Some info.account.id | _ -> None in
  if List.mem ("bill:" ^ Analytics.merchant_key t) bills then None
  else
    match expense t.debit_account_id, expense t.credit_account_id with
    | Some k, None when is_cash t.credit_account_id -> Some (t.credit_account_id, k, Analytics.amount t)
    | None, Some k when is_cash t.debit_account_id -> Some (t.debit_account_id, k, Int64.neg (Analytics.amount t))
    | _ -> None

(* Seasons: with more than a year of history, how each calendar month of the last twelve compared
   with their average, as a factor in basis points, shrunk halfway towards normal (one past
   December is one sample) and kept between half and double. *)
let seasons (input : input) ~data_start ~spent_in =
  match data_start with
  | Some start when not (before (Date.add_days input.today (-395)) start) ->
      let y, m, _ = Date.to_ymd input.today in
      let months = List.init 12 (fun i -> Cycle.shift_month y m (i - 12)) in
      let totals = List.map (fun ym -> (ym, spent_in ym)) months in
      let average = Int64.div (List.fold_left (fun sum (_, v) -> sum +: v) 0L totals) 12L in
      if Int64.compare average 0L <= 0 then []
      else
        List.map
          (fun (ym, total) ->
            let ratio = Int64.to_int (Int64.div (Int64.mul total 10_000L) average) in
            (ym, max 5_000 (min 20_000 (10_000 + ((ratio - 10_000) / 2)))))
          totals
  | _ -> []

let weight seasons date =
  let _, month, _ = Date.to_ymd date in
  Option.value (List.find_map (fun ((_, m), bp) -> if m = month then Some bp else None) seasons) ~default:10_000

(* Usual spending: what each cash account paid for in the last three complete cycles, less the
   bills the series already cover, spread over the days ahead. A part is (account, total, over,
   seasonal): [total] over [over] basis-point days, spread by the season's weight when
   [seasonal], else evenly. *)
type usual = {
  parts : (Account_id.t * int64 * int64 * bool) list;
  cumulative : int64 array;  (** the season's weight of the first n days after today *)
  seasons : ((int * int) * int) list;
  per_cycle : int64;
  first : Date.t;
  last : Date.t;
  days : int;
  short : bool;
}

let usual_spending (input : input) cash scenario ~horizon_end =
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
  let spending = spending input ~is_cash ~bills in
  let spent_in (y, m) =
    List.fold_left
      (fun sum (t : Transfer.t) ->
        let ty, tm, _ = Date.to_ymd t.value_date in
        if ty = y && tm = m then match spending t with Some (_, _, v) -> sum +: v | None -> sum else sum)
      0L posted
  in
  let seasons = seasons input ~data_start ~spent_in in
  let spent = Hashtbl.create 16 in
  let add a k v = Hashtbl.replace spent (a, k) (v +: Option.value (Hashtbl.find_opt spent (a, k)) ~default:0L) in
  if days > 0 then
    List.iter
      (fun (t : Transfer.t) ->
        if (not (before t.value_date first)) && not (before last t.value_date) then
          match spending t with Some (a, k, v) -> add a k v | None -> ())
      posted;
  let entries = List.sort compare (Hashtbl.fold (fun (a, k) v acc -> (Account_id.to_string a, Account_id.to_string k, a, k, v) :: acc) spent []) in
  let budgeted k = if scenario.follow_budgets then List.find_map (fun (c, b) -> if Account_id.equal c k then Some b else None) input.budgets else None in
  let total = List.fold_left (fun sum (_, _, _, _, v) -> sum +: v) 0L entries in
  let cycle_days = Cycle.day_count current in
  (* The history's own season is taken out before the coming months' is put in. Less than a
     cycle of history is spread over a whole cycle: a few days that happen to hold the rent aren't
     the daily rate. *)
  let window = if days > 0 then List.fold_left (fun sum d -> sum + weight seasons d) 0 (days_from first last) else 0 in
  let over = Int64.of_int (max window (cycle_days * 10_000)) in
  let history_parts =
    List.map
      (fun (info : Account_info.t) ->
        let a = info.account.id in
        let own = List.fold_left (fun sum (_, _, a', k, v) -> if Account_id.equal a a' && Option.is_none (budgeted k) then sum +: v else sum) 0L entries in
        (a, own, over, true))
      cash
  in
  let flat = Int64.of_int (cycle_days * 10_000) in
  (* What the bills still scheduled in a category cost a cycle: keeping to its budget leaves only
     the rest to spend, since the bills are already items. *)
  let scheduled k =
    List.fold_left
      (fun sum (s : Series.t) ->
        match s.kind, s.category with
        | Bill, Some c when Account_id.equal c.account.id k && not (List.mem s.id scenario.leave_out) ->
            let a = Int64.abs s.amount in
            sum
            +:
            (match s.cadence with
            | Monthly -> a
            | Weekly -> Int64.div (Int64.mul a (Int64.of_int cycle_days)) 7L
            | Quarterly -> Int64.div a 3L
            | Yearly -> Int64.div a 12L)
        | _ -> sum)
      0L input.series
  in
  let budget_parts =
    if not scenario.follow_budgets then []
    else
      List.concat_map
        (fun (k, budget) ->
          let budget = Int64.max 0L (budget -: scheduled k) in
          let shares = List.filter_map (fun (_, _, a, k', v) -> if Account_id.equal k k' then Some (a, v) else None) entries in
          let category_total = List.fold_left (fun sum (_, v) -> sum +: v) 0L shares in
          if Int64.compare category_total 0L > 0 then List.map (fun (a, v) -> (a, Int64.div (Int64.mul budget v) category_total, flat, false)) shares
          else match main_account input cash with Some a -> [ (a, budget, flat, false) ] | None -> [])
        input.budgets
  in
  let ahead = max 0 (Date.days_between ~first:input.today ~last:horizon_end) in
  let cumulative = Array.make (ahead + 1) 0L in
  for n = 1 to ahead do
    cumulative.(n) <- cumulative.(n - 1) +: Int64.of_int (weight seasons (Date.add_days input.today n))
  done;
  { parts = (if days > 0 then history_parts else []) @ budget_parts; cumulative; seasons;
    per_cycle = (if days > 0 then Int64.div (Int64.mul total flat) over else 0L);
    first; last; days = max 0 days; short = days <= 0 || before c3.first first }

(* Spending on the [n]th day after today, in whole minor units that add up exactly. *)
let spent_on usual account n =
  List.fold_left
    (fun sum (a, total, over, seasonal) ->
      if not (Account_id.equal a account) then sum
      else
        let weight n = if seasonal then usual.cumulative.(n) else Int64.of_int (n * 10_000) in
        let upto n = Int64.div (Int64.mul total (weight n)) over in
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
  let usual = usual_spending input cash scenario ~horizon_end:last in
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

(* The range: the same projection made from a start every week over the last year, as it would
   have been made that morning (the balances at the end of the day before, only what was known by
   then, imports complete, no plan, goals or what-if), compared with the cash that followed. A
   start needs 100 days of history before it. The range for a day ahead is the baseline plus the
   10th and 90th percentile of the errors at that distance, as far as there are 10 of them; the
   chance of staying above zero is the share of whole-horizon error paths that, added to the
   baseline, never go below zero, given 8 of them. *)
let backtest (input : input) ~(baseline : point list) =
  let cash = cash_accounts input in
  let is_cash id = List.exists (fun (info : Account_info.t) -> Account_id.equal info.account.id id) cash in
  let posted = List.filter Analytics.posted input.transfers in
  let changes =
    List.filter_map
      (fun (t : Transfer.t) ->
        let v = Analytics.amount t in
        match is_cash t.debit_account_id, is_cash t.credit_account_id with
        | true, true -> None
        | true, false -> Some (t.value_date, [ (t.debit_account_id, v) ])
        | false, true -> Some (t.value_date, [ (t.credit_account_id, Int64.neg v) ])
        | false, false -> None)
      posted
  in
  (* Cash per account at the end of [day]: today's posted balance less what came after. *)
  let balance_after (info : Account_info.t) day =
    List.fold_left
      (fun bal (d, moves) ->
        if before day d then List.fold_left (fun bal (a, v) -> if Account_id.equal a info.account.id then bal -: v else bal) bal moves else bal)
      (Account.posted_balance info.account) changes
  in
  let data_start = List.fold_left (fun earliest (d, _) -> Some (match earliest with Some e -> Date.min e d | None -> d)) None changes in
  let starts =
    match data_start with
    | None -> []
    | Some start ->
        List.filter (fun s -> not (before s (Date.add_days start 100))) (List.init 52 (fun k -> Date.add_days input.today (-7 * (k + 1))))
  in
  let actual = Hashtbl.create 512 in
  let total day = List.fold_left (fun sum info -> sum +: balance_after info day) 0L cash in
  let actual_on day =
    match Hashtbl.find_opt actual day with
    | Some v -> v
    | None -> let v = total day in Hashtbl.replace actual day v; v
  in
  let ahead = List.length baseline - 1 in
  let paths =
    List.map
      (fun s ->
        let the_day_before = Date.add_days s (-1) in
        let accounts =
          List.map
            (fun (info : Account_info.t) ->
              if not (is_cash info.account.id) then info
              else
                let b = balance_after info the_day_before in
                let balances =
                  if Int64.compare b 0L >= 0 then { Account.zero_balances with debits_posted = b } else { Account.zero_balances with credits_posted = Int64.neg b }
                in
                { info with account = { info.account with balances } })
            input.accounts
        in
        let known = List.filter (fun (t : Transfer.t) -> before t.value_date s) input.transfers in
        let recent = List.filter (fun (t : Transfer.t) -> not (before t.value_date (Date.add_days s (-400)))) known in
        let past =
          { input with today = s; accounts; transfers = known; series = Series.detect input.book ~cash:is_cash ~setting:input.setting recent;
            imported_through = List.map (fun (info : Account_info.t) -> (info.account.id, the_day_before)) cash; contribution = None; goals = [];
            budgets = [] }
        in
        let run = run past no_scenario in
        List.filteri (fun d _ -> d <= ahead) run.points
        |> List.filter_map (fun (p : point) -> if before p.date input.today then Some (actual_on p.date -: p.cash) else None))
      starts
  in
  let errors_at d = List.filter_map (fun path -> List.nth_opt path d) paths in
  let quantile sorted q = sorted.(int_of_float (Float.round (q *. float_of_int (Array.length sorted - 1)))) in
  let rec range d = function
    | [] -> []
    | (p : point) :: rest -> (
        match errors_at d with
        | errors when List.length errors >= 10 ->
            let sorted = Array.of_list (List.sort Int64.compare errors) in
            (p.date, p.cash +: quantile sorted 0.1, p.cash +: quantile sorted 0.9) :: range (d + 1) rest
        | _ -> [])
  in
  let range = range 0 baseline in
  let whole = List.filter (fun path -> List.length path > ahead) paths in
  let chance_positive =
    if List.length whole < 8 then None
    else
      let stays path = List.for_all2 (fun (p : point) e -> Int64.compare (p.cash +: e) 0L >= 0) baseline (List.filteri (fun d _ -> d <= ahead) path) in
      Some (int_of_float (Float.round (100. *. float_of_int (List.length (List.filter stays whole)) /. float_of_int (List.length whole))))
  in
  let assumption =
    match range, List.rev starts with
    | [], _ | _, [] -> No_range_yet
    | _, earliest :: _ -> Range_from { forecasts = List.length starts; since = earliest }
  in
  (range, chance_positive, assumption)

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
  let horizon_end = Analytics.add_months input.today input.horizon in
  let months_ahead =
    List.sort_uniq compare (List.map (fun d -> let _, m, _ = Date.to_ymd d in m) (days_from (Date.add_days input.today 1) horizon_end))
  in
  let seasonal =
    List.filter_map
      (fun month ->
        List.find_map
          (fun ((year, m), bp) -> if m = month && abs (bp - 10_000) >= 1_000 then Some (Seasonal { month; year; percent = (bp - 10_000) / 100 }) else None)
          usual.seasons)
      months_ahead
  in
  let range, chance_positive, range_assumption = backtest input ~baseline:baseline.points in
  let assumptions = assumptions @ seasonal @ [ range_assumption ] in
  let current = Cycle.cycle_of input.setting input.today in
  let rec back cycle n = if n <= 0 then cycle else back (Cycle.previous input.setting cycle) (n - 1) in
  let lowest = List.fold_left (fun (ld, lv) p -> if Int64.compare p.cash lv < 0 then (p.date, p.cash) else (ld, lv)) ((List.hd baseline.points).date, (List.hd baseline.points).cash) baseline.points in
  { today = input.today; imported_through; horizon_end;
    history = history input ~first:(back current (min input.horizon 3)).first; baseline = baseline.points;
    scenario = (if scenario = no_scenario then None else Some (run input scenario).points); lowest; range; chance_positive; accounts = baseline.outlooks;
    items = baseline.run_items; assumptions }
