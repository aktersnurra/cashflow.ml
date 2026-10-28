(* Analytics as pure functions over accounts and transfers. A scope is a set of owners: an account
   is inside when its owner is in the set and it is in the reported currency. A transfer between an
   inside and an outside account is money moving between books (a contribution to the household,
   a settlement), never income or spending. *)

type scope = Me | Household | Family

let scope_to_string = function Me -> "me" | Household -> "household" | Family -> "family"
let scope_of_string = function "me" -> Some Me | "household" -> Some Household | "family" -> Some Family | _ -> None

type book = {
  inside : Account_id.t -> Account_info.t option;  (** in scope and in the reported currency *)
  currency : Currency.t;
}

let make_book ~(accounts : Account_info.t list) ~owners ~currency =
  let table = Hashtbl.create 64 in
  List.iter
    (fun (info : Account_info.t) ->
      if List.exists (Owner.equal info.account.owner) owners && Currency.equal info.account.currency currency then
        Hashtbl.replace table info.account.id info)
    accounts;
  { inside = Hashtbl.find_opt table; currency }

let posted (transfer : Transfer.t) = match transfer.kind with Single | Post_pending _ -> true | Pending _ | Void_pending _ -> false
let amount (transfer : Transfer.t) = Amount.to_int64 transfer.amount
let in_range ~first ~last (transfer : Transfer.t) = Date.compare first transfer.value_date <= 0 && Date.compare transfer.value_date last <= 0
let is kind (info : Account_info.t option) = match info with Some i -> i.account.kind = kind | None -> false
let balance_sheet (info : Account_info.t option) = is Asset info || is Liability info

type flows = { income : int64; spending : int64; transfers_in : int64; transfers_out : int64 }

let zero_flows = { income = 0L; spending = 0L; transfers_in = 0L; transfers_out = 0L }
let ( +: ) = Int64.add
let ( -: ) = Int64.sub

(* How one posted transfer changes the scope's flows. Debiting an expense is spending, crediting
   one (a refund, a reversal, a reclassification away) takes it back; income mirrors that. *)
let flow book flows (transfer : Transfer.t) =
  let d = book.inside transfer.debit_account_id and c = book.inside transfer.credit_account_id in
  let a = amount transfer in
  let flows = if is Expense d then { flows with spending = flows.spending +: a } else flows in
  let flows = if is Expense c then { flows with spending = flows.spending -: a } else flows in
  let flows = if is Income c then { flows with income = flows.income +: a } else flows in
  let flows = if is Income d then { flows with income = flows.income -: a } else flows in
  match d, c with
  | Some _, None when balance_sheet d -> { flows with transfers_in = flows.transfers_in +: a }
  | None, Some _ when balance_sheet c -> { flows with transfers_out = flows.transfers_out +: a }
  (* A move between a member and the household that came in as two imported rows, paired: each
     bank side was imported against a suspense account in its own scope, and this entry clears
     the two across the boundary, so it is the contribution. Nothing else crosses the boundary
     between such accounts, so a reversal that does is one of these undone: the move back. *)
  | Some _, None when transfer.code = Contribution || transfer.code = Reversal -> { flows with transfers_in = flows.transfers_in +: a }
  | None, Some _ when transfer.code = Contribution || transfer.code = Reversal -> { flows with transfers_out = flows.transfers_out +: a }
  | _ -> flows

let flows_between book ~first ~last transfers =
  List.fold_left (fun flows t -> if posted t && in_range ~first ~last t then flow book flows t else flows) zero_flows transfers

let savings flows = flows.income -: flows.spending +: flows.transfers_in -: flows.transfers_out

(* Basis points, so the rate is exact; [None] without income, where a rate means nothing. *)
let savings_rate_bp flows =
  if Int64.compare flows.income 0L <= 0 then None else Some (Int64.to_int (Int64.div (Int64.mul (savings flows) 10_000L) flows.income))

(* [complete]: the statements cover the whole cycle. A cycle that isn't complete has figures only
   through [covered_through], and comparing them with whole cycles says nothing: right after
   payday the salary is in and the bills aren't. *)
type cycle_summary = {
  cycle : Cycle.t;
  flows : flows;
  savings : int64;
  savings_rate_bp : int option;
  reconciled : bool;
  covered_through : Date.t option;
  complete : bool;
}

(* How far the books are known: the earliest latest-imported day among the scope's bank accounts and
   cards that statements feed (and have fed), never after today. Anything later is not imported
   yet, not zero. [None] when no statement has been imported: then only today bounds it. *)
let covered_through ~(accounts : Account_info.t list) ~owners ~(imported_through : (Account_id.t * Date.t) list) ~today =
  let fed =
    List.filter_map
      (fun (info : Account_info.t) ->
        if info.details.statement_backed && (not info.account.closed) && List.exists (Owner.equal info.account.owner) owners
           && (info.account.kind = Asset || info.account.kind = Liability)
        then List.find_map (fun (id, d) -> if Account_id.equal id info.account.id then Some d else None) imported_through
        else None)
      accounts
  in
  match fed with [] -> None | d :: rest -> Some (Date.min today (List.fold_left Date.min d rest))

let cashflow_summary ?(covered_through : Date.t option) book ~cycles ~reconciled_through transfers =
  List.map
    (fun (cycle : Cycle.t) ->
      let flows = flows_between book ~first:cycle.first ~last:cycle.last transfers in
      let reconciled = match reconciled_through with Some date -> Date.compare cycle.last date <= 0 | None -> false in
      let complete = match covered_through with Some date -> Date.compare cycle.last date <= 0 | None -> true in
      { cycle; flows; savings = savings flows; savings_rate_bp = savings_rate_bp flows; reconciled; covered_through; complete })
    cycles

(* Net amount per inside account of one kind over a range, largest first. *)
let totals_by_account book kind ~first ~last transfers =
  let table = Hashtbl.create 32 in
  let add id delta =
    match book.inside id with
    | Some info when info.account.kind = kind ->
        Hashtbl.replace table id (info, Int64.add delta (Option.fold ~none:0L ~some:snd (Hashtbl.find_opt table id)))
    | _ -> ()
  in
  List.iter
    (fun (t : Transfer.t) ->
      if posted t && in_range ~first ~last t then (
        let a = amount t in
        let sign = if Account.debit_normal kind then 1L else -1L in
        add t.debit_account_id (Int64.mul sign a);
        add t.credit_account_id (Int64.mul (Int64.neg sign) a)))
    transfers;
  Hashtbl.fold (fun _ entry acc -> entry :: acc) table [] |> List.sort (fun (_, a) (_, b) -> Int64.compare b a)

type category_total = { category : Account_info.t; total : int64; previous : int64 option }

let spending_by_category book ~first ~last ?compare_to transfers =
  let current = totals_by_account book Expense ~first ~last transfers in
  let previous =
    Option.map (fun (first, last) -> totals_by_account book Expense ~first ~last transfers) compare_to
  in
  List.map
    (fun ((info : Account_info.t), total) ->
      let previous =
        Option.map
          (fun previous ->
            Option.fold ~none:0L ~some:snd
              (List.find_opt (fun ((i : Account_info.t), _) -> Account_id.equal i.account.id info.account.id) previous))
          previous
      in
      { category = info; total; previous })
    current

let category_trend book category ~cycles transfers =
  let sign = match book.inside category with Some info when not (Account.debit_normal info.account.kind) -> -1L | _ -> 1L in
  List.map
    (fun (cycle : Cycle.t) ->
      let total =
        List.fold_left
          (fun total (t : Transfer.t) ->
            if not (posted t && in_range ~first:cycle.first ~last:cycle.last t) then total
            else if Account_id.equal t.debit_account_id category then total +: amount t
            else if Account_id.equal t.credit_account_id category then total -: amount t
            else total)
          0L transfers
      in
      (cycle, Int64.mul sign total))
    cycles

(* How much an account's balance moved on a day: debits minus credits. *)
type balance_change = { account_id : Account_id.t; on : Date.t; change : int64 }

let balance_changes transfers =
  List.concat_map
    (fun (t : Transfer.t) ->
      if posted t then
        [ { account_id = t.debit_account_id; on = t.value_date; change = amount t };
          { account_id = t.credit_account_id; on = t.value_date; change = Int64.neg (amount t) } ]
      else [])
    transfers

(* Assets minus liabilities after each date, from every balance change up to it. *)
let net_worth_of_changes book ~dates changes =
  let dates = List.sort Date.compare dates in
  (* Both kinds count debit minus credit: a liability's balance is negative worth, so a
     debit (paying it down) raises net worth and a credit (borrowing) lowers it. *)
  let counted =
    List.filter (fun c -> match book.inside c.account_id with Some info -> info.account.kind = Asset || info.account.kind = Liability | None -> false) changes
    |> List.sort (fun a b -> Date.compare a.on b.on)
  in
  let rec go dates changes worth acc =
    match dates with
    | [] -> List.rev acc
    | date :: rest -> (
        match changes with
        | c :: more when Date.compare c.on date <= 0 -> go dates more (worth +: c.change) acc
        | _ -> go rest changes worth ((date, worth) :: acc))
  in
  go dates counted 0L []

let net_worth_series book ~dates transfers = net_worth_of_changes book ~dates (balance_changes transfers)

(* Spending transfers, largest first: those debiting an inside expense account. *)
let largest_expenses book ~first ~last ~limit transfers =
  transfers
  |> List.filter (fun (t : Transfer.t) -> posted t && in_range ~first ~last t && is Expense (book.inside t.debit_account_id) && t.code <> Reclassify)
  |> List.sort (fun a b -> Int64.compare (amount b) (amount a))
  |> List.filteri (fun i _ -> i < limit)

let is_investment (info : Account_info.t) = List.mem "investment" info.details.tags

(* Liquid assets: inside asset accounts not tagged as investments. *)
let liquid_assets book (accounts : Account_info.t list) =
  List.fold_left
    (fun total (info : Account_info.t) ->
      match book.inside info.account.id with
      | Some info when info.account.kind = Asset && not (is_investment info) -> total +: Account.posted_balance info.account
      | _ -> total)
    0L accounts

type runway = { liquid : int64; average_monthly_spending : int64; months : float option }

let runway book accounts ~cycles transfers =
  let liquid = liquid_assets book accounts in
  let spent = List.map (fun (c : Cycle.t) -> (flows_between book ~first:c.first ~last:c.last transfers).spending) cycles in
  let average = match spent with [] -> 0L | _ -> Int64.div (List.fold_left Int64.add 0L spent) (Int64.of_int (List.length spent)) in
  { liquid; average_monthly_spending = average;
    months = (if Int64.compare average 0L > 0 then Some (Int64.to_float liquid /. Int64.to_float average) else None) }

(* Recurring payments: the same merchant charged at a regular interval. *)
type cadence = Weekly | Monthly | Quarterly | Yearly

let cadence_to_string = function Weekly -> "weekly" | Monthly -> "monthly" | Quarterly -> "quarterly" | Yearly -> "yearly"

type recurring = {
  merchant : string;  (** normalised, for grouping and keys *)
  merchant_name : string;  (** as the latest charge wrote it, for display *)
  cadence : cadence;
  typical_amount : int64;
  last_amount : int64;
  previous_amount : int64;
  first_date : Date.t;
  last_date : Date.t;
  next_expected : Date.t;
  occurrences : int;
  category : Account_info.t option;
  paid_from : Account_id.t;  (** the account the latest charge came out of *)
}

let merchant_key (t : Transfer.t) =
  Merchant.key ~description:t.memo.description ~counterparty:t.memo.counterparty

(* The same day [months] later, clamped to the end of a shorter month. *)
let add_months date months =
  let year, month, day = Date.to_ymd date in
  let year, month = Cycle.shift_month year month months in
  Option.get (Date.of_ymd year month (min day (Date.days_in_month year month)))

let median compare values =
  let sorted = List.sort compare values in
  List.nth sorted (List.length sorted / 2)

let cadence_of_interval days =
  if days >= 6 && days <= 8 then Some (Weekly, 7)
  else if days >= 26 && days <= 35 then Some (Monthly, 30)
  else if days >= 85 && days <= 95 then Some (Quarterly, 91)
  else if days >= 355 && days <= 375 then Some (Yearly, 365)
  else None

(* Payments at a regular interval, the test every recurring series passes: at least three, and a
   usual interval that matches a cadence. A [steady] series also costs about the same each time;
   regular visits to a shop or café don't. At least two thirds of the payments must be within 25%
   of the typical amount, which still lets a price change or a varying utility bill through.
   Payments are put oldest first, same-day ones by id, so the order they came in doesn't matter. *)
type regular = { payments : Transfer.t list; regular_cadence : cadence; typical : int64; next : Date.t }

let next_after cadence date =
  match cadence with
  | Monthly -> add_months date 1
  | Quarterly -> add_months date 3
  | Yearly -> add_months date 12
  | Weekly -> Date.add_days date 7

let regular ~steady payments =
  let payments =
    List.sort
      (fun (a : Transfer.t) (b : Transfer.t) -> match Date.compare a.value_date b.value_date with 0 -> Transfer_id.compare a.id b.id | c -> c)
      payments
  in
  let count = List.length payments in
  if count < 3 then None
  else
    let intervals =
      List.filteri (fun i _ -> i > 0) payments
      |> List.mapi (fun i (t : Transfer.t) -> Date.days_between ~first:(List.nth payments i).value_date ~last:t.value_date)
    in
    let typical = median Int64.compare (List.map amount payments) in
    let near_typical (t : Transfer.t) = Int64.compare (Int64.mul (Int64.abs (Int64.sub (amount t) typical)) 4L) (Int64.abs typical) <= 0 in
    let close = List.length (List.filter near_typical payments) in
    match cadence_of_interval (median Int.compare intervals) with
    | None -> None
    | Some _ when steady && close * 3 < count * 2 -> None
    | Some (cadence, _) -> Some { payments; regular_cadence = cadence; typical; next = next_after cadence (List.nth payments (count - 1)).value_date }

let recurring_payments book transfers =
  let groups = Hashtbl.create 32 in
  List.iter
    (fun (t : Transfer.t) ->
      if posted t && t.code <> Reclassify && t.code <> Reversal && is Expense (book.inside t.debit_account_id) then
        let key = merchant_key t in
        if key <> "" then Hashtbl.replace groups key (t :: Option.value (Hashtbl.find_opt groups key) ~default:[]))
    transfers;
  Hashtbl.fold
    (fun merchant charges acc ->
      match regular ~steady:true charges with
      | None -> acc
      | Some { payments = charges; regular_cadence = cadence; typical; next } ->
          let count = List.length charges in
          let last = List.nth charges (count - 1) and previous = List.nth charges (count - 2) in
          let merchant_name = String.trim (match last.memo.counterparty with Some c when String.trim c <> "" -> c | _ -> last.memo.description) in
          { merchant; merchant_name; cadence; typical_amount = typical; last_amount = amount last; previous_amount = amount previous;
            first_date = (List.hd charges).value_date; last_date = last.value_date; next_expected = next; occurrences = count;
            category = book.inside last.debit_account_id; paid_from = last.credit_account_id }
          :: acc)
    groups []
  |> List.sort (fun a b -> Int64.compare b.typical_amount a.typical_amount)

(* A price change: the latest charge differs from the one before by more than 2%. *)
let price_changed r =
  let diff = Int64.abs (Int64.sub r.last_amount r.previous_amount) in
  Int64.compare (Int64.mul diff 50L) r.previous_amount > 0

(* Spending that stands out: a category already well past what a whole cycle usually costs, in
   [history] (complete cycles), by at least [threshold]. Comparing the cycle so far with whole
   cycles keeps it quiet early in a cycle, when partial figures are noisy. A category needs to
   appear in two of the history cycles to have a usual. *)
type unusual = { unusual_category : Account_info.t; spent_so_far : int64; usual : int64 }

let unusual_spending book ~(current : Cycle.t) ~today ~(history : Cycle.t list) ~threshold transfers =
  let totals first last = totals_by_account book Expense ~first ~last transfers in
  let past = List.map (fun (c : Cycle.t) -> totals c.first c.last) history in
  let total_in totals id = Option.map snd (List.find_opt (fun ((i : Account_info.t), _) -> Account_id.equal i.account.id id) totals) in
  List.filter_map
    (fun ((info : Account_info.t), spent) ->
      let seen = List.filter_map (fun totals -> total_in totals info.account.id) past in
      if List.length seen < 2 then None
      else
        let usual = Int64.div (List.fold_left Int64.add 0L seen) (Int64.of_int (List.length history)) in
        if Int64.compare (Int64.mul spent 2L) (Int64.mul usual 3L) > 0 && Int64.compare (spent -: usual) threshold >= 0 then
          Some { unusual_category = info; spent_so_far = spent; usual }
        else None)
    (totals current.first (Date.min today current.last))

(* An account the recurring payments still due before [until] would take below zero. Payments
   expected up to a few days ago count too: the charge may simply not be imported yet. *)
type shortfall = { short_account : Account_info.t; available : int64; due : int64; upcoming : recurring list }

let shortfalls book ~today ~until recurring =
  let due_soon r = Date.compare (Date.add_days today (-5)) r.next_expected <= 0 && Date.compare r.next_expected until <= 0 in
  let by_account = Hashtbl.create 8 in
  List.iter
    (fun r -> if due_soon r then Hashtbl.replace by_account r.paid_from (r :: Option.value (Hashtbl.find_opt by_account r.paid_from) ~default:[]))
    recurring;
  Hashtbl.fold
    (fun account_id upcoming acc ->
      match book.inside account_id with
      | Some info when info.account.kind = Asset && not (is_investment info) ->
          let b = info.account.balances in
          (* What is left once the pending card reservations go through. *)
          let available = b.debits_posted -: b.credits_posted -: b.credits_pending in
          let due = List.fold_left (fun sum r -> sum +: r.typical_amount) 0L upcoming in
          if Int64.compare due available > 0 then
            { short_account = info; available; due; upcoming = List.sort (fun a b -> Date.compare a.next_expected b.next_expected) upcoming } :: acc
          else acc
      | _ -> acc)
    by_account []

(* Budgets *)

type budget = { category_id : Account_id.t; monthly_amount : int64; rollover : bool }

type budget_line = {
  line_category : Account_info.t;
  budgeted : int64;
  rollover : bool;
  carried : int64;  (** rolled over from earlier cycles: positive when underspent *)
  spent : int64;
  expected_by_now : int64;
  status : [ `On_pace | `Ahead_of_pace | `Over_budget ];
}

let spent_in book category ~(cycle : Cycle.t) transfers =
  List.fold_left
    (fun total (t : Transfer.t) ->
      if not (posted t && in_range ~first:cycle.first ~last:cycle.last t) then total
      else if Account_id.equal t.debit_account_id category then total +: amount t
      else if Account_id.equal t.credit_account_id category then total -: amount t
      else total)
    0L transfers
  |> fun total ->
  ignore book;
  total

(* [budgets] are the versions in force in [cycle]; [budget_for earlier category] finds the one in
   force in an earlier cycle. Rollover carries the
   difference between budget and spending forward from up to twelve earlier cycles. *)
let budget_status book ~(cycle : Cycle.t) ~budgets ~previous_cycles ~today ~budget_for transfers =
  let elapsed =
    if Date.compare today cycle.first < 0 then 0
    else if Date.compare today cycle.last > 0 then Cycle.day_count cycle
    else Date.days_between ~first:cycle.first ~last:today + 1
  in
  List.filter_map
    (fun (budget : budget) ->
      match book.inside budget.category_id with
      | None -> None
      | Some category ->
          let carried =
            if not budget.rollover then 0L
            else
              List.fold_left
                (fun carried (earlier : Cycle.t) ->
                  match budget_for earlier budget.category_id with
                  | Some (b : budget) when b.rollover -> carried +: (b.monthly_amount -: spent_in book budget.category_id ~cycle:earlier transfers)
                  | _ -> 0L)
                0L previous_cycles
          in
          let available = budget.monthly_amount +: carried in
          let spent = spent_in book budget.category_id ~cycle transfers in
          let expected = Int64.div (Int64.mul available (Int64.of_int elapsed)) (Int64.of_int (Cycle.day_count cycle)) in
          let status =
            if Int64.compare spent available > 0 then `Over_budget
            else if Int64.compare (Int64.mul spent 100L) (Int64.mul expected 115L) > 0 then `Ahead_of_pace
            else `On_pace
          in
          Some { line_category = category; budgeted = budget.monthly_amount; rollover = budget.rollover; carried; spent; expected_by_now = expected; status })
    budgets

(* Contribution planning: split a target in proportion to income. Rounded to whole currency
   units, with the remainder on the highest earner, so the parts add up to the target. *)
let plan_contributions ~unit ~target incomes =
  let total = List.fold_left (fun total (_, income) -> Int64.add total (Int64.max 0L income)) 0L incomes in
  if Int64.compare total 0L <= 0 || incomes = [] then None
  else
    let round_down value = Int64.mul (Int64.div value unit) unit in
    let shares =
      List.map
        (fun (member, income) ->
          (* target * income / total, computed in floats only for the ratio's product to avoid overflow. *)
          let share = Int64.of_float (Int64.to_float target *. (Int64.to_float (Int64.max 0L income) /. Int64.to_float total)) in
          (member, income, round_down share))
        incomes
    in
    let assigned = List.fold_left (fun sum (_, _, share) -> Int64.add sum share) 0L shares in
    let remainder = Int64.sub target assigned in
    let top, _, _ = List.fold_left (fun (bm, bi, bs) (m, i, s) -> if Int64.compare i bi > 0 then (m, i, s) else (bm, bi, bs)) (List.hd shares) shares in
    Some (List.map (fun (member, income, share) -> (member, income, if member = top then Int64.add share remainder else share)) shares)
