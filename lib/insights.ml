(* Analytics for a signed-in user: resolves the scope's owners, currency and pay cycle, loads the
   transfers they may see, and runs the pure functions in Analytics. *)
let ( let* ) = Result.bind

type ctx = {
  service : Service.t;
  access : Access.t;
  scope : Analytics.scope;
  owners : Owner.t list;
  currency : Currency.t;
  setting : Pay_cycle_setting.t;
  today : Date.t;
}

let context (service : Service.t) access ~scope ?currency () =
  let user = Access.user access in
  let household = Access.household access in
  let* owners =
    match scope, household with
    | Analytics.Me, _ -> Ok [ Access.personal access ]
    | (Household | Family), None -> Error (`Invalid "you don't belong to a household, so use scope \"me\"")
    | Household, Some h -> Ok [ Owner.Household h.id ]
    | Family, Some h -> Ok [ Access.personal access; Owner.Household h.id ]
  in
  let setting, default_currency =
    match scope, household with
    | Analytics.Me, _ | _, None -> (user.pay_cycle, user.default_currency)
    | (Household | Family), Some h -> (h.pay_cycle, h.default_currency)
  in
  let* currency = Service.currency service (Option.value currency ~default:default_currency) in
  Ok { service; access; scope; owners; currency; setting; today = Service.today service access }

let accounts ctx = Service.accounts ctx.service ctx.access
let book ctx = Analytics.make_book ~accounts:(accounts ctx) ~owners:ctx.owners ~currency:ctx.currency
let transfers ctx ~first ~last = Transfer_query.between ctx.service.db ctx.access ~first ~last
let current_cycle ctx = Cycle.cycle_of ctx.setting ctx.today

(* The current cycle and [count - 1] before it, oldest first. *)
let recent_cycles ctx count =
  let rec go cycle n acc = if n <= 0 then acc else go (Cycle.previous ctx.setting cycle) (n - 1) (cycle :: acc) in
  go (current_cycle ctx) (max 1 count) []

let cycle_at ctx offset =
  let rec go cycle n = if n >= 0 then cycle else go (Cycle.previous ctx.setting cycle) (n + 1) in
  go (current_cycle ctx) (min 0 offset)

let span cycles = ((List.hd cycles : Cycle.t).first, (List.nth cycles (List.length cycles - 1) : Cycle.t).last)

(* The earliest date every statement-backed balance-sheet account is reconciled through; an
   owner without any has nothing to reconcile against, so nothing counts as reconciled. *)
let reconciled_through ctx =
  let book = book ctx in
  let statement_accounts =
    List.filter
      (fun (info : Account_info.t) -> info.details.statement_backed && Option.is_some (book.inside info.account.id))
      (accounts ctx)
  in
  match statement_accounts with
  | [] -> None
  | accounts ->
      List.fold_left
        (fun through (info : Account_info.t) ->
          match through, Planning_store.reconciled_through ctx.service.db info.account.id ~ledger_balance_on:(Import_store.posted_balance_on ctx.service.db info.account) with
          | Some (Some a), Some b -> Some (Some (Date.min a b))
          | None, b -> Some b
          | _, None | Some None, _ -> Some None)
        None accounts
      |> Option.join

let cashflow_summary ctx ~cycles =
  let cycles = recent_cycles ctx cycles in
  let first, last = span cycles in
  Analytics.cashflow_summary (book ctx) ~cycles ~reconciled_through:(reconciled_through ctx) (transfers ctx ~first ~last)

type period = Cycle_offset of int | Range of Date.t * Date.t

let period_range ctx = function
  | Cycle_offset offset -> let c = cycle_at ctx offset in (c.first, c.last)
  | Range (first, last) -> (first, last)

let spending_by_category ctx ~period ~compare_previous =
  let first, last = period_range ctx period in
  let compare_to =
    if not compare_previous then None
    else
      match period with
      | Cycle_offset offset -> let c = cycle_at ctx (offset - 1) in Some (c.first, c.last)
      | Range (first, last) ->
          let days = Date.days_between ~first ~last + 1 in
          Some (Date.add_days first (-days), Date.add_days first (-1))
  in
  let from = match compare_to with Some (f, _) -> f | None -> first in
  Analytics.spending_by_category (book ctx) ~first ~last ?compare_to (transfers ctx ~first:from ~last)

let category_trend ctx ~category ~cycles =
  let cycles = recent_cycles ctx cycles in
  let first, last = span cycles in
  let book = book ctx in
  match book.inside category with
  | Some info when info.account.kind = Expense || info.account.kind = Income ->
      Ok (info, Analytics.category_trend book category ~cycles (transfers ctx ~first ~last))
  | _ -> Error (`Not_found "category in this scope")

let net_worth ctx ~cycles =
  let cycles = recent_cycles ctx cycles in
  let dates = List.map (fun (c : Cycle.t) -> Date.min c.last ctx.today) cycles in
  let last = List.fold_left Date.max ctx.today dates in
  Analytics.net_worth_series (book ctx) ~dates (Transfer_query.all_before ctx.service.db ctx.access (Date.add_days last 1))

let largest_expenses ctx ~period ~limit =
  let first, last = period_range ctx period in
  Analytics.largest_expenses (book ctx) ~first ~last ~limit (transfers ctx ~first ~last)
  |> List.map (Service.transfer_view ctx.service ctx.access)

(* Average spending over the last three complete cycles. *)
let runway ctx =
  let cycles = List.filteri (fun i _ -> i < 3) (recent_cycles ctx 4) in
  let first, last = span cycles in
  Analytics.runway (book ctx) (accounts ctx) ~cycles (transfers ctx ~first ~last)

let recurring ctx =
  let last = ctx.today in
  Analytics.recurring_payments (book ctx) (transfers ctx ~first:(Date.add_days last (-400)) ~last)

(* Unbudgeted spending already well past a usual whole cycle, against the three cycles before. *)
let unusual_spending ctx =
  let cycles = recent_cycles ctx 4 in
  let history = List.filteri (fun i _ -> i < 3) cycles and current = current_cycle ctx in
  let first, _ = span cycles in
  let threshold = Int64.of_float (100. *. (10. ** float_of_int (Currency.scale ctx.currency))) in
  Analytics.unusual_spending (book ctx) ~current ~today:ctx.today ~history ~threshold (transfers ctx ~first ~last:ctx.today)

(* This cycle's spending in one category, largest first. *)
let category_spending ctx category =
  let current = current_cycle ctx in
  transfers ctx ~first:current.first ~last:ctx.today
  |> List.filter (fun (t : Transfer.t) -> Analytics.posted t && Account_id.equal t.debit_account_id category && t.code <> Reclassify)
  |> List.sort (fun (a : Transfer.t) (b : Transfer.t) -> Amount.compare b.amount a.amount)

(* Accounts the bills still due before payday would overdraw. *)
let shortfalls ctx = Analytics.shortfalls (book ctx) ~today:ctx.today ~until:(current_cycle ctx).last (recurring ctx)

let budget_owner ctx = match ctx.scope with Analytics.Household -> List.hd ctx.owners | Me | Family -> Access.personal ctx.access

type budget_report = { report_cycle : Cycle.t; lines : Analytics.budget_line list }

let budget_status ctx ~cycle_offset =
  let cycle = cycle_at ctx cycle_offset in
  let in_force, budget_for = Planning_store.budgets ctx.service.db (budget_owner ctx) in
  let rec previous c n acc = if n = 0 then acc else let p = Cycle.previous ctx.setting c in previous p (n - 1) (p :: acc) in
  let previous_cycles = previous cycle 12 [] in
  let first = (List.hd previous_cycles).first in
  let lines =
    Analytics.budget_status (book ctx) ~cycle ~budgets:(in_force cycle) ~previous_cycles ~today:ctx.today ~budget_for
      (transfers ctx ~first ~last:cycle.last)
  in
  { report_cycle = cycle; lines }

let set_budget ctx ~category_id ~monthly_amount ~rollover ~effective_from =
  let book = book ctx in
  match book.inside category_id with
  | Some info when info.account.kind = Expense ->
      let effective_from = Option.value effective_from ~default:(Cycle.label (current_cycle ctx)) in
      Planning_store.set_budget ctx.service.db ~now:(ctx.service.now ()) (budget_owner ctx) ~category_id ~effective_from
        ~monthly_amount ~rollover;
      Ok effective_from
  | _ -> Error (`Not_found "expense category in this scope")

(* Contributions *)

type member_plan = { member : User_id.t; member_name : string; income : int64 option; contribution : int64; contributed : int64 }
type plan = { plan_cycle : Cycle.t; target : int64; target_basis : string; members : member_plan list }

let household_ctx ctx =
  match Access.household ctx.access with
  | None -> Error (`Invalid "you don't belong to a household")
  | Some h -> Ok (h, { ctx with scope = Household; owners = [ Owner.Household h.id ]; setting = h.pay_cycle })

(* A member's net income in the household's last complete cycle. Computed with that member's own
   access, so the result is only an aggregate: no transfer of theirs is ever shown to anyone else. *)
let member_income ctx (member : Household.member) =
  match Directory.find_user ctx.service.db member.user_id with
  | None -> 0L
  | Some user ->
      let access = Service.access_for ctx.service user in
      let cycle = cycle_at ctx (-1) in
      let book = Analytics.make_book ~accounts:(Service.accounts ctx.service access) ~owners:[ Owner.User user.id ] ~currency:ctx.currency in
      (Analytics.flows_between book ~first:cycle.first ~last:cycle.last
         (Transfer_query.between ctx.service.db access ~first:cycle.first ~last:cycle.last)).income

let contribution_plan ctx ~target ~incomes =
  let* household, ctx = household_ctx ctx in
  let members = List.filter (fun (m : Household.member) -> m.status = Active) (Directory.members ctx.service.db household.id) in
  let cycle = current_cycle ctx in
  let target, target_basis =
    match target with
    | Some target -> (target, "given")
    | None ->
        let in_force, _ = Planning_store.budgets ctx.service.db (Owner.Household household.id) in
        let budgets = List.fold_left (fun sum (b : Analytics.budget) -> Int64.add sum b.monthly_amount) 0L (in_force cycle) in
        let goals =
          List.fold_left (fun sum (g : Planning_store.goal) -> Int64.add sum (Option.value g.monthly_amount ~default:0L)) 0L
            (Planning_store.goals ctx.service.db (Owner.Household household.id))
        in
        (Int64.add budgets goals, "household budgets plus savings goals")
  in
  let me = (Access.user ctx.access).id in
  let incomes =
    List.map
      (fun (m : Household.member) ->
        match List.assoc_opt (User_id.to_string m.user_id) incomes with
        | Some income -> (m, income)
        | None -> (m, member_income ctx m))
      members
  in
  (* Contributions so far this cycle: money into household accounts from each member's own accounts. *)
  let view = Store_sqlite.view ctx.service.db in
  let book = book ctx in
  let contributed member =
    List.fold_left
      (fun sum (t : Transfer.t) ->
        match Analytics.posted t, book.inside t.debit_account_id, view.account t.credit_account_id with
        | true, Some _, Some { owner = User id; _ } when User_id.equal id member -> Int64.add sum (Amount.to_int64 t.amount)
        | _ -> sum)
      0L (transfers ctx ~first:cycle.first ~last:cycle.last)
  in
  let unit = Int64.of_float (10. ** float (Currency.scale ctx.currency)) in
  match Analytics.plan_contributions ~unit ~target (List.map (fun ((m : Household.member), income) -> (m.user_id, income)) incomes) with
  | None -> Error (`Invalid "no member has any income to split by: pass incomes explicitly")
  | Some shares ->
      Ok
        { plan_cycle = cycle; target; target_basis;
          members =
            List.map
              (fun (member, income, contribution) ->
                let m = List.find (fun (m : Household.member) -> User_id.equal m.user_id member) members in
                { member; member_name = m.display_name; income = (if User_id.equal member me then Some income else None);
                  contribution; contributed = contributed member })
              shares }

(* Settle-up: what the household owes each member for shared costs they paid personally. *)
type owed = { owed_member : User_id.t; owed_name : string; owed : int64 }

let settle_up ctx =
  let* household, ctx = household_ctx ctx in
  let members = Directory.members ctx.service.db household.id in
  Ok
    (List.filter_map
       (fun (info : Account_info.t) ->
         match info.details.role, info.details.member_id with
         | Some Payable, Some member when Currency.equal info.account.currency ctx.currency ->
             let name = Option.fold ~none:"member" ~some:(fun (m : Household.member) -> m.display_name)
                 (List.find_opt (fun (m : Household.member) -> User_id.equal m.user_id member) members) in
             let owed = Account.posted_balance info.account in
             if Int64.equal owed 0L then None else Some { owed_member = member; owed_name = name; owed }
         | _ -> None)
       (Account_store.for_owner ctx.service.db (Owner.Household household.id)))

(* Savings goals *)

type goal_progress = {
  goal : Planning_store.goal;
  saved : int64;
  remaining : int64;
  cycles_left : int option;
  required_monthly : int64 option;
  on_track : bool;
}

let goal_progress ctx (goal : Planning_store.goal) =
  let saved = List.fold_left (fun sum (_, amount) -> Int64.add sum amount) 0L goal.earmarks in
  let remaining = Int64.max 0L (Int64.sub goal.target_amount saved) in
  let cycles_left =
    Option.map (fun target_date -> List.length (Cycle.between ctx.setting ~first:ctx.today ~last:target_date)) goal.target_date
  in
  let required_monthly = Option.map (fun n -> if n <= 0 then remaining else Int64.div remaining (Int64.of_int n)) cycles_left in
  let on_track =
    Int64.equal remaining 0L
    || match required_monthly, goal.monthly_amount with
       | Some required, Some planned -> Int64.compare planned required >= 0
       | None, _ -> true
       | Some _, None -> false
  in
  { goal; saved; remaining; cycles_left; required_monthly; on_track }

let goals ctx =
  List.concat_map (fun owner -> List.map (goal_progress ctx) (Planning_store.goals ctx.service.db owner)) ctx.owners

let save_goal ctx ?goal_id ~name ~target_amount ~target_date ~monthly_amount () =
  let owner = budget_owner ctx in
  let name = String.trim name in
  if name = "" then Error (`Invalid "goal name must not be empty")
  else if Int64.compare target_amount 0L <= 0 then Error (`Invalid "target amount must be positive")
  else
    let* goal_id =
      match goal_id with
      | None -> Ok (Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()))
      | Some id -> (
          match Planning_store.find_goal ctx.service.db id with
          | Some g when Access.can_use ctx.access g.goal_owner -> Ok id
          | _ -> Error (`Not_found "goal"))
    in
    let goal : Planning_store.goal =
      { goal_id; goal_owner = owner; name; currency = Currency.code ctx.currency; target_amount; target_date; monthly_amount; earmarks = [] }
    in
    Planning_store.save_goal ctx.service.db ~now:(ctx.service.now ()) goal;
    Ok (goal_progress ctx (Option.get (Planning_store.find_goal ctx.service.db goal_id)))

(* Earmarked money can't exceed what the account holds. *)
let earmark ctx ~goal_id ~account_id ~amount =
  match Planning_store.find_goal ctx.service.db goal_id with
  | Some goal when Access.can_use ctx.access goal.goal_owner -> (
      let* info = Service.find_account ctx.service ctx.access account_id in
      if info.account.kind <> Asset then Error (`Invalid "only asset accounts can hold earmarked savings")
      else if Currency.code info.account.currency <> goal.currency || not (Currency.equal ctx.currency info.account.currency) then
        Error (`Invalid (Printf.sprintf "the goal is in %s; earmark from an account in %s" goal.currency goal.currency))
      else
        let elsewhere = Planning_store.earmarked_elsewhere ctx.service.db ~account_id ~goal_id in
        let balance = Account.posted_balance info.account in
        if Int64.compare (Int64.add elsewhere amount) balance > 0 then
          Error (`Invalid (Printf.sprintf "only %Ld is unallocated on that account" (Int64.sub balance elsewhere)))
        else (
          Planning_store.set_earmark ctx.service.db ~goal_id ~account_id ~amount;
          Ok (goal_progress ctx (Option.get (Planning_store.find_goal ctx.service.db goal_id)))))
  | _ -> Error (`Not_found "goal")
