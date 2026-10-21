module Access = Cashflow_core.Access
module Analytics = Cashflow_core.Analytics
module Cycle = Cashflow_core.Cycle

type report = { context : Reporting_context.t; cycles : Analytics.cycle_summary list }
let ( let* ) = Result.bind

let cashflow_summary ~access ~scope ?currency ~cycles () =
  let user = Access.user access in
  let household = Access.household access in
  let* default_currency = match scope, household with
    | Analytics.Me, _ -> Ok user.default_currency
    | (Household | Family), None -> Error Reporting_error.Household_required
    | (Household | Family), Some h -> Ok h.default_currency
  in
  let* currency = Effects.perform (Resolve_currency (Option.value currency ~default:default_currency)) in
  let now = Effects.perform Now in
  let* context = Reporting_context.make ~access ~scope ~currency ~now in
  let setting = Reporting_context.setting context in
  let rec recent cycle n acc =
    if n <= 0 then acc else recent (Cycle.previous setting cycle) (n - 1) (cycle :: acc)
  in
  let cycles = recent (Cycle.cycle_of setting (Reporting_context.today context)) (max 1 cycles) [] in
  let first = (List.hd cycles).Cycle.first in
  let last = (List.nth cycles (List.length cycles - 1)).Cycle.last in
  let* request = Cashflow_request.make ~context ~first ~last in
  let* facts = Effects.perform (Load_cashflow request) in
  let book = Analytics.make_book ~accounts:facts.accounts ~owners:(Reporting_context.owners context) ~currency in
  Ok {context; cycles=Analytics.cashflow_summary book ~cycles ~reconciled_through:facts.reconciled_through facts.transfers}
