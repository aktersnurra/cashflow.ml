module Access = Cashflow_core.Access
module Analytics = Cashflow_core.Analytics
module Cycle = Cashflow_core.Cycle

type report = { context : Reporting_context.t; cycles : Analytics.cycle_summary list }
let ( let* ) = Result.bind

let cashflow_summary ~access ~scope ?currency ~cycles () =
  let* default_currency = Reporting_context.default_currency ~access ~scope in
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

type forecast_report = { context : Reporting_context.t; forecast : Cashflow_core.Forecast.t }

(* The what-if input in the report's currency, refused before anything is loaded. *)
let scenario_of ~currency ~today ~last (input : Forecast_request.scenario_input) =
  let module F = Cashflow_core.Forecast in
  let module Date = Cashflow_core.Date in
  let invalid reason = Error (Reporting_error.Invalid_scenario reason) in
  if List.length input.one_off > 10 then invalid "at most 10 one-off amounts"
  else
    let one_off (date, text, description) =
      match Cashflow_core.Decimal.parse ~scale:(Cashflow_core.Currency.scale currency) (String.trim text) with
      | Error _ -> invalid "amount must be a decimal like 1240.50"
      | Ok 0L -> invalid "amount must not be zero"
      | Ok _ when Date.compare date today < 0 || Date.compare date last > 0 ->
          invalid (Printf.sprintf "one-off dates must be from %s to %s" (Date.to_string today) (Date.to_string last))
      | Ok amount -> Ok { F.date; amount; description }
    in
    let* one_off = List.fold_right (fun o acc -> let* acc = acc in let* o = one_off o in Ok (o :: acc)) input.one_off (Ok []) in
    Ok { F.leave_out = input.leave_out; one_off; follow_budgets = input.follow_budgets }

let forecast ~access ~scope ?currency ~horizon ~scenario () =
  let module F = Cashflow_core.Forecast in
  let* default_currency = Reporting_context.default_currency ~access ~scope in
  let* currency = Effects.perform (Resolve_currency (Option.value currency ~default:default_currency)) in
  let now = Effects.perform Now in
  let* context = Reporting_context.make ~access ~scope ~currency ~now in
  let* request = Forecast_request.make ~context ~horizon in
  let today = Reporting_context.today context in
  let* scenario = scenario_of ~currency ~today ~last:(Analytics.add_months today horizon) scenario in
  let* facts = Effects.perform (Load_forecast request) in
  let owners = Reporting_context.owners context and setting = Reporting_context.setting context in
  let book = Analytics.make_book ~accounts:facts.accounts ~owners ~currency in
  let cash id = match book.inside id with Some info -> info.account.kind = Asset && not (Analytics.is_investment info) | None -> false in
  (* Series come from the last 400 days; the older history is for the backtest. *)
  let recent = List.filter (fun (t : Cashflow_core.Transfer.t) -> Cashflow_core.Date.compare t.value_date (Cashflow_core.Date.add_days today (-400)) >= 0) facts.transfers in
  let series = Cashflow_core.Series.detect book ~cash ~setting recent in
  let input : F.input =
    { today; setting; horizon; book; owners; accounts = facts.accounts; imported_through = facts.imported_through; transfers = facts.transfers;
      series; budgets = facts.budgets; goals = facts.goals; contribution = facts.contribution }
  in
  Ok { context; forecast = F.project input scenario }
