open Cashflow_core
open Cashflow_workflow

let log_src = Logs.Src.create "cashflow.reporting" ~doc:"Reporting storage"
module Log = (val Logs.src_log log_src : Logs.LOG)

let storage operation f =
  try f () with Sql.Sql_error message ->
    Log.err (fun m -> m "report storage error: %s" message);
    Error (Reporting_error.Storage_unavailable operation)

let resolve_currency db code =
  storage Reporting_error.Resolve_currency (fun () ->
    match Sql.(run db (stmt "SELECT scale FROM currencies WHERE code = ?" (text @-> nil) (opt (row (int @-> nil) Fun.id)))) code with
    | None -> Error (Reporting_error.Unknown_currency code)
    | Some scale -> Result.map_error (fun (`Invalid_currency reason) -> Reporting_error.Invalid_currency reason) (Currency.make ~code ~scale))

(* A request is served only to the user it was made for, and only for owners they may use. *)
let authorized access context =
  User_id.equal (Access.user access).id (Access.user (Reporting_context.access context)).id
  && List.for_all (Access.can_use access) (Reporting_context.owners context)

let load_cashflow db access request =
  let context = Cashflow_request.context request in
  if not (authorized access context) then
    Error Reporting_error.Access_denied
  else storage Reporting_error.Load_cashflow (fun () ->
    Sql.read_snapshot db (fun () ->
      let accounts = List.concat_map (Account_store.for_owner db) (Access.owners access) in
      let transfers = Transfer_query.between db access ~first:(Cashflow_request.first request) ~last:(Cashflow_request.last request) in
      let book = Analytics.make_book ~accounts ~owners:(Reporting_context.owners context) ~currency:(Reporting_context.currency context) in
      let reconciled_through = Reconciliation.reconciled_through db ~accounts ~book in
      Ok { Cashflow_facts.accounts; transfers; reconciled_through }))

(* The household contribution plan, as the forecast's planned transfers: mine to the household
   in my scope, every member's in the household's. A plan with nothing to collect is none. *)
let contribution (service : Service.t) access context =
  let currency = Currency.code (Reporting_context.currency context) in
  let plan () =
    match Insights.context service access ~scope:Me ~currency () with
    | Error _ -> None
    | Ok ctx -> (
        match Insights.contribution_plan ctx ~target:None ~incomes:[] with
        | Ok (plan : Insights.plan) when Int64.compare plan.target 0L > 0 -> Some plan
        | _ -> None)
  in
  let due (m : Insights.member_plan) = Int64.max 0L (Int64.sub m.contribution m.contributed) in
  match Reporting_context.scope context, Access.household access with
  | Analytics.Me, Some _ ->
      let me = (Access.user access).id in
      Option.bind (plan ()) (fun plan ->
          List.find_map
            (fun (m : Insights.member_plan) ->
              if User_id.equal m.member me then Some (Forecast.Outgoing { due_now = due m; per_cycle = m.contribution }) else None)
            plan.members)
  | Household, Some _ ->
      Option.map
        (fun (plan : Insights.plan) ->
          Forecast.Incoming
            (List.map (fun (m : Insights.member_plan) -> { Forecast.member_name = m.member_name; due_now = due m; per_cycle = m.contribution }) plan.members))
        (plan ())
  | _ -> None

let load_forecast (service : Service.t) access request =
  let context = Forecast_request.context request in
  let db = service.db in
  if not (authorized access context) then Error Reporting_error.Access_denied
  else storage Reporting_error.Load_forecast (fun () ->
    let today = Reporting_context.today context and owners = Reporting_context.owners context in
    let currency = Reporting_context.currency context in
    let facts =
      Sql.read_snapshot db (fun () ->
        let accounts = List.concat_map (Account_store.for_owner db) (Access.owners access) in
        let transfers = Transfer_query.between db access ~first:(Forecast_request.since request) ~last:today in
        let imported_through =
          List.filter_map (fun (info : Account_info.t) -> Option.map (fun d -> (info.account.id, d)) (Import_store.imported_through db info.account.id)) accounts
        in
        let budget_owner = match Reporting_context.scope context with Analytics.Household -> List.hd owners | Me | Family -> Access.personal access in
        let in_force, _ = Planning_store.budgets db budget_owner in
        let budgets =
          List.map (fun (b : Analytics.budget) -> (b.category_id, b.monthly_amount)) (in_force (Cycle.cycle_of (Reporting_context.setting context) today))
        in
        let goals =
          List.concat_map (Planning_store.goals db) owners
          |> List.filter (fun (g : Planning_store.goal) -> g.currency = Currency.code currency)
          |> List.map (fun (g : Planning_store.goal) ->
                 { Forecast.goal_name = g.name; earmarked = List.fold_left (fun sum (_, a) -> Int64.add sum a) 0L g.earmarks;
                   monthly = g.monthly_amount; target = g.target_amount })
        in
        { Forecast_facts.accounts; transfers; imported_through; budgets; goals; contribution = None })
    in
    Ok { facts with contribution = contribution service access context })

let run ~(service : Service.t) ~access f =
  let db = service.db in
  Effects.run { Effects.handle = (fun (type a) (operation : a Effect_ops.t) : a ->
    match operation with
    | Now -> service.now ()
    | Resolve_currency code -> resolve_currency db code
    | Load_cashflow request -> load_cashflow db access request
    | Load_forecast request -> load_forecast service access request) } f
