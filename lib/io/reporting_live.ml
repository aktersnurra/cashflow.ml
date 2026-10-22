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

let load_cashflow db access request =
  let context = Cashflow_request.context request in
  if not (User_id.equal (Access.user access).id (Access.user (Reporting_context.access context)).id)
     || not (List.for_all (Access.can_use access) (Reporting_context.owners context)) then
    Error Reporting_error.Access_denied
  else storage Reporting_error.Load_cashflow (fun () ->
    Sql.read_snapshot db (fun () ->
      let accounts = List.concat_map (Account_store.for_owner db) (Access.owners access) in
      let transfers = Transfer_query.between db access ~first:(Cashflow_request.first request) ~last:(Cashflow_request.last request) in
      let book = Analytics.make_book ~accounts ~owners:(Reporting_context.owners context) ~currency:(Reporting_context.currency context) in
      let reconciled_through = Reconciliation.reconciled_through db ~accounts ~book in
      Ok { Cashflow_facts.accounts; transfers; reconciled_through }))

let run ~db ~access ~(now : unit -> Ptime.t) f =
  Effects.run { Effects.handle = (fun (type a) (operation : a Effect_ops.t) : a ->
    match operation with
    | Now -> now ()
    | Resolve_currency code -> resolve_currency db code
    | Load_cashflow request -> load_cashflow db access request) } f
