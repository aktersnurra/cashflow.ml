type report = { context : Reporting_context.t; cycles : Cashflow_core.Analytics.cycle_summary list }
val cashflow_summary : access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> ?currency:string -> cycles:int -> unit -> (report, Reporting_error.t) result

type forecast_report = { context : Reporting_context.t; forecast : Cashflow_core.Forecast.t }

val forecast :
  access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> ?currency:string -> horizon:int ->
  scenario:Forecast_request.scenario_input -> unit -> (forecast_report, Reporting_error.t) result
(** Resolves the currency, reads the clock, checks the horizon and the what-if input, then loads
    the facts once and projects. One-off amounts are decimals in the report's currency, not zero,
    dated from today to the horizon's end, at most 10. Unknown [leave_out] ids are ignored: a
    reload can make one stale. *)

type outlook_report = { context : Reporting_context.t; outlook : Cashflow_core.Outlook.t }

val period_outlook :
  access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> ?currency:string -> unit -> (outlook_report, Reporting_error.t) result
(** The current pay cycle as far as the statements reach, set beside the same days of earlier cycles,
    and where it is heading (Cashflow_core.Outlook). *)
