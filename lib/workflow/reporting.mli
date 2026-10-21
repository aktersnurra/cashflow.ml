type report = { context : Reporting_context.t; cycles : Cashflow_core.Analytics.cycle_summary list }
val cashflow_summary : access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> ?currency:string -> cycles:int -> unit -> (report, Reporting_error.t) result
