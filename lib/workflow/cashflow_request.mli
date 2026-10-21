type t
val make : context:Reporting_context.t -> first:Cashflow_core.Date.t -> last:Cashflow_core.Date.t -> (t, Reporting_error.t) result
val context : t -> Reporting_context.t
val first : t -> Cashflow_core.Date.t
val last : t -> Cashflow_core.Date.t
