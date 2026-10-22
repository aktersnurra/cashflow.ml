type t
val default_currency : access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> (string, Reporting_error.t) result
val make : access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> currency:Cashflow_core.Currency.t -> now:Ptime.t -> (t, Reporting_error.t) result
val access : t -> Cashflow_core.Access.t
val scope : t -> Cashflow_core.Analytics.scope
val owners : t -> Cashflow_core.Owner.t list
val currency : t -> Cashflow_core.Currency.t
val setting : t -> Cashflow_core.Pay_cycle_setting.t
val today : t -> Cashflow_core.Date.t
