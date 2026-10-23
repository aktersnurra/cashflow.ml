type _ t =
  | Now : Ptime.t t
  | Resolve_currency : string -> (Cashflow_core.Currency.t, Reporting_error.t) result t
  | Load_cashflow : Cashflow_request.t -> (Cashflow_facts.t, Reporting_error.t) result t
  | Load_forecast : Forecast_request.t -> (Forecast_facts.t, Reporting_error.t) result t
