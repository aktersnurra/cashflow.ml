type operation = Resolve_currency | Load_cashflow

type t =
  | Unknown_currency of string
  | Invalid_currency of string
  | Household_required
  | Access_denied
  | Invalid_range of { first : Cashflow_core.Date.t; last : Cashflow_core.Date.t }
  | Storage_unavailable of operation
