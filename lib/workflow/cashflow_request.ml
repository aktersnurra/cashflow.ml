type t = { context : Reporting_context.t; first : Cashflow_core.Date.t; last : Cashflow_core.Date.t }
let make ~context ~first ~last =
  if Cashflow_core.Date.compare first last > 0 then Error (Reporting_error.Invalid_range {first;last})
  else Ok {context;first;last}
let context t = t.context
let first t = t.first
let last t = t.last
