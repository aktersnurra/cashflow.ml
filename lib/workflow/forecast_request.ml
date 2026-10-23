type t = { context : Reporting_context.t; horizon : int }
type scenario_input = { leave_out : string list; one_off : (Cashflow_core.Date.t * string * string) list; follow_budgets : bool }
let no_scenario = { leave_out = []; one_off = []; follow_budgets = false }
let make ~context ~horizon = if List.mem horizon [1; 3; 6] then Ok {context; horizon} else Error (Reporting_error.Invalid_horizon horizon)
let context t = t.context
let horizon t = t.horizon
let since t = Cashflow_core.Date.add_days (Reporting_context.today t.context) (-800)
