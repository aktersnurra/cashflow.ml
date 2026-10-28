(** What a forecast needs loaded: the context and how far ahead it looks. The what-if input is
    carried as received, since its amounts can only be read in the report's currency. *)
type t

type scenario_input = {
  leave_out : string list;
  one_off : (Cashflow_core.Date.t * string * string) list;  (** date, amount as text, description *)
  follow_budgets : bool;
}

val no_scenario : scenario_input

(** [horizon] is 1, 3 or 6 months. *)
val make : context:Reporting_context.t -> horizon:int -> (t, Reporting_error.t) result

val context : t -> Reporting_context.t
val horizon : t -> int
val since : t -> Cashflow_core.Date.t  (** 800 days back: a year of backtests, each with its own history *)
