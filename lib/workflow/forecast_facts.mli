(** A detached snapshot of what the forecast reads: visible accounts and transfers since the
    request's [since], the latest imported date per account, budgets, goals and the household
    contribution plan. *)
type t = {
  accounts : Cashflow_core.Account_info.t list;
  transfers : Cashflow_core.Transfer.t list;
  imported_through : (Cashflow_core.Account_id.t * Cashflow_core.Date.t) list;
  budgets : (Cashflow_core.Account_id.t * int64) list;  (** in force this cycle, for the budget owner *)
  goals : Cashflow_core.Forecast.goal list;  (** in the report's currency *)
  contribution : Cashflow_core.Forecast.contribution option;
}
