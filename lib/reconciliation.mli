(** Earliest matching checkpoint across the statement-backed accounts inside [book].
    Returns [None] when there are no such accounts or any lacks a matching checkpoint.
    Uses the supplied materialized accounts; callers own transaction/guard policy.
    Storage failures and cancellation propagate to the caller. *)
val reconciled_through :
  Sql.db ->
  accounts:Cashflow_core.Account_info.t list ->
  book:Cashflow_core.Analytics.book ->
  Cashflow_core.Date.t option
