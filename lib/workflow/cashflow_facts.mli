type t = {
  accounts : Cashflow_core.Account_info.t list;
  transfers : Cashflow_core.Transfer.t list;
  reconciled_through : Cashflow_core.Date.t option;
  imported_through : (Cashflow_core.Account_id.t * Cashflow_core.Date.t) list;  (** latest imported statement day per account *)
}
