type t = {
  accounts : Cashflow_core.Account_info.t list;
  transfers : Cashflow_core.Transfer.t list;
  imported_through : (Cashflow_core.Account_id.t * Cashflow_core.Date.t) list;
  budgets : (Cashflow_core.Account_id.t * int64) list;
  goals : Cashflow_core.Forecast.goal list;
  contribution : Cashflow_core.Forecast.contribution option;
}
