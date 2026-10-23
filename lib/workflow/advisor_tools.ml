open Cashflow_core

type account_reference = string
(** Model references are syntax-checked opaque aliases, never database IDs. *)

type series_reference = string
type currency_code = string
type common = { currency : currency_code option }
type period = Cycle_offset of int

type scenario = { leave_out : series_reference list; input : Forecast_request.scenario_input }
(** [input.leave_out] must be empty until IO resolves [leave_out]. *)

type request =
  | Cycle_summary of common * int
  | Category_spending of common * period
  | Category_trend of common * account_reference * int
  | Relevant_transactions of common * period * account_reference option * int
  | Recurring of common
  | Budget_status of common * int
  | Goals of common
  | Forecast of common * int * scenario

type account = { id : Account_id.t; kind : Account.kind }
type category = { account : account; total : int64; previous : int64 option }

type transaction = {
  date : Date.t;
  amount : int64;
  description : string;
  debit : account option;
  credit : account option;
}

type payment = {
  merchant : string;
  cadence : Analytics.cadence;
  typical : int64;
  latest : int64;
  changed : bool;
  last_date : Date.t;
  next_expected : Date.t;
  occurrences : int;
  category : account option;
  paid_from : account option;
}

type budget = {
  category : account;
  budgeted : int64;
  carried : int64;
  rollover : bool;
  spent : int64;
  expected : int64;
  status : [ `On_pace | `Ahead_of_pace | `Over_budget ];
}

type goal = {
  target : int64;
  saved : int64;
  remaining : int64;
  target_date : Date.t option;
  monthly : int64 option;
  required : int64 option;
  on_track : bool;
}

type forecast_item = {
  series_id : string option;
  date : Date.t;
  kind : Forecast.kind;
  amount : int64;
  account : account option;
  category : account option;
}

type forecast = {
  today : Date.t;
  imported_through : Date.t option;
  horizon_end : Date.t;
  lowest : Date.t * int64;
  baseline : Forecast.point list;
  scenario : Forecast.point list option;
  range : (Date.t * int64 * int64) list;
  chance_positive : int option;
  items : forecast_item list;
  item_count : int;
  baseline_count : int;
  range_count : int;
  scenario_count : int;
}

type result =
  | Cycle_summary_result of Analytics.cycle_summary list
  | Category_spending_result of category list
  | Category_trend_result of account * (Cycle.t * int64) list
  | Relevant_transactions_result of transaction list
  | Recurring_result of payment list
  | Budget_status_result of Cycle.t * budget list
  | Goals_result of goal list
  | Forecast_result of forecast

type evidence = {
  id : Advisor.Evidence_id.t;
  scope : Advisor.scope;
  sampled_at : Ptime.t;
  currency : Currency.t;
  result : result;
  total_count : int;
  returned_count : int;
  has_more : bool;
  import_status : Advisor.import_status;
  imported_through : Date.t option;
  reconciled : bool;
  assumptions : string list;
  source_references : string list;
}

let valid prefix s =
  String.starts_with ~prefix s
  && String.length s > String.length prefix
  && String.length s <= 100
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false) s

let account_reference s = if valid "account_" s then Some s else None
let series_reference s = if valid "series_" s then Some s else None
let account_reference_text x = x
let series_reference_text x = x

let common = function
  | Cycle_summary (c, _)
  | Category_spending (c, _)
  | Category_trend (c, _, _)
  | Relevant_transactions (c, _, _, _)
  | Recurring c
  | Budget_status (c, _)
  | Goals c
  | Forecast (c, _, _) ->
      c

let name = function
  | Cycle_summary_result _ -> "cycle_summary"
  | Category_spending_result _ -> "category_spending"
  | Category_trend_result _ -> "category_trend"
  | Relevant_transactions_result _ -> "relevant_transactions"
  | Recurring_result _ -> "recurring"
  | Budget_status_result _ -> "budget_status"
  | Goals_result _ -> "goals"
  | Forecast_result _ -> "forecast"

let currency_code s =
  if String.length s = 3 && String.for_all (function 'A' .. 'Z' -> true | _ -> false) s then Some s else None

let currency_code_text x = x
