open Cashflow_core

type account_reference = private string
(** Model references are syntax-checked opaque aliases, never database IDs. *)

type series_reference = private string
type row_reference = private string
type goal_reference = private string

val account_reference : string -> account_reference option
val series_reference : string -> series_reference option
val row_reference : string -> row_reference option
val goal_reference : string -> goal_reference option
val goal_reference_text : goal_reference -> string
val account_reference_text : account_reference -> string
val series_reference_text : series_reference -> string
val row_reference_text : row_reference -> string

type currency_code = private string

val currency_code : string -> currency_code option
val currency_code_text : currency_code -> string

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
  | Period_outlook of common  (** the cycle in progress so far, and where it is heading *)
  | Inbox_rows of common
  | Categories of common
  | Bank_accounts of common
  | Propose_entry of { from_account : account_reference; to_account : account_reference; amount : string; date : string option; description : string }
      (** Money moving between two accounts the user has, entered by hand, for the user to confirm. *)
  | Propose_rule of { pattern : string; category : account_reference; direction : [ `In | `Out ] option }
      (** A merchant text that suggests a category on import (never applies it by itself). *)
  | Propose_goal of { name : string; target_amount : string; target_date : string option; monthly_amount : string option }
  | Propose_update_goal of { goal : goal_reference; name : string option; target_amount : string option; target_date : string option; monthly_amount : string option }
  | Propose_category of { name : string; kind : [ `Expense | `Income ] }
  | Propose_categorize of { rows : (row_reference * account_reference) list }
      (** Up to 20 uncategorized rows, each with the category to file it under, for the user to confirm. *)
  | Propose_budget of { category : account_reference; monthly_amount : string; rollover : bool }
      (** A change for the user to confirm. Currency and scope are the conversation's. *)
  | File_inbox of { keep_on : bool }
      (** Files, without asking, every uncategorized row cashflow is sure enough of, when the user asked for it;
          [keep_on] files later imports the same way. The one tool that writes by itself. *)

val common : request -> common

type account = { id : Account_id.t; kind : Account.kind; name : string option }
(* [name] is the user's own label for an income or expense category. Bank and other balance-sheet
   accounts have none: their names can carry account numbers, so the model sees only an alias. *)
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
  reference : goal_reference;
  name : string;
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

type inbox_row = {
  reference : row_reference;
  date : Date.t;
  amount : int64;  (** minor units, always positive *)
  outflow : bool;
  description : string;
  suggestion : (account * int * string) option;  (** category, confidence 0-100, where it came from *)
}

type categorize_row = {
  transfer_id : Transfer_id.t;
  date : Date.t;
  amount : int64;
  outflow : bool;
  description : string;
  category : account;
  revision : int;  (** the inbox item's revision when proposed *)
  current : Account_id.t;  (** where the row sat when proposed *)
}

type bank_account = {
  account : account;
  institution : string option;
  hint : string;  (** the account's name with every digit and asterisk removed: enough to match "my savings", never a number *)
  statement_backed : bool;
}

type budget_proposal = {
  category : account;
  current : int64 option;  (** the monthly budget in force this cycle, if any *)
  current_rollover : bool option;  (** whether that budget carries over *)
  proposed : int64;
  rollover : bool;
  from_cycle : string;  (** the cycle label it would apply from *)
}

(** What a proposal looks like to the user and the model, built once by the projection from checked values: the card's
    facts, the fields the model is shown, the accounts those name (so they get aliases), and what is stored to apply or
    undo it later (JSON text, typed by the proposal's [kind]). *)
type model_value = Text_value of string | Number_value of int64 | Bool_value of bool | Account_value of account | Null_value

type proposal_card = {
  tool : string;
  kind : string;
  facts : Advisor.fact list;
  model : (string * model_value) list;
  accounts : account list;
  arguments : string;
  precondition : string;
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
  | Budget_proposal_result of budget_proposal
  | Inbox_rows_result of inbox_row list
  | Categories_result of account list
  | Categorize_proposal_result of categorize_row list
  | Proposal_card_result of proposal_card
  | Bank_accounts_result of bank_account list
  | Period_outlook_result of Outlook.t
  | Filed_inbox_result of { filed : inbox_row list; unsure : inbox_row list; kept_on : bool }
      (** [suggestion] is where a filed row went, or the best guess for one left for the user. *)

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

val name : result -> string
