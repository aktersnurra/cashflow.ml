(** How much cash there will be, day by day: from today's balances, the recurring series, the
    household contribution plan and usual spending. Deterministic: it says what it assumes and
    never claims a range. Cash is the scope's asset accounts not tagged "investment". *)

type one_off = { date : Date.t; amount : int64; description : string }  (** amount signed: negative is money out *)

type scenario = {
  leave_out : string list;  (** series ids *)
  one_off : one_off list;
  follow_budgets : bool;  (** budgeted categories cost their budget instead of their history *)
}

val no_scenario : scenario

type goal = { goal_name : string; earmarked : int64; monthly : int64 option; target : int64 }

type member_contribution = { member_name : string; due_now : int64; per_cycle : int64 }

type contribution =
  | Outgoing of { due_now : int64; per_cycle : int64 }  (** my planned transfer to the household *)
  | Incoming of member_contribution list  (** each member's planned transfer, seen from the household *)

type input = {
  today : Date.t;
  setting : Pay_cycle_setting.t;
  horizon : int;  (** months ahead; the horizon ends the same day [horizon] months from today *)
  book : Analytics.book;
  owners : Owner.t list;  (** the scope's owners, to name cash accounts left out for their currency *)
  accounts : Account_info.t list;
  imported_through : (Account_id.t * Date.t) list;  (** latest imported value date per account *)
  transfers : Transfer.t list;
  series : Series.t list;
  budgets : (Account_id.t * int64) list;  (** expense category, budget per cycle *)
  goals : goal list;
  contribution : contribution option;
}

type kind = Bill | Income | Transfer | Contribution | One_off

val kind_to_string : kind -> string

type item = {
  item_id : string;  (** the series id for a series' payments; stable across requests *)
  date : Date.t;
  kind : kind;
  name : string;
  amount : int64;  (** signed, as seen from [account] *)
  account : Account_id.t;
  series : Series.t option;
}

type point = { date : Date.t; cash : int64; earmarked : int64 }
type outlook = { account : Account_info.t; start : int64; lowest : Date.t * int64; overdraw : Date.t option }

type assumption =
  | Usual_spending of { per_cycle : int64; first : Date.t; last : Date.t; days : int }
  | Stopped of Series.t  (** more than one interval overdue, so left out *)
  | Stale_import of Date.t  (** the cash accounts are imported only up to this date *)
  | Other_currency of Account_info.t list
  | Short_history of int  (** usual spending comes from this many days, fewer than three cycles *)
  | No_income_detected

type t = {
  today : Date.t;
  imported_through : Date.t option;
  horizon_end : Date.t;
  history : (Date.t * int64) list;  (** cash at the end of each day before today *)
  baseline : point list;  (** from today to [horizon_end] *)
  scenario : point list option;  (** [None] without a scenario *)
  lowest : Date.t * int64;
  accounts : outlook list;
  items : item list;  (** by date *)
  assumptions : assumption list;
}

val project : input -> scenario -> t
