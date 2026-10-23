(* Reading transfers for people. A transfer is visible when at least one of its accounts belongs to
   an owner the reader may use: their own personal scope, or their active household. *)
module Access = Cashflow_core.Access
module Account_id = Cashflow_core.Account_id
module Date = Cashflow_core.Date
module Household_id = Cashflow_core.Household_id
module Transfer = Cashflow_core.Transfer
module User_id = Cashflow_core.User_id

open Sql

type filter = {
  from_date : Date.t option;
  to_date : Date.t option;
  account_id : Account_id.t option;
  text : string option;  (** matched against description and counterparty *)
  tag : string option;
  code : Transfer.code option;
  without_triage : bool;  (** leave out the transfers inbox triage posted, before the limit counts *)
  limit : int;
}

let default_filter =
  { from_date = None; to_date = None; account_id = None; text = None; tag = None; code = None; without_triage = false; limit = 100 }
let max_limit = 500

(* ?1 user id, ?2 household id or ''. *)
let visible alias =
  Printf.sprintf "((%s.owner_kind = 'user' AND %s.owner_id = ?1) OR (%s.owner_kind = 'household' AND %s.owner_id = ?2))" alias
    alias alias alias

let filtered =
  stmt
    ("SELECT " ^ Store_sqlite.transfer_columns
   ^ " FROM transfers t JOIN accounts da ON da.id = t.debit_account_id JOIN accounts ca ON ca.id = t.credit_account_id \
      WHERE (" ^ visible "da" ^ " OR " ^ visible "ca"
   ^ ") \
      AND (?3 IS NULL OR t.value_date >= ?3) AND (?4 IS NULL OR t.value_date <= ?4) \
      AND (?5 IS NULL OR t.debit_account_id = ?5 OR t.credit_account_id = ?5) \
      AND (?6 IS NULL OR t.description LIKE '%' || ?6 || '%' OR t.counterparty LIKE '%' || ?6 || '%') \
      AND (?7 IS NULL OR EXISTS (SELECT 1 FROM transfer_tags g WHERE g.transfer_id = t.id AND g.tag = ?7)) \
      AND (?8 IS NULL OR t.code = ?8) \
      AND (?9 = 0 OR NOT EXISTS (SELECT 1 FROM triage_transfers x WHERE x.transfer_id = t.id)) \
      ORDER BY t.value_date DESC, t.seq DESC LIMIT ?10")
    Codec.(text @-> text @-> nullable date @-> nullable date @-> nullable account_id @-> nullable text @-> nullable text
           @-> nullable transfer_code @-> bool @-> int @-> nil)
    (many Store_sqlite.transfer_row)

let scope_params access =
  ( User_id.to_string (Access.user access).id,
    match Access.household access with Some h -> Household_id.to_string h.id | None -> "" )

let run_filter db access filter =
  let user_id, household_id = scope_params access in
  run db filtered user_id household_id filter.from_date filter.to_date filter.account_id filter.text filter.tag filter.code
    filter.without_triage (max 1 (min max_limit filter.limit))

(* Every visible posted or pending transfer in a date range, oldest first, for analytics. *)
let in_range =
  stmt
    ("SELECT " ^ Store_sqlite.transfer_columns
   ^ " FROM transfers t JOIN accounts da ON da.id = t.debit_account_id JOIN accounts ca ON ca.id = t.credit_account_id \
      WHERE (" ^ visible "da" ^ " OR " ^ visible "ca" ^ ") AND t.value_date >= ?3 AND t.value_date <= ?4 ORDER BY t.value_date, t.seq")
    Codec.(text @-> text @-> date @-> date @-> nil)
    (many Store_sqlite.transfer_row)

let between db access ~first ~last =
  let user_id, household_id = scope_params access in
  run db in_range user_id household_id first last

let before =
  stmt
    ("SELECT " ^ Store_sqlite.transfer_columns
   ^ " FROM transfers t JOIN accounts da ON da.id = t.debit_account_id JOIN accounts ca ON ca.id = t.credit_account_id \
      WHERE (" ^ visible "da" ^ " OR " ^ visible "ca" ^ ") AND t.value_date < ?3 ORDER BY t.value_date, t.seq")
    Codec.(text @-> text @-> date @-> nil)
    (many Store_sqlite.transfer_row)

let all_before db access date =
  let user_id, household_id = scope_params access in
  run db before user_id household_id date
