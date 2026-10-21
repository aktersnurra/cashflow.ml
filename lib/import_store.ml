(* Import batches, their rows, inbox items and categorisation rules. *)
open Sql

type batch = {
  batch_id : string;
  account_id : Account_id.t;
  file_sha256 : string;
  file_name : string option;
  format : string;
  row_count : int;
  new_count : int;
  duplicate_count : int;
  matched_count : int;
  first_date : Date.t option;
  last_date : Date.t option;
  created_at : Ptime.t;
  undone : bool;
}

let batch_row =
  row
    Codec.(text @-> account_id @-> text @-> nullable text @-> text @-> int @-> int @-> int @-> int @-> nullable date @-> nullable date @-> time @-> nullable text @-> nil)
    (fun batch_id account_id file_sha256 file_name format row_count new_count duplicate_count matched_count first_date last_date created_at undone_at ->
      { batch_id; account_id; file_sha256; file_name; format; row_count; new_count; duplicate_count; matched_count; first_date; last_date;
        created_at; undone = Option.is_some undone_at })

let batch_columns =
  "id, account_id, file_sha256, file_name, format, row_count, new_count, duplicate_count, matched_count, first_date, last_date, created_at, undone_at"

let active_batch_for_file =
  stmt ("SELECT " ^ batch_columns ^ " FROM import_batches WHERE account_id = ? AND file_sha256 = ? AND undone_at IS NULL")
    Codec.(account_id @-> text @-> nil) (opt batch_row)

let batch_by_id = stmt ("SELECT " ^ batch_columns ^ " FROM import_batches WHERE id = ?") (text @-> nil) (opt batch_row)

let batches_of_account =
  stmt ("SELECT " ^ batch_columns ^ " FROM import_batches WHERE account_id = ? ORDER BY created_at DESC LIMIT ?")
    Codec.(account_id @-> int @-> nil) (many batch_row)

let last_import =
  stmt "SELECT max(created_at) FROM import_batches WHERE account_id = ? AND undone_at IS NULL" Codec.(account_id @-> nil)
    (one (row Codec.(nullable time @-> nil) Fun.id))

let insert_batch =
  stmt
    "INSERT INTO import_batches (id, account_id, file_sha256, file_name, format, row_count, new_count, duplicate_count, \
     matched_count, first_date, last_date, created_by, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    Codec.(text @-> account_id @-> text @-> nullable text @-> text @-> int @-> int @-> int @-> int @-> nullable date @-> nullable date @-> user_id @-> time @-> nil)
    exec

let mark_undone = stmt "UPDATE import_batches SET undone_at = ? WHERE id = ?" Codec.(time @-> text @-> nil) exec

let insert_row =
  stmt "INSERT INTO import_rows (batch_id, row_index, fingerprint, transfer_id, status) VALUES (?, ?, ?, ?, ?)"
    Codec.(text @-> int @-> text @-> nullable transfer_id @-> text @-> nil)
    exec

let created_transfers =
  stmt "SELECT transfer_id FROM import_rows WHERE batch_id = ? AND status IN ('new', 'matched') AND transfer_id IS NOT NULL ORDER BY row_index"
    (text @-> nil) (many (row Codec.(transfer_id @-> nil) Fun.id))

let find_active_batch db ~account_id ~sha = run db active_batch_for_file account_id sha
let find_batch db id = run db batch_by_id id
let batches db ~account_id ~limit = run db batches_of_account account_id limit
let last_import_at db account_id = run db last_import account_id
let save_batch db ~user_id (b : batch) =
  run db insert_batch b.batch_id b.account_id b.file_sha256 b.file_name b.format b.row_count b.new_count b.duplicate_count b.matched_count
    b.first_date b.last_date user_id b.created_at
let save_row db ~batch_id ~index ~fingerprint ~transfer_id ~status = run db insert_row batch_id index fingerprint transfer_id status
let undo_batch db ~now id = run db mark_undone now id
let batch_transfers db id = run db created_transfers id

(* The ledger balance of an account after every posted transfer up to a date. *)
let balance_on =
  stmt
    "SELECT coalesce(sum(CASE WHEN debit_account_id = ?1 THEN amount ELSE -amount END), 0) FROM transfers \
     WHERE (debit_account_id = ?1 OR credit_account_id = ?1) AND kind IN ('single', 'post_pending') AND value_date <= ?2"
    Codec.(account_id @-> date @-> nil) (one (row (int64 @-> nil) Fun.id))

(* Transfers in effect on an account that day: reversed ones and their reversals cancel out. *)
let posted_on =
  stmt
    "SELECT count(*) FROM transfers t WHERE (debit_account_id = ?1 OR credit_account_id = ?1) AND kind IN ('single', 'post_pending') \
     AND value_date = ?2 AND reverses IS NULL AND NOT EXISTS (SELECT 1 FROM transfers r WHERE r.reverses = t.id)"
    Codec.(account_id @-> date @-> nil) (one (row (int @-> nil) Fun.id))

let posted_count_on db (account : Account.t) date = run db posted_on account.id date

let posted_balance_on db (account : Account.t) date =
  let debit_minus_credit = run db balance_on account.id date in
  if Account.debit_normal account.kind then debit_minus_credit else Int64.neg debit_minus_credit

(* Inbox *)

type item = {
  transfer_id : Transfer_id.t;
  bank_account_id : Account_id.t;
  current_account_id : Account_id.t;
  revision : int;
  suggested_account_id : Account_id.t option;
  suggestion_source : string option;
  suggestion_confidence : int option;
  candidate_pending_id : Transfer_id.t option;
  merchant_key : string;
  status : string;
}

let item_row =
  row
    Codec.(transfer_id @-> account_id @-> account_id @-> int @-> nullable account_id @-> nullable text @-> nullable int @-> nullable transfer_id @-> text @-> text @-> nil)
    (fun transfer_id bank_account_id current_account_id revision suggested_account_id suggestion_source suggestion_confidence candidate_pending_id merchant_key status ->
      { transfer_id; bank_account_id; current_account_id; revision; suggested_account_id; suggestion_source; suggestion_confidence;
        candidate_pending_id; merchant_key; status })

let item_columns =
  "i.transfer_id, i.bank_account_id, i.current_account_id, i.revision, i.suggested_account_id, i.suggestion_source, \
   i.suggestion_confidence, i.candidate_pending_id, i.merchant_key, i.status"

let insert_item =
  stmt
    "INSERT OR IGNORE INTO inbox_items (transfer_id, bank_account_id, current_account_id, suggested_account_id, suggestion_source, \
     suggestion_confidence, candidate_pending_id, merchant_key, status, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    Codec.(transfer_id @-> account_id @-> account_id @-> nullable account_id @-> nullable text @-> nullable int @-> nullable transfer_id @-> text @-> text @-> time @-> time @-> nil)
    exec

let item_by_transfer = stmt ("SELECT " ^ item_columns ^ " FROM inbox_items i WHERE i.transfer_id = ?") Codec.(transfer_id @-> nil) (opt item_row)

let open_items_for =
  stmt
    ("SELECT " ^ item_columns ^ " FROM inbox_items i JOIN accounts a ON a.id = i.bank_account_id JOIN transfers t ON t.id = i.transfer_id \
      WHERE i.status = 'open' AND ((a.owner_kind = 'user' AND a.owner_id = ?1) OR (a.owner_kind = 'household' AND a.owner_id = ?2)) \
      ORDER BY t.value_date DESC, t.seq DESC LIMIT ?3")
    (text @-> text @-> int @-> nil) (many item_row)

let count_open_for =
  stmt
    "SELECT count(*) FROM inbox_items i JOIN accounts a ON a.id = i.bank_account_id WHERE i.status = 'open' \
     AND ((a.owner_kind = 'user' AND a.owner_id = ?1) OR (a.owner_kind = 'household' AND a.owner_id = ?2))"
    (text @-> text @-> nil) (one (row (int @-> nil) Fun.id))

let update_item =
  stmt "UPDATE inbox_items SET current_account_id = ?, revision = ?, status = ?, updated_at = ? WHERE transfer_id = ?"
    Codec.(account_id @-> int @-> text @-> time @-> transfer_id @-> nil)
    exec

let history_for =
  stmt
    "SELECT i.current_account_id, count(*) FROM inbox_items i JOIN accounts a ON a.id = i.current_account_id \
     WHERE i.merchant_key = ? AND i.status = 'done' AND a.owner_kind = ? AND a.owner_id = ? AND a.closed = 0 \
     GROUP BY i.current_account_id ORDER BY max(i.updated_at) DESC LIMIT 1"
    (text @-> text @-> text @-> nil) (opt (row Codec.(account_id @-> int @-> nil) (fun id n -> (id, n))))

let add_item db ~now (i : item) =
  run db insert_item i.transfer_id i.bank_account_id i.current_account_id i.suggested_account_id i.suggestion_source i.suggestion_confidence
    i.candidate_pending_id i.merchant_key i.status now now

let find_item db id = run db item_by_transfer id
let open_items db access ~limit = let u, h = Transfer_query.scope_params access in run db open_items_for u h limit
let open_count db access = let u, h = Transfer_query.scope_params access in run db count_open_for u h
let save_item db ~now (i : item) = run db update_item i.current_account_id i.revision i.status now i.transfer_id
let history db owner merchant = run db history_for merchant (Codec.owner_kind owner) (Codec.owner_id owner)

(* Rules *)

let direction = map ~name:"direction" text ~to_db:(function `In -> "in" | `Out -> "out") ~of_db:(function "in" -> Some `In | "out" -> Some `Out | _ -> None)

let rule_row =
  row
    Codec.(text @-> text @-> text @-> text @-> nullable direction @-> nullable int64 @-> nullable int64 @-> nullable account_id @-> account_id @-> text @-> bool @-> bool @-> nil)
    (fun rule_id kind id pattern direction min_amount max_amount bank_account_id category_id tags shared auto_apply : Suggest.rule ->
      { rule_id; owner = Codec.owner kind id; pattern; direction; min_amount; max_amount; bank_account_id; category_id;
        tags = List.filter (( <> ) "") (String.split_on_char ',' tags); shared; auto_apply })

let rules_of =
  stmt
    "SELECT id, owner_kind, owner_id, pattern, direction, min_amount, max_amount, bank_account_id, category_account_id, tags, shared, auto_apply \
     FROM categorisation_rules WHERE owner_kind = ? AND owner_id = ? ORDER BY created_at"
    (text @-> text @-> nil) (many rule_row)

let insert_rule =
  stmt
    "INSERT INTO categorisation_rules (id, owner_kind, owner_id, pattern, direction, min_amount, max_amount, bank_account_id, \
     category_account_id, tags, shared, auto_apply, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    Codec.(text @-> text @-> text @-> text @-> nullable direction @-> nullable int64 @-> nullable int64 @-> nullable account_id @-> account_id @-> text @-> bool @-> bool @-> time @-> nil)
    exec

let delete_rule = stmt "DELETE FROM categorisation_rules WHERE id = ? AND owner_kind = ? AND owner_id = ?" (text @-> text @-> text @-> nil) exec

let rules db owner = run db rules_of (Codec.owner_kind owner) (Codec.owner_id owner)

let add_rule db ~now (r : Suggest.rule) =
  run db insert_rule r.rule_id (Codec.owner_kind r.owner) (Codec.owner_id r.owner) r.pattern r.direction r.min_amount r.max_amount
    r.bank_account_id r.category_id (String.concat "," r.tags) r.shared r.auto_apply now

let remove_rule db owner id =
  run db delete_rule id (Codec.owner_kind owner) (Codec.owner_id owner);
  Sql.changes db > 0
