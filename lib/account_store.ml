(* Account details and tags, and queries over accounts with them. *)

module Account = Cashflow_core.Account
module Account_info = Cashflow_core.Account_info

open Sql

let role = map ~name:"account role" text ~to_db:Account_info.role_to_string ~of_db:Account_info.role_of_string

let details_row =
  row
    Codec.(nullable text @-> nullable text @-> nullable role @-> nullable user_id @-> bool @-> nullable text @-> nil)
    (fun institution number_last4 role member_id statement_backed tags : Account_info.details ->
      { institution; number_last4; role; member_id; statement_backed; tags = Codec.tags tags })

let details_by_account =
  stmt
    "SELECT d.institution, d.number_last4, d.role, d.member_id, d.statement_backed, \
     (SELECT group_concat(tag, char(31)) FROM account_tags WHERE account_id = ?1) \
     FROM account_details d WHERE d.account_id = ?1"
    Codec.(account_id @-> nil) (opt details_row)

let upsert_details =
  stmt
    "INSERT INTO account_details (account_id, institution, number_last4, role, member_id, statement_backed) \
     VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (account_id) DO UPDATE SET institution = excluded.institution, \
     number_last4 = excluded.number_last4, role = excluded.role, member_id = excluded.member_id, \
     statement_backed = excluded.statement_backed"
    Codec.(account_id @-> nullable text @-> nullable text @-> nullable role @-> nullable user_id @-> bool @-> nil)
    exec

let delete_tags = stmt "DELETE FROM account_tags WHERE account_id = ?" Codec.(account_id @-> nil) exec
let insert_tag = stmt "INSERT OR IGNORE INTO account_tags (account_id, tag) VALUES (?, ?)" Codec.(account_id @-> text @-> nil) exec

let details db id = Option.value (run db details_by_account id) ~default:Account_info.no_details

let save_details db id (details : Account_info.details) =
  run db upsert_details id details.institution details.number_last4 details.role details.member_id details.statement_backed;
  run db delete_tags id;
  List.iter (run db insert_tag id) (List.sort_uniq String.compare details.tags)

let info db (account : Account.t) = { Account_info.account; details = details db account.id }

let accounts_of_owner =
  stmt
    ("SELECT " ^ Store_sqlite.account_columns ^ Store_sqlite.accounts_from
   ^ " LEFT JOIN account_details d ON d.account_id = a.id WHERE a.owner_kind = ? AND a.owner_id = ? \
      ORDER BY a.kind, d.sort_order, a.name")
    (text @-> text @-> nil) (many Store_sqlite.account_row)

let for_owner db owner = List.map (info db) (run db accounts_of_owner (Codec.owner_kind owner) (Codec.owner_id owner))

let with_role =
  stmt
    ("SELECT " ^ Store_sqlite.account_columns ^ Store_sqlite.accounts_from
   ^ " JOIN account_details d ON d.account_id = a.id WHERE a.owner_kind = ? AND a.owner_id = ? AND d.role = ? \
      AND a.kind = ? AND a.currency = ? AND a.closed = 0 ORDER BY a.created_at LIMIT 1")
    Codec.(text @-> text @-> role @-> account_kind @-> text @-> nil)
    (opt Store_sqlite.account_row)

let find_role db owner role kind ~currency = run db with_role (Codec.owner_kind owner) (Codec.owner_id owner) role kind currency
