(* The ledger's SQLite store. Statements are typed (see Sql), so the columns below and the
   constructors that decode them can't drift apart without a compile error. *)
module Account = Cashflow_core.Account
module Currency = Cashflow_core.Currency
module Ledger_core = Cashflow_core.Ledger_core
module Transfer = Cashflow_core.Transfer

open Sql

type t = Sql.db

let account_columns =
  "a.id, a.owner_kind, a.owner_id, a.idempotency_key, a.kind, a.currency, c.scale, a.name, \
   a.debits_must_not_exceed_credits, a.credits_must_not_exceed_debits, a.closed, \
   a.debits_posted, a.credits_posted, a.debits_pending, a.credits_pending, a.created_at"

let account_row =
  row
    Codec.(account_id @-> text @-> text @-> key @-> account_kind @-> text @-> int @-> text @-> bool @-> bool @-> bool
           @-> int64 @-> int64 @-> int64 @-> int64 @-> time @-> nil)
    (fun id owner_kind owner_id key kind code scale name debits_limited credits_limited closed debits_posted
         credits_posted debits_pending credits_pending created_at : Account.t ->
      let currency =
        match Currency.make ~code ~scale with Ok currency -> currency | Error (`Invalid_currency m) -> raise (Sql_error m)
      in
      { id; key; owner = Codec.owner owner_kind owner_id; kind; currency; name;
        flags = { debits_must_not_exceed_credits = debits_limited; credits_must_not_exceed_debits = credits_limited };
        closed; balances = { debits_posted; credits_posted; debits_pending; credits_pending }; created_at })

let accounts_from = " FROM accounts a JOIN currencies c ON c.code = a.currency"

let account_by_id =
  stmt ("SELECT " ^ account_columns ^ accounts_from ^ " WHERE a.id = ?") Codec.(account_id @-> nil) (opt account_row)

let account_by_key =
  stmt
    ("SELECT " ^ account_columns ^ accounts_from ^ " WHERE a.owner_kind = ? AND a.owner_id = ? AND a.idempotency_key = ?")
    Codec.(text @-> text @-> key @-> nil)
    (opt account_row)

let all_accounts = stmt ("SELECT " ^ account_columns ^ accounts_from ^ " ORDER BY a.created_at, a.id") nil (many account_row)

let upsert_account =
  stmt
    "INSERT INTO accounts (id, owner_kind, owner_id, idempotency_key, kind, currency, name, \
     debits_must_not_exceed_credits, credits_must_not_exceed_debits, closed, debits_posted, credits_posted, \
     debits_pending, credits_pending, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
     ON CONFLICT (id) DO UPDATE SET name = excluded.name, closed = excluded.closed, \
     debits_posted = excluded.debits_posted, credits_posted = excluded.credits_posted, \
     debits_pending = excluded.debits_pending, credits_pending = excluded.credits_pending"
    Codec.(account_id @-> text @-> text @-> key @-> account_kind @-> text @-> text @-> bool @-> bool @-> bool @-> int64
           @-> int64 @-> int64 @-> int64 @-> time @-> nil)
    exec

let ensure_currency = stmt "INSERT OR IGNORE INTO currencies (code, scale) VALUES (?, ?)" (text @-> int @-> nil) exec

let save_account db (account : Account.t) =
  run db ensure_currency (Currency.code account.currency) (Currency.scale account.currency);
  let b = account.balances in
  run db upsert_account account.id (Codec.owner_kind account.owner) (Codec.owner_id account.owner) account.key account.kind
    (Currency.code account.currency) account.name account.flags.debits_must_not_exceed_credits
    account.flags.credits_must_not_exceed_debits account.closed b.debits_posted b.credits_posted b.debits_pending
    b.credits_pending account.created_at

let transfer_columns =
  "t.id, t.scope_kind, t.scope_id, t.idempotency_key, t.debit_account_id, t.credit_account_id, t.amount, t.kind, \
   t.pending_id, t.expires_on, t.code, t.description, t.counterparty, t.value_date, t.reverses, t.recorded_at, \
   (SELECT group_concat(tag, char(31)) FROM transfer_tags WHERE transfer_id = t.id)"

let transfer_kind kind pending_id expires_on : Transfer.kind =
  match kind, pending_id with
  | "single", None -> Single
  | "pending", None -> Pending { expires_on }
  | "post_pending", Some pending_id -> Post_pending { pending_id }
  | "void_pending", Some pending_id -> Void_pending { pending_id }
  | _ -> raise (Sql_error ("invalid transfer kind " ^ kind))

let transfer_row =
  row
    Codec.(transfer_id @-> text @-> text @-> key @-> account_id @-> account_id @-> amount @-> text
           @-> nullable transfer_id @-> nullable date @-> transfer_code @-> text @-> nullable text @-> date
           @-> nullable transfer_id @-> time @-> nullable text @-> nil)
    (fun id scope_kind scope_id key debit_account_id credit_account_id amount kind pending_id expires_on code
         description counterparty value_date reverses recorded_at tags : Transfer.t ->
      { id; scope = Codec.owner scope_kind scope_id; key; debit_account_id; credit_account_id; amount;
        kind = transfer_kind kind pending_id expires_on; code;
        memo = { description; counterparty; tags = Codec.tags tags }; value_date; reverses; recorded_at })

let transfer_where condition params =
  stmt ("SELECT " ^ transfer_columns ^ " FROM transfers t WHERE " ^ condition ^ " ORDER BY t.seq") params (opt transfer_row)

let transfer_by_id = transfer_where "t.id = ?" Codec.(transfer_id @-> nil)
let transfer_by_key = transfer_where "t.scope_kind = ? AND t.scope_id = ? AND t.idempotency_key = ?" Codec.(text @-> text @-> key @-> nil)
let resolution_of = transfer_where "t.pending_id = ?" Codec.(transfer_id @-> nil)
let reversal_of = transfer_where "t.reverses = ?" Codec.(transfer_id @-> nil)
let all_transfers = stmt ("SELECT " ^ transfer_columns ^ " FROM transfers t ORDER BY t.seq") nil (many transfer_row)

let open_pending_transfers =
  stmt
    ("SELECT " ^ transfer_columns
   ^ " FROM transfers t WHERE t.kind = 'pending' AND NOT EXISTS (SELECT 1 FROM transfers r WHERE r.pending_id = t.id) \
      ORDER BY t.seq")
    nil (many transfer_row)

(* Posted manual entries touching an account whose side on that account no statement has
   confirmed yet: the other side's statement posted them (see Import.classify). *)
let unconfirmed_posts =
  stmt
    ("SELECT " ^ transfer_columns
   ^ " FROM transfers t WHERE t.kind = 'post_pending' AND (t.debit_account_id = ?1 OR t.credit_account_id = ?1) \
      AND NOT EXISTS (SELECT 1 FROM transfers r WHERE r.reverses = t.id) \
      AND NOT EXISTS (SELECT 1 FROM statement_legs l WHERE l.entry_id = t.pending_id AND l.account_id = ?1) ORDER BY t.seq")
    Codec.(account_id @-> nil) (many transfer_row)

let insert_transfer =
  stmt
    "INSERT INTO transfers (id, scope_kind, scope_id, idempotency_key, debit_account_id, credit_account_id, amount, \
     kind, pending_id, expires_on, code, description, counterparty, value_date, reverses, recorded_at) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    Codec.(transfer_id @-> text @-> text @-> key @-> account_id @-> account_id @-> amount @-> text
           @-> nullable transfer_id @-> nullable date @-> transfer_code @-> text @-> nullable text @-> date
           @-> nullable transfer_id @-> time @-> nil)
    exec

let insert_tag = stmt "INSERT OR IGNORE INTO transfer_tags (transfer_id, tag) VALUES (?, ?)" Codec.(transfer_id @-> text @-> nil) exec

let write_transfer db (transfer : Transfer.t) =
  let kind, pending_id, expires_on =
    match transfer.kind with
    | Single -> ("single", None, None)
    | Pending { expires_on } -> ("pending", None, expires_on)
    | Post_pending { pending_id } -> ("post_pending", Some pending_id, None)
    | Void_pending { pending_id } -> ("void_pending", Some pending_id, None)
  in
  run db insert_transfer transfer.id (Codec.owner_kind transfer.scope) (Codec.owner_id transfer.scope) transfer.key
    transfer.debit_account_id transfer.credit_account_id transfer.amount kind pending_id expires_on transfer.code
    transfer.memo.description transfer.memo.counterparty transfer.value_date transfer.reverses transfer.recorded_at;
  List.iter (run db insert_tag transfer.id) transfer.memo.tags

let view db : Ledger_core.view =
  { account = run db account_by_id;
    account_by_key = (fun owner key -> run db account_by_key (Codec.owner_kind owner) (Codec.owner_id owner) key);
    transfer = run db transfer_by_id;
    transfer_by_key = (fun owner key -> run db transfer_by_key (Codec.owner_kind owner) (Codec.owner_id owner) key);
    resolution = run db resolution_of; reversal = run db reversal_of }

let transaction = Sql.transaction

let write db applied =
  List.iter (write_transfer db) (Ledger_core.transfers applied);
  List.iter (save_account db) (Ledger_core.accounts applied)

let accounts db = run db all_accounts
let transfers db = run db all_transfers
let open_pending db = run db open_pending_transfers
let unconfirmed_posts_on db account_id = run db unconfirmed_posts account_id

let open_memory () =
  let db = Sql.open_db ":memory:" in
  Schema.migrate db;
  db
