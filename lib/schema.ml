(* The schema is embedded so the installed binary doesn't depend on its working directory.
   Migrations are forward-only; each version runs in one transaction. *)
let migrations =
  [ ( 1,
      {|
CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL);

CREATE TABLE currencies (code TEXT PRIMARY KEY, scale INTEGER NOT NULL CHECK (scale BETWEEN 0 AND 4));
INSERT INTO currencies (code, scale) VALUES ('SEK', 2), ('EUR', 2), ('USD', 2), ('NOK', 2), ('DKK', 2), ('GBP', 2);

CREATE TABLE users (
  id TEXT PRIMARY KEY, oidc_issuer TEXT NOT NULL, oidc_subject TEXT NOT NULL,
  display_name TEXT NOT NULL, email TEXT,
  locale TEXT NOT NULL DEFAULT 'en', timezone TEXT NOT NULL DEFAULT 'Europe/Stockholm',
  default_currency TEXT NOT NULL DEFAULT 'SEK' REFERENCES currencies(code),
  pay_cycle TEXT NOT NULL DEFAULT '25;previous;SE;covered',
  is_admin INTEGER NOT NULL DEFAULT 0 CHECK (is_admin IN (0, 1)),
  created_at TEXT NOT NULL,
  UNIQUE (oidc_issuer, oidc_subject)
);

CREATE TABLE households (
  id TEXT PRIMARY KEY, name TEXT NOT NULL,
  default_currency TEXT NOT NULL REFERENCES currencies(code),
  report_split_bp INTEGER NOT NULL DEFAULT 5000 CHECK (report_split_bp BETWEEN 0 AND 10000),
  pay_cycle TEXT NOT NULL DEFAULT '25;previous;SE;covered',
  created_by TEXT NOT NULL REFERENCES users(id), created_at TEXT NOT NULL
);

CREATE TABLE memberships (
  household_id TEXT NOT NULL REFERENCES households(id), user_id TEXT NOT NULL REFERENCES users(id),
  status TEXT NOT NULL CHECK (status IN ('invited', 'active', 'declined', 'left')),
  invited_by TEXT NOT NULL REFERENCES users(id), created_at TEXT NOT NULL, updated_at TEXT NOT NULL,
  PRIMARY KEY (household_id, user_id)
);
-- A user belongs to at most one household at a time.
CREATE UNIQUE INDEX memberships_one_active_idx ON memberships(user_id) WHERE status = 'active';

CREATE TABLE accounts (
  id TEXT PRIMARY KEY,
  owner_kind TEXT NOT NULL CHECK (owner_kind IN ('user', 'household')), owner_id TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('asset', 'liability', 'equity', 'income', 'expense')),
  currency TEXT NOT NULL REFERENCES currencies(code), name TEXT NOT NULL,
  debits_must_not_exceed_credits INTEGER NOT NULL CHECK (debits_must_not_exceed_credits IN (0, 1)),
  credits_must_not_exceed_debits INTEGER NOT NULL CHECK (credits_must_not_exceed_debits IN (0, 1)),
  closed INTEGER NOT NULL DEFAULT 0 CHECK (closed IN (0, 1)),
  debits_posted INTEGER NOT NULL, credits_posted INTEGER NOT NULL,
  debits_pending INTEGER NOT NULL, credits_pending INTEGER NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE (owner_kind, owner_id, idempotency_key)
);
CREATE INDEX accounts_owner_idx ON accounts(owner_kind, owner_id);

-- Display details kept apart from the ledger's own account record: they can change freely.
CREATE TABLE account_details (
  account_id TEXT PRIMARY KEY REFERENCES accounts(id),
  institution TEXT, number_last4 TEXT CHECK (number_last4 IS NULL OR length(number_last4) = 4),
  role TEXT CHECK (role IN ('uncategorized', 'untracked_cash', 'opening', 'unrealized', 'receivable', 'payable', 'cash')),
  member_id TEXT REFERENCES users(id),
  statement_backed INTEGER NOT NULL DEFAULT 0 CHECK (statement_backed IN (0, 1)),
  sort_order INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE account_tags (account_id TEXT NOT NULL REFERENCES accounts(id), tag TEXT NOT NULL, PRIMARY KEY (account_id, tag));

-- seq orders transfers as they were recorded. The unique constraints on pending_id and
-- reverses back up the ledger's rules: a pending transfer is resolved once, a transfer reversed once.
CREATE TABLE transfers (
  seq INTEGER PRIMARY KEY AUTOINCREMENT,
  id TEXT NOT NULL UNIQUE,
  scope_kind TEXT NOT NULL CHECK (scope_kind IN ('user', 'household')), scope_id TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  debit_account_id TEXT NOT NULL REFERENCES accounts(id),
  credit_account_id TEXT NOT NULL REFERENCES accounts(id),
  amount INTEGER NOT NULL CHECK (amount > 0),
  kind TEXT NOT NULL CHECK (kind IN ('single', 'pending', 'post_pending', 'void_pending')),
  pending_id TEXT UNIQUE REFERENCES transfers(id),
  expires_on TEXT,
  code TEXT NOT NULL, description TEXT NOT NULL, counterparty TEXT,
  value_date TEXT NOT NULL,
  reverses TEXT UNIQUE REFERENCES transfers(id),
  recorded_at TEXT NOT NULL,
  UNIQUE (scope_kind, scope_id, idempotency_key),
  CHECK ((kind IN ('post_pending', 'void_pending')) = (pending_id IS NOT NULL))
);
CREATE INDEX transfers_debit_date_idx ON transfers(debit_account_id, value_date);
CREATE INDEX transfers_credit_date_idx ON transfers(credit_account_id, value_date);
CREATE INDEX transfers_open_pending_idx ON transfers(kind) WHERE kind = 'pending';
CREATE TABLE transfer_tags (transfer_id TEXT NOT NULL REFERENCES transfers(id), tag TEXT NOT NULL, PRIMARY KEY (transfer_id, tag));
CREATE INDEX transfer_tags_tag_idx ON transfer_tags(tag);
|}
    ) ]

let latest_version = List.fold_left (fun latest (version, _) -> max latest version) 0 migrations

let applied db version =
  Sql.(run db (stmt "SELECT 1 FROM schema_migrations WHERE version = ?" (int @-> nil) (opt (row (int @-> nil) Fun.id)))) version
  |> Option.is_some

let table_exists db name =
  Sql.(run db (stmt "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" (text @-> nil) (opt (row (int @-> nil) Fun.id))))
    name
  |> Option.is_some

(* schema_migrations is created by the first migration, so without it nothing has been applied. *)
let migrate db =
  List.iter
    (fun (version, sql) ->
      if not (table_exists db "schema_migrations" && applied db version) then
        Sql.transaction db (fun () ->
            Sql.exec_script db sql;
            Sql.(run db (stmt "INSERT INTO schema_migrations (version, applied_at) VALUES (?, strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))" (int @-> nil) exec))
              version))
    migrations
