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
    );
    ( 2,
      {|
-- A budget changes by adding a version that applies from a cycle ("YYYY-MM") onward, so past
-- cycles keep the budget they had. An amount of zero removes the budget from that cycle on.
CREATE TABLE budget_versions (
  owner_kind TEXT NOT NULL, owner_id TEXT NOT NULL,
  category_account_id TEXT NOT NULL REFERENCES accounts(id),
  effective_from TEXT NOT NULL,
  monthly_amount INTEGER NOT NULL CHECK (monthly_amount >= 0),
  rollover INTEGER NOT NULL CHECK (rollover IN (0, 1)),
  created_at TEXT NOT NULL,
  PRIMARY KEY (category_account_id, effective_from)
);

CREATE TABLE savings_goals (
  id TEXT PRIMARY KEY, owner_kind TEXT NOT NULL, owner_id TEXT NOT NULL,
  name TEXT NOT NULL, currency TEXT NOT NULL REFERENCES currencies(code),
  target_amount INTEGER NOT NULL CHECK (target_amount > 0), target_date TEXT,
  monthly_amount INTEGER CHECK (monthly_amount IS NULL OR monthly_amount > 0),
  archived INTEGER NOT NULL DEFAULT 0 CHECK (archived IN (0, 1)), created_at TEXT NOT NULL
);
-- A goal earmarks money in real savings accounts rather than holding any of its own.
CREATE TABLE goal_earmarks (
  goal_id TEXT NOT NULL REFERENCES savings_goals(id), account_id TEXT NOT NULL REFERENCES accounts(id),
  amount INTEGER NOT NULL CHECK (amount >= 0), PRIMARY KEY (goal_id, account_id)
);

CREATE TABLE reconciliation_checkpoints (
  id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES accounts(id),
  on_date TEXT NOT NULL, statement_balance INTEGER NOT NULL, ledger_balance INTEGER NOT NULL,
  source TEXT NOT NULL CHECK (source IN ('manual', 'import')), import_batch_id TEXT,
  created_by TEXT NOT NULL REFERENCES users(id), created_at TEXT NOT NULL
);
CREATE INDEX reconciliation_account_date_idx ON reconciliation_checkpoints(account_id, on_date);
|}
    );
    ( 3,
      {|
CREATE TABLE import_batches (
  id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES accounts(id),
  file_sha256 TEXT NOT NULL, file_name TEXT, format TEXT NOT NULL,
  row_count INTEGER NOT NULL, new_count INTEGER NOT NULL, duplicate_count INTEGER NOT NULL, matched_count INTEGER NOT NULL,
  first_date TEXT, last_date TEXT,
  created_by TEXT NOT NULL REFERENCES users(id), created_at TEXT NOT NULL, undone_at TEXT
);
-- The same file imports once, unless that import was undone.
CREATE UNIQUE INDEX import_batches_file_idx ON import_batches(account_id, file_sha256) WHERE undone_at IS NULL;
CREATE INDEX import_batches_account_idx ON import_batches(account_id, created_at);
CREATE TABLE import_rows (
  batch_id TEXT NOT NULL REFERENCES import_batches(id), row_index INTEGER NOT NULL,
  fingerprint TEXT NOT NULL, transfer_id TEXT REFERENCES transfers(id),
  status TEXT NOT NULL CHECK (status IN ('new', 'duplicate', 'decided_duplicate', 'matched', 'confirmed_leg')),
  PRIMARY KEY (batch_id, row_index)
);

-- Transfers waiting to be categorised: imported rows still posted against an uncategorized account.
CREATE TABLE inbox_items (
  transfer_id TEXT PRIMARY KEY REFERENCES transfers(id),
  bank_account_id TEXT NOT NULL REFERENCES accounts(id),
  current_account_id TEXT NOT NULL REFERENCES accounts(id),
  revision INTEGER NOT NULL DEFAULT 0,
  suggested_account_id TEXT REFERENCES accounts(id), suggestion_source TEXT, suggestion_confidence INTEGER,
  candidate_pending_id TEXT REFERENCES transfers(id),
  merchant_key TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('open', 'done', 'paired', 'dismissed')),
  created_at TEXT NOT NULL, updated_at TEXT NOT NULL
);
-- Which bank-account sides of a manual entry a statement has confirmed. A move between two
-- statement-backed accounts is confirmed by a row on each; the first posts the entry, the second
-- only confirms it.
CREATE TABLE statement_legs (
  entry_id TEXT NOT NULL REFERENCES transfers(id),
  account_id TEXT NOT NULL REFERENCES accounts(id),
  batch_id TEXT NOT NULL REFERENCES import_batches(id),
  PRIMARY KEY (entry_id, account_id)
);
-- Every transfer a triage action posted, against each inbox row it was about (a pair is about
-- two), so undoing an import reverses exactly those and a retry finds what it did.
CREATE TABLE triage_transfers (
  item_transfer_id TEXT NOT NULL REFERENCES inbox_items(transfer_id),
  transfer_id TEXT NOT NULL REFERENCES transfers(id),
  PRIMARY KEY (item_transfer_id, transfer_id)
);
CREATE INDEX triage_transfers_transfer_idx ON triage_transfers(transfer_id);
CREATE INDEX inbox_items_open_idx ON inbox_items(status, bank_account_id);
CREATE INDEX inbox_items_merchant_idx ON inbox_items(merchant_key, status);

CREATE TABLE categorisation_rules (
  id TEXT PRIMARY KEY, owner_kind TEXT NOT NULL, owner_id TEXT NOT NULL,
  pattern TEXT NOT NULL, direction TEXT CHECK (direction IN ('in', 'out')),
  min_amount INTEGER, max_amount INTEGER,
  bank_account_id TEXT REFERENCES accounts(id),
  category_account_id TEXT NOT NULL REFERENCES accounts(id),
  tags TEXT NOT NULL DEFAULT '', shared INTEGER NOT NULL DEFAULT 0 CHECK (shared IN (0, 1)),
  auto_apply INTEGER NOT NULL DEFAULT 0 CHECK (auto_apply IN (0, 1)),
  created_at TEXT NOT NULL
);
CREATE INDEX categorisation_rules_owner_idx ON categorisation_rules(owner_kind, owner_id);
|}
    );
    ( 4,
      {|
-- Web sessions. Only a hash of the session token is stored, so a copy of the database can't be
-- used to sign in.
CREATE TABLE sessions (
  token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), csrf_token TEXT NOT NULL,
  created_at TEXT NOT NULL, last_seen_at TEXT NOT NULL, expires_at TEXT NOT NULL
);
CREATE INDEX sessions_user_idx ON sessions(user_id);
CREATE TABLE login_attempts (
  state_hash TEXT PRIMARY KEY, code_verifier TEXT NOT NULL, nonce TEXT NOT NULL, return_to TEXT NOT NULL,
  created_at TEXT NOT NULL
);
|}
    );
    ( 5,
      {|
-- What a user did with an attention card: snoozed it until a date, or resolved it (with a note,
-- e.g. the budget status that was acknowledged, so a worse one shows the card again).
CREATE TABLE attention_cards (
  user_id TEXT NOT NULL REFERENCES users(id), card_key TEXT NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('snoozed', 'resolved')), until_date TEXT, note TEXT,
  updated_at TEXT NOT NULL, PRIMARY KEY (user_id, card_key)
);

-- LLM output is cached (a month summary is written once) and every call is logged, so the
-- user can see what was sent where.
CREATE TABLE llm_cache (cache_key TEXT PRIMARY KEY, content TEXT NOT NULL, model TEXT NOT NULL, created_at TEXT NOT NULL);
CREATE TABLE llm_calls (
  id INTEGER PRIMARY KEY AUTOINCREMENT, user_id TEXT NOT NULL REFERENCES users(id), purpose TEXT NOT NULL,
  item_count INTEGER NOT NULL, model TEXT NOT NULL, ok INTEGER NOT NULL, created_at TEXT NOT NULL
);
|}
    );
    ( 6,
      {|
CREATE TABLE logos (
  id INTEGER PRIMARY KEY AUTOINCREMENT, sha256 TEXT NOT NULL UNIQUE,
  content_type TEXT NOT NULL, width INTEGER NOT NULL, height INTEGER NOT NULL,
  bytes BLOB NOT NULL, source_url TEXT NOT NULL, fetched_at TEXT NOT NULL
);
CREATE TABLE merchant_logos (
  owner_kind TEXT NOT NULL CHECK (owner_kind IN ('user', 'household')),
  owner_id TEXT NOT NULL, merchant_key TEXT NOT NULL,
  source TEXT NOT NULL CHECK (source IN ('user', 'llm', 'none')),
  status TEXT NOT NULL CHECK (status IN ('resolved', 'unknown', 'failed')),
  website TEXT, logo_id INTEGER REFERENCES logos(id), confidence INTEGER CHECK (confidence BETWEEN 0 AND 100),
  resolved_at TEXT NOT NULL,
  PRIMARY KEY (owner_kind, owner_id, merchant_key)
);
CREATE INDEX merchant_logos_logo_idx ON merchant_logos(logo_id);
|}
    );
    ( 7,
      {|
CREATE TABLE advisor_conversations (
 id TEXT PRIMARY KEY, owner_id TEXT NOT NULL REFERENCES users(id), projection TEXT NOT NULL,
 version INTEGER NOT NULL CHECK(version >= 0), updated_at TEXT NOT NULL
);
CREATE INDEX advisor_conversations_owner ON advisor_conversations(owner_id, updated_at, id);
CREATE TABLE advisor_turns (
 id TEXT PRIMARY KEY, conversation_id TEXT NOT NULL REFERENCES advisor_conversations(id) ON DELETE CASCADE,
 ordinal INTEGER NOT NULL CHECK(ordinal > 0), user_message_id TEXT NOT NULL UNIQUE, user_text TEXT NOT NULL,
 UNIQUE(conversation_id, ordinal)
);
CREATE TABLE advisor_attempts (
 id TEXT PRIMARY KEY, turn_id TEXT NOT NULL REFERENCES advisor_turns(id) ON DELETE CASCADE,
 attempt_ordinal INTEGER NOT NULL CHECK(attempt_ordinal > 0), assistant_message_id TEXT NOT NULL UNIQUE,
 UNIQUE(turn_id, attempt_ordinal)
);
CREATE TABLE advisor_runs (
 id TEXT PRIMARY KEY, attempt_id TEXT NOT NULL UNIQUE REFERENCES advisor_attempts(id) ON DELETE CASCADE,
 conversation_id TEXT NOT NULL REFERENCES advisor_conversations(id) ON DELETE CASCADE,
 owner_id TEXT NOT NULL REFERENCES users(id), originating_session TEXT NOT NULL, model TEXT NOT NULL,
 protocol_version INTEGER NOT NULL DEFAULT 1, prompt_version INTEGER NOT NULL DEFAULT 1,
 started_at TEXT NOT NULL, updated_at TEXT NOT NULL, terminal_at TEXT,
 state TEXT NOT NULL CHECK(state IN ('queued','awaiting','streaming','tools','cancelling','completed','cancelled','failed')),
 failure TEXT, output_bytes INTEGER NOT NULL DEFAULT 0 CHECK(output_bytes >= 0), model_rounds INTEGER NOT NULL DEFAULT 0 CHECK(model_rounds >= 0),
 tool_calls INTEGER NOT NULL DEFAULT 0 CHECK(tool_calls >= 0), evidence_bytes INTEGER NOT NULL DEFAULT 0 CHECK(evidence_bytes >= 0),
 journal_bytes INTEGER NOT NULL DEFAULT 0 CHECK(journal_bytes >= 0), last_sequence INTEGER NOT NULL DEFAULT 0 CHECK(last_sequence >= 0),
 replay_floor INTEGER NOT NULL DEFAULT 0 CHECK(replay_floor >= 0),
 CHECK((terminal_at IS NOT NULL) = (state IN ('completed','cancelled','failed'))),
 CHECK((failure IS NOT NULL) = (state = 'failed'))
);
CREATE UNIQUE INDEX advisor_runs_active_owner ON advisor_runs(owner_id) WHERE terminal_at IS NULL;
CREATE UNIQUE INDEX advisor_runs_active_conversation ON advisor_runs(conversation_id) WHERE terminal_at IS NULL;
CREATE TABLE advisor_segments (
 run_id TEXT NOT NULL REFERENCES advisor_runs(id) ON DELETE CASCADE, id TEXT NOT NULL,
 ordinal INTEGER NOT NULL, projection TEXT NOT NULL, PRIMARY KEY(run_id,id), UNIQUE(run_id,ordinal)
);
CREATE TABLE advisor_evidence (
 run_id TEXT NOT NULL REFERENCES advisor_runs(id) ON DELETE CASCADE, id TEXT NOT NULL,
 ordinal INTEGER NOT NULL, projection TEXT NOT NULL, PRIMARY KEY(run_id,id), UNIQUE(run_id,ordinal)
);
CREATE TABLE advisor_events (
 run_id TEXT NOT NULL REFERENCES advisor_runs(id) ON DELETE CASCADE, sequence INTEGER NOT NULL CHECK(sequence > 0),
 projection TEXT NOT NULL, PRIMARY KEY(run_id, sequence)
);
-- No transcript foreign keys: deletion retains only fingerprints and accepted IDs for 24 hours.
CREATE TABLE advisor_requests (
 owner_id TEXT NOT NULL REFERENCES users(id), key TEXT NOT NULL, operation TEXT NOT NULL,
 fingerprint TEXT NOT NULL, conversation_id TEXT NOT NULL, accepted TEXT NOT NULL, deleted_at TEXT,
 PRIMARY KEY(owner_id,key)
);
CREATE TABLE advisor_user_consent (user_id TEXT PRIMARY KEY REFERENCES users(id), enabled INTEGER NOT NULL CHECK(enabled IN (0,1)));
CREATE TABLE advisor_household_consent (household_id TEXT PRIMARY KEY REFERENCES households(id), enabled INTEGER NOT NULL CHECK(enabled IN (0,1)));
CREATE TABLE advisor_account_aliases (
 conversation_id TEXT NOT NULL REFERENCES advisor_conversations(id) ON DELETE CASCADE,
 account_id TEXT NOT NULL REFERENCES accounts(id), alias TEXT NOT NULL,
 PRIMARY KEY(conversation_id,account_id), UNIQUE(conversation_id,alias)
);
|}
    );
    ( 8,
      {|
-- Notes the user writes for the advisor. A row exists only while the user keeps it; forgetting deletes it.
CREATE TABLE advisor_memories (
 id TEXT PRIMARY KEY, owner_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
 text TEXT NOT NULL CHECK(length(text) BETWEEN 1 AND 1120), created_at TEXT NOT NULL, updated_at TEXT NOT NULL
);
CREATE INDEX advisor_memories_owner ON advisor_memories(owner_id, created_at, id);
|}
    );
    ( 9,
      {|
-- A bank account whose confident history and LLM category suggestions are applied on import without
-- asking. A row exists only while the setting is on; no row means every suggestion waits in the inbox.
CREATE TABLE account_auto_apply (
 account_id TEXT PRIMARY KEY REFERENCES accounts(id),
 min_confidence INTEGER NOT NULL CHECK(min_confidence BETWEEN 50 AND 100)
);
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
