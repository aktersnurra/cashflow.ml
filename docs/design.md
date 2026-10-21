# cashflow — design

Status: draft for review. Nothing here is implemented yet. Open questions are collected in §17.

## 1. Goals

A personal and household cashflow ledger with:

- an overview of income, spending and savings across personal and shared bank accounts;
- double-entry bookkeeping that follows TigerBeetle's rules (immutable transfers, posted/pending balances, linked transfers, idempotent writes);
- CSV import from the bank as the main way data gets in, with duplicates ignored automatically;
- a Bonsai web UI in a minimal, keyboard-first style (Linear is the reference), whose landing page shows what needs attention right now;
- an MCP server, used mainly for analytics but also able to add transactions, for Claude and OpenAI clients;
- PocketID (OIDC) authentication, hosted on the NUC next to `kcal`.

Non-goals for v1: live bank sync (designed for in §6.6, not built), tracking investment holdings per security, more than one household per user, tax reporting.

## 2. Architecture

One OCaml binary, `cashflow`, with the same shape as `kcal`:

```
                 Caddy (TLS)
                     │
             cashflow serve  ── SQLite (WAL)
            ┌────────┼─────────────┬──────────────┐
          /mcp     /api/*      /auth/*        /  (static)
       JSON-RPC   JSON for     OIDC code      Bonsai bundle
       bearer     the web UI   flow + session (js_of_ocaml)
            └────────┴─────┬───────┘
                       Service layer  (authorisation, use cases)
                           │
                  Ledger core (pure)  +  Store (SQLite)
```

- **Server:** OCaml 5.5, `eio_posix`, SQLite, forward-only SQL migrations, `logs` (all as in kcal).
- **Client:** Bonsai + `virtual_dom`, compiled with js_of_ocaml and served as static files by the same binary. The Jane Street dependency stack (`core`, `async_js`) is limited to the client build.
- **Shared library `cashflow_protocol`:** request/response types and their JSON codecs, compiled for both native and js_of_ocaml, so the server and the UI can't drift apart. It contains no I/O and no Eio code.
- **Service layer:** MCP and `/api` are two thin adapters over the same service functions. Every use case (post a transfer, import a file, get the spending breakdown) exists once.
- **Ledger core:** pure functions that validate a batch of transfers against account state and return either new balances or a list of errors. The store applies the result inside a single SQLite transaction. This is the part that copies TigerBeetle, and it can be tested exhaustively without a database.
- External calls (the OpenRouter LLM, and later a bank-sync provider) are injected as records of functions, like `Withings.request` in kcal.

The generic pieces of kcal (OIDC verification, MCP dispatch, HTTP adapter, logging, migrations, date and timezone handling) are copied into cashflow for now, each copy marked with a `(* copied from kcal: … *)` header. They get improved here as needed, and are extracted into shared libraries once both copies have settled. That extraction is a later topic.

### 2.1 Using OCaml's type system

OCaml's advanced features are used where they rule out a class of bugs or remove duplicated code, and nowhere else. Each entry below names what it buys.

**GADTs**
- **Typed analytics queries.** `type _ query = Spending_by_category : Range.t * Scope.t -> Category_total.t list query | Net_worth : … -> Net_worth_point.t list query | …`, with a single `run : type a. a query -> a` and `codec : type a. a query -> a Jsont.t`. The MCP tool table, the `/api` routes and the UI's calls are all generated from this one type. Adding an analytics function is one constructor, and the compiler points out every place that still needs it.
- **Typed SQL statements.** `('params, 'row) Stmt.t` is a thin GADT layer of about 200 lines over `sqlite3`, the binding kcal already uses. Parameters and result rows are typed lists of fields (`int64 @> text @> date @> nil`), so the types of a query's parameters and result rows are checked when the code compiles, and running it needs neither Lwt nor Caqti. SQL is written by hand, because the analytics need SQLite-specific window functions and CTEs. Petrol was considered and rejected: it depends on Lwt and on Caqti below 3.0, its last release was in 2023, and it would bring a second concurrency library into an Eio program.
- **Card state machines.** `type (_, _) transition = Upload : (stale, uploading) transition | Preview : (uploading, preview) transition | …`. An attention card (§11.3) can only move along transitions that exist, and a card that ends in a terminal state can't go anywhere else.
- **Not for the pending lifecycle.** Indexing transfers by their status (`[ `Pending | `Posted | `Voided ]`) was considered and dropped. A transfer's status lives in the store and changes as other transfers in the same batch resolve it, so a type index would only move the same runtime check into a lookup function. The core checks the status once, at the point where it applies the event (§3.3, rule 5), and the property test checks that no pending transfer is resolved twice.

**Abstract and phantom types**
- **IDs:** each ID type comes from the generative functor `Id.Make ()`, so `Account_id.t` and `Transfer_id.t` can't be swapped by mistake.
- **Money:** `Amount.t` is a private `int64` built only by smart constructors; there are no floats.
- **Validation state:** `Ledger_core.applied` is abstract and only `Ledger_core.apply` can construct one, so a store can only ever write transfers and balances that passed every rule ("parse, don't validate").
- **Authorisation as a capability:** `Access.t` is a proof that the user may use an owner scope. Only the service layer's authorisation check can create one, and every store function requires one, so a call that skips authorisation doesn't compile.

**Functors and first-class modules**
- **Two store implementations:** `Ledger.Make (Store : STORE)` runs against SQLite in production and against an in-memory store in property tests.
- **Import formats:** a registry of first-class modules `(module Statement_format.S)`, so adding a bank is one module.
- **Pay cycles:** `Cycle.Make (Holidays : HOLIDAYS)`, so the Swedish calendar is one implementation among others.
- **Effects at the edges:** the LLM and the clock are passed in as module or record arguments, so tests never touch the network or real time.

**Higher-order functions and composition**
- **The import pipeline** is a chain of `'a -> ('b, error) result` stages composed with `let*` and Kleisli composition (`>=>`): `parse >=> match_account >=> fingerprint >=> dedupe >=> match_pending >=> categorise >=> post`. Each stage can be tested and reused on its own, and bank sync (§6.6) will reuse everything after `parse`.
- **Category suggestions** are suggester functions combined with a first-success operator (`<|>`): rules, then history, then the LLM.
- **Categorisation rules** are compiled once into closures, instead of being interpreted again for every row.
- **HTTP middleware:** `handler -> handler` layers for logging, sessions, CSRF checks and scope checks, combined with `@@`.

**Polymorphic variants for errors.** Each stage declares exactly which errors it can return, for example ``[> `Exists_with_different_fields | `Linked_event_failed ]``. The type of a composed pipeline is the union of its stages' errors, and the adapter's final `match` is checked for exhaustiveness.

**Tests**
- **[Hegel](https://ocaml.org/p/hegel/latest/README.html)** is the property-testing library: it is built on Hypothesis's engine and shrinks failures automatically. Tests use `let%hegel_test` from `ppx_hegel_test` and run under Alcotest.
- **Model-based ledger test:** a `module%hegel_state_machine` (Hegel's `Stateful`) applies random rules to `Ledger.Make (Mem_store)`. The rules submit random batches (linked chains, pending transfers, posts and voids, reversals, unknown ids, keys from a small pool so they collide), retry batches that succeeded, and void expired pending transfers. After every step it checks the invariants: balances match `verify`, total debits equal total credits, balance limits hold, no pending transfer is resolved or reversed twice, chains are all-or-nothing, and the store holds exactly the transfers reported as created. Hegel shrinks a failure down to the shortest sequence of steps that reproduces it. Planting a bug in a partial post, or in rolling back a failed chain, is caught within a few steps.
- **Platform note:** Hegel loads a native `libhegel`, which is published for Linux and macOS (Apple Silicon) only. Tests therefore run on the development machine and in CI, not on the FreeBSD NUC; the production binary doesn't depend on Hegel. CI pins the `libhegel` version and sets `HEGEL_LIBHEGEL_NO_DOWNLOAD=1` once it is cached.
- **Invariants:** balances equal the sum of transfers, replaying a batch changes nothing, and posted balances never break an account's flags.

**Not used:** OCaml 5 effect handlers are not used directly beyond what Eio provides, and there are no ppx-heavy DSLs. JSON codecs use `jsont` combinators, which work on both the server and in js_of_ocaml.

## 3. Ledger core (TigerBeetle rules)

### 3.1 Currencies

| field | notes |
|---|---|
| `code` | ISO 4217, e.g. `SEK`, `EUR` |
| `scale` | number of decimals in the minor unit: 2 for SEK and EUR, 0 for JPY |

An account's currency is set when the account is created and never changes. A transfer must use the same currency on both accounts; this plays the role of TigerBeetle's `ledger` field. Users and households each have a configurable default currency. Nothing in the code assumes SEK.

Amounts are stored as `int64` minor units (öre, cent). TigerBeetle uses u128, but int64 covers ±92 quadrillion öre. Amounts are never floats, not in the database, the API, or the UI.

### 3.2 Accounts

| field | notes |
|---|---|
| `id` | UUIDv7 |
| `owner` | `User of user_id` or `Household of household_id` (§4) |
| `type` | `Asset` · `Liability` · `Equity` · `Income` · `Expense` |
| `currency` | §3.1 |
| `name` | display name, e.g. "Handelsbanken lönekonto", "Mat & hushåll" |
| `institution`, `number_last4` | optional; only the last four digits are stored, displayed as `****1234`, and the full account number is never kept |
| `flags` | `debits_must_not_exceed_credits`, `credits_must_not_exceed_debits`, `closed` |
| `tags` | free-form labels |
| `debits_posted`, `credits_posted`, `debits_pending`, `credits_pending` | maintained by the core |

Every account has a normal balance side. Asset and Expense balances are debits minus credits; Liability, Equity and Income balances are credits minus debits. The UI and the analytics only ever show signed balances computed this way, so nobody has to think in debits and credits.

Category accounts (Groceries, Rent, Salary) are ordinary Expense and Income accounts. A new user or household starts from a seeded chart of accounts, named in the creator's language (§11.4) and renamable afterwards:

- `Equity:Opening balances`
- `Expense:Uncategorized`, the suspense account for imports (§6.4)
- `Equity:Unrealized gains` (§3.7)
- a starter set of categories

### 3.3 Transfers

| field | notes |
|---|---|
| `id` | UUIDv7, assigned by the server |
| `idempotency_key` | required; unique per owner scope (§3.5) |
| `debit_account_id`, `credit_account_id` | must differ and must share a currency |
| `amount` | > 0, minor units |
| `kind` | `Posted` · `Pending` · `Post_pending` · `Void_pending` |
| `pending_id` | set for `Post_pending` and `Void_pending` |
| `linked` | flag: this transfer and the next one in the batch succeed or fail together (TigerBeetle's `linked` flag) |
| `value_date` | calendar date with no time or zone (§9) |
| `recorded_at` | UTC instant, set by the server |
| `description`, `counterparty` | text as it appears on the bank statement, plus an optional cleaned-up merchant name |
| `tags` | free-form labels |
| `code` | small integer for the purpose: `purchase`, `income`, `contribution`, `reclassify`, `reversal`, `revaluation`, `opening`, `settlement`, `adjustment` |
| `reverses_id` | set for reversals |
| `source` | `Manual of user_id` · `Mcp of client` · `Import of batch_id` |

**Rules the core enforces.** A batch is rejected item by item with a TigerBeetle-style error per transfer.

1. Both accounts exist and are open, they share a currency, they are different accounts, and `amount > 0`.
2. The posting user may write to both accounts (§4.3).
3. Balance flags hold after applying the transfer. For example, a debit card's checking account can't go below zero if its flag says so. The check covers posted plus pending debits.
4. A chain of transfers marked `linked` is applied atomically. If one fails, every transfer in the chain gets `linked_event_failed`, as in TigerBeetle.
5. A `Pending` transfer moves its amount into the pending columns. `Post_pending` may post up to the pending amount, so a partial capture releases the rest. `Void_pending` releases all of it. A pending transfer can be resolved exactly once. Pending transfers may carry an `expires_on` date, after which they are voided automatically.
6. **Immutability:** transfers are never updated or deleted. Corrections are made with new transfers:
   - `reverse(transfer)` posts the mirror transfer with `code=reversal` and `reverses_id` set;
   - `correct(transfer, changes)` is a linked pair: the reversal plus a new transfer;
   - `reclassify(transfer, category)` is a linked transfer from the old category to the new one, with the same `value_date`, used to categorise an import (§6.4).
7. Balances are stored on the account rows and updated in the same SQLite transaction as the transfer rows. `cashflow verify` recomputes every balance from the transfers and fails loudly if any differ.

### 3.4 Batches

Each write takes a batch of transfers, at most 8 190 as in TigerBeetle, though in practice an import has a few hundred rows. Validation and application run in a single `BEGIN IMMEDIATE` transaction. The result is a list of `(index, error)` pairs for the transfers that failed. Unlinked transfers in a batch are independent of each other.

### 3.5 Idempotency

The idempotency key underpins every write.

- **Key scope:** keys are unique per owner scope, either a user or a household.
- **Same key, same fields:** returns the existing transfer with `status: exists` (not an error). This makes LLM retries and re-uploads safe.
- **Same key, different fields:** rejected with `exists_with_different_fields`, TigerBeetle's own name for this error.
- **Where keys come from:**
  - MCP and API clients must supply one (`add_transaction` requires it);
  - imports derive it from the row fingerprint (§6.3);
  - the server derives keys for the transfers it creates itself, such as reversals and revaluations, from their inputs.

### 3.6 Reconciliation

A checkpoint records `(account, date, statement_balance)` together with the ledger balance at that date.

- **Matching balances:** the period up to that date counts as reconciled.
- **Mismatches:** the difference is shown, and the user can post an `adjustment` transfer against `Expense:Uncategorized`. Nothing is ever adjusted silently.
- **Imports:** CSV imports that include a running-balance column create checkpoints automatically (§6.5).

Analytics report, for each period, whether it is reconciled, and the UI marks periods that aren't (§10).

### 3.7 Investments (occasional)

Investment accounts, such as Avanza and Montrose, are Asset accounts. In v1 they get no CSV import, only transfers to and from checking, plus occasional revaluations.

- **Contributions and withdrawals** are ordinary transfers to and from checking.
- **Market value:** `set_market_value(account, value, date)` posts the difference between the current ledger balance and the new value, with `code=revaluation`, against `Equity:Unrealized gains`.
- **Views:** net worth includes revaluations; the savings rate and the spending analytics exclude them.

Individual holdings are out of scope.

### 3.8 Cash

Each person who uses cash gets an Asset account, `Cash (<name>)`, which has no bank file behind it.

- **ATM withdrawals:** in the bank CSV, a withdrawal is categorised as a transfer from checking to the cash account. The rules (§7) recognise `Uttag`/ATM rows automatically.
- **Cash spending:** entered by hand, usually from the phone's Add sheet. These entries are posted immediately, since there is no bank to confirm them.
- **Reconciliation:** you count your wallet now and then ("I have 340 kr"). That creates a checkpoint (§3.6), and any difference is posted to `Expense:Untracked cash` instead of `Uncategorized`. Analytics show untracked cash as its own line, so it never hides inside a real category.
- **Attention card:** if cash has been withdrawn but no cash spending has been entered for 30 days, a card suggests counting your wallet.

## 4. Ownership, households and sharing

### 4.1 Entities

- **user:** OIDC `sub`, display name, email (shown only), timezone, default currency, a `locale`, and an `is_admin` flag taken from the `groups` claim (membership of the PocketID group `cashflow-admin`).
- **household:** name, default currency, an `income_split` policy (§4.5), and a `report_split` (default 50/50) used only by the "my share" reports.
- **membership:** `(household, user, status)`, where `status` is `Invited` → `Active` | `Declined`, or `Active` → `Left`. In v1 a user belongs to at most one household.

### 4.2 Linking accounts

The admin creates a household and its accounts, or moves an existing personal account into the household, and invites users by their PocketID identity. The invited user sees an invitation card when they next sign in (§11.3). Accepting it makes them a member, which gives them access to every household-owned account. Personal accounts are never shared; sharing works by household ownership only, not per account.

### 4.3 Who can see and write what

| | own personal accounts | household accounts | another member's personal accounts |
|---|---|---|---|
| see balances and transfers | yes | yes | no |
| post | yes | yes, every active member | no |

A transfer between a personal account and a household account is visible to both sides, but the personal side is masked for other members. They see "From Gustaf", not the account name, number or balance. Analytics apply the same rules, so a household member can never infer another member's personal balances.

### 4.4 Contributions and "my" spending

Moving money from personal checking to the household account is a single asset-to-asset transfer with `code=contribution`. No double booking is needed.

- **Personal view:** income, own expenses, and *household contributions* listed as a separate outflow line, not as spending.
- **Household view:** contributions from each member come in; household expenses go out.
- **Family total:** household expenses plus personal expenses, with contributions excluded so nothing is counted twice.
- **"My share":** a report-only figure, `report_split × household expenses`. Changing the split rewrites no history.

If a member doesn't have a login yet, their contributions come from an `External: <name>` equity account until they join.

### 4.5 Contribution planner

`plan_household_contributions(target, incomes?)`:

- **Income split:** each member contributes in proportion to income, `contribution_i = target × income_i / Σ income`.
- **Incomes:** if they aren't supplied, they default to each member's net income (credits to Income accounts) for the last complete month. A member can enter an income manually instead, so their personal accounts never need to be shared.
- **Target:** either a fixed amount, or `Σ household budgets + household savings goal` (§8).
- **Rounding:** results are rounded to whole currency units, and any remainder goes to the higher earner, so the parts always add up to the target.

The UI shows the result as a card ("transfer 14 200 kr and 9 800 kr this month"), with each member's progress so far this month.

### 4.6 Paying shared costs from a personal card

During triage (§6.4), a row can be marked *shared*. That posts a linked pair of transfers:

1. debit personal `Asset:Household owes me` (a receivable), credit personal `Expense:Uncategorized`: this clears the suspense entry, and the expense is not counted as mine;
2. debit household `Expense:<category>`, credit household `Liability:Owed to <member>`: the expense appears in the household books.

The two transfers belong to different owners but are linked, so either both are posted or neither is.

To settle up, a transfer from the household account to personal checking clears both the receivable and the liability (again a linked pair). The UI shows a balance card ("Household owes you 1 240 kr", with a Settle button) and a list of the open items.

## 5. Authentication and authorisation

- **Issuer:** PocketID is the OIDC provider.
- **MCP:** bearer tokens verified as in kcal, and the RFC 9728 metadata is published at `/.well-known/oauth-protected-resource`.
- **Web UI:** authorization code flow with PKCE, handled by the server (the backend-for-frontend pattern). The browser only ever holds an `HttpOnly; Secure; SameSite=Lax` session cookie and never sees a token. Every state-changing `/api` call requires a CSRF token in a header. Sessions are stored in SQLite and expire after 30 days of inactivity.
- **Scopes** (for MCP; web sessions get all scopes the user is allowed):
  - `ledger:read`
  - `ledger:write`: post, reverse, import, categorise, budgets
  - `ledger:admin`: create households, link accounts, close accounts; also requires `is_admin`
- **Authorisation:** checked in the service layer on every call, never only in the adapters.

## 6. Import

### 6.1 Flow

```
drop CSV ─▶ detect format ─▶ parse rows ─▶ match account ─▶ fingerprint & dedupe
        ─▶ preview (n new · m duplicates · balance check) ─▶ confirm
        ─▶ post new rows against Expense:Uncategorized ─▶ auto-categorise ─▶ Inbox
```

- **Who can import:** imports can be run from the UI (drag a file anywhere on the page) or over MCP (`import_statement`, which takes the file content).
- **Import batches:** each import creates an `import_batch` with the file's SHA-256 hash, its format, the account, and counts. Uploading the same file a second time is a no-op that the UI reports as such.
- **Undoing an import:** `undo_import(batch)` reverses every transfer the batch created.

### 6.2 Formats

There is one parser module per format, behind a common signature (`detect`, `parse`, `account_hint`). The first one is Handelsbanken. I need an anonymised sample of its export, because the columns, the date format, the decimal separator and the encoding all decide the parser (§17). Avanza and Montrose imports are out of scope for v1. Adding one later means writing one format module: it maps deposits, withdrawals, dividends and fees to transfers, ignores individual trades, and reuses the rest of the pipeline.

### 6.3 Row fingerprint and duplicates

`fingerprint = sha256(account_id ‖ value_date ‖ amount ‖ normalised description ‖ running_balance ‖ occurrence)`

- **The running balance makes rows unique:** two identical coffees on the same day have different balances after them.
- **Fallback:** when a format has no running-balance column, `occurrence` (the row's position among otherwise identical rows that day within the file) keeps rows apart.
- **Idempotency:** the fingerprint is used as the idempotency key, so a row that has already been imported is skipped.
- **Overlapping exports:** files that overlap each other are therefore safe.

### 6.4 Uncategorised rows and the Inbox

New rows are posted at once against `Expense:Uncategorized` (or `Income:Uncategorized` for incoming money). The bank account's ledger balance therefore matches the bank as soon as the import finishes, and a typo in a category can't throw the balance off.

Categorising a row is a `reclassify` transfer (§3.3). The **Inbox** lists the uncategorised rows. Triage is done with the keyboard: `j`/`k` to move, `c` to pick a category, `s` to mark the row shared, `t` to tag, `x` to mark it as a transfer between own accounts, and `enter` to accept a suggestion.

### 6.5 Automatic reconciliation

When the file has a running balance, the last row of the import gives a reconciliation checkpoint (§3.6). If the ledger disagrees, the preview reports "ledger is 312 kr off from the bank on 2026-10-18" before anything is posted.

### 6.6 Bank sync later

Handelsbanken's PSD2 API is only open to licensed providers. Enable Banking's free restricted-production mode, which covers only accounts you link yourself, is the realistic route; whether it supports Handelsbanken still has to be checked. A sync source would produce the same rows as a parser, keyed by the bank's own transaction ID, and feed the same pipeline. Nothing in §6.1–6.5 has to change for it.

### 6.7 Manual entries the bank confirms later

Someone can enter spending on a bank account by hand, through the Add sheet or `add_transaction` over MCP, before the CSV that contains it has been imported. That would be counted twice if the import simply posted it again. Instead, a manual entry against an account that has a bank file behind it uses TigerBeetle's two-phase transfers:

1. **When entered,** the manual entry is posted as a `Pending` transfer, already in its category: "expected, not yet confirmed by the bank". The account's posted balance still matches the bank, and the pending columns show what is expected.
2. **On import,** each new row is first matched against pending transfers on the same account, with the same direction, the same transaction date (the date of purchase, not the bank's booking date) and the same amount. A row with the same date but a different amount (tips, currency conversion) is never matched automatically. It goes to the Inbox with the pending entry offered as a candidate.
   - With exactly one candidate, the import posts that pending transfer (`Post_pending`) for the bank's amount and date. It uses the category from the manual entry and doesn't add a new row to the Inbox.
   - With several candidates, the row goes to the Inbox with the candidates attached, and the user picks one.
3. **Unmatched entries:** if a pending entry has still not been matched once an import has covered its date plus seven days, a "Not found on the bank statement" card asks you to void it, keep waiting, or move it to Cash.

Card reservations from the bank (§3.3, rule 5) use the same mechanism. A pending transfer means "not yet confirmed by the bank", whoever created it.

If the bank amount is larger than the pending amount, the import posts the full pending amount, and the difference goes to the same category as a second, linked transfer.

## 7. Categorisation

Suggestions come from three sources, in order:

1. **Rules:** `(match on description or counterparty, optional amount range, account) → category, tags, shared`. When the same merchant has been categorised the same way three times, the UI offers to create a rule.
2. **History:** the category this exact cleaned-up merchant name was given last time.
3. **LLM** (optional, §12): a batch of uncategorised rows, together with the list of category names, is sent to OpenRouter, which returns `{row, category, confidence}`.

Suggestions are never posted automatically, except by rules the user has explicitly set to auto-apply. Everything else waits in the Inbox with a suggested category ready to accept with `enter`.

Transfers between your own accounts, for example checking to Avanza, are recognised by matching amounts on both sides within ±3 days and offered as a single `transfer` pairing that replaces the two uncategorised entries.

## 8. Budgets

UX best practice is monthly envelopes with history that never changes. In more detail:

- **A budget** is `(owner, category, monthly_amount, effective_from_month)`. Editing a budget creates a new version from a chosen month onward, so past months keep the budget they actually had.
- **Rollover** is set per category: unspent money, or overspending, carries into next month's budget. It is on for things like clothes and off for groceries.
- **Sinking funds** spread an annual cost evenly over the months, `annual_amount / 12`, so insurance and car service don't wreck one month. The balance builds up and is drawn down when the bill is paid. (v1.1)
- **Pace** is shown instead of a bare percentage: "Groceries: 4 100 / 6 000 kr, day 21 of 31 — on pace". It is computed linearly, or, for categories with a dominant recurring payment, from when that payment usually lands.
- **Savings goals** can be several, each owned by a user or the household: `name, target_amount, target_date?, monthly_amount?`. Examples are a buffer, a vacation and a car.
  - A goal is an *earmark* on one or more savings accounts, not a ledger account of its own, so the account's balance still matches the bank. The total earmarked can't exceed the account balance, and anything left is shown as "unallocated".
  - Progress shows how much is saved, the target, whether you're on track for the target date, and the monthly amount needed to get there.
  - The monthly amounts of the household's goals go into the contribution planner's target (§4.5).

## 9. Time

- **Bank data is the truth.** Every bank account's posted balance must match the bank's CSV. A manual entry against a bank account therefore stays provisional until the bank confirms it (§6.7).
- **`value_date`:** a plain calendar date with no timezone, because bank statements give dates, not times. All analytics group transfers by `value_date`.
- **`recorded_at`:** a UTC instant, used for auditing and for "last imported" checks.
- **User timezone:** set in Settings (default `Europe/Stockholm`, using kcal's built-in table). It only decides what "today" is and how instants are displayed.
- **Month = pay cycle.** Budgets, the monthly overview and the contribution planner use a *cycle*. Each user and each household configures its own in Settings:
  - **Start:** a day of the month from 1 to 28, or *last banking day*. Day 1 gives calendar months.
  - **Adjustment:** when the start falls on a weekend or public holiday, either move it to the previous banking day (the default, which is how Swedish salaries are paid), move it to the next banking day, or don't move it.
  - **Holiday calendar:** the default is Swedish, from a built-in table like kcal's timezone table.
  - **Naming:** after the month the cycle mostly covers, or after the month it starts in.
  - **Defaults:** the 25th, previous banking day, Swedish holidays, named after the month it mostly covers, so 25 Oct – 24 Nov is "November".
  - **Changes:** changing the setting applies from the next cycle on, so past cycles keep their boundaries.
  - **Calendar months:** analytics can still group by calendar month when asked.

## 10. Analytics

These live in the service layer and are shared by the MCP and the UI. Each one takes a scope (`me` · `household` · `family`), a date range, and an optional currency for the result.

| function | output |
|---|---|
| `cashflow_summary(period)` | income, spending, contributions, savings and savings rate per month, plus a `reconciled` flag per month |
| `spending_by_category(range, compare_to?)` | totals per category and the change against the comparison range |
| `category_trend(category, months)` | one total per month |
| `net_worth_series(range, step)` | assets minus liabilities over time, with revaluations shown separately |
| `budget_status(month)` | per category: budget, actual, pace, rollover |
| `recurring_payments()` | detected subscriptions and fixed bills: merchant, cadence, typical amount, next expected date, and changes such as "Spotify went up 20 kr" |
| `largest_transactions(range, n)` | the n largest transactions |
| `runway()` | liquid assets divided by average monthly spending over the last three months |
| `contribution_plan(target?)` | §4.5 |
| `settle_up_status()` | §4.6 |

When the ledger has no data for a period, these return `null` with a reason, not zero (the kcal convention). An unreconciled month is returned with a flag rather than hidden.

## 11. Web UI

### 11.1 Style

The UI follows Linear's style:

- **Look:** a dense, quiet layout using a system font stack, one accent colour, and light and dark themes.
- **Navigation:** a command palette (`⌘K`) for every action and page, keyboard shortcuts everywhere (shown in tooltips), and optimistic updates.
- **Charts:** hand-written SVG through `virtual_dom`'s SVG module (sparklines, bars, an income→spending flow), with no JavaScript chart library.

The phone is a first-class target, designed mobile-first:

- below 768 px, a bottom tab bar (Home · Inbox · Add · Budgets · More) replaces the sidebar;
- in the Inbox, cards are swiped right to accept a suggestion and left to choose a category;
- **Add** is a quick-entry sheet for cash and for spending you want logged before the bank file arrives (§6.7);
- the app can be installed as a PWA (manifest plus icon; no offline writes in v1).

On desktop, the same pages add the sidebar, the command palette and keyboard shortcuts.

### 11.2 Pages

| page | contents |
|---|---|
| **Home** | the attention feed (§11.3) above the month overview |
| **Inbox** | uncategorised and suggested rows, triaged from the keyboard |
| **Accounts** | grouped by owner and type; balance, sparkline, last imported, reconciliation state |
| **Account** | transfer list (reversals shown inline with their originals), CSV drop zone, reconciliation history |
| **Budgets** | envelopes for this cycle, editing with effective dates, rollover, savings goals |
| **Household** | members, contribution plan and progress, settle-up, household spending |
| **Insights** | the analytics in §10 with selectable range and scope |
| **Settings** | timezone, currency, cycle start day, rules, LLM on or off |
| **Admin** | households, invitations, linking accounts (admins only) |

### 11.3 Attention engine (the landing page's state machine)

Home is a ranked feed of cards. The server works out which cards to show from facts about the user's data, and each card has a small state machine of its own. The ranking is deterministic, so it is fast, testable and predictable; the LLM only writes text (§12).

**Facts:**

- the last import date per account
- the number of Inbox rows
- failed reconciliation checkpoints
- pending invitations
- pending transfers past their expiry date
- budget lines over pace
- open settle-up balance
- contribution progress
- whether the previous month has been reviewed
- new or changed recurring payments
- recurring payments still due before payday, against the paying account's balance
- spending in categories without a budget compared with the three cycles before

**Cards, from highest priority to lowest:**

| card | appears when | state machine |
|---|---|---|
| Invitation | membership is `Invited` | `Shown → Accepted / Declined` |
| Reconciliation mismatch | a checkpoint failed | `Shown → Adjusted / Investigating (snoozed)` |
| Import | any account has had no import for more than N days (default 7) | `Stale → Uploading → Preview → Imported(n) → Inbox` handoff |
| Inbox | Inbox count > 0 | `Pending(n) → Triaging → Clear` (then the card disappears) |
| Short before payday | the recurring payments expected from an account before payday (including any up to five days overdue) are more than what is left in it after pending reservations | `Shown → Done / Snoozed` |
| Month review | a new month has started and the previous one hasn't been reviewed | `Ready → Reviewing → Reviewed` |
| Budget alert | a category is more than 15% over pace | `Shown → Acknowledged` (stays acknowledged until the category gets worse) |
| Unusual spending | a category without a budget has already spent 1.5 times what a whole cycle usually costs (the average of the three cycles before, where it appears in at least two), and at least 100 in the currency more; the LLM adds a sentence on which transactions drove it | `Shown → Done / Snoozed` |
| Contributions | it's the cycle start and contributions haven't been transferred | `Due → Partially done → Done` |
| Settle-up | open balance > 0 | `Open → Settled` |
| Unmatched entry | a pending manual entry wasn't found on the bank statement (§6.7) | `Shown → Voided / Waiting / Moved to cash` |
| Count your cash | cash withdrawn, but no cash entries for 30 days (§3.8) | `Shown → Counted / Snoozed` |
| Goal off track | a savings goal is behind its target date | `Shown → Adjusted / Acknowledged` |
| Recurring change | a new subscription or a price change was detected | `Shown → Kept / Flagged` |
| Overview | always, as the last card | — |

Example: you open the app after ten days away. The Import card is at the top. You drop the file, and the card goes from preview to "Imported 43 · 38 auto-categorised", then hands over to the Inbox card ("5 to review"). When the Inbox is empty, that card is gone and Overview is at the top.

Every card can be snoozed. Card state lives on the server, so the feed looks the same on every device and over MCP (`get_attention_feed`, so Claude can say "you have 5 uncategorised rows and you're over on restaurants").

### 11.4 Translations (Unicode MessageFormat 2)

Messages use Unicode MessageFormat 2 (MF2), the successor to ICU MessageFormat, which Unicode made stable as part of CLDR/LDML. MF2 standardises the syntax of a single message. It does not yet have a settled standard file format for collections of messages, so cashflow uses a small file format of its own (below).

**Messages** have stable IDs rather than English text as the key:

```
# locales/en.mf2
inbox.title = Inbox
inbox.count =
  .input {$n :integer}
  .match $n
  one {{{$n} row to review}}
  *   {{{$n} rows to review}}
budget.pace = {$spent :currency} of {$budget :currency}, {$status}

# locales/sv.mf2
inbox.count =
  .input {$n :integer}
  .match $n
  one {{{$n} rad att granska}}
  *   {{{$n} rader att granska}}
```

**Compiled to typed OCaml.** A dune rule runs a generator, `mf2c`, over `locales/*.mf2` and produces a module `Msg` with one function per message. The function's arguments come from the message's variables:

```ocaml
val inbox_count : n:int -> Locale.t -> string
val budget_pace : spent:Money.t -> budget:Money.t -> status:string -> Locale.t -> string
```

The build then fails if:
- a message ID is used in code but missing from a locale;
- two locales disagree on a message's variables or their types;
- a `.match` has no `*` fallback, or is missing a plural category that the locale needs.

A translation bug becomes a type error instead of a blank label in production.

**Plural rules and formatting.** The CLDR plural rules for `en` and `sv` (`one` for the integer 1, `other` for everything else) are part of the generator. So are the formatting functions MF2 messages use: `:integer`, `:number`, `:currency`, `:date`.
- `sv`: `1 240,50 kr`, `25 okt.`
- `en`: `SEK 1,240.50`, `Oct 25`

The generator writes plain OCaml with no runtime dependencies, so the same code works on the server and in the js_of_ocaml client.

**Choosing the language:** from the user's `locale` setting, falling back to `Accept-Language` and then to `en`. MCP replies stay in English with machine-readable fields, because the agent translates for the user anyway.

**Names are data:** account and category names are stored as entered, not translated. The seeded chart of accounts is created in the creator's language.

**Tooling:** the `.mf2` files are plain text with one message per ID, so an LLM can draft new locales, and `mf2c --check` validates them in CI.

## 12. LLM features (OpenRouter)

This is optional and turned off until an API key is configured. The model can be changed in configuration.

- **Categorisation suggestions** (§7): the model receives the cleaned-up merchant name, the amount, the date and the category names. It never receives account names, numbers, balances or other members' data.
- **Narrative text** for the Month review and Overview cards ("Spending was 8% below September, mostly fewer restaurant visits; Spotify went up"). The model receives the *already computed* figures from §10, never raw transactions, and the result is cached per period.
- **Privacy:** requests use OpenRouter's provider settings to turn off data collection, and a log of every call (the prompt's purpose, the number of rows, the model) is visible in Settings.

The LLM never posts transfers and never decides which cards are shown.

## 13. MCP tools

| tool | scope | notes |
|---|---|---|
| `list_accounts` | read | owner, type, signed balance, last import, reconciled-through date |
| `query_transactions` | read | filter by date range, account, category, tag, counterparty or text; `limit` up to 500 |
| `get_transaction` | read | includes linked transfers and reversals |
| `cashflow_summary`, `spending_by_category`, `category_trend`, `net_worth_series`, `budget_status`, `recurring_payments`, `largest_transactions`, `runway`, `contribution_plan`, `settle_up_status` | read | §10 |
| `get_attention_feed` | read | §11.3 |
| `add_transaction` | write | `idempotency_key` required; simple form (amount, from account, to category, date, description), which the server turns into a transfer |
| `add_transfers` | write | raw batch, linked transfers, pending transfers |
| `post_pending`, `void_pending` | write | resolve a pending transfer |
| `reverse_transaction`, `correct_transaction` | write | the tool descriptions tell the agent to confirm with the user before calling these |
| `categorize` | write | reclassify one or more Inbox rows |
| `import_statement` | write | CSV content and account; returns the preview, then needs `confirm: true` |
| `reconcile` | write | statement balance check, optional adjustment |
| `set_market_value` | write | §3.7 |
| `set_budget` | write | §8 |
| `create_account`, `close_account`, `invite_member` | admin | |

As in kcal, an argument mistake the model can correct comes back as `isError: true` with an explanation.

## 14. Storage

The main tables in SQLite:

- `currency`, `user`, `household`, `membership`
- `account`, `transfer`, `transfer_tag`, `account_tag`
- `import_batch`, `import_row` (fingerprint → transfer)
- `rule`, `budget_version`, `reconciliation_checkpoint`
- `attention_card_state`, `session`, `llm_call_log`

`transfer` gets a unique index on `(owner_scope, idempotency_key)` and indexes on `(debit_account_id, value_date)` and `(credit_account_id, value_date)`.

Analytics are SQL aggregations over `transfer` grouped by `value_date`. With a few thousand transfers a year, no materialised views are needed in v1.

## 15. Security and privacy

- Full account numbers are never stored, and money is never shown in logs.
- Tokens and secrets are wrapped in kcal's `Secret` type, so they can't leak into logs or responses.
- What another member can see is masked server-side (§4.3).
- CSV uploads are limited in size (5 MB) and must be UTF-8 or Latin-1 encoded; they are parsed defensively and never executed.
- Backups use SQLite's `VACUUM INTO` from cron (deployed outside the repo, as kcal's is).

## 16. Milestones

| step | contents |
|---|---|
| **M1 · Ledger core** | currencies, accounts, transfers, batches, linked and pending transfers, idempotency, reversals, `verify`; property tests (balances always equal the sum of transfers; replaying a batch changes nothing) |
| **M2 · Server and MCP** | OIDC, scopes, `add_transaction`, queries, the first analytics (`cashflow_summary`, `spending_by_category`) |
| **M3 · Import** | Handelsbanken parser, fingerprints, preview and confirm, suspense account, automatic reconciliation, `undo_import` |
| **M4 · Web UI shell** | BFF login, Bonsai app, Accounts, Account, CSV drop zone, Inbox, command palette |
| **M5 · Household** | memberships, invitations, masking, contributions, planner, settle-up |
| **M6 · Budgets and insights** | budget versions, pace, rollover, recurring-payment detection, net worth, attention engine |
| **M7 · LLM** | categorisation suggestions, narrative cards |
| **Later** | sinking funds, investment-account CSVs, bank sync, extracting shared libraries from cashflow and kcal |

## 16a. Deployment notes

- **The server** builds and runs natively on FreeBSD. Eio picks its POSIX backend; io_uring is Linux-only and would make no measurable difference at this scale.
- **The web UI:** Bonsai v0.17 (the last opam release) needs OCaml 5.1, while the server uses 5.5. The UI therefore builds in a second opam switch through `dune-workspace.web`, with every stanza marked for its context. The MF2 runtime, the generated messages and `mf2c` are plain OCaml and build in both. The UI's output is static files, built in CI or anywhere else and served by `cashflow serve`.
- **Commands:** every use case is one entry in `lib/commands.ml`. The MCP tools and the web API (`/api/commands/<name>`) are both generated from that table, which replaces the separate REST routes sketched in §13.
- **Configuration** is one typed S-expression file (`cashflow.example.sexp`), parsed with derived converters into a record. Unknown fields are errors with their position, and secrets can be inline, in a file or in an environment variable. `cashflow check-config` validates it.
- **JSON** uses yojson throughout (as kcal does) rather than `jsont`; the UI reads the same JSON with small helpers.

## 17. Open questions

1. **Handelsbanken CSV:** I need an anonymised export, a few rows with the amounts changed. One is being prepared. It also decides whether the transaction date and the booking date are separate columns, which §6.7 depends on.

Resolved:
- **Admin:** from the PocketID `groups` claim.
- **Household posting:** every active member may post to household accounts.
- **Months:** a configurable pay cycle, defaulting to the 25th.
- **Phone:** a first-class target.
- **Translations:** MessageFormat 2, compiled to typed OCaml, with `en` and `sv`.
- **Investment imports:** Avanza and Montrose are out of scope for v1.
- **Savings goals:** there can be several.
- **Matching manual entries:** same transaction date and the same amount.
- **Shared code:** copied from kcal for now, extracted later.
