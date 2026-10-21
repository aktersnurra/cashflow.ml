# cashflow

A double-entry ledger of personal and household money, following TigerBeetle's rules, with an MCP server for Claude and other agents and a Bonsai web UI. The design is in [`docs/design.md`](docs/design.md).

It is a single OCaml binary serving MCP, a JSON API and the web UI over HTTP, backed by SQLite, signing people in with an OIDC provider (Pocket ID). It runs natively on FreeBSD (Eio's POSIX backend), like kcal.

## What it does

- **Ledger:**
  - accounts and immutable transfers, posted in batches with linked chains;
  - pending transfers (post, partial post, void, expiry);
  - reversals and corrections;
  - idempotency keys on every write;
  - `cashflow verify` recomputes every balance from its transfers.
- **Households:**
  - personal and shared accounts, invitations, an admin role from a Pocket ID group;
  - another member's personal side of a shared transfer is masked;
  - contributions split in proportion to income;
  - shared costs paid from a personal card are tracked until settled.
- **Import:**
  - Handelsbanken's .xlsx export, and CSV statements using common Swedish and English column names;
  - a statement for another account is refused, checked against the account number's last four digits;
  - fingerprinted rows, so overlapping files are safe; a row the data can't place is asked about, never dropped;
  - manual entries a statement confirms are matched instead of counted twice, including both sides of a move between two imported accounts;
  - the balance is checked against the bank on every import, and a first import suggests the opening balance the bank's figures imply;
  - an import can be undone, together with everything triage did with its rows.
- **Inbox:** categorise, share with the household, or pair the two sides of a move between accounts. Suggestions come from rules, then history, then (optionally) an LLM.
- **Planning:**
  - pay cycles (by default the 25th, or the banking day before);
  - budgets with pace and rollover;
  - savings goals that earmark real accounts.
- **Analytics** for me, the household or the family:
  - cash flow and savings rate per cycle, with a "reconciled" flag;
  - spending by category and category trends;
  - net worth;
  - runway;
  - recurring payments and price changes;
  - largest transactions.
- **Landing page:** a ranked feed of what needs attention, each card with a small typed lifecycle:
  - statements due, the inbox, unconfirmed entries;
  - an account the bills due before payday would overdraw;
  - budget alerts, and unusual spending in categories without a budget;
  - contributions due, settle-up, goals behind, new or changed subscriptions, and the month review.
- **LLM (optional, OpenRouter):** category suggestions, month summaries, and a sentence on what drove unusual spending. It only ever sees merchant text, amounts, dates, category names and computed totals.
- **Languages:** English and Swedish, written in MessageFormat 2 and compiled to typed OCaml by `tools/mf2c`.

## Building

The server needs OCaml 5.5:

```sh
opam install --deps-only --with-test .
dune build
dune test
```

The web UI needs Bonsai v0.17, which needs OCaml 5.1, so it builds in a second opam switch named `cashflow-web`. The main switch's dune drives both:

```sh
opam switch create cashflow-web ocaml-base-compiler.5.1.1
opam install --switch=cashflow-web bonsai.v0.17.0 js_of_ocaml-compiler js_of_ocaml-ppx ppx_jane yojson
dune build --workspace dune-workspace.web --profile release @web/default
```

The UI is static files (`_build/web/web/`), so it can be built anywhere and copied to the server. `web/e2e/run.sh` drives the real server and UI in Chromium (with Node and Playwright).

Property tests use [Hegel](https://ocaml.org/p/hegel/latest/README.html), which needs a native `libhegel` (Linux, or macOS on Apple Silicon). They run on a development machine or in CI; nothing in production depends on Hegel. Three state machines drive random use and check invariants after every step:
- **Ledger:** debits equal credits, limits hold, chains are all or nothing, and `verify` passes.
- **Import:** overlapping, cut-off exports in either order, with re-uploads and undo, hold every row exactly once.
- **Service:** two household members importing, triaging, sharing, pairing, undoing and retrying keep every scope's savings equal to the change in its accounts, and every bank account equal to its statements.

## Running

```sh
cashflow migrate              # apply SQLite migrations
cashflow serve                # MCP at /mcp, the API at /api, the UI at /
cashflow verify               # recompute balances; non-zero exit on a mismatch
cashflow void-expired         # daily, from cron
cashflow check-config         # validate the configuration, secrets included
cashflow read-statement FILE  # read a bank export as an import would, without the database
```

`read-statement` prints the format, the account the file is for and every row. Use it to check a bank's export before importing it.

`cashflow create-session SUBJECT` signs a user in without the identity provider, for development and browser tests only. It refuses to run unless `CASHFLOW_ALLOW_DEV_SESSIONS=1`.

### Configuration

Configuration is code: one typed S-expression file, `/usr/local/etc/cashflow.sexp` by default (or `--config FILE`, or `$CASHFLOW_CONFIG`). Start from [`cashflow.example.sexp`](cashflow.example.sexp) and check it with `cashflow check-config`.

- **Parsing:** the file is read with derived converters, so a misspelt field is an error that names its line and column. Missing optional fields take their defaults.
- **Secrets:** a secret is `(Inline "...")`, `(File /path)` or `(Env NAME)`, so the file itself can live in git with the rest of the jail's configuration.
- **Logging:** `log_level` is `Quiet`, `Error`, `Warning`, `Info` (the default) or `Debug`. `CASHFLOW_LOG_LEVEL` overrides it for a single run.

### Deployment

- **Web sign-in:** needs a confidential OIDC client in Pocket ID, with the callback `<public_url>/auth/callback`.
- **Admin:** members of the `cashflow-admin` group are admins. They create the household and add its shared bank accounts.
- **Static files:** `static_dir` points at the built UI.
- **Reverse proxy:** as with kcal, Caddy owns public TLS. Let it compress responses (`encode zstd gzip`).

## Layout

- `lib/ledger_core.ml`, `lib/ledger.ml`: the ledger's rules, pure, and `Ledger.Make (Store)`
- `lib/sql.ml`, `lib/db_guard.ml`: typed SQLite statements (a printf-style GADT), and request fibers taking turns with the connection
- `lib/service.ml`, `lib/access.ml`: use cases, and the capability that scopes them to what a user may see
- `lib/statement_format.ml`, `lib/xlsx.ml`, `lib/csv.ml`: bank formats, and reading spreadsheets and CSV into cells
- `lib/import.ml`, `lib/inbox.ml`, `lib/suggest.ml`: the statement pipeline and triage
- `lib/analytics.ml`, `lib/insights.ml`, `lib/query.ml`: analytics, and the query GADT the MCP tools come from
- `lib/attention.ml`: the landing page's feed
- `lib/commands.ml`, `lib/mcp.ml`, `lib/http_server.ml`, `lib/web_auth.ml`: the outside world
- `web/`: the Bonsai UI; `locales/`: its messages; `tools/mf2c`: the message compiler
- `test/`: Alcotest suites against real SQLite, and the Hegel state machines
