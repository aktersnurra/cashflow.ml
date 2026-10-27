# cashflow

A double-entry ledger of personal and household money, following TigerBeetle's rules, with an MCP server for Claude and other agents and a Bonsai web UI. The design is in [`design/design.md`](design/design.md).

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

Native tests also require Python 3 (`conf-python-3`, test-only), for the library
boundary regression using the stock compiler's `ocamldep`.

The server needs OCaml 5.5:

```sh
opam install --deps-only --with-test .
dune build
dune test
```

The web UI needs Bonsai v0.17, which needs OCaml 5.1, so it builds in a second opam switch named `cashflow-web`. The main switch's dune drives both:

```sh
opam switch create cashflow-web ocaml-base-compiler.5.1.1
opam install --switch=cashflow-web bonsai.v0.17.0 js_of_ocaml-compiler js_of_ocaml-ppx ppx_jane yojson cmarkit.0.4.0
dune build --workspace dune-workspace.web --profile release @web/default
```

The UI is static files, so it can be built anywhere and copied to the server. `web/build-bundle.sh <out-dir>` runs that build and copies exactly the deployable files into `<out-dir>`, ready to install as `static_dir`. CI uses it too. `web/e2e/run.sh` drives the real server and UI in Chromium (with Node and Playwright).

`CASHFLOW_TEST_SWITCH=5.5.1 bash web/e2e/run_advisor.sh` builds and runs the synthetic Advisor native/browser matrix using the existing Playwright installation (`NODE_PATH` may override its location). It retains external evidence and cleans its temporary fixture data/processes. Both runners default to native switch `5.5.1`. See [Advisor deployment gates](design/advisor-deployment.md): real provider capability/privacy, deployed proxy streaming and FreeBSD/TLS checks remain release blockers; local synthetic passes do not enable production.

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

#### OpenRouter privacy

Enable the optional assistant with:

```lisp
(openrouter
 ((api_key (Env OPENROUTER_API_KEY))
  (model anthropic/claude-haiku-5.5)
  (provider_only (google-vertex amazon-bedrock azure))))
```

`provider_only` is optional and defaults to the list above; an empty list is rejected.
Every completion sends `provider.only` from this list and unconditionally sets
`provider.zdr = true` and `provider.data_collection = "deny"`. These privacy restrictions
cannot be disabled in the configuration. Existing configurations inherit the restricted defaults.

The model must have an available ZDR endpoint among the allowed providers. If none qualifies,
the completion fails; the client never retries with a broader allowlist or weaker policy.
The application continues without AI suggestions or generated summaries. No live provider/model
availability is guaranteed by the sample configuration.

[OpenRouter's ZDR setting](https://openrouter.ai/docs/guides/features/zdr) restricts inference
provider retention; denying data collection alone is not a zero-retention guarantee. Review
OpenRouter account-level privacy settings as well. No external plugins or server tools are enabled
by this client.

### Deployment

- **Web sign-in:** needs a confidential OIDC client in Pocket ID, with the callback `<public_url>/auth/callback`.
- **Admin:** members of the `cashflow-admin` group are admins. They create the household and add its shared bank accounts.
- **Static files:** `static_dir` points at the built UI.
- **Reverse proxy:** as with kcal, Caddy owns public TLS. Let it compress responses (`encode zstd gzip`).

## Layout

`lib/core/` is the native-only wrapped `cashflow_core` library, exposed as
`Cashflow_core.<Module>`. It has no direct system IO dependencies. Legacy ledger
callbacks can still be IO-backed by callers; this is not a referential-transparency
guarantee for production writes. `lib/workflow/` is wrapped `cashflow_workflow`
(`Cashflow_workflow.<Module>`), depending only on core, Base and Ptime.
`lib/io/` is wrapped `cashflow_io` (`Cashflow_io.<Module>`), depending on both
and the concrete adapters. There is no flat `cashflow` library; the executable
and package are still named `cashflow`.

Only the cashflow-summary query is effect-backed in production: `Query.run`
installs `Reporting_live` for captured access, one clock sample and detached
cashflow facts, then creates the legacy JSON encoder context. HTTP/API and MCP
reach it through the registered query, without extra outer handlers. The
attention feed still uses legacy `Insights.cashflow_summary`, sharing the IO
`Reconciliation` rule with the live interpreter.

The remaining IO workflows are ledger writes/pending lifecycle, service commands,
planning/valuation and other queries, statement import/inbox triage, and attention
orchestration (including optional LLM calls). `Ledger` and mutable `Mem_store`
remain in IO. Next extract store-owned records before migrating their write
workflows; migrate writes, then import orchestration, then attention. This is the
reporting milestone, not a fully effect-backed application. The browser and
shared MF2 libraries remain independent of all three native libraries.

`test/check_library_boundaries.py` checks Dune library declarations and real
`ocamldep` source/interface dependency closure, including indirect forbidden
edges and unallocated implementations. `dune runtest` also exercises owned
negative fixtures; it does not prove purity of injected ledger callbacks.

- `lib/core/ledger_core.ml`, `lib/io/ledger.ml`: the ledger's rules and `Ledger.Make (Store)`
- `lib/io/sql.ml`, `lib/io/db_guard.ml`: typed SQLite statements (a printf-style GADT), and request fibers taking turns with the connection
- `lib/io/service.ml`, `lib/core/access.ml`: use cases, and the capability that scopes them to what a user may see
- `lib/core/statement_format.ml`, `lib/core/xlsx.ml`, `lib/core/csv.ml`: bank formats, and reading spreadsheets and CSV into cells
- `lib/io/import.ml`, `lib/io/inbox.ml`, `lib/core/suggest.ml`: the statement pipeline and triage
- `lib/core/analytics.ml`, `lib/io/insights.ml`, `lib/io/query.ml`: analytics, and the query GADT the MCP tools come from
- `lib/io/attention.ml`: the landing page's feed
- `lib/io/commands.ml`, `lib/io/mcp.ml`, `lib/io/http_server.ml`, `lib/io/web_auth.ml`: the outside world
- `web/`: the Bonsai UI; `locales/`: its messages; `tools/mf2c`: the message compiler
- `test/`: Alcotest suites against real SQLite, and the Hegel state machines
