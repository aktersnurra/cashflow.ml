# Effect-backed Reporting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans for native execution, or superpowers:subagent-driven-development only if the operator authorizes delegation. Implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the pure domain library and put the production cashflow-summary query behind stock-OCaml effects with live and deterministic test interpreters.

**Architecture:** Introduce wrapped `cashflow_core`, `cashflow_workflow` and `cashflow_io` libraries. Core is pure; the migrated reporting workflow performs typed operations; IO interprets them. Existing write workflows remain explicitly transitional IO responsibilities, not falsely advertised as migrated workflows.

**Tech Stack:** Stock OCaml 5.5.1, `Stdlib.Effect.Deep`, Dune >=3.24, existing Eio/SQLite stack, Alcotest/Hegel, separate OCaml 5.1.1/Bonsai web switch. Base is the default for new workflow modules; add its direct package dependency without upgrading unrelated packages.

**Spec:** `docs/superpowers/specs/2026-10-22-effect-backed-modules-design.md` (staged design approved).

## Global Constraints

- Keep stock OCaml for now, preserving native FreeBSD deployment.
- This is a behavior-preserving refactor; preserve command names, MCP schemas, JSON responses, database schema, authorization, transaction semantics and the P1/P2 regressions.
- Core cannot import workflow or IO; workflow cannot import IO. Eio and SQLite belong to IO, not workflow.
- Use `Stdlib.Effect` and `Stdlib.Effect.Deep`; do not introduce `Handled_effect`, `@local`, OxCaml, Lwt or a second scheduler.
- Write the `.mli` before the implementation of every new boundary; retain existing abstract/private types.
- No compiler-wide `-open`, `include_subdirs unqualified`, giant re-export modules or permanent catch-all `cashflow` compatibility library.
- The first delivery deliberately excludes write-effect migration. Do not add generic SQL, arbitrary HTTP or begin/commit/rollback effects.
- Preserve the existing Bonsai toolchain; do not make the browser depend on native libraries.
- Do not claim FreeBSD verification from Linux test results.

## Review Focus

- Another member's personal data or a forged request access value must not enter a report: Task 4 tests both owner and principal mismatches.
- Handler failure/cancellation must unwind suspended workflow cleanup exactly once: Tasks 3 and 4 test exception propagation, Eio cancellation and guard reuse.
- A report spanning multiple reads must be a detached coherent snapshot: Task 4 tests one guarded snapshot unit and independence from later writes.
- Library extraction must not hide clock/randomness or drag application JSON into core: Tasks 1 and 2 cut `Id.fresh`, `Suggest.merchant_key` and `Statement_format -> Json` dependencies.
- Error status/envelopes and edge-cycle reconciliation must not change: Tasks 4 and 5 test storage failures, unknown currency, household-less scope, mixed currencies, reversals and reconciliation fixtures.

---

## File and dependency ownership

### Pure core membership (exact initial inventory)

Move these implementations, together with existing `.mli` files, into `lib/core/`:

`access`, `account`, `account_id`, `account_info`, `amount`, `analytics`, `chart`, `csv`, `currency`, `cycle`, `date`, `holidays`, `household`, `household_id`, `i18n_locale`, `id`, `idempotency_key`, `ledger_core`, `ledger_error`, `ledger_event`, `owner`, `pay_cycle_setting`, `statement_format`, `str_compat`, `suggest`, `time`, `transfer`, `transfer_id`, `user`, `user_id`, `xlsx`.

Also move the new `decimal` and `merchant` modules from Task 1. `Id` no longer generates values. `Statement_format` no longer imports `Json`. `Suggest` no longer generates dummy IDs. Retain the existing public representations of account/transfer records: this is not a ledger representation redesign.

Core dependencies: `base`, `ptime`, `uuidm`, `zipc`, `xmlm`, `sexplib0`, with existing `ppx_sexp_conv` preprocessing as required. Do not link `ptime.clock.os`, Unix, Eio, SQLite, JSON/application adapters or network libraries into this library.

### Initial workflow membership

Create `lib/workflow/{reporting_error,reporting_context,cashflow_request,cashflow_facts,effect_ops,effects,reporting}.{mli,ml}` plus `lib/workflow/dune`. Dependencies: `cashflow_core`, `base`, `ptime` only; no `Service`, `Insights`, store, JSON or Eio types in these contracts.

### Initial IO membership

At Task 5 move every remaining flat `lib/*.ml`/`.mli` into `lib/io/`, including the Task 1 `id_generator` and Task 4 `reporting_live`. These include `Service`, `Insights`, `Import`, `Inbox`, planning stores, `Ledger`, `Mem_store`, auth, LLM, HTTP/MCP and command/query/JSON adapters. Keep their existing module names under `Cashflow_io`.

This is an intentional intermediate allocation: it is not a claim that `Service`, imports or attention have already become effect-backed workflows. There will be no leftover flat implementation or permanent compatibility namespace at the end of this plan.

## Task 1: Cut hidden impurity and the JSON dependency

**Files:**
- Create initially at `lib/`: `id_generator.{mli,ml}`, `decimal.{mli,ml}`, `merchant.{mli,ml}`, `id.mli`.
- Modify: `lib/id.ml`, `lib/analytics.ml`, `lib/suggest.ml`, `lib/json.ml`, `lib/statement_format.ml`, `lib/directory.ml`, `lib/service.ml`, `bin/cashflow.ml`.
- Modify fresh-ID consumers: `test/test_support.ml`, `test/test_ledger.ml`, `test/test_ledger_properties.ml`.
- Create: `test/test_core.ml`; register in `test/dune`, initially against the existing library.

**Interfaces:**
- `Id.S`: preserve `type t`, `of_string`, `to_string`, `equal`, `compare`; remove only `fresh`. `Id.Make () : S` remains generative.
- `Id_generator.account : unit -> Account_id.t`, `.transfer : unit -> Transfer_id.t`, `.user : unit -> User_id.t`, `.household : unit -> Household_id.t`.
- `Merchant.key : description:string -> counterparty:string option -> string`.
- `Decimal.error = Invalid_scale of int | Invalid_format | Excess_precision of { scale : int } | Out_of_range`.
- `Decimal.parse : scale:int -> string -> (int64, error) result`; scales are 0..4. `Decimal.format : scale:int -> int64 -> string` keeps the current formatting algorithm for valid scales, with an immediate assertion on an invalid programmer-supplied scale.
- Preserve `Json.parse_decimal`'s existing option API using `Result.to_option`, and preserve `Json.decimal` as a delegation to the pure formatter. Statement parsing also converts the typed parse result to its existing option contract.

- [ ] Add failing tests naming `Decimal`, `Merchant` and live ID-generator behavior before creating implementations. Pin literal expectations: formatting `124050L` at scale 2 is `"1240.50"`; parsing `"-3.25"` is `Ok (-325L)`; `"1.234"` at scale 2 is `Excess_precision { scale = 2 }`; invalid text, scale and overflow return their named errors. Keep existing accepted/rejected amount cases unchanged.
- [ ] Pin merchant expectations: description `"CARD 123  SPOTIFY"`, no counterparty -> `"card spotify"`; counterparty `Some "  ICA 123  "` overrides the description -> `"ica"`; blank counterparty falls back to description.
- [ ] Run `opam exec -- dune exec test/test_core.exe`; observe missing-module/signature failures, then implement the new interfaces first. Copy existing decimal/normalization algorithms rather than fixing adjacent numeric edge cases.
- [ ] Move UUIDv7 generation into `Id_generator`, preserving its time/random generation strategy and nominal return types. Generated UUID parsing is an internal invariant, not an expected business failure. Replace every current typed-ID `.fresh` consumer with the corresponding IO generator; do not change goal/import string-ID behavior in this slice.
- [ ] Make `Analytics.merchant_key` delegate using the real transfer memo; make `Suggest.merchant_key` delegate using the statement row, with no fabricated transfer.
- [ ] Run `opam exec -- dune build @all` and `opam exec -- dune runtest --force`. Verify the new tests and existing amount/import/property cases pass, and inspect the dependency cut rather than merely checking source size.
- [ ] Inspect `jj diff`, then commit: `jj commit -m "refactor(core): isolate identifiers and pure parsing helpers"`.

## Task 2: Enforce the pure-core library boundary

**Files:**
- Move the exact core inventory above into `lib/core/`; create `lib/core/dune`.
- Modify: `lib/dune`, `cashflow.opam`, `bin/dune`, `test/dune`, all native consumers of moved modules, and relevant README paths.
- Preserve `web/` and the existing `shared/`, `tools/`, `locales/` ownership.

**Interfaces:**
- Produces `Cashflow_core.<Module>`; existing module signatures remain unchanged except the Task 1 changes.
- The transitional flat `cashflow` library depends on `cashflow_core`; core never depends on that library.
- `test/test_core.ml` depends only on `cashflow_core` plus Alcotest, with no IO/workflow dependency.

- [ ] Record the baseline commands/results and add the core-only test stanza before moving implementations; verify it cannot yet resolve the intended `Cashflow_core` namespace.
- [ ] Add the explicit Base package dependency and the wrapped core stanza, enabled only for the default/native context. Move `.ml`/`.mli` pairs together.
- [ ] Qualify consumer references or use narrow module aliases; do not add broad opens or generate one giant compatibility facade. Preserve nominal IDs and `Ledger_core.applied` abstraction.
- [ ] Compile the core library/test target independently, then run the complete native build/test suite. Resolve upward dependencies by moving the appropriate pure contract/helper downward, not by adding IO dependencies to core.
- [ ] Inspect `ocamldep`/Dune dependencies and the core identifier API: no direct ID generation, system-clock calls, Eio, SQL or `Json`/`Service` imports. Preserve the legacy `Ledger_core.env`/view callbacks and explicitly document that their callers can still supply IO-backed implementations; this milestone is not a proof of referential transparency for production writes. Core-only tests supply deterministic callbacks. Existing in-memory mutations used purely within a calculation are not external IO.
- [ ] Run the separate web build to ensure the default-only library split has not contaminated the web context. Commit: `jj commit -m "refactor(core): enforce a wrapped pure domain library"`.

## Task 3: Define the reporting contracts and safe effect bridge

**Files:**
- Create the seven workflow `.mli`/`.ml` pairs listed above and `lib/workflow/dune`.
- Create: `test/test_effects.ml`, `test/test_reporting.ml`; update `test/dune`.

**Interfaces:**
- `Reporting_error.operation = Resolve_currency | Load_cashflow`.
- `Reporting_error.t = Unknown_currency of string | Household_required | Access_denied | Invalid_range of { first : Date.t; last : Date.t } | Storage_unavailable of operation`.
- `Reporting_context.t` is abstract. `make : access:Access.t -> scope:Analytics.scope -> currency:Currency.t -> now:Ptime.t -> (t, Reporting_error.t) result`; accessors: `access`, `scope`, `owners`, `currency`, `setting`, `today`. Owners/default pay cycle and timezone follow the existing `Insights.context`/`Service.today` semantics exactly; no database/clock operation occurs inside `make`.
- `Cashflow_request.t` is abstract. `make : context:Reporting_context.t -> first:Date.t -> last:Date.t -> (t, Reporting_error.t) result`; accessors: `context`, `first`, `last`.
- `Cashflow_facts.t = { accounts : Account_info.t list; transfers : Transfer.t list; reconciled_through : Date.t option }`; lists contain materialized values, never DB-backed closures.
- `Effect_ops.t` is a GADT with `Now : Ptime.t t`, `Resolve_currency : string -> (Currency.t, Reporting_error.t) result t`, and `Load_cashflow : Cashflow_request.t -> (Cashflow_facts.t, Reporting_error.t) result t`.
- `Effects.handler = { handle : 'a. 'a Effect_ops.t -> 'a }`; `Effects.perform : 'a Effect_ops.t -> 'a`; `Effects.run : handler -> (unit -> 'a) -> 'a`. Keep the extension constructor private inside `Effects.ml`.
- `Reporting.report = { context : Reporting_context.t; cycles : Analytics.cycle_summary list }`.
- `Reporting.cashflow_summary : access:Access.t -> scope:Analytics.scope -> ?currency:string -> cycles:int -> unit -> (report, Reporting_error.t) result`.

- [ ] Write bridge tests before implementation: typed scripted answers, normal return, expected-error return, forwarding an unrelated outer effect, nested handling, and a concrete operation that raises while the suspended workflow owns `Fun.protect` cleanup. Assert cleanup runs exactly once and the original exception reaches the caller.
- [ ] Observe failures, write the `.mli` contracts, then implement `Effects` with `Effect.Deep.try_with`. Capture operation exceptions separately from continuation resumption; forward unknown effects with `None`. Do not catch downstream continuation exceptions and then resume/discontinue the same continuation again.
- [ ] Write deterministic reporting tests before its implementation. Reuse literal fixtures from `test/test_analytics.ml`: personal income `30000.00`, spending `4119.00`, outgoing `10000.00`, savings `15881.00`; household incoming `10000.00`, spending `9000.00`; family spending `13119.00`, outgoing `0.00`. Add zero-income savings-rate, mixed-currency exclusion, reversal and reconciliation cases.
- [ ] Implement the workflow using only effects plus pure core calls. Resolve owners/household-required failure before currency, use the existing scope-specific default currency, then read `Now` and construct the context. Compute recent cycles exactly as before (`max 1 cycles` for internal callers), create the requested range, load one fact bundle, and call pure `Analytics.cashflow_summary`.
- [ ] Add two interleaved Eio-fiber tests with different clock and access handlers, each installing its own `Effects.run`; assert no cross-request answers. Eio belongs to this test executable, not the workflow library. Assert that calling a performed operation without a handler raises `Effect.Unhandled`; do not pretend this is statically prevented.
- [ ] Run `opam exec -- dune exec test/test_effects.exe`, `opam exec -- dune exec test/test_reporting.exe`, then the full native checks. Commit: `jj commit -m "feat(workflow): add typed cashflow effects and reporting"`.

## Task 4: Interpret reporting against SQLite and preserve adapter errors

**Files:**
- Create initially at `lib/`: `reporting_live.{mli,ml}`.
- Modify: `lib/query.ml`, `lib/service.ml`, `lib/http_server.ml`, `lib/mcp.ml`.
- Modify tests: `test/test_analytics.ml`, `test/test_http.ml`, `test/test_sql.ml`; create `test/test_reporting_live.ml`; register tests.

**Interfaces:**
- `Reporting_live.run : db:Sql.db -> access:Access.t -> now:(unit -> Ptime.t) -> (unit -> 'a) -> 'a`.
- `Reporting_live` implements only the three current operations; there is no new arbitrary-query, write or transaction effect.
- Extend the transitional `Service.error` carrier with `` `Storage_unavailable of Cashflow_workflow.Reporting_error.operation ``. It formats as `"internal error"`; raw SQL text remains in server logs only.
- `Query.run` retains its existing GADT signature. For `Cashflow_summary`, call the new workflow under `Reporting_live.run`, then build the existing encoder `Insights.ctx` from the returned context and existing service. Do not create the legacy context first or read the clock twice for this branch. All other query branches remain unchanged.

- [ ] Add live tests proving parity with the scripted fixture and the previous literal query outputs, including household-less scope and unsupported currency. Observe the missing live adapter/path failures before implementation.
- [ ] Implement currency lookup with the same configured table and semantics as `Service.currency`. For `Load_cashflow`, compare principal IDs and check every requested owner against the handler-captured access; use captured access for SQL visibility, not authority supplied by the request.
- [ ] Load accounts, range transfers and the existing reconciliation calculation under one `Sql.transaction`/connection-guard unit. Filter statement accounts with the same scope/currency book as current `Insights.reconciled_through`. Materialize results before leaving the boundary. Never return `Store_sqlite.view` or another lazy DB lookup.
- [ ] Catch only expected `Sql.Sql_error` at this live boundary, log privately and return the appropriate structured storage error. Let cancellation and unrelated exceptions discontinue/unwind normally. Keep the existing SQL guard/savepoint implementation unchanged.
- [ ] Test a foreign principal and an unauthorized owner -> `Access_denied`. Test snapshot detachment by loading, mutating the database afterward and verifying the loaded bundle is unchanged. Wrap the existing test SQL guard to count/yield on outer acquisition: the snapshot uses one outer guarded unit; a competing fiber cannot interleave its statements with that unit. No production-only test hook.
- [ ] Test cancellation while a live operation is blocked on the connection guard, then release the owning fiber and verify a later request succeeds. Preserve the existing nested-rollback/interleaving test. This does not claim unmigrated write effects have been tested.
- [ ] Route the cashflow query branch through the workflow. Map known context/access errors to the existing legacy status/messages. Map structured storage unavailability to the new carrier: HTTP returns status 500 with the existing `internal error` JSON; MCP returns JSON-RPC error `-32603` with `Internal error`, not a successful tool-error envelope. Add explicit cases before generic error handling in both adapters. Existing SQL-exception handling remains for unmigrated paths.
- [ ] Capture expected complete HTTP/MCP envelopes in tests, including unknown currency, household-required scope and simulated unavailable storage. Assert schema/name stability. Do not use exceptions to unwrap expected workflow results.
- [ ] The old `Insights.cashflow_summary` used by the unmigrated attention feed stays explicitly transitional in this milestone; the externally registered cashflow-summary query must not call it. Do not accidentally migrate attention/LLM month-review behavior in this task.
- [ ] Run focused new tests, the native build/test suite and browser suite against the changed production query. Commit: `jj commit -m "feat(io): interpret cashflow effects in production"`.

## Task 5: Complete namespaces and verify the milestone

**Files:**
- Move all remaining flat library source/interface files into `lib/io/`; create `lib/io/dune`; remove the old `lib/dune` library stanza.
- Modify: `bin/cashflow.ml`, `bin/dune`, `test/*.ml`, `test/dune`, `README.md`, active architecture documentation and `.github/workflows/ci.yml` only if actual target paths require it.
- Add an architecture regression script `test/check_library_boundaries.py` and a Dune `runtest` rule invoking it. It validates the declared project dependency graph and core source dependency closure; it is not a grep-for-code-style test.

**Interfaces:**
- Final native libraries: `cashflow_core`, `cashflow_workflow`, `cashflow_io`; no flat `cashflow` library remains.
- IO depends on core/workflow and its existing concrete dependencies. Preserve `(name cashflow)`/`public_name cashflow` for the executable and its CLI behavior.
- Tests explicitly consume owning namespaces. Core-only tests remain linked solely to core; mixed tests may use all three libraries.

- [ ] Add the architecture rule before the final move and observe failure for the remaining flat library/missing IO boundary. Its forbidden edges are core -> workflow/IO/SQLite/Eio/clock/JSON adapter and workflow -> IO/SQLite/Eio. It must also detect added unallocated flat library implementations.
- [ ] Move remaining sources/interfaces, qualify references and update Dune consumers. Do not introduce module aliases that re-export the entire old namespace. `Ledger` and mutable `Mem_store` stay in IO for now; pure `Ledger_core` remains in core.
- [ ] Confirm all native entry points that invoke the migrated query install the reporting handler, while the browser/shared MF2 libraries remain independent. Document the exact remaining legacy workflows in IO and subsequent migration order; do not label the whole application effect-backed yet.
- [ ] Self-review dependency cycles, continuation resumption paths, scope filtering, error envelope parity and absence of DB-backed facts. Inspect the complete `jj diff` for unrelated changes.
- [ ] Run `opam exec -- dune build @all`, `opam exec -- dune runtest --force`, `opam exec -- dune build --workspace dune-workspace.web --profile release @web/default`, and `bash web/e2e/run.sh` with Playwright resolvable (the local ignored install may be supplied through `NODE_PATH`). All must pass; report any baseline/environment failure by name.
- [ ] Run FreeBSD checks only on an available authorized host/runner; otherwise record that limitation, not a success claim. Do not deploy to production as a verification step.
- [ ] Commit: `jj commit -m "refactor: finalize core workflow and IO library boundaries"`. Follow publication authorization separately; do not merge or rewrite the P1/P2 bookmarks as part of this plan.

## Plan self-review

The three-library end state, first-milestone scope, stock compiler, interface-first contracts, hidden-impurity cuts, meaningful effects, typed errors, handler forwarding/cleanup, principal isolation, detached reads, protocol parity and native/web checks have owning tasks above. Writes, attention orchestration and import effect migration are deliberately deferred, not coverage gaps disguised as completed work.

The dependency investigation found an additional concrete core cut: `Statement_format` used `Json.parse_decimal`, while `Json` depends on `Service`; Task 1 handles it explicitly. The design also explicitly distinguishes a computation-only dependency graph from purity of caller-supplied legacy ledger callbacks; their full write-side preparation is deferred rather than falsely claimed complete. Remaining SQL/store-owned domain records are not moved into the workflow in this milestone, so their later extraction must precede their respective write-workflow migrations.
