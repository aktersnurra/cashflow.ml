# Effect-backed module boundaries

Status: staged design approved; implementation plan awaiting review.

## Intent and agreed decisions

Make cashflow's domain, orchestration and concrete IO independently understandable and testable. Borrow Toison's enforced library boundaries, not just its directory names. Application workflows will perform declared algebraic effects; live and deterministic test handlers will interpret the same workflows.

User decisions:

- Use application-level effect handlers.
- Keep stock OCaml for now, preserving native FreeBSD deployment.
- Follow interface-first OCaml/type-driven design and the existing Bonsai compiler/API generation.

This is a behavior-preserving refactor. Money calculations, authorization, transaction semantics, command names, MCP schemas, JSON responses, database schema and browser behavior must remain unchanged. The P1/P2 regressions already on the development stack remain acceptance tests.

## Architecture

Introduce three wrapped Dune libraries, enabled only in the native/default context:

```text
cashflow_core       -> pure computation dependencies
cashflow_workflow   -> cashflow_core
cashflow_io         -> cashflow_core + cashflow_workflow + concrete IO dependencies
bin                -> libraries it actually consumes
```

The corresponding namespaces are `Cashflow_core`, `Cashflow_workflow`, and `Cashflow_io`. Each has its own `dune` stanza. Dependency arrows mean imports, not execution order. Core cannot import workflow or IO; workflow cannot import IO. Eio and SQLite belong to IO, not workflow. HTTP and MCP continue to share one command-description/dispatch boundary in IO.

No compiler-wide `-open`, `include_subdirs unqualified`, giant re-export modules or permanent catch-all `cashflow` compatibility library. Migration commits may temporarily retain the legacy library, but each such commit must name its remaining responsibilities; the final architecture removes it. Library moves and behavioral redesign must not be combined silently.

### Responsibility allocation

| Destination | Existing responsibilities |
| --- | --- |
| `lib/core/` | Amount/currency/date/time calculations, identifier parsing/comparison, accounts/transfers/ledger events and errors, ledger validation, account details, user/household records, pure access policy, cycles/holidays, analytics, chart definitions, CSV/XLSX/statement parsing, suggestion rules and merchant normalization. |
| `lib/workflow/` | Authorized account/household/transaction use cases, import preview/confirm/undo orchestration, inbox triage, scoped reporting, planning and attention orchestration; declared effects; workflow result/view types. |
| `lib/io/` | SQL/SQLite/guard/schema/codecs and stores, production ID generation/clock, OIDC/session/network/LLM clients, configuration/logging, live effect interpreters, JSON/command/query adapters, HTTP and MCP. |
| `bin/` | CLI argument parsing, resource ownership and construction of live interpreters. |
| `web/` | Existing Bonsai client, built in `cashflow-web`; never depends on native Eio/SQLite/workflow libraries. |

These are responsibilities, not permission to move whole mixed files unchanged. In particular:

- `Id.fresh` currently reads the clock and seeds randomness. Identifier parsing/comparison remains core; generation moves to IO behind declared operations. Existing UUIDv7 representation and behavior stay unchanged.
- `Suggest.merchant_key` currently fabricates transfers using fresh IDs. Extract the existing text normalization from `Analytics.merchant_key` into a pure input-based function; do not make core generate irrelevant IDs.
- `Statement_format` currently calls `Json.parse_decimal`, while `Json` depends on `Service`. Extract the pure decimal codec before moving statement parsing into core; keep JSON response encoding in IO.
- Goal/checkpoint/import/rule records currently owned by SQL store modules must move to domain or workflow contracts before workflows can stop importing stores. Persistence implementations depend on those contracts, not vice versa.
- `Service.transfer_view` is an application projection. Move its type below JSON/attention consumers before splitting `Service`; never make core depend on `Service` or JSON.
- Split `Attention` into typed card/lifecycle contracts, workflow generation, persistence and encoders. Card payloads may use workflow projections: not every card type belongs in core merely because it is a GADT.
- Preserve the typed query GADT. Its result types must refer to core/workflow contracts rather than adapter modules. Argument/schema parsing and JSON encoding remain delivery concerns.
- `Ledger_core.applied` remains abstract. Its existing environment/view interfaces and validated-write invariant must not be weakened. These legacy interfaces accept clock/ID-generator/read callbacks; stock OCaml cannot prove those callbacks pure. This milestone removes direct IO dependencies and uses deterministic callbacks in core-only tests, but does not claim production ledger writes have become referentially transparent. Preparing immutable write inputs/snapshots belongs to the later atomic write-workflow slice.

## Effect contracts

### Standard OCaml, not OxCaml

Use `Stdlib.Effect` and `Stdlib.Effect.Deep` on OCaml 5.5.1. Do not introduce `Handled_effect`, `@local`, an OxCaml switch, Lwt or a second scheduler. This is the explicit stock-compiler adaptation of the OCaml skill.

A GADT checks each operation's response type. Stock OCaml does **not** prove handler availability or prevent a capability/continuation escaping its intended scope. Public entry points, narrow interpreters and tests establish those runtime obligations; documentation must not claim an effect row or a local-mode safety guarantee.

### Definitions and elimination

Write the `.mli` before the implementation of every new boundary. Keep the operation GADT and its eliminator close:

```ocaml
(* Cashflow_workflow.Effect_ops: illustrative shape, not a complete operation inventory. *)
type _ t =
  | Now : Ptime.t t
  | Load_cashflow : Cashflow_request.t ->
      (Cashflow_facts.t, Reporting_error.t) result t
```

`Cashflow_request.t` is constructed from authenticated access, resolved scope/currency and a date range. It is not a generic map or SQL string. `Cashflow_facts.t` is a detached immutable bundle of visible accounts/transfers and the reconciliation projection needed by the report.

`Effects` owns the private bridge to `Stdlib.Effect.t`, exposes `perform : 'a Effect_ops.t -> 'a`, and a rank-polymorphic interpreter contract:

```ocaml
type handler = { handle : 'a. 'a Effect_ops.t -> 'a }
val run : handler -> (unit -> 'a) -> 'a
```

The live interpreter dispatches meaningful typed operations, not `Execute_sql`, arbitrary callbacks or arbitrary HTTP requests. Extend the GADT only when migrating a consuming workflow. Do not predeclare operations for every hypothetical future feature.

Handler requirements:

1. Resume each captured continuation exactly once, or discontinue it with the exception raised by the concrete operation.
2. If the concrete operation raises, discontinue the suspended continuation so its cleanup/unwind behavior runs. Do not raise directly out of the handler callback while abandoning the continuation.
3. Do not wrap `continue` in a catch that would attempt `discontinue` after the continuation itself has already run/raised.
4. Return `None` for effects not owned by this bridge, allowing Eio effects to reach the enclosing runtime.
5. Do not store continuations, replay writes, retry an interrupted operation, or spawn fibers from an interpreter unless the migrated operation explicitly requires it.
6. Never convert cancellation, unhandled effects or programming exceptions into success or an expected domain-error result.

A live handler is installed inside the request/command fiber that consumes it. Child Eio fibers do not receive an implicit promise of inherited application handlers: each entry point installs its own handler. Concurrent requests must never share mutable principal/scope state. A handler captures the authenticated request's access and checks effect requests against it; accepting an arbitrary `Access.t` in an operation is not an authorization grant.

### Expected failures

New contracts use named ADTs, not SQLite messages or generic string errors. For the first reporting slice, distinguish unknown currency, household-required scope and unavailable storage/read operation. Error context is typed (currency code, scope or operation), and formatting happens in the existing delivery adapters. Preserve current HTTP statuses and user-visible error behavior.

Keep existing ledger/domain error variants when forwarding their meaning. This refactor does not justify converting every historical error at once. Remaining legacy error interfaces are explicitly transitional; new APIs must not introduce fresh stringly error categories. `result` binding remains appropriate for expected failures even when IO is expressed as effects.

## Read and write consistency

### Reads

A report must not assemble one logical snapshot by yielding between individually locked SQL statements. The live `Load_cashflow` handler obtains the connection guard, loads the complete authorized fact bundle in a short read transaction, materializes all results, and releases the guard before pure report calculation. No lazy DB cursors, DB-backed lookup closures or prepared statements may escape in the bundle.

Pure lookups over in-memory snapshots are allowed. Scope/currency filtering and reconciliation behavior remain equivalent to current `Insights`/`Analytics`. Do not expose hidden member-account details merely to simplify a snapshot contract.

### Writes

Later write slices must preserve the current `Sql.transaction_result` and nested-savepoint behavior: an expected `Error`, exception or cancellation rolls back the whole relevant unit; a linked ledger batch remains atomic. The connection guard remains owned by one Eio fiber and is reentrant only for that fiber.

Do not model a transaction as independent `Begin`, `Commit` and `Rollback` effects. An atomic operation must have one boundary containing its fresh reads, verification and writes. A plan prepared outside that boundary is not proof the database is unchanged.

For imports, confirmation must still re-plan against current data inside its transaction. Network/LLM proposals are prepared outside the write transaction; they are advisory and must be deterministically revalidated. Do not introduce network suspension while holding the write lock, even if the current implementation happens to permit it. A later import slice must specify how suggestions/cache behavior remain compatible before changing this ordering.

The first delivery deliberately excludes write-effect migration. Its design must not be presented as proving cancellation/atomicity for unmigrated writes.

## First delivery and staged scope

The first implementation plan covers **pure-core extraction plus one production-used effect-backed reporting slice**, not a simultaneous rewrite of every workflow.

1. Establish the pure-core namespace/library, extracting ID generation and merchant normalization so purity is real rather than a folder label.
2. Define workflow errors, scoped reporting context, cashflow request/fact contracts and typed effect bridge.
3. Implement the existing cashflow-summary orchestration as a workflow using those effects. Implement a SQLite/clock live interpreter and deterministic test interpreter.
4. Route the shared HTTP/MCP cashflow-summary descriptor through the new workflow, maintaining its query/result/schema behavior.
5. Remove obsolete implementations of that migrated path, update build consumers and document remaining legacy responsibilities.

Remaining workflows stay in the explicitly transitional implementation until subsequent individually planned slices migrate accounts/households, planning, transfers, imports/triage and attention. Each write slice must settle its atomic operation contract before implementation. The target is the three-library graph above; the initial milestone must be described honestly as partial migration.

There is no new UX. Existing client screens, commands, configuration, authentication flow and persistence continue to work. Do not combine this work with CLI-library conversion, Base/Core-wide migration, a test-framework switch, dependency upgrades, performance claims or unrelated bug fixes. New small modules may use Base according to the OCaml convention, but introducing it must not trigger wholesale rewrites of existing pure code.

## Tests and acceptance

Before implementation, record actual results of the native build/test suite, web build and browser suite. Add tests before changing production behavior/interfaces, and verify their relevant failure before implementation.

Required evidence for the first delivery:

- Core dependency inspection plus a core-only test executable demonstrate that domain calculations need no direct SQLite, Eio, network client, system-clock dependency or workflow/IO import. Existing injected ledger callbacks remain an explicit limitation, not a compiler purity proof.
- Identifier round-trips and nominal ID separation remain valid; live UUID generation remains UUIDv7 and is no longer exposed by core identifier APIs. Preserve the legacy injectable `Ledger_core.env` until its write-workflow slice.
- Merchant normalization produces the same literal outputs without generating IDs.
- The effect bridge supports typed responses, nested handling/forwarding, normal return, expected-error return and concrete-operation exceptions. Cleanup in the suspended workflow runs exactly once on discontinuation; no continuation is resumed twice.
- Two Eio fibers install different deterministic clock/principal handlers, interleave, and never observe each other's answers or accounts.
- The live cashflow loader rejects mismatched access, returns a detached coherent snapshot, and cannot leak another user's personal account data.
- New workflow results match existing literal personal/household/family, mixed-currency, reversal and reconciliation fixtures. HTTP and MCP responses remain structurally identical, not merely numerically close.
- The existing SQL fiber/nested-rollback test and ledger/import/service property suites remain passing. Their success protects existing behavior but is not a substitute for a write-handler test in a later write slice.
- Native `opam exec -- dune build @all` and `opam exec -- dune runtest --force` pass.
- `opam exec -- dune build --workspace dune-workspace.web --profile release @web/default` and `bash web/e2e/run.sh` pass with Playwright available.
- Validate on a real FreeBSD runner/host when available. Linux success is not a claim of FreeBSD verification; report missing access explicitly.

## Evidence and open decisions

Inspected cashflow's OCaml module dependencies with `ocamldep -modules`; the current native build passes. Key evidence: `lib/service.ml`, `lib/insights.ml`, `lib/sql.ml`, `lib/db_guard.ml`, `lib/id.ml`, `lib/suggest.ml`, `test/test_sql.ml`, and `bin/cashflow.ml`. Toison reference: `/home/aktersnurra/projects/toison/docs/superpowers/specs/2026-10-16-library-boundaries-design.md`, its `lib/workflow/effects.mli`, and `lib/io/live.ml`.

The stock-compiler decision is settled. Before implementation, review the first-delivery scope and the intentional staged migration. The detailed implementation plan must enumerate exact core module membership, cut all upward dependencies, settle the new `.mli` signatures, and attach each acceptance case to an owning task. It must not silently broaden this first delivery into the complete write-workflow migration.
