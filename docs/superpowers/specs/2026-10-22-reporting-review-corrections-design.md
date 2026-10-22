# Reporting review corrections

## Approved scope

The user approved fixing the locking regression and all three minor findings in Claude's review of the effect-backed-reporting patch. This is a follow-up to the reporting milestone, not permission to rebase, publish or migrate additional workflows. Start from local head `34d7d4ca714b402b05acb93eb6c37fcd58879aa3` in the existing dedicated workspace.

## Read snapshots versus writes

`Reporting_live.load_cashflow` currently uses `Sql.transaction`, whose outer `BEGIN IMMEDIATE` reserves SQLite's writer slot. That is a regression under the existing WAL/independent-command model: a cron writer can make an otherwise readable report fail with SQLITE_BUSY, and a report unnecessarily excludes writers. This is not merely an untested alternative deployment.

Add `Sql.read_snapshot : db -> (unit -> 'value) -> 'value`. At depth zero it uses `BEGIN DEFERRED`; at positive depth it uses the existing SAVEPOINT behavior. It retains the same exclusive connection guard, exception cleanup and depth tracking. The first read pins the WAL snapshot. Its callback is intended for reads; stock OCaml and this callback signature do not enforce SQL read-only capability. Do not claim a new read-only-connection deployment contract.

Keep `Sql.transaction` and `Sql.transaction_result` write semantics unchanged, including outer `BEGIN IMMEDIATE`, nested rollback and result-error rollback. Do not add a busy timeout or retry as a substitute for removing the report's writer-lock request. Only the cashflow loader moves to the new read boundary; its facts remain fully materialized inside the transaction.

## Query dispatch

Keep the existing query GADT and `Query.run` result/encoder contracts. Use a single top-level exhaustive constructor match. Legacy branches use one local `with_legacy_ctx` function, while cashflow-summary continues to install its live handler without constructing a legacy context first or sampling the clock twice. Remove the dead summary `assert false` arm. It is currently unreachable, not a demonstrated crash.

## Scope selection

Make `Reporting_context` the single owner of the pure scope/household decision: owners, pay-cycle setting and default currency. Keep its full context abstract and retain the existing `make` signature. Add only:

```ocaml
val default_currency : access:Cashflow_core.Access.t -> scope:Cashflow_core.Analytics.scope -> (string, Reporting_error.t) result
```

A private selection function supplies both `make` and this accessor; `Reporting.cashflow_summary` uses the accessor before resolving currency. Do not fabricate a context/time/currency merely to discover defaults. Preserve household-required precedence even with an explicit invalid currency, user timezone, scope-specific settings and the one-clock invariant.

## Currency validation parity

Add named `Reporting_error.Invalid_currency of string`. If an existing currency row fails `Currency.make`, retain its validation reason through this variant and map it to the existing legacy `Invalid reason` at the query boundary. Missing rows remain `Unknown_currency`; actual SQL failures remain operation-specific storage errors. No new Service error or HTTP/MCP schema is needed.

A concrete valid-SQL but invalid-domain row is code `BAD!`, scale `2`; its expected reason is `currency code "BAD!" must be three uppercase letters`. Preserve the complete domain-error envelope, not just its status. This is narrow validation-message parity, not a general database-corruption recovery guarantee.

## Verification and exclusions

Use interface-first TDD. Before production edits, a real file-backed, two-connection WAL reporting regression must fail against the existing handler because of the writer lock. Also prove a read snapshot stays coherent while another connection commits, nested read/write savepoints and cleanup remain sound, and write transactions still reserve their writer slot. Use owned temporary files and deterministic ordering, not sleeps.

Before the currency fix, observe a runtime assertion failure for the wrong stored-row message. Use existing passing parity/clock/scope tests for the behavior-preserving refactors, adding characterization before extraction where needed. Keep all prior financial, authorization, cancellation, envelope and architecture regressions.

Keep stock native OCaml 5.5.1, independent cashflow-web OCaml 5.1.1, Stdlib.Effect/Effect.Deep, Eio and the three wrapped libraries. No compiler migration, dependencies, new effects, production test hooks or broader SQL/write changes. Run native build/full suite, independent web build and local Chromium after integration. Do not claim FreeBSD execution without an authorized host. Keep the workspace and audit evidence; do not rewrite prior commits, alter other workspaces or publish/rebase.
