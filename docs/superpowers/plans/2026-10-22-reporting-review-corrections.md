# Reporting Review Corrections Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to execute the already-approved review corrections as one cohesive implementation task and independent review. Steps use checkbox syntax for tracking.

**Goal:** Fix the read-report writer-lock regression and all three approved review minors without altering write behavior or publication state.

**Architecture:** Share the existing guarded transaction engine but select deferred outer transactions for read snapshots only. Keep scope decisions in the abstract workflow context, preserve validation reasons with a named error case, and flatten GADT dispatch without changing encoders.

**Tech Stack:** Stock OCaml 5.5.1, Stdlib effects, Eio, SQLite/WAL, Base/Ptime, Dune; existing OCaml 5.1.1 web context.

**Spec:** `docs/superpowers/specs/2026-10-22-reporting-review-corrections-design.md` (records the user's approved Claude-review scope); original milestone design remains applicable except its prohibition on SQL changes is superseded narrowly by the approved read-snapshot correction.

## Global Constraints

- Keep stock native OCaml 5.5.1 and the independent cashflow-web OCaml 5.1.1/Bonsai toolchain.
- Preserve financial behavior, authorization, schemas, command names, JSON envelopes, one-clock reporting and detached materialized facts.
- Retain the connection guard, nested SAVEPOINT behavior and existing write transaction/result rollback semantics.
- Core cannot depend on workflow or IO; workflow cannot depend on IO, SQLite or production Eio.
- Write changed `.mli` contracts before their implementations; expected failures use named errors/results.
- No new effects, scheduler, dependencies, global timeout/retry policy, production test hooks or adjacent cleanup.
- Use jj; keep the current workspace and prior commits. No rebase, push, PR, merge, deployment or other-workspace edits.
- No FreeBSD claim from Linux results. Local browser verification must not contact, reuse or stop unrelated services.

## Review Focus

1. Existing cron/CLI writers hold the WAL writer slot: ordinary reports must still read committed data. The first native regression targets the real report path, not merely toy SQL.
2. A writer commits between reads: a deferred snapshot must stay coherent and must not exclude that writer. A two-connection snapshot test pins both assertions.
3. Reads nest inside writes and callbacks raise/cancel: savepoints, guard ownership and later reuse must remain correct; existing cancellation/rollback tests stay enabled.
4. Household-less scope with explicit bad currency: household error still precedes currency resolution/clock; all scopes retain their default currency and pay cycle.
5. Invalid stored currency versus missing row versus storage failure: retain distinct domain reasons and full HTTP/MCP envelopes.

## File responsibilities

| Files | Responsibility |
| --- | --- |
| `lib/io/sql.{mli,ml}` | New deferred read boundary; shared engine without changing write behavior |
| `lib/io/reporting_live.ml` | Use read snapshot and retain stored-currency validation reasons |
| `lib/workflow/reporting_error.{mli,ml}` | Named invalid-currency reason |
| `lib/workflow/reporting_context.{mli,ml}`, `reporting.ml` | One private scope selector; minimal default-currency accessor |
| `lib/io/query.ml` | Exhaustive top-level dispatch and existing legacy error mapping |
| `test/test_sql.ml`, `test/test_reporting_live.ml` | Real WAL contention/snapshot/nesting regressions |
| `test/test_reporting.ml`, `test/test_analytics.ml`, `test/test_http.ml` | Scope/clock/parity and complete currency-error envelopes |

Existing test support/Dune registration may change only if required by a concrete new fixture; no new production hooks.

### Task 1: Correct all four review findings

**Interfaces:**
- Add `Sql.read_snapshot : db -> (unit -> 'value) -> 'value`; keep existing transaction signatures and write behavior.
- Add `Reporting_error.Invalid_currency of string` in interface first; translate it to legacy `Invalid reason` in Query.
- Add `Reporting_context.default_currency` with the exact signature in the spec; retain abstract `t` and existing `make`.
- Keep `Query.run : Service.t -> Access.t -> 'value Query.t -> ('value * Insights.ctx, [> Service.error]) result`.

- [ ] Add `report_reads_during_writer` against the current public reporting path, using two owned file-backed WAL connections with zero busy timeout. A held writer transaction must not turn a read report into Storage_unavailable. Compile/run before production changes and retain the actual assertion-level SQLITE_BUSY RED.
- [ ] Declare the read-snapshot interface, minimally share the transaction engine and route only the reporting loader through deferred mode. No timeout masking and no blanket replacement of write transactions.
- [ ] Add/run `snapshot_survives_writer_commit`: first read sees 0; another connection commits 1 while reader stays active; second read still sees 0; a fresh read after completion sees 1.
- [ ] Add/run nested read-inside-write/read-inside-read exception/savepoint checks; outer write behavior and transaction_result rollback remain covered. Confirm write BEGIN IMMEDIATE still conflicts with another writer, and report guard/cancellation/reuse tests stay green.
- [ ] Seed code `BAD!`, scale 2 in an owned test DB. Before currency edits, assert the existing legacy validation reason through the query/complete adapters and observe the incorrect unknown-currency message RED. Retain the exact reason from the spec.
- [ ] Extend the workflow error interface before implementation, preserve the Currency.make reason in the live interpreter and map it to the existing legacy Invalid carrier. Missing/SQL-error cases remain unchanged.
- [ ] Characterize scope defaults/pay cycles, overrides, household-error precedence and single-clock behavior before refactoring. Add the context accessor interface, centralize the private pure selection rule, and make reporting use it. No fabricated context values or new public selection record.
- [ ] Refactor Query.run to one exhaustive top-level match and a local with_legacy_ctx; preserve every legacy branch calculation/encoder result. Remove common_of only if made genuinely unused. No dead Cashflow_summary assertion.
- [ ] Run focused SQL/live/reporting/analytics/HTTP tests during development. Keep actual red/green logs and distinguish behavior fixes from green-to-green refactors; do not manufacture chronology.
- [ ] Run final `opam exec --switch=5.5.1 -- dune build @all`, `dune runtest --force` through that switch, separate web build `--workspace dune-workspace.web --profile release @web/default`, and local Chromium with the existing ignored Playwright install resolvable through NODE_PATH. Run once on final production sources; record real exit/output and any failure by name.
- [ ] Inspect the complete task diff for scope, captured authority, callback/continuation cleanup, dependencies and unchanged public envelopes. Commit coherent fixes with conventional messages, retain the task base across commits and leave @ empty.
- [ ] Write the full report and exact immutable-range review package; independent review must explicitly assess all four findings and write-vs-read lock behavior. No rebase or publication.

## Self-review

The single implementation task owns the coupled workflow-error/query/context/live interfaces and the shared SQL boundary. There is no cross-task producer/consumer mismatch. All five review risks above have explicit regression ownership. The new helper adds only an IO boundary; write semantics, core allocation and browser dependencies are unchanged. The review's three minors are scoped corrections, not a general corruption/purity/performance redesign.
