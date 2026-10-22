# Streaming Financial Advisor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver private, consented, read-only financial conversations that start seamlessly from Home, stream real model output, and support bounded tools, Stop, retry, recovery, and deletion.

**Architecture:** Preserve the native core/workflow/IO boundaries. Pure domain transitions and declared application effects drive an Eio-supervised run; OpenRouter SSE becomes persisted typed events delivered over authenticated SSE to a shell-owned Bonsai conversation. Financial queries are reused behind a dedicated scope-pinned, redacting allowlist, never through arbitrary command dispatch.

**Tech Stack:** OCaml 5.5.1, Eio, Cohttp_eio/Tls_eio, Httpun_eio, SQLite/typed Sql, Yojson; OCaml 5.1.1, Bonsai v0.17, Async_js/js_of_ocaml; Alcotest/Hegel and Playwright for tests.

**Spec:** [Streaming financial advisor](../specs/2026-10-22-financial-advisor-design.md), approved for planning on 2026-10-22. Read it before this plan. The [mockup](../mockups/2026-10-22-advisor.html) illustrates interaction continuity; its financial approval controls are **not** in this slice.

Status: implementation plan for review. No production implementation has begun. Memory, financial mutations, proposal application, and new background intelligence remain later work.

## Global Constraints

- Keep OCaml throughout: Eio on the native server; Bonsai/Async_js in the browser; typed events over HTTP/SSE between them. No second native scheduler, React, TypeScript application, or Node backend.
- State the useful explanation up front. Remove filler and redundant subtitles, not the explanation itself. Essential consequences remain beside approval.
- Stream actual generated text as it arrives. Do not simulate streaming after receiving a complete response.
- Do not hold its guard or a transaction across an upstream request, tool wait, or client write.
- All upstream calls, including tool-follow-up rounds, preserve `provider.only`, `provider.zdr = true`, and `provider.data_collection = "deny"`.
- One active run per user/conversation; eight globally. Reject excess capacity rather than building an indefinite queue.
- User message: 8 KiB UTF-8; generated public text: 64 KiB per run.
- Four upstream model rounds and eight total read-tool executions per run.
- Tool result: 16 KiB each; combined selected evidence: 64 KiB.
- Model context: at most the last twelve completed turns within a 64 KiB serialized conversation budget, plus separately bounded evidence and system instructions.
- 120-second run deadline and 30-second upstream inactivity deadline, enforced across tool/model rounds. Provider keepalives do not extend the total deadline.
- SSE frame cap: 256 KiB; subscriber backlog cap: 256 KiB; journal cap: 1 MiB per run.
- Keep semantic replay events for 24 hours after a run terminates, then prune them; keep the materialized transcript until deletion.
- A run with no authorized connected subscriber for 30 seconds is cancelled. Originating-session invalidation has no such grace period.
- Persist before broadcast; flush pending text before terminal events. Server batching is at most 100 ms or 4 KiB; browser rendering initially batches at 40 ms.
- Conversation ownership is private; scope is pinned to Personal or a particular Household. Family scope and shared conversations are not supported. Household export requires both admin enablement and user consent.
- Session A starting/B subscribing does not transfer ownership of generation: A logout/expiry cancels it; B logout does not cancel A's otherwise valid run. Read checks do not renew session lifetime.
- One turn has one user message and one or more attempts; each attempt has exactly one run and assistant message. Retry is only for the latest turn's failed/cancelled latest attempt, with a fresh key/version and fresh evidence.
- Keep normal API/MCP, existing LLM categorisation/summary behavior, authentication, masking, and accounting unchanged. No production test bypasses or real financial/model data in tests.
- All new native module interfaces precede their implementations. Shared protocol code must compile in both switches without any native library dependency. No broad cleanup or migration of legacy queries.

## Review Focus

These are the five cross-cutting cases most likely to escape isolated happy-path tests; their owning tasks below include explicit assertions.

1. **Two tabs retrying one failed turn:** one new attempt wins; no duplicated user message, hidden new generation, or mixed old-run text. Tasks 3, 9, 10.
2. **A quiet stream loses its originating session:** no further disclosure or tool execution; another subscribed session does not keep the run alive. Tasks 4, 8, 9.
3. **Import changes evidence after Home rendered:** server resolves current authorized facts, preserves the source finding, and does not trust browser totals or merge different sampled times. Tasks 5, 7, 11.
4. **Mixed currencies and integers outside JavaScript's exact range:** no implicit currency merging or float rounding; model/browser amounts remain decimal strings. Tasks 2, 5, 10.
5. **Disconnect between the last text delta and terminal event, including pruned replay:** snapshot plus cursor reconstructs exactly one answer and terminal state without issuing another provider request. Tasks 3, 8, 10, 12.

---

## Workspace and execution rules

Work in `/home/aktersnurra/projects/vibe/cashflow.ml.workspaces/advisor-design`. The design commit is `e9203393a5dddfd1516d399ba2c31fc261f3c6b6`; this plan is the next documentation change. Do not modify the default workspace or copy its live `.superpowers` state, session key, or server files into commits.

Before execution: `jj st`, record `jj log --no-graph -r @- -T commit_id`, and confirm a clean working copy. Before each task use the empty `@` left by the preceding `jj commit`. Run `jj diff` before each Conventional Commit. Keep one writer in this workspace. Do not push, merge, or rebase unrelated work.

Verified available compiler switches during planning: `5.5.1` reports 5.5.1; `cashflow-web` reports 5.1.1. Commands below select them explicitly; do not copy the old browser runner's assumption that a `cashflow` switch exists.

Initial execution baseline (not run merely to author this plan):

```sh
opam exec --switch=5.5.1 -- dune build
opam exec --switch=5.5.1 -- dune test
opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default
```

Report baseline failures before editing. Do not install a new scheduler or change compiler versions to bypass a failure. Use existing package setup if dependencies are missing. Playwright/Node are test dependencies only.

Task dependencies are sequential except where noted. Tasks 3 and 5 could be developed independently after their contracts exist, but this plan assumes serial integration and does not authorize parallel writers.

## File and responsibility map

Paired `.ml/.mli` entries mean create both interfaces and implementations. Tests each receive explicit native-only stanzas in `test/dune`; every new browser executable/library has explicit module ownership to avoid Dune assigning one module to multiple executables.

| Owner | New files | Responsibility |
| --- | --- | --- |
| Task 1 | `shared/advisor/{dune,sse.ml,sse.mli}`; `lib/io/{http_sse.ml,http_sse.mli}`; `web/advisor_stream/{dune,transport.ml,transport.mli}`; `web/advisor_stream/smoke/{dune,main.ml,index.html}` | Bounded SSE framing, native streaming writer, browser EventSource adapter, test-only Bonsai streaming proof. |
| Task 2 | `lib/core/{advisor.ml,advisor.mli}`; `shared/advisor/{wire.ml,wire.mli}`; `lib/io/{advisor_codec.ml,advisor_codec.mli}` | Domain IDs/states/limits/records; versioned transport DTOs; native-to-wire mapping. |
| Task 3 | `lib/io/{advisor_store.ml,advisor_store.mli}`; `test/{advisor_test_support.ml,advisor_test_support.mli}` | Atomic conversations/attempts/runs, consent records, checkpoints/replay, test fixtures. |
| Task 4 | `lib/io/{advisor_access.ml,advisor_access.mli}` | Session-origin binding, consent/scope checks, revoke/delete coordination. |
| Task 5 | `lib/workflow/{advisor_tools.ml,advisor_tools.mli}`; `lib/io/{advisor_projection.ml,advisor_projection.mli}` | Closed read-tool request/result contracts, query adapters, model-safe evidence. |
| Task 6 | `lib/io/{advisor_provider.ml,advisor_provider.mli}` | OpenRouter request construction and streaming normalization. |
| Task 7 | `lib/workflow/{advisor_effects.ml,advisor_effects.mli,advisor_workflow.ml,advisor_workflow.mli}` | Declared run effects, deterministic bounded model/tool loop. |
| Task 8 | `lib/io/{advisor_runtime.ml,advisor_runtime.mli}` | Eio supervision, cancellation/deadlines, persistence-before-broadcast and subscribers. |
| Task 9 | `lib/io/{advisor_http.ml,advisor_http.mli}` | Session/CSRF-protected commands, snapshots/history and SSE admission. |
| Task 10 | `shared/advisor/{client_state.ml,client_state.mli}`; `web/{advisor_api.ml,advisor_api.mli,advisor.ml,advisor.mli,advisor_view.ml,advisor_view.mli}` | Typed client reducer, commands, one controller and keyed conversation view. |
| Tasks 11–12 | `web/e2e/{advisor.js,run_advisor.sh}`; `design/advisor-deployment.md` | Product integration, browser scenarios, proxy/FreeBSD release checks. |

Existing seams modified by tasks: `lib/io/{https.ml,schema.ml,web_auth.ml,http_server.ml,dune}`, `bin/cashflow.ml`, `web/{main.ml,home.ml,route.ml,settings_page.ml,household_page.ml,api.ml,dune,style.css}`, `locales/{en.mf2,sv.mf2}`, `test/{dune,check_library_boundaries.py,test_check_library_boundaries.py}`, `README.md`. Task-specific Files blocks narrow ownership. Do not import native `Advisor` into the browser merely because both contain conversation concepts.

## Public contract vocabulary

Native snippets use local aliases `A = Cashflow_core.Advisor`, `T = Cashflow_workflow.Advisor_tools`. Browser snippets use `W = Cashflow_advisor_protocol.Wire` and `C = Cashflow_advisor_protocol.Client_state`. These aliases are local, not new re-export libraries.

Task 2 defines `A`'s following concrete records/variants and opaque IDs before later tasks reference them:

- Nominal Conversation/Turn/Attempt/Run/Message/Evidence IDs follow `Id.S`; generation is IO-only. `Session_ref.t` is a separate internal non-wire reference to the existing session hash, not a UUID or authentication token.
- `scope = Personal | Household of Household_id.t`; `phase = Awaiting_model | Streaming_text | Running_tools`.
- `run_state = Queued | Running of phase | Cancel_requested | Completed | Cancelled | Failed of error`.
- `error = { code : error_code; context : error_context }`: codes from spec §5, plus typed conflict/capacity/cursor variants needed by the endpoints; context is a closed sum of limit, version, cursor, and safe resource-kind fields, not a raw exception/provider body. HTTP/MF2 owns user text.
- `submit`: optional conversation ID; scope; currency; locale; user text; idempotency key; expected version; optional source-finding key. New conversation uses expected version 0. Existing scope/currency/locale cannot be changed by a submit.
- `accepted`: conversation/turn/attempt/user-message/assistant-message/run IDs, accepted version, and whether this is an idempotent repeat.
- `retry`: turn ID, expected conversation version, idempotency key. `run`: all accepted IDs, owner ID, originating-session reference, model/protocol/prompt versions, started/updated times, state, and counters.
- `event_payload`: the spec's closed public payload union; `event`: accepted IDs, per-run sequence and one `event_payload`. Text deltas identify their segment. Evidence payload is a typed display projection, never arbitrary model JSON. `snapshot`: pinned scope/currency/locale, optional source finding, ordered turns/attempts/segments, active run, conversation version, and per-attempt last sequence.
- Source findings are immutable reference records captured at acceptance: kind/key/scope, authorized display fields, sampled time. They are not model instructions or trusted client JSON.

Keep internal record layouts explicit in `advisor.mli`; do not expose mutable strings or use JSON as a domain type. Later tasks add their own locally owned types as listed below, not undocumented fields to `A.run`.

Assertion sketches below belong inside the named Alcotest/browser cases. Bind their observations from the test fixture in that task; do not add production introspection solely to satisfy tests. New test helper functions named in a step are implemented in that step's test file/support module.

### Task 1: Prove incremental transport across Eio and Bonsai

**Files:** Create the Task 1 map entries; create `test/{test_advisor_stream.ml,advisor_fixture_server.ml}` and `web/e2e/advisor_stream_smoke.js`; modify `lib/io/{https.ml,dune}`, `test/dune`, `web/dune` only for the new library dependency if needed.

**Interfaces:**

- `Cashflow_advisor_protocol.Sse`: `type frame = { id : string option; event : string option; data : string }`; `create : max_frame_bytes:int -> t`; `feed : t -> string -> (frame list, error) result`; `finish : t -> (unit, error) result`; `encode : frame -> string`. Ignore comments, preserve complete UTF-8 data, and bound incomplete frames.
- `Https.stream_post`: same environment construction pattern as existing `post`; its returned function has type `uri:string -> headers:(string * string) list -> body:string -> on_chunk:(string -> unit) -> (unit, stream_error) result`. Deliver bytes only for a successful response, retain TLS verification, and propagate Eio cancellation rather than converting it to an ordinary string error. Keep `post` unchanged.
- `Http_sse`: `open_response : Httpun.Reqd.t -> writer`; `write_frame : writer -> Sse.frame -> (unit, write_error) result`; `heartbeat : writer -> (unit, write_error) result`; `close : writer -> unit`. Use installed Httpun body-writer/flush APIs, not whole-response strings.
- Browser `Transport`: `connect : url:string -> after:string option -> on_frame:(Sse.frame -> unit) -> on_connection:(connection -> unit) -> subscription`; `close : subscription -> unit`. The server EventSource event name is `advisor`; connection variants are Connecting/Connected/Reconnecting/Closed. Native HTTP admission handles detailed error diagnostics later.

- [ ] **Step 1: Write failing framing and real browser timing tests.** The synthetic test server waits on an explicit continue gate between sending `first` and completing. Build a minimal Bonsai smoke executable in its own Dune directory using the reusable transport library. Test controls bind loopback only and are never compiled into the production binary.

```ocaml
(* test_split_frame: feed every split point of this frame. *)
let sample = "id: 1\r\nevent: advisor\r\ndata: {\"text\":\"å\"}\r\n\r\n" in
Alcotest.(check string) "decoded data" "{\"text\":\"å\"}" frame.data;
Alcotest.(check int) "one frame" 1 (List.length frames)
```

```javascript
await page.getByTestId('stream-text').filter({hasText: /^first$/}).waitFor();
assert.equal(await fixture.isReleased(), false);
await fixture.release();
await page.getByTestId('stream-text').filter({hasText: /^first second$/}).waitFor();
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe`; expect a missing interface or failed framing assertion, not a missing compiler. Record the smoke build/API blocker if installed APIs cannot satisfy incremental output.
- [ ] **Step 3: Implement the interfaces and test fixture.** Add wrapped `cashflow_advisor_protocol` (Stdlib/Yojson only, both contexts) and web-only `cashflow_advisor_stream` (js_of_ocaml/Async_js/protocol). Put smoke `main.ml` in its own executable directory so it cannot enter `web/main`'s module set. Native fixture serves its built assets and gated SSE. Use a fake streaming reader plus a synthetic trusted-TLS test fixture for upstream chunking; do not weaken production TLS or add public fixture routes.
- [ ] **Step 4: Run green.** Run the native test; build `opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/advisor_stream/smoke/default`; run the fixture-backed Playwright smoke. Pass means a Bonsai-visible delta before release, ordered later text, cancellation closing resources, 256 KiB frame rejection, and no database guard held during waits. Save a screenshot only as supporting evidence, not the timing proof.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): establish incremental SSE transport"`. **Stop the plan here if incremental transport is unproven.**

### Task 2: Define domain transitions and the versioned wire protocol

**Files:** Create Task 2 map entries and `test/test_advisor_protocol.ml`; modify `shared/advisor/dune`, `lib/io/dune`, `test/dune`, `test/{check_library_boundaries.py,test_check_library_boundaries.py}` only to cover the new independent protocol library without relaxing native restrictions.

**Interfaces:** `A.step : run_state -> transition -> (run_state, error) result`; `A.is_terminal : run_state -> bool`; `A.limits : limits` with the spec's exact values. Transition variants: Start, Set_phase, Request_cancel, Finish, Fail, Confirm_cancel, Recover_interrupted. The last specifically maps any unfinished persisted state, including Queued, to Failed(Server_restarted) without a provider call. Define the contract records above. `Wire.decode_event : string -> (event, decode_error) result`; `encode_event : event -> string`; corresponding total request/snapshot codecs. `Advisor_codec.event : A.event -> Wire.event` and `snapshot : A.snapshot -> Wire.snapshot`; inbound conversions return `A.error` on invalid IDs/scopes/versions.

Wire version is 1. Encode money and sequence/version counters as validated decimal strings; never convert native `int64` money through a JS float. Reject unknown payload variants/version and invalid required fields. Public errors exclude internal session references. Domain references to wire or IO are forbidden.

- [ ] **Step 1: Write failing `terminal_transitions_and_wire_roundtrip` tests.** Include Hegel-generated transition sequences proving terminal immutability, then every terminal transition, round-trip codec, and unsafe value. Include the normal eight public event variants, duplicate terminal attempts, cancellation before Start, malformed UUIDs, unknown wire versions, negative counters, and mixed currencies.

```ocaml
Alcotest.(check bool) "terminal immutable" true
  (Result.is_error (A.step Completed Start));
Alcotest.(check int) "global runs" 8 A.limits.active_runs;
Alcotest.(check int) "run text bytes" 65536 A.limits.output_bytes;
Alcotest.(check string) "large money preserved" "9007199254740993.01"
  decoded_amount.decimal
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_protocol.exe`; expect missing constructors/codecs or failed assertions.
- [ ] **Step 3: Implement the minimal pure model/codecs.** Keep complete-state transitions guarded and request/result IDs distinct. Add real negative boundary fixtures showing shared code cannot depend directly or indirectly on native libraries. Retain existing reporting constraints unchanged.
- [ ] **Step 4: Run green.** Native protocol tests; `opam exec --switch=5.5.1 -- dune build @test/check-library-boundaries`; build the web smoke again. Pass means both compilers accept the same protocol and negative dependency fixtures are still rejected.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): define typed runs and wire events"`.

### Task 3: Persist turns, attempts, events, and consent atomically

**Files:** Create `lib/io/advisor_store.ml/.mli`, `test/advisor_test_support.ml/.mli`, `test/test_advisor_store.ml`; modify `lib/io/schema.ml`, `test/dune`.

**Interfaces:** `Advisor_store.create : Sql.db -> t`; `accept : t -> owner:User_id.t -> session:A.Session_ref.t -> now:Ptime.t -> model:string -> source_finding:A.source_finding option -> A.submit -> (A.accepted, A.error) result`; `retry : t -> owner:User_id.t -> session:A.Session_ref.t -> now:Ptime.t -> model:string -> A.retry -> (A.accepted, A.error) result`; `append : t -> run:A.Run_id.t -> expected_sequence:int64 -> A.event_payload list -> (A.event list, A.error) result`; `snapshot : t -> owner:User_id.t -> A.Conversation_id.t -> (A.snapshot, A.error) result`; `replay : t -> run:A.Run_id.t -> after:int64 -> limit:int -> (replay, A.error) result`; `transition : t -> run:A.Run_id.t -> A.transition -> (A.run_state, A.error) result`; `delete : t -> owner:User_id.t -> now:Ptime.t -> A.Conversation_id.t -> (A.Run_id.t list, A.error) result`; `recover : t -> now:Ptime.t -> unit`; `prune : t -> now:Ptime.t -> unit`. `replay = Events of A.event list | Resync_required of int64`.

Add scoped consent read/write functions using `User of User_id.t | Household of Household_id.t`, disabled by default; authorization of these functions belongs to Task 4. Add paginated metadata/list reads (default 20, maximum 100), active-run reads used by Task 8, and `account_alias : t -> conversation:A.Conversation_id.t -> Account_id.t -> (string, A.error) result` for an already-authorized account. Resolve a submitted finding in Task 5's adapter before acceptance and pass its immutable captured projection separately from the untrusted request. Low-level store functions never make network calls and are not exposed directly as model tools.

- [ ] **Step 1: Write failing `attempt_idempotency_and_replay` SQLite tests.** Use `Test_service_support.make_service`/`sign_in` and deterministic IDs/time in the new support module. Race two fibers through the real Db_guard; simulate commit failure and a late callback after deletion.

```ocaml
Alcotest.(check int) "one user message after two retries" 1 observed.user_messages;
Alcotest.(check int) "three distinct attempts" 3 observed.attempts;
Alcotest.(check int) "one winner for concurrent retry" 1 observed.retry_winners;
Alcotest.(check bool) "same key is same result" true observed.same_accepted_ids;
Alcotest.(check bool) "deleted turn cannot reappear" false observed.recreated;
Alcotest.(check bool) "old cursor needs snapshot" true observed.resync_required
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_store.exe`.
- [ ] **Step 3: Append migration 6 at this base and implement typed statements.** Tables: `advisor_conversations`, `advisor_turns`, `advisor_attempts`, `advisor_runs`, `advisor_segments`, `advisor_evidence`, `advisor_events`, `advisor_requests`, `advisor_user_consent`, `advisor_household_consent`, `advisor_account_aliases`. User text/message ID lives on its turn; assistant message ID lives on its attempt and content in segments. Account aliases are unique per conversation and never enter UI metadata. Keep migrations 1–5 byte-identical. Use `(turn_id, attempt_ordinal)`, `(run_id, sequence)`, one attempt-to-run/message relationship, and partial unique active-run indexes per owner/conversation. Global count check and acceptance occur in the same write transaction. Store originating session reference without deletion cascading into the transcript.

  Checkpoint segments and events in one transaction. A terminal transition commits its status and exactly one terminal event together; `append` rejects attempts to manufacture terminal events independently. Resolve an existing idempotency key before testing current version/busy state, after current authorization and payload-fingerprint comparison; another session never rebinds the original accepted run. JSON storage is allowed only as an IO serialization of typed versioned records. Deleted request tombstones retain operation/key fingerprint and IDs without text for 24 hours, then prune; returned tombstones cannot recreate resources. Reject stale-version/old-turn retry, key-payload conflicts, and late terminal/deleted writes. Startup marks unfinished runs Failed(Server_restarted) once. Pruning uses a fake clock in tests, not sleeps.
- [ ] **Step 4: Run green.** Store tests and `opam exec --switch=5.5.1 -- dune exec test/test_sql.exe`. Include migration from a populated v5 database twice, FK consistency, partial retention after failure, journal pruning at 24 hours, one active run, global limit 8, and persistence-before-broadcast visibility.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): persist conversations and replay checkpoints"`.

### Task 4: Enforce consent, scope, and originating-session lifetime

**Files:** Create `lib/io/advisor_access.ml/.mli`, `test/test_advisor_access.ml`; modify `lib/io/web_auth.ml`, `test/dune`.

**Interfaces:** Add read-only `Web_auth.inspect_session : Service.t -> now:Ptime.t -> A.Session_ref.t -> session_identity option` and `session_ref : string -> A.Session_ref.t`; `session_identity` has user ID, CSRF token, expiry and reference. Keep ordinary `Web_auth.session` touch/refresh behavior unchanged. `Advisor_access.principal` is opaque; `authenticate : Service.t -> now:Ptime.t -> token:string -> (principal, A.error) result`; `authorize_scope : Service.t -> principal -> A.scope -> (unit, A.error) result`; `authorize_run : Service.t -> subscriber:principal -> run:A.Run_id.t -> (A.run, A.error) result`; `authorize_history : Service.t -> principal -> A.Conversation_id.t -> (unit, A.error) result`; `revoke_session : Service.t -> now:Ptime.t -> A.Session_ref.t -> A.Run_id.t list`. Principal accessors expose current user/access/expiry/session to IO interpreters only.

Also expose `set_user_consent`, `set_household_consent`, and `delete_conversation` functions taking a principal and returning affected run IDs after a committed operation. Household consent requires current admin authority. Deletion requires ownership but not continued household data access. Do not make session references a wire field.

- [ ] **Step 1: Write the failing `origin_session_and_scope_permissions` matrix.** Create Alice sessions A/B, Bob, a household/admin, an unrelated household, and mutable fake time. Check owner/history authorization separately from live-run origin authorization.

```ocaml
Alcotest.(check bool) "default consent denied" false observed.can_start;
Alcotest.(check bool) "B logout leaves A run valid" true observed.a_run_valid;
Alcotest.(check bool) "A logout cancels despite B" true observed.origin_cancelled;
Alcotest.(check bool) "read does not touch expiry" true observed.expiry_unchanged;
Alcotest.(check bool) "owner can delete after household loss" true observed.can_delete;
Alcotest.(check bool) "B can read saved partial after A logout" true observed.can_read_partial
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_access.exe`.
- [ ] **Step 3: Implement the gates and atomic revocation.** Session revocation deletes that session and marks its unfinished runs Cancel_requested in one transaction; return IDs for post-transaction runtime signaling. Consent/membership revocation follows the same no-new-disclosure policy. Inspection does not invoke the existing session-touch function. Stream subscribers always have their own session validated in addition to the origin.
- [ ] **Step 4: Run green.** Access and existing HTTP tests. Assert CSRF/secret/session hashes never occur in public projections, other-session logout does not cancel the wrong run, expiration is enforced without SSE heartbeats renewing it, and household loss never changes a conversation to Personal.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): enforce consent and session-bound authority"`.

### Task 5: Expose only bounded, redacted financial reads

**Files:** Create `lib/workflow/advisor_tools.ml/.mli`, `lib/io/advisor_projection.ml/.mli`, `test/test_advisor_tools.ml`; extend native-to-wire evidence mapping in `advisor_codec.ml/.mli`; modify `test/dune`. Modify `lib/io/insights.ml` only if a bounded category transaction read is required; do not add a new financial computation.

**Interfaces:** `T.request` is a closed variant: Cycle_summary, Category_spending, Category_trend, Relevant_transactions, Recurring, Budget_status, Goals, Forecast. Its arguments are typed period/currency/IDs/counts and existing `Forecast_request.scenario_input`; owner/scope are absent. `T.evidence` owns detached typed per-tool projections plus sampled time, currency, reconciliation/import freshness, assumptions, bounded source references and truncation/count metadata. Define separate result constructors for the eight tools, not arbitrary `(string * json)` fields. Native `A.Evidence_added` references the evidence ID and a small typed display projection; full model evidence is run-local.

`Advisor_projection.parse_call : name:string -> arguments:string -> (T.request, A.error) result`; `read : Service.t -> principal:Advisor_access.principal -> scope:A.scope -> T.request -> (T.evidence, A.error) result`; `model_json : aliases:aliases -> T.evidence -> (Yojson.Safe.t, A.error) result`; `display : T.evidence -> A.evidence_display`; `resolve_finding : Service.t -> principal:Advisor_access.principal -> scope:A.scope -> key:string -> (A.source_finding, A.error) result`. `aliases` is an opaque per-conversation account mapping maintained in IO/storage, with no raw IDs in the model payload.

- [ ] **Step 1: Write failing `closed_allowlist_and_redacted_reads` tests.** Cover all eight tools and hostile inputs. Seed typed ledger facts via existing support functions. Use Personal/Household and two currencies. After every tool, compare financial tables before/after. Read-only SQL may update neither consent nor financial state. Include account named “SECRET ACCOUNT”, raw IDs, a masked personal counterpart, merchant prompt-injection text, stale card totals, and no eligible category rows.

```ocaml
Alcotest.(check bool) "no account name or raw ID in prompt" false observed.leaked;
Alcotest.(check bool) "all ledger facts unchanged" true observed.financially_unchanged;
Alcotest.(check int) "bounded transactions" 20 observed.returned_rows;
Alcotest.(check bool) "truncation is explicit" true observed.has_more;
Alcotest.(check bool) "mixed currencies rejected" true observed.currency_error;
Alcotest.(check bool) "card totals recomputed after import" true observed.used_current_facts
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_tools.exe`.
- [ ] **Step 3: Implement scope-pinned wrappers around the inspected `Query` constructors.** Maximum six cycles; at most twenty transactions/payments/budgets/goals; forecast horizons exactly 1/3/6 and existing scenarios only. Use `Sql.read_snapshot`, detach results, then serialize outside the guard. Explicitly whitelist DTO fields; never pass `Query`'s UI/MCP JSON through unchanged. Interpret merchant strings only as data. Find the existing card by key without invoking another optional narrative LLM; source resolution uses deterministic facts. Bound JSON before provider delivery at 16 KiB/result and 64 KiB combined evidence; truncation must produce a valid explicit bounded projection or a limit error.
- [ ] **Step 4: Run green.** Tool tests plus existing analytics/reporting/forecast tests and the library-boundary alias. Each result has a sampled time; no cross-result claim of an atomic run snapshot. Preserve unknown/stale forecast-series semantics from the existing forecast contract rather than inventing new mathematics.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): add permission-scoped financial read tools"`.

### Task 6: Normalize OpenRouter streaming without weakening privacy

**Files:** Create `lib/io/advisor_provider.ml/.mli`, `test/test_advisor_provider.ml`; modify `test/dune`. Keep `lib/io/llm.ml`'s public record shape and existing complete implementation compatible.

**Interfaces:** `Advisor_provider.create : post:Https.stream_post -> api_key:string -> model:string -> provider_only:string list -> t`; `stream : t -> request -> on_event:(provider_event -> unit) -> (usage, A.error) result`. `request` contains system instructions, ordered user/assistant/tool messages, and the eight tool schemas; it has no caller-controlled privacy overrides. `provider_event = Text of string | Calls_ready of raw_call list | Round_finished of finish_reason`; `raw_call` contains provider call ID, name, complete argument string, and index. Internal JSON parsing/fragments never become public events. Define `request`, `usage`, `finish_reason` and `raw_call` in this module's interface; workflow equivalents are detached primitives mapped by the live interpreter in Task 8.

- [ ] **Step 1: Write failing `fragmented_provider_rounds` tests.** Reuse Task 1 framing and the existing `test_llm.ml` request-capture pattern. Test every byte split, interleaved tool indices, UTF-8, comment keepalives, repeated finish reason in usage, error after HTTP 200, malformed arguments, missing `[DONE]`, empty allowlist, and cancellation before a tool round completes.

```ocaml
Alcotest.(check bool) "ZDR on every round" true observed.all_zdr;
Alcotest.(check (list string)) "allowlist unchanged" ["amazon-bedrock"] observed.providers;
Alcotest.(check string) "data collection denied" "deny" observed.data_collection;
Alcotest.(check int) "partial tool calls exposed" 0 observed.partial_calls;
Alcotest.(check int) "one round finish" 1 observed.round_finishes;
Alcotest.(check bool) "premature EOF fails" true observed.failed_without_done
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_provider.exe`.
- [ ] **Step 3: Implement request construction and finite-round decoding.** Set `stream:true`, preserve provider restrictions, exclude provider plugins/server tools and reasoning payloads. Buffer fragments by index/ID with bounded memory until completed call arguments can be parsed. Distinguish provider EOF/error from a valid tool or text completion. Coalesce/ignore duplicate accounting frames without double finishing. Do not catch Eio cancellation as a normal retryable provider failure; no retry with relaxed policy.
- [ ] **Step 4: Run green.** Provider and existing LLM tests; inspect captured synthetic requests for every round and assert no network waits inside `Sql.in_transaction`. No live OpenRouter call required.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): normalize private OpenRouter streams"`.

### Task 7: Implement the deterministic bounded run workflow

**Files:** Create `lib/workflow/advisor_effects.ml/.mli`, `lib/workflow/advisor_workflow.ml/.mli`, `test/test_advisor_workflow.ml`; modify `test/dune`. Do not add Advisor operations to the closed reporting `Effect_ops` and force unrelated interpreters to change.

**Interfaces:** `Advisor_effects` owns `type _ operation`, `type handler = { handle : 'a. 'a operation -> 'a }`, `perform : 'a operation -> 'a` and `run : handler -> (unit -> 'a) -> 'a`, following existing `Effects` without importing Eio/SQL/JSON. Run handlers capture one run; command handlers capture one authenticated principal. No native stream handle escapes either.

Define workflow-owned `context` (system instructions, last completed turns, permitted source/evidence, locale/currency, safe model aliases, configured context-token budget), `context_size = { serialized_bytes : int; conservative_tokens : int; output_reserve_tokens : int }`, and `model_event` with Text/Complete_calls/Round_finished; `raw_call` has the four primitive fields from Task 6. The operation GADT uses these exact inputs/results:

| Operation | Input → returned type |
| --- | --- |
| Check_authority | unit → `(unit, A.error) result` |
| Now / Check_cancelled | unit → `Ptime.t` / `bool` |
| Load_context | unit → `(context, A.error) result` |
| Measure_context | context → `(context_size, A.error) result` |
| Begin_model_round | context → `(unit, A.error) result` |
| Read_model_event | unit → `(model_event option, A.error) result` |
| Close_model_round | unit → `unit` |
| Parse_tool | raw_call → `(T.request, A.error) result` |
| Read_tool | T.request → `(T.evidence, A.error) result` |
| Append_events | A.event_payload list → `(unit, A.error) result` |
| Commit_terminal | A.transition → `(A.run_state, A.error) result` |
| Persist_turn / Persist_retry | A.submit / A.retry → `(A.accepted, A.error) result` |
| Start_run | A.accepted → `unit` |
| Request_cancel | A.Run_id.t → `(A.run_state, A.error) result` |
| Delete_owned_conversation | A.Conversation_id.t → `(unit, A.error) result` |

`Advisor_workflow.execute : run:A.run -> (A.run_state, A.error) result`; `select_history : A.snapshot -> (context_turn list, A.error) result` selects one user message and successful attempt per completed turn. Command entry points: `accept_turn : A.submit -> (A.accepted, A.error) result`, `retry_turn : A.retry -> (A.accepted, A.error) result`, `cancel_run : A.Run_id.t -> (A.run_state, A.error) result`, `delete_conversation : A.Conversation_id.t -> (unit, A.error) result`. They validate authority/request shape, perform the atomic store operation, then start/signal work outside storage. Task 8 owns the live interpreters; Task 9 calls those wrappers, not raw stores. Provider request/model DTO mapping and byte/token measurement remain in IO.

- [ ] **Step 1: Write a deterministic interpreter and failing `bounded_workflow_trace` tests.** The fixture in this test file supplies time, predetermined model events/tool results and stored transitions; records operation order; never calls production IO. Test failed/cancelled attempts, new evidence after imports, current scope, single/parallel-returned calls executed sequentially, malformed unknown calls, and every budget.

```ocaml
Alcotest.(check int) "maximum model rounds" 4 observed.rounds;
Alcotest.(check int) "maximum tool executions" 8 observed.tool_calls;
Alcotest.(check int) "maximum completed context turns" 12 observed.context_turns;
Alcotest.(check int) "only one copy of retried user text" 1 observed.user_text_copies;
Alcotest.(check int) "tools after accepted cancellation" 0 observed.tools_after_cancel;
Alcotest.(check bool) "authorize before every disclosure" true observed.authorization_order
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_workflow.exe`.
- [ ] **Step 3: Implement command orchestration, the loop and history selection.** Test accepted-before-start, idempotent start, cancel-after-commit and deletion revocation through the deterministic command interpreter too. System instructions explicitly deny financial execution authority, distinguish sampled evidence from assumptions, and treat merchant/user strings as data rather than permissions. Parse all completed tool calls before invoking allowed reads; return results with original call IDs. Refresh authorization and cancellation at every external boundary. Context includes only completed-turn successful attempts, not failed output or stale tool results. Cap serialized conversation/evidence independently and enforce the configured model's token/context budget conservatively: if no reliable model limit is available, refuse generation with a safe configuration error, not a guessed large limit. Expose context selection metadata; do not silently summarize financial history. Treat unmatched evidence references as plain text, not authorized links.
- [ ] **Step 4: Run green.** Workflow tests and the boundary alias. Assert exact terminal ordering, one terminal event, no successful completion after cancellation acceptance, and no content/raw errors logged by interpreters. Test 120-second total deadline even when provider comments keep arriving.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): implement bounded read-only conversation workflow"`.

### Task 8: Supervise Eio runs and durable live/replay delivery

**Files:** Create `lib/io/advisor_runtime.ml/.mli`, `test/test_advisor_runtime.ml`; modify `test/dune` and the test fixture server to inject this runtime.

**Interfaces:** `type clock = { now : unit -> Ptime.t; monotonic_s : unit -> float; sleep_s : float -> unit }`; `create : sw:Eio.Switch.t -> service:Service.t -> provider:Advisor_provider.t -> clock:clock -> t`; `start : t -> A.accepted -> unit`; `cancel : t -> principal:Advisor_access.principal -> A.Run_id.t -> (A.run_state, A.error) result`; `signal_cancel : t -> A.Run_id.t list -> unit`; `subscribe : t -> principal:Advisor_access.principal -> run:A.Run_id.t -> after:int64 -> (subscription, A.error) result`; `next : subscription -> (delivery, A.error) result`; `close : subscription -> unit`. `delivery = Events of A.event list | Heartbeat | Resync_required of int64 | End`. Also expose `submit : t -> principal:Advisor_access.principal -> A.submit -> (A.accepted, A.error) result`, `retry : t -> principal:Advisor_access.principal -> A.retry -> (A.accepted, A.error) result`, and `delete : t -> principal:Advisor_access.principal -> A.Conversation_id.t -> (unit, A.error) result`. These and `cancel` run Task 7's command workflow with permission-checked live handlers; raw store methods never become endpoint handlers. The runtime owns its store, effect interpreters, run switches and bounded broadcasters. Store creation/recovery does not start provider calls.

- [ ] **Step 1: Write failing `durable_delivery_and_runtime_deadlines` Eio tests.** Use fake clocks/provider gates and real guarded SQLite and inject storage failure. Capture persisted sequences before subscriber delivery. Exercise eight active users, ninth rejection, duplicate start, lost subscriber, origin-session expiry with a silent provider, logout B versus A, terminal races, slow-reader overflow, deleted run callback, and restart.

```ocaml
Alcotest.(check bool) "persisted before observed" true observed.durable_before_delivery;
Alcotest.(check bool) "guard released for provider/client waits" true observed.guard_free;
Alcotest.(check bool) "cancel at absent-subscriber deadline" true observed.cancelled_at_30s;
Alcotest.(check bool) "origin logout needs no 30s grace" true observed.cancelled_on_revoke;
Alcotest.(check int) "maximum queue bytes" 262144 observed.queue_cap;
Alcotest.(check int) "restarted provider calls" 0 observed.restart_calls
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_runtime.exe`.
- [ ] **Step 3: Implement the live interpreter/supervisor.** Use separate run/subscriber scopes. Bridge the push provider API into workflow reads with a bounded per-round Eio channel and child fiber; closing/cancelling the round closes both producer and consumer, never leaving a blocked writer or retaining more than 256 KiB. Flush text at 100 ms/4 KiB or terminal, commit, then publish. The subscriber records its wakeup before durable catch-up and always re-reads from the last sequence before waiting; do not join a live queue only after a race-prone replay query. A bounded wakeup mechanism can coalesce notifications since the durable journal is authoritative. Overflow yields resync/close, never blocks model generation. Schedule origin expiry (re-read if ordinary activity extended it), total/inactivity/no-subscriber deadlines, and ~15-second heartbeats. Invoke access checks without touching session expiry. Run pruning/recovery with short transactions and bounded batches. Reserve 4 KiB inside the 1 MiB journal cap for the bounded terminal event; test that limit failure can be recorded without exceeding the cap.
- [ ] **Step 4: Run green.** Runtime/store/workflow tests. Verify one completion in the cancel race, retained partials on deadline/error, 1 MiB journal rejection without uncommitted delivery, and final-delta/terminal replay across connection loss. Verify every provider and writer callback occurs outside the DB guard.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): supervise resumable streaming runs"`.

### Task 9: Add authenticated HTTP endpoints and production wiring

**Files:** Create `lib/io/advisor_http.ml/.mli`, `test/test_advisor_http.ml`; modify `lib/io/http_server.ml`, `bin/cashflow.ml`, `test/{test_http.ml,dune,advisor_fixture_server.ml}`. Modify `lib/io/config.ml`, `cashflow.example.sexp`, and `test/test_config.ml` only for an optional `advisor_context_tokens` setting if Task 7 cannot derive a reliable model-context budget; validate positive limits, include input and output reserve, and document refusal when no trustworthy budget is available. No guessed model capacity.

**Interfaces:** `Advisor_http.create : service:Service.t -> runtime:Advisor_runtime.t -> t`; `handle : t -> request -> response option`. Define Advisor-owned request/response records, not imports from Http_server (avoid a cycle): method/path/headers/body in; `Buffered of status * headers * string | Stream of subscription` out. `Http_server` converts these at its edge. Add `advisor : Advisor_http.t option` to HTTP deps; existing tests use None. SSE routes bypass the string-response path; ordinary routes are unchanged.

- [ ] **Step 1: Write failing `authenticated_commands_and_real_sse` tests for every spec endpoint.** Cover `/turns`, `/turns/:id/retry`, list/snapshot, `/runs/:id/events`, `/cancel`, conversation `/delete`, and `/consent`. Extend consent with an explicit admin-only household target. Add a read-only `/api/advisor/status` endpoint for model availability/consent UI; it sends no model request. Test session cookies, CSRF, origin, JSON parsing, UUIDs/cursors and unsupported methods, unauthenticated subscription, two-session logout, and expected-version conflicts.

```ocaml
Alcotest.(check int) "missing cookie" 401 missing_cookie.status;
Alcotest.(check int) "wrong CSRF" 403 wrong_csrf.status;
Alcotest.(check int) "conflicting retry" 409 concurrent_loser.status;
Alcotest.(check bool) "GET starts no model" true observed.no_get_generation;
Alcotest.(check bool) "no content-length on SSE" true observed.streaming_headers;
Alcotest.(check bool) "ordinary response unchanged" true observed.legacy_response
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_http.exe`.
- [ ] **Step 3: Implement parsers/admission and wire resources in `serve`.** All mutating endpoints, including consent/delete/retry/cancel, require JSON and CSRF. Use 401 for authentication, 403 for denied consent/scope, 404 for inaccessible IDs, 409 for busy/version/key conflict, 422 for invalid supported fields, 503 for capacity/model unavailable; map safe typed errors consistently. New conversation uses version 0 and returns the accepted version/IDs. Repeated keys return the original acceptance; do not start a second worker.

  Construct the streaming provider from the same OpenRouter configuration as existing `Llm.complete`; do not change existing `Service.assistant` or any model restrictions. Keep supervisor lifetime under the server switch, with no request owning the run. Logout uses Task 4's atomic revocation then Task 8 signaling; no raw token reaches events. SSE sends no fixed length, no-store/no-transform/security headers, and flushes through Http_sse. Before HTTP 200 reject invalid subscribers; after opening, access loss closes the stream and browser status probing clears content. Handle compacted cursors with a typed resync event then close. Accept decimal-string cursor via initial `after` query and Last-Event-ID on reconnect, neither a credential.
- [ ] **Step 4: Run green.** New HTTP tests, existing `test_http`, and the real-socket gated streaming test using the actual Advisor path. Verify active session checks, quiet-stream expiry, logout cancellation, deletion late callbacks and exact-body idempotency. No production route accepts fake provider modes or disables TLS/session verification.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): expose authenticated streaming conversations"`.

### Task 10: Build the typed browser controller and message view

**Files:** Create Task 10 map entries and `test/test_advisor_client_state.ml`; modify `shared/advisor/dune`, `web/advisor_stream/transport.ml/.mli`, `web/dune`, `test/dune`. Browser commands use a new Advisor API module rather than changing all existing command handling.

**Interfaces:** `Client_state.empty : t`; `reduce : t -> action -> t * command list`; `view : t -> view`. Actions cover submit acceptance, snapshots/events, draft changes, Stop/retry/delete, connection changes, auth loss and scroll-follow state. Commands are closed: Post_turn, Retry_turn, Cancel_run, Fetch_snapshot, Subscribe, Unsubscribe, Delete_conversation, Announce_status. Snapshot/events carry the full turn/attempt/run identity; obsolete source IDs are rejected before sequence handling.

`Advisor_api` exposes typed Deferred result wrappers for each Task 9 endpoint, with `Api`'s CSRF token accessed through a narrow accessor. `Advisor.component : ctx:Ctx.t -> graph -> controller Bonsai.t`; controller has `view`, `open_finding`, `open_conversation`, `open_latest`, `leave_surface`, and `stop`, all as values/effects, with one connection owner. `Advisor_view.render : locale:string -> view:Client_state.view -> dispatch:(Client_state.action -> unit Effect.t) -> Vdom.Node.t`. Native/domain imports are forbidden.

- [ ] **Step 1: Write failing `snapshot_and_event_reduction` tests and browser fixtures.** Add generated duplicate/out-of-order event sequences. Test duplicate/gapped/out-of-order events, old-attempt completion after Retry, terminal after snapshot, no saved draft in localStorage/sessionStorage, UTF-8, large decimal money, unknown event versions, backgrounded tab backlog/resync, and stop errors. Case name `snapshot_between_last_delta_and_terminal` must resume the same run once.

```ocaml
Alcotest.(check string) "deduplicated text" "hello" observed.text;
Alcotest.(check int) "new provider requests after reconnect" 0 observed.post_commands;
Alcotest.(check bool) "old run cannot alter new attempt" true observed.attempt_isolated;
Alcotest.(check bool) "access loss clears transcript" true observed.empty_after_auth_loss;
Alcotest.(check string) "exact money" "9007199254740993.01" observed.decimal
```

- [ ] **Step 2: Run red.** `opam exec --switch=5.5.1 -- dune exec test/test_advisor_client_state.exe`; compile the browser fixture and expect the missing component/broken assertions, not native library leakage.
- [ ] **Step 3: Implement the pure reducer, command adapter and single Bonsai controller.** Decode before reducing, coalesce text updates every 40 ms, flush terminal immediately, key each message/segment by stable identity, render untrusted content only as text. Evidence links come only from the authorized mapping. Stop uses POST and never assumes the socket close succeeded in cancelling; retry preserves prior partial attempts. On EventSource error probe session/run status so auth failure does not become infinite reconnect. On resync replace from snapshot plus cursor before reattaching. Bound local buffering independently of EventSource internals and resync a backgrounded client rather than accumulating deltas indefinitely.

  Keep connection cleanup at signed-in shell deactivation/logout, not Home/Advisor route deactivation. Scroll-follow respects user intent; aria-live announces meaningful states, not tokens. A model/context/config failure presents a retry or settings action and retained partial text, never a successful empty answer.
- [ ] **Step 4: Run green.** Pure reducer tests; native boundary alias; full web build; browser smoke for text before completion, cancellation/reconnect, user scrolling, XSS-looking input, and at most one subscription when toggling routes.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): render typed streaming conversations in Bonsai"`.

### Task 11: Integrate the continuous Home/Advisor experience and consent UI

**Files:** Modify `web/{main.ml,home.ml,route.ml,settings_page.ml,household_page.ml,api.ml,style.css}`, `locales/{en.mf2,sv.mf2}`; create `web/e2e/advisor.js`; modify controller/view interfaces only to satisfy the explicit shell contract. Keep ranking and existing card actions unchanged.

**Interfaces:** Add `Route.Advisor of string option` with `/advisor` and `/advisor/:id`. Extend Home's component input with a typed `on_ask : finding_key:string -> scope:Wire.scope -> unit Effect.t`; do not place a controller inside each card. The selected keyed card hosts the shared conversation outlet in a stable ancestor of the Home/Advisor route alternatives. The one shell-owned `Advisor.component` survives both alternatives and other temporary financial-page visits. URI replaceState happens only after first-turn acceptance; explicit navigation uses pushState.

- [ ] **Step 1: Write failing `continuous_finding_conversation` browser tests using the Task 9 fake-provider server.** Seed an existing unusual-spending card and a shortfall card using real ledger/import fixtures, not the mockup's nonexistent preferred-buffer setting. Cover first-use opt-in, admin household enablement, status-disabled configuration, consent decline, Personal/Household labels, direct URLs/history, conversation deletion, unsupported financial writes, source refresh after import, and finding disappearance while chatting.

```javascript
const finding = page.getByTestId('selected-finding');
const original = await finding.elementHandle();
const before = await finding.boundingBox();
await page.getByRole('button', {name: 'Ask advisor', exact: true}).click();
await sendQuestion('Why did spending increase?');
const after = await finding.boundingBox();
assert.deepEqual([after.x, after.y, after.width], [before.x, before.y, before.width]);
assert.equal(await original.evaluate(el => el === document.querySelector('[data-testid="selected-finding"]')), true);
assert.equal(await page.locator('[data-advisor-controller]').count(), 1);
assert.equal(await fixture.providerStarts(), 1);
await page.getByRole('link', {name: 'Home', exact: true}).click();
await page.getByRole('link', {name: 'Advisor', exact: true}).click();
assert.equal(await page.getByRole('textbox', {name: 'Message advisor'}).inputValue(), draft);
```

- [ ] **Step 2: Run red.** Build the web bundle and run the product browser test against the synthetic native fixture. Expect missing routes/consent/continuity assertions.
- [ ] **Step 3: Integrate the shared surface without a drawer or second landing page.** Keep selected card identity, heading, content width and viewport anchor. Preserve unrelated Home content until moving it cannot displace the anchor; use a captured source finding if the live card resolves during the conversation. The source remains historical and current evidence is revalidated server-side. No model calls merely to render Home or visit Advisor. Display a simple composer when no thread exists; a compact history selector for old threads. Explicit new-scope conversation never copies the old thread. Keep drafts per conversation in memory only.

  Add minimal opt-in copy naming OpenRouter, financial context sent and local history until deletion; separate admin household switch. Include Stop, retry, history/delete and the active scope, not a page of explanatory subtitles. Show calculation/estimate reasons up front where they matter. In this slice no memory controls or approval buttons from the prototype appear. Localize UI copy through MF2; preserve existing mobile navigation instead of adding unlabeled icons.
- [ ] **Step 4: Run green.** Browser cases on desktop and mobile, keyboard/focus/screen-reader status, both locales, no horizontal overflow or anchor shift, route Back/Forward/reload, one active subscription, preserved draft/scroll, and unmodified existing Home ranking/import widgets. The actual streamed text must be visible while the fake provider completion gate is still closed.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "feat(advisor): continue Home findings in a seamless conversation"`.

### Task 12: Close the end-to-end reliability and deployment gates

**Files:** Create `web/e2e/run_advisor.sh`, `design/advisor-deployment.md`; extend `web/e2e/advisor.js`, `test/advisor_fixture_server.ml`; modify `README.md` and `web/e2e/run.sh` only to parameterize its native switch as `${CASHFLOW_TEST_SWITCH:-5.5.1}` instead of assuming `cashflow`.

**Interfaces:** `run_advisor.sh` builds/starts a test-only server with temporary SQLite and synthetic sessions/provider, runs the browser tests, and always cleans up child processes. It uses `CASHFLOW_TEST_SWITCH` and the existing Playwright installation; fixture control APIs bind loopback and never exist in `bin/cashflow.exe`. Fixture modes include gated text/tool calls, invalid stream, silent provider, session revocation, replay pruning, and server restart.

- [ ] **Step 1: Add failing `privacy_and_recovery_end_to_end` scenarios.** Exercise full POST → Eio → fake upstream → durable event → SSE → Bonsai, not intercepted browser responses. Record a financial-table digest before every scenario and compare afterwards. Cover two simultaneous retries, A/B logout during silence, evidence change between tools, disconnect at terminal boundary, replay after pruning, deleting while a callback is blocked, malicious descriptions, 8 KiB input/64 KiB output limits, global capacity and no accidental regeneration.

```javascript
assert.equal(after.financialDigest, before.financialDigest);
assert.equal(recovered.assistantMessagesForAttempt, 1);
assert.equal(recovered.providerRequests, beforeDisconnect.providerRequests);
assert.equal(afterDelete.lateRows, 0);
assert.equal(afterOriginLogout.toolCalls, callsBeforeLogout);
assert.equal(await fixture.secretInLogs(), false);
```

- [ ] **Step 2: Run red on each added scenario.** `bash web/e2e/run_advisor.sh`; keep outputs/snapshots in files and report only failures. Do not label missing test dependencies as an application regression.
- [ ] **Step 3: Fix only demonstrated integration gaps and document deployment.** Explain opt-in, scope/history/deletion, privacy restrictions, unavailable models, cancellation limits, clocks/timeouts, shared build contexts and no-store SSE. Provide Caddy guidance to bypass buffering/compression for `/api/advisor/runs/*/events` if needed; preserve normal compression. Document synthetic proxy timing validation and native FreeBSD compilation/TLS/socket checks as release gates, not assumptions proven by Linux tests. Consent/history documentation must state that backups are not immediately purged by deletion and that local Stop cannot guarantee provider-side billing cancellation. Keep provider model-context capability configuration explicit; no unsupported automatic fallback.
- [ ] **Step 4: Run the final verification matrix.**

```sh
opam exec --switch=5.5.1 -- dune build
opam exec --switch=5.5.1 -- dune test
opam exec --switch=5.5.1 -- dune build @test/check-library-boundaries
opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default
CASHFLOW_TEST_SWITCH=5.5.1 bash web/e2e/run.sh
CASHFLOW_TEST_SWITCH=5.5.1 bash web/e2e/run_advisor.sh
```

  Expected: all relevant native/property/library-boundary/browser tests pass; first delta precedes fake upstream completion; no financially mutating tool is registered; no new native Async dependency; provider restrictions preserved; no credentials/financial values in logs; both compiler contexts build. Run deployment-level synthetic streaming through Caddy and FreeBSD checks when those environments are available; otherwise explicitly record them as release blockers, not passed tests. A real-provider smoke test is optional, synthetic-only, and needs operator approval for credentials/cost.
- [ ] **Step 5: Commit.** `jj diff`; `jj commit -m "test(advisor): verify streaming privacy and recovery end to end"`. Record the exact parent revision and verification commands. Do not deploy or merge as part of this plan.

## Self-review and handoff

Coverage map: spec §§1–3 → Tasks 10–12; §4 → Tasks 3–5/8–9/11; §5 → Tasks 2–4/7–10; §6 → Tasks 1–2/5–8; §7 → Tasks 3/8–10/12; §8 → Tasks 1–2/5–10; §9 explicitly deferred; §10 → all task tests and Task 12; §11 → first slice only. The five Review Focus cases each have a named owning test above.

Keep the plan's interface names consistent across tasks. Follow the inspected source rather than treating old README descriptions of a flat library or missing forecast as current. If a plan interface cannot fit the supported library API or strict privacy contract, stop at that task and propose the smallest revision; do not improvise a second scheduler or silently expand financial authority.

Execution method has not yet been selected. Review this plan before production changes. Recommended: subagent-driven, with one implementing writer at a time and a fresh review gate per task, because security, streaming, persistence, and UI continuity each need independent checks at their boundaries. Native execution remains a lower-overhead option, with one whole-branch review at the end. Neither choice authorizes building the later memory/proposal slices.
