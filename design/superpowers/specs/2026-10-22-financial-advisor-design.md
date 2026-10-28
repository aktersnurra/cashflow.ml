# Streaming financial advisor

Status: approved by the user on 2026-10-22 for implementation planning. Implementation awaits review of the separate plan and selection of its execution method; production data changes are not authorized.

## 1. Intent and decisions

Make cashflow feel attentive: explain what matters, help investigate it, and eventually prepare changes the user controls. Intelligence belongs throughout the app, not in a separate chatbot that needs briefing each time.

### Agreed with the user

- Home and Advisor are one continuous interaction. Following up on a Home finding must not feel like opening another application surface.
- Add an Advisor sidebar destination, but no drawer-to-page handoff, second landing page, or repeated context entry.
- State the useful explanation up front. Remove filler and redundant subtitles, not the explanation itself. Essential consequences remain beside approval.
- Stream actual generated text as it arrives. Do not simulate streaming after receiving a complete response.
- Keep OCaml throughout: Eio on the native server; Bonsai/Async_js in the browser; typed events over HTTP/SSE between them. No second native scheduler, React, TypeScript application, or Node backend.
- Model consequential workflows with small typed state machines. The LLM proposes; application code enforces transitions and permissions.
- Long-term direction includes scoped memory, event-triggered findings, and approval-gated actions.
- Deliver the first slice as seamless conversation, streaming, read-only financial tools, and cancellation. Add memory and financial actions afterwards.

### Defaults approved with this document

The user approved the design as a whole, including the author-proposed defaults: private conversation ownership; pinned personal/household data scopes; separate consent for Advisor data disclosure; HTTP endpoint shapes; durable replay and recovery rules; bounded run limits; history retention; the initial tool allowlist; originating-session cancellation; and retry attempts under the same user turn. These were not individually user-selected requirements. Sections below make each concrete. Exact module and endpoint names may be refined in the implementation plan without weakening these contracts.

### Non-goals and anti-goals

The first slice does not change transactions, budgets, rules, goals, household arrangements, or bank balances. No action/proposal tool is advertised to the model in that slice. (Later slice: the model may call `propose_set_budget`; it records a typed proposal and a card the user applies or dismisses with a click, and the model is only told it awaits confirmation. See the write-actions section below.) Saving chat and run state is allowed; “read-only” refers to financial domain state.

No general autonomous agent framework, arbitrary SQL/code execution, web browsing, external provider tools, vector database, multi-agent orchestration, new forecasting mathematics, or automatic memory extraction. No notification spam, generic financial coaching, hidden assumptions, or extra prose to make the system appear busy. A chat message is not authorization to move money.

## 2. Repository grounding

Inspected base: `99213355f3957b8eff414a1372610ba19bea1a99`, in the isolated `advisor-design` workspace. The default workspace is left unchanged.

| Existing seam | Consequence for this design |
| --- | --- |
| `lib/io/llm.ml` | Currently exposes a whole-response `complete` function. Preserve it for existing features; add a streaming boundary for Advisor. |
| `lib/io/https.ml` | Cohttp_eio currently reads the entire body with `take_all`. Advisor needs incremental reads with bounded parsing and cancellation. Do not change OIDC clients to streaming incidentally. |
| `lib/io/http_server.ml` | Ordinary responses are strings with Content-Length. Add a separate streaming response path; never force SSE through `respond_with_string`. |
| `web/api.ml` | Async_js currently awaits full responses. Add a small browser EventSource adapter, not server-side Async. |
| `web/main.ml`, `web/route.ml` | Route-local rendering must not own the active conversation or connection. The signed-in shell owns their lifetime. |
| `lib/io/query.ml` | Typed queries already cover summaries, categories, budgets, recurring payments, goals, and forecasts. Reuse permitted computations, not the whole command registry. |
| `lib/core/access.mli`, `lib/io/insights.ml` | Access and scope are application-owned. Rebuild authorization at delivery and tool boundaries; never accept model-supplied authority. |
| `lib/io/db_guard.ml` | The SQLite connection is guarded. Never hold its guard or a transaction across an upstream request, tool wait, or client write. |
| `lib/core/`, `lib/workflow/`, `lib/io/` | Preserve enforced dependency boundaries and the established application-effect architecture. |
| `README.md`, `dune-workspace.web`, `web/dune` | Native OCaml 5.5 and browser OCaml 5.1/Bonsai v0.17 are separate build contexts. Shared wire code must compile in both without importing native workflow code. |

Forecasting is already present, including existing scenario inputs. Earlier conversational descriptions of forecasting as entirely future work are superseded by this inspected code. Conversely, the prototype's preferred 3,000 kr buffer and actionable transfer proposal are illustrative future features, not evidence that those account settings or commands exist today.

Use the repository's tracked `design/superpowers/` convention; `docs/` is ignored here. The companion [interaction mockup](../mockups/2026-10-22-advisor.html) is a throwaway illustration, not application code or an implementation specification. Its approval controls illustrate a later slice.

## 3. Product contract

### Continuous Home-to-Advisor interaction

The existing deterministic attention engine decides which findings appear and their priority. Do not replace Home with a universal chat transcript or make an LLM responsible for ranking urgent facts.

A finding has a concise explanation and a way to respond. On first submission:

1. Keep the selected finding, title, content width, draft/composer location model, and viewport anchor in the same shared component. The server captures the finding reference and its current evidence, not browser-supplied totals.
2. Create a private conversation and its first run. The original finding becomes its introductory context, not a duplicated assistant message.
3. Append the user's message and streamed answer below it. Update the Advisor sidebar selection and address to the conversation without remounting the shared component or resetting scroll.
4. A conversation's title remains about the topic, not a switch from “Home” to a generic “Advisor” heading.

Preserve the existing Home feed outside this active interaction. Its unrelated cards must not suddenly disappear above the user's viewport anchor when conversation starts; placement/folding must keep the selected block stationary. The shared conversation component is mounted above the Home/Advisor route switch. Other financial pages remain available normally.

Proposed routes: `/advisor` opens the most recently active permitted conversation or an empty composer; `/advisor/<conversation_id>` restores a specific conversation. Starting from Home replaces the current history entry with the conversation address rather than creating a spurious intermediate page. Deliberate navigation uses normal history. Returning to Home or Advisor preserves the in-progress draft, run, and per-surface viewport position. Browser reload restores persisted content and the live run; unsent drafts are held in memory, not silently stored in browser persistent storage.

Older conversations are available through a compact history control within Advisor. Starting a genuinely new conversation is explicit; visiting the sidebar item does not create one. Mobile uses the same flow, not a separate drawer or modal.

### Copy, evidence, and streaming

- Give the short explanation first: what changed, why, and its significance. Supporting transactions and calculations are available without hiding the essential reason behind “Why?”.
- Render code-computed amounts, dates, assumptions, and freshness in the appropriate existing financial components. Avoid wrapping each fact in a separate decorative card.
- A streaming response is visibly unfinished and has a Stop control. Batch incremental rendering on a short timer (initial target 30–50 ms); never wait for the full response to display it.
- Show brief tool progress only when useful, e.g. “Checking upcoming bills.” Do not expose tool arguments, internal reasoning, or a theatrical activity log.
- Auto-follow new output only while the user is already near its end. Scrolling away pauses auto-follow; indicate new output without taking over the viewport. Do not announce every chunk to screen readers; announce meaningful status changes and completed messages.
- User and model text are untrusted. First slice uses escaped plain text with paragraph breaks, plus application-rendered evidence references. No raw HTML or arbitrary model-supplied URLs. Rich Markdown is not required to deliver this slice.
- Keep concise caveats when they change interpretation: unreconciled accounts, incomplete imports, estimated bill dates, unsupported forecast horizon. Do not present a stale import as reassurance.
- English/Swedish UI strings use the existing MF2 pipeline; the model receives the conversation locale.

### First-slice example

The user follows up on an existing unusual-spending finding: “Why did spending increase this cycle?” The advisor reads allowed category comparisons and a bounded set of relevant transactions, then streams a concise explanation with evidence references. “What about next cycle?” can use the existing forecast. It cannot create a buffer setting, promise an unsupported scenario, or record a transfer. If asked to change a budget, it states that it cannot make changes yet and links to the existing permitted screen.

## 4. Ownership, privacy, and disclosure

### Conversation visibility is not financial scope

A conversation belongs to one authenticated user and is private in the first slice. It has a pinned data scope: `Personal` or one identified `Household`. Household scope does not share the conversation with household members. Shared conversations and family-combined scope are deferred.

Changing scope is explicit and starts a new conversation; never recycle personal history into household context automatically. A household conversation's results contain only facts authorized for that household scope, not another member's private counterpart, private income details, or personal memories. Reuse existing masking and add dedicated model-facing DTO tests.

Model calls cannot choose a different owner, user, household, or scope. These come from the authenticated conversation. Resource IDs in tool arguments must resolve inside that scope. Inaccessible resources fail without revealing their existence.

### Advisor expands today's model-visible data

The existing summarizer intentionally excludes balances and account names. Advisor investigation can require balances, upcoming commitments, forecast inputs/results, and selected transactions. This is a deliberate new disclosure boundary, not an unnoticed extension to existing categorisation consent.

Proposed release policy:

- Advisor is separately disabled until the user opts in after seeing the data categories sent to OpenRouter and the local history-retention policy.
- Household data additionally requires an administrator-enabled household Advisor setting and the individual user's opt-in. Existing household membership alone is not export consent.
- Pseudonymize account identifiers and labels for model context (`account_1`, with a role if known). Keep the mapping server-side; the UI resolves evidence to real labels after authorization. Never send full account numbers, authentication material, or another member's masked fields.
- Merchant descriptions, categories, dates, amounts, and the user's submitted message may be sent where relevant. Tell the user that submitted text is sent too; pseudonymization is not a guarantee that free text lacks identifying information.
- All upstream calls, including tool-follow-up rounds, preserve `provider.only`, `provider.zdr = true`, and `provider.data_collection = "deny"`. Never relax these constraints on retry or model failure. Provider compatibility must include streaming/tool support under the same restrictions.
- No external provider plugins, server tools, or browsing. Model-returned tool names must match the application allowlist.
- Record call purpose, model, timing, counts and outcomes, not financial content. No prompts, outputs, SSE payloads, amounts, or merchant strings in operational/error logs.

Conversation history is local financial data stored in SQLite, not merely transient model context. It remains until the owner deletes it; first slice includes deletion. Deletion cancels active work, revokes subscribers, removes associated messages, evidence, and replay events, and prevents late worker writes from recreating them. Minimal idempotency tombstones may retain non-content identifiers until their retry window expires. Backups are outside immediate deletion and must be disclosed. No promise of provider deletion beyond the configured retention policy.

Authorization is checked on every API request, before each tool read/upstream disclosure, and before each emitted or replayed event batch. Changed membership, household opt-out, or user opt-out stops affected delivery/generation. An access-loss response clears the active UI transcript. Previously delivered bytes cannot be revoked. A pinned household conversation becomes inaccessible if its underlying authorization is lost; it must not fall back to personal scope. Its owner may still delete it through an ownership-only deletion operation that returns no content.

**Proposed session default:** conversations belong to users, but each run is bound to the authenticated session that initiated that attempt. Logging out or expiring that session cancels its unfinished runs even if another session of the same user is subscribed. Other authenticated sessions of the owner may subscribe to or Stop the active run while their own session and the originating session remain valid. Persisted history, including the cancelled run's partial answer, remains readable under current owner/scope authorization after the originating session ends. Losing a non-originating subscriber's session disconnects only that subscriber; it does not cancel a run still authorized by its originating session. No subscription silently transfers the run's session binding. A new explicit Retry binds to the session making that request.

Store only an internal session reference on the run, never a raw token or CSRF value; no session reference enters model context, public events, or logs. Session deletion/expiry must not cascade-delete the transcript. Logout invalidates the session and marks affected runs for cancellation within a short storage boundary, then signals their Eio cancellation scopes after releasing the guard. Schedule expiry checks against the session's actual expiry, re-reading it if normal authenticated activity extended it. Stream authorization checks do not themselves refresh session lifetime. There is no disconnected-client grace period for known session invalidation; recheck before sending any subsequent event batch or starting any new tool/model round.

## 5. Domain objects and state machines

Use opaque IDs and explicit tagged states. The following are contracts, not final `.mli` declarations.

- **Conversation:** owner, pinned scope, locale, optional source-finding reference, timestamps, and ordered turns.
- **Turn:** one immutable accepted user message and an ordered set of answer attempts. The first attempt is created with the turn. An explicit Retry adds an attempt, not another copy of the user message. An idempotency key resolves to the same accepted operation/result or a payload-conflict error.
- **Attempt:** exactly one assistant message and one generation run, with an attempt ID and ordinal within its turn. Failed/cancelled attempts retain their partial content and status; a later attempt does not overwrite them.
- **Run:** initiating-session reference, model and prompt/protocol versions, limits, current state, cancellation status, and ordered public events.
- **Message:** immutable user text, or ordered assistant text/evidence segments with a completion status. Multiple upstream tool rounds still belong to the same attempt's assistant message.
- **Evidence:** a permitted tool result projection, its source references, scope/currency, sampled time, import/reconciliation status, and assumptions. It is distinct from model-authored prose.

### Generation

```text
Queued -> Running(Awaiting_model | Streaming_text | Running_tools)
Queued | Running -> Cancel_requested -> Cancelled
Running -> Completed | Failed(reason)
Cancel_requested -> Cancelled | Failed(reason)
```

A completion/cancellation race is resolved atomically against stored state: whichever transition legally commits first wins; a terminal run cannot change to another terminal state. A Stop request against a completed run returns that completion rather than pretending cancellation. No new model round or tool call begins after cancellation is accepted. At most one active run exists per conversation and per user in the initial slice; a second submission returns a typed busy conflict. The user may keep composing while a run is active.

A request is accepted and persisted before its supervised Eio worker starts. Queued state covers that handoff, not an unbounded job queue. On process restart, unfinished runs become `Failed(Server_restarted)` with their persisted partial content. Do not silently resend prompts to the provider.

### Browser connection

```text
Disconnected -> Connecting -> Connected -> Reconnecting
                       \-> Authentication_required | Unavailable
```

Connection loss is not generation failure. Reconnecting subscribes to the same run. Stop uses an authenticated HTTP command; closing an EventSource alone is not cancellation. A route change within the signed-in app is neither Stop nor a new generation.

A run with no authorized connected subscriber for 30 seconds is cancelled to bound abandoned work, including accepted runs whose browser never attaches. A reconnect within that grace period resumes the same run, provided its originating session and current permissions remain valid. Another authorized session of the same owner may sustain the subscription during that period but cannot replace an invalid originating session. Slow or backgrounded clients cannot accumulate an unbounded in-memory event queue. Originating-session invalidation cancels without this grace period; an unrelated session's logout does not.

### Errors

Use stable error codes plus safe user-facing messages and structured context. Include `Access_denied`, `Advisor_disabled`, `Consent_required`, `Scope_mismatch`, `Run_busy`, `Idempotency_conflict`, `Invalid_tool_arguments`, `Tool_not_allowed`, `Provider_unavailable`, `Invalid_stream`, `Run_limit_reached`, `Storage_unavailable`, and `Server_restarted`. Cancellation is a distinct outcome, not a generic failure. Do not forward provider bodies or exception strings to the UI.

## 6. Streaming and tool execution

### Wire events

Version every public event. Each has conversation/turn/attempt/run/message IDs and a monotonically increasing per-run sequence number. Define a closed payload union for:

- `Run_started`.
- `Text_appended`: segment ID plus a text delta.
- `Tool_started` / `Tool_finished`: safe display label and outcome, not raw arguments/results.
- `Evidence_added`: a verified evidence projection/reference.
- `Run_completed`, `Run_cancelled`, or `Run_failed`: exactly one terminal event.

Keep connection heartbeats separate from durable semantic events. No chain-of-thought/reasoning frames are exposed or persisted as public text. Providers may emit reasoning fields; ignore them for this product.

Store a text checkpoint and its event sequence in the same short transaction before broadcasting that event. Coalesce upstream deltas for bounded writes (initial maximum 100 ms or 4 KiB per flush, flushing immediately on terminal state). The browser batches rendering independently. Flush the final pending delta before recording the terminal event. The first visible text must arrive while the upstream stream remains open, not after completion.

### OpenRouter adapter

Add streaming alongside `Llm.complete`, with injectable transport and clock/cancellation for tests. Read Cohttp_eio response bodies incrementally in an Eio cancellation scope. Parse SSE frames across arbitrary byte boundaries, including split UTF-8, CRLF, multiline data, blank delimiters, comments, and keepalives. Reject oversized or malformed frames safely.

Normalize provider chunks into internal events. A repeated finish reason in a usage frame is not a second completion. `[DONE]` ends a well-formed completion stream; unexpected EOF without a valid completion fails the run. An error frame can occur after HTTP success and partial text. Do not surface partial tool arguments as text or actions.

Buffer tool-call fragments by call identity until the upstream round has finished and complete arguments have passed typed parsing. Execute permitted calls sequentially in the first slice. Return bounded, redacted results to the next model round, preserving call IDs and upstream message ordering. Limit the whole loop rather than treating every follow-up as an unlimited new request.

### First tool allowlist

These are dedicated Advisor tools that adapt existing computations. Do not expose `Commands.execute`, `Api.all`, SQL, or an arbitrary query-name dispatcher.

| Tool | Existing computation and bound |
| --- | --- |
| Cycle summary | `Query.Cashflow_summary`, current and up to five prior cycles. |
| Category spending | `Query.Spending_by_category`, current or selected recent cycle, with previous comparison. |
| Category trend | `Query.Category_trend`, one authorized category, at most six cycles. |
| Relevant transactions | `Query.Largest_expenses` plus permitted category filtering where supported; at most twenty rows, explicit truncation/count. Do not claim a complete explanation from a partial sample. |
| Upcoming recurring payments | `Query.Recurring`, bounded to twenty relevant payments, preserving estimated dates and change evidence. |
| Budget status | `Query.Budget_status`, selected cycle, at most twenty categories. |
| Goals | `Query.Goals`, at most twenty permitted goals. |
| Forecast | Existing `Query.Forecast`, horizons 1, 3 or 6 cycles and already-supported scenario inputs only. User-specified assumptions stay distinguishable from observed facts. |

Use typed scope-pinned wrappers around query parsing/results. JSON descriptors can be reused where appropriate, but existing UI/MCP response JSON is not automatically safe model context. Unknown fields, unknown tools, out-of-scope resources, overlarge requests, and arbitrary new scenario shapes are rejected.

Each tool takes a short authorized database snapshot, computes deterministic figures, and releases the guard before network IO. Legacy query implementations may require a dedicated projection adapter in IO; do not migrate all reporting as part of Advisor. Results carry their own sampled time: do not claim a run-wide snapshot if an import occurs between tool reads. If a newer read supersedes earlier evidence, disclose the update rather than combining inconsistent totals.

The model explains; application code calculates. Evidence links must resolve to evidence actually supplied to that run and permitted now. Streamed prose is still fallible model output, not proof of numerical correctness. Authoritative financial cards use computed results; no LLM-authored number may become ledger input or a verified action.

## 7. HTTP transport and persistence

### Proposed browser endpoints

All are same-origin and session-authenticated. State-changing POSTs require the existing CSRF token. Resource IDs are not credentials; no bearer/session tokens in URLs or browser-visible OpenRouter calls.

| Endpoint | Contract |
| --- | --- |
| `POST /api/advisor/turns` | Start a conversation/turn or append to an existing conversation; requires a client idempotency key and expected conversation version. Optional source-finding reference is resolved by the server. Return conversation, turn, attempt, message and run IDs plus accepted version. |
| `POST /api/advisor/turns/:id/retry` | Add an answer attempt to the latest failed/cancelled turn only. Requires a new idempotency key and expected conversation version; recheck consent/scope and bind the new run to the requesting session. Return new attempt/message/run IDs without duplicating the user message. |
| `GET /api/advisor/conversations` | Private, paginated conversation metadata; omit inaccessible content/scopes. |
| `GET /api/advisor/conversations/:id` | Authorized transcript and evidence snapshot, active run and sequence cursor. |
| `GET /api/advisor/runs/:id/events` | Read-only EventSource subscription and replay. Never starts a generation. |
| `POST /api/advisor/runs/:id/cancel` | Idempotent Stop; returns authoritative state. |
| `POST /api/advisor/conversations/:id/delete` | Cancel, revoke delivery, and remove the owner's conversation. |
| `POST /api/advisor/consent` | Explicit per-user opt-in/out; household enablement remains admin-authorized. |

History and snapshot reads use no-store responses. SSE uses `text/event-stream`, appropriate no-cache/no-transform headers, no fixed Content-Length, and a body writer that flushes without buffering the whole response. Maintain existing security headers; do not enable cross-origin subscriptions. Send comment heartbeats around every 15 seconds to detect dead sessions/clients and avoid idle timeouts. Test through deployed Caddy rules; disable buffering/compression for this endpoint if it delays chunks. Normal HTTP responses remain unchanged.

### Replay and bounded retention

Persist conversations, turns, attempts, messages/segments, runs, evidence, consent, and an ordered run-event journal in versioned SQLite migrations. Exact table names/indexes belong to the plan. Enforce uniqueness for `(run_id, sequence)`, `(turn_id, attempt_ordinal)`, one run/assistant message per attempt, idempotency keys scoped to owner, and active-run exclusion atomically. Idempotency fingerprints include the operation and target; a repeated submission from another valid session returns the original accepted result without rebinding its run.

On EventSource reconnect use its Last-Event-ID. On reload the transcript snapshot includes a cursor; subscribe after that cursor. Do not append duplicate events; discard sequence numbers already applied. A gap or compacted cursor causes an authorized snapshot resync, not speculative text repair or a new provider request. Establish replay/live handoff without a race: catch up from the durable cursor and subscribe/poll the bounded broadcaster until caught up, then deliver live events with the same sequence discipline.

Keep semantic replay events for 24 hours after a run terminates, then prune them; keep the materialized transcript until deletion. A snapshot remains sufficient to render completed or partial text after journal pruning. Use bounded replay pages and bounded subscriber queues; overflow closes that subscription and requires resync rather than slowing the provider or exhausting memory. Journals, transcripts, and snapshots are all equally protected financial content.

### Proposed starting limits

These defaults are tunable implementation constants/configuration, not claims about provider performance:

- One active run per user/conversation; eight globally. Reject excess capacity rather than building an indefinite queue.
- User message: 8 KiB UTF-8; generated public text: 64 KiB per run.
- Four upstream model rounds and eight total read-tool executions per run.
- Tool result: 16 KiB each; combined selected evidence: 64 KiB. Mark explicit bounded projections/truncation; never silently clip JSON.
- Model context: at most the last twelve completed turns within a 64 KiB serialized conversation budget, plus separately bounded evidence and system instructions. If the requested context cannot fit the configured model's token budget, fail safely or select fewer whole turns with visible context scope; never silently summarize financial facts into durable memory.
- 120-second run deadline and 30-second upstream inactivity deadline, enforced across tool/model rounds. Provider keepalives do not extend the total deadline.
- SSE frame cap: 256 KiB; subscriber backlog cap: 256 KiB; journal cap: 1 MiB per run. Limit errors preserve already committed partial text.

Cancelled/failed assistant content is displayed with its status. Retry creates a new attempt, assistant message and run under the same user turn, never a hidden provider retry once text has been delivered. Keep earlier attempts inspectable with their status; the latest attempt is the foreground answer. A new attempt re-reads permitted financial evidence and does not inherit the failed attempt's partial output or tool results as authoritative context.

Retry is allowed only for the most recent conversation turn when its latest attempt is failed or cancelled and there is no active run. Retrying an older turn after a follow-up is rejected as a stale-version conflict; branching and regeneration of successful answers are deferred. Concurrent Retry submissions respect the same conversation version and active-run guard as new turns. Repeating a retry's key retrieves that same attempt; a deliberate later retry uses a new key.

Subsequent model context contains each completed turn's user message once and its successful assistant attempt once. Failed/cancelled attempts never enter context automatically; a turn with no completed answer is excluded from the completed-turn history. The user may explicitly quote partial text as unverified content in a new turn. Attempt identities in replay prevent output from an old run being appended to its replacement.

## 8. Module and effect boundaries

Implement interfaces before bodies. Keep new domain states, error variants, and pure transition functions in core. Put application orchestration in workflow using the project's declared application effects, with test/live interpreters; keep Eio resources and resumable provider streams in IO.

Proposed seams:

- **Core:** Advisor identifiers, generation/message states, transition validation, pure limits and event sequencing.
- **Workflow:** accept turn, assemble permitted context, advance a run, cancel, complete, and delete; declared effects for permission-checked snapshots, persistence, provider-event acquisition, clock and cancellation. Do not import `Query` or `Service` into workflow; live adapters bridge legacy computations to detached typed projections.
- **IO:** SQLite stores, authorization refresh, model-facing projection/redaction, OpenRouter stream decoder, supervised Eio run lifetime, journal/broadcast and HTTP endpoints.
- **Shared wire library:** versioned serializable event/command DTOs and total codecs supported by both compiler contexts. No Eio, Async, SQLite, workflow effects, or native-only types. Internal domain types map explicitly to/from wire types; do not make the native domain depend on the browser library.
- **Web:** one shell-owned conversation model/connection, a browser EventSource adapter, typed reducer, keyed message/segment rendering, composer, and existing evidence components.

Do not hold native effects/continuations open across browser requests. The live supervisor owns a run's cancellation scope; transient subscriber fibers only observe it. Effects have narrow interpreter boundaries and cannot be used by model data to choose arbitrary operations. Deterministic tests drive the same orchestration without network access.

The initial interface spike must prove incremental upstream reading, flushed downstream events, and Bonsai updates using installed library versions. This is an implementation-plan gate, not permission to switch runtimes if an API is inconvenient. Pin/choose compatible small bindings if necessary and report a genuine platform blocker before changing the design.

## 9. Later slices: reserved boundaries, not first-slice scaffolding

### Scoped memory

Borrow Hermes's separation of compact durable facts from searchable history. Store structured records in SQLite, not a single prose file. Memory scopes are user, household, and a financial account; each entry has provenance, confirmation status, visibility, applicability/expiry, and revision/supersession. Personal information never silently becomes household memory.

Remember intentions and reasons; retrieve history; recompute money. A parameter affecting calculations becomes a typed setting/constraint, not just a prompt note. Explicit corrections are valuable, but a transaction correction, rule proposal, and remembered preference are different operations. Start with explicit confirmed preferences; inferred memories remain suggestions. Inspection, editing, forgetting, and cancellation of derived jobs must be part of that slice. No memory infrastructure needs to be built before the first read-only conversation works.

### Proposals and application

```text
Draft -> Validated -> Awaiting_approval -> Approved -> Applied
           |                  |              |
         Invalid           Rejected        Stale
```

A typed proposal includes the exact version, source facts/versions, permission scope, effects preview, and verification result. Only application-issued approval of that version can authorize application. Recheck authorization and relevant facts atomically at application time; stale proposals must be recalculated and reapproved. Keep the LLM away from approval tokens and direct writes.

`Applied` means the application recorded the intended change. A planned bank transfer has its own `Planned -> Reported_completed -> Confirmed_by_statement` lifecycle, with a direct `Planned -> Confirmed_by_statement` path. Cancelling a plan does not cancel a bank transfer. A finding is resolved only by its underlying condition, not because chat discussed it or the user approved a plan.

### Background intelligence

Use committed import/reconciliation, cycle, budget, goal, and recurring-payment events to refresh relevant derived findings. Coalesce work and make it idempotent. Deterministic code still decides eligibility and priority; the model supplies interpretation where useful. Persist the fact revision behind an explanation and invalidate it when facts change. Dismissal/snooze behavior remains explicit. No event loop that calls the LLM on every transaction, and no new external notifications by default.

## 10. Acceptance and verification

### Domain and security

- State-machine/property tests cover arbitrary event order, duplicate events, cancellation/completion races, terminal immutability, late callbacks after deletion, one active run, and idempotent submission. Retry tests prove one user message with multiple distinct attempts, preserved partials, correct successful-attempt context selection, no stale-run delta mixing, and rejection of old-turn or concurrent retries.
- Read-only tool tests cover unknown names/fields, scope injection, unauthorized IDs, household masking, bounded lists/results, currencies, estimates, supported forecast assumptions, and no financial writes under any tool response.
- Consent and access tests cover a personal user, household member, unrelated member, administrator, logout, expiry, membership loss, opt-out, replay after revocation, and ownership-only deletion. Multi-session tests cover A starting/B subscribing, A logout or expiry cancelling the run, B logout leaving A's run intact, read checks not extending session lifetime, and an explicit new Retry from B binding only the new run to B.
- Every upstream round enforces the same provider restrictions. Model availability failure never weakens privacy. Untrusted merchant text cannot create instructions, tools, memory, or approvals.
- Model-facing DTO snapshots prove excluded names, numbers, secrets, private counterpart fields, and forbidden log content stay excluded.

### Streaming and persistence

- A fake upstream produces text, pauses before completion, and proves a real browser receives/renders the first delta during that pause. Test through the HTTP server, not only a parser unit test.
- Split every SSE/UTF-8 boundary; cover comments, multiline data, tool fragments, usage frames/repeated finish reasons, malformed frames, size limits, error-after-success, and premature EOF.
- Stop cancels a running stream and pending tool round, closes upstream resources promptly, preserves partial text, and prevents new tool execution. Provider-side billing cancellation is not guaranteed by a successful local Stop.
- Disconnect/reconnect/reload replays once with stable IDs. Missed/capped/pruned journal ranges resync from snapshots. Browser closure triggers the no-subscriber grace deadline; server restart reports interrupted work without reissuing it.
- Storage failure never broadcasts uncommitted text. Backpressure, global limits, deadlines, and pruning remain bounded. Existing API/MCP and existing LLM features continue to work.

### Interaction

- Home follow-up retains the finding's element identity/geometry and viewport anchor; no drawer, secondary landing page, or duplicated briefing.
- Home/Advisor switching during a response preserves partial text, draft, connection, and scroll. Reload restores committed text and the active run. Initial empty/history/error/consent states have a useful action without filler.
- Important explanation, estimate caveats, and financial consequences are visible up front. No UI step is needed merely to understand the finding.
- Desktop/mobile tests cover long responses, scroll-away behavior, keyboard/Stop, screen-reader status announcements, stale imports, unsupported requests, unauthorized conversation URLs, and AI-disabled behavior.
- Build shared DTOs in both OCaml contexts and run the library-boundary regression. Browser code must never acquire native effect/IO dependencies.

For the later implementation, use the repository's documented `dune build`, `dune test`, web build, and browser-test setup; add focused streaming fixtures rather than requiring real financial data or paid provider calls in tests. A live provider smoke test is optional and uses only synthetic data with the same privacy restrictions. Verify FreeBSD/native adapter compatibility and reverse-proxy streaming before release.

This design-only change is verified by relative-link/path checks, isolated jj diff inspection, and an independent builder check. It does not claim production streaming or passing application tests.

## 11. Delivery sequence and approval boundary

1. **First slice:** consent and private conversations; continuous Home/Advisor surface; real streaming; bounded read-only tools; Stop, durable partials, reconnect/restart behavior; deletion and acceptance tests above. No financial mutations or inferred memory.
2. **Second slice:** inspectable explicit memory and remembered decisions, with scope/visibility/expiry and typed settings kept distinct.
3. **Third slice:** a small set of typed, previewable, approval-gated changes and their lifecycle tests. No bank execution implied.
4. **Subsequent work:** targeted background interpretation and richer scenarios, only where existing findings and user priorities justify them.

After approval of this document, write a separate implementation plan with small test-driven tasks and integration gates. Do not begin product implementation or treat the throwaway mockup as production code in this design workspace.

## References

- [Existing product design](../../design.md)
- [Effect-backed module boundaries](2026-10-22-effect-backed-modules-design.md)
- [Hermes persistent memory](https://github.com/NousResearch/hermes-agent/blob/main/website/docs/user-guide/features/memory.md): bounded curated context separate from searchable history; not a financial authorization model.
- [OpenRouter streaming](https://openrouter.ai/docs/api/reference/streaming): SSE chunks, keepalives, usage frames, and provider-dependent cancellation.
- [openrouter-ocaml](https://github.com/mt-caret/openrouter-ocaml): Async-based client; useful reference, not an approved native dependency.

## Write actions: propose, then the user applies (first slice: `propose_set_budget`)

The model never writes. It may call `propose_set_budget` (category alias, monthly amount, rollover); currency and
scope come from the pinned conversation and a model-supplied currency is rejected. The projection resolves the alias
(only a category already shown in the conversation can be proposed), computes the current budget, and builds a
server-rendered evidence card with `proposal = Awaiting`. The runtime stores a row in `advisor_proposals` (typed
arguments, the budget it would replace, 24-hour expiry); the model's tool result says `awaiting_user_confirmation`.

`POST /api/advisor/proposals/{id}/apply` and `/dismiss` are owner-only, session and CSRF protected. Apply runs the
same `Insights.set_budget` as the manual command with the clicking user's access, after checking the proposal is
undecided, unexpired and still based on the budget the user saw (otherwise `Out_of_date`, nothing written). A repeated
click returns the recorded state. States move from `Awaiting` to exactly one of `Applied`, `Declined`, `Out_of_date`,
`Not_applied`, `Lapsed`. Snapshots overlay the recorded state onto the card, so a reload, a second tab and a restart agree.
Goals, categorising and entries are further `propose_*` tools on the same path.

### `propose_categorize` (same path)

The model reads `inbox_rows` (waiting rows with run-local `row_...` aliases, never ids) and `categories` (expense and income
categories with account aliases), then calls `propose_categorize` with up to 20 `{row, category}` pairs. The projection only
accepts rows this run was shown, money out to an expense category and money in to an income one, in the bank account's own
owner and the conversation's pinned scope and currency. The card lists each transaction, amount and category; the proposal
stores the transfer and category ids with each row's revision and position. Apply re-checks every row is still open, unchanged
and the category still valid, then claims the proposal and files all rows through `Inbox.categorize` in one transaction.

### Undo

`POST /api/advisor/proposals/{id}/undo` takes an applied proposal back; the state then reads `Reverted` (stored as
`undone_at` on an `applied` row, since what happened stays recorded). Budget: set the previous budget again for the same
pay cycle, or 0 if there was none. Categorize: `Inbox.reopen` reverses the transfer each row's categorisation posted and puts
the row back where it was, moving its revision on so a new categorisation never finds the reversed one by its idempotency key.
Both require the change to be exactly as the proposal left it; otherwise `Version_conflict` and nothing moves. Claiming the
undo and reversing the change are one transaction.
