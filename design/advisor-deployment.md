# Advisor: deployment and verification gates

**Release status: BLOCKED.** Local synthetic success is not approval to enable a real
provider. No eligible real model/route has been attested by this implementation.

## Operator prerequisites

Advisor uses the existing optional OpenRouter configuration, separately from the
legacy complete-response features. Inspect `cashflow.example.sexp` and
`lib/io/advisor_provider.mli` before configuring it. Explicitly establish, for the
exact model and **every eligible provider endpoint**:

- streaming and the eight application read tools work under the configured
  `provider.only` allowlist, `zdr:true`, `data_collection:"deny"`, and
  `require_parameters:true`, on every round;
- trusted context and completion limits, whole-completion coverage (including tool
  arguments/framing and ignored reasoning), and conservative request-token
  factor/overhead bounds covering tokenizer and provider-added framing;
- privacy/retention eligibility and failure behavior without policy relaxation.

Positive integers or generic OpenRouter parameter support are not capability
attestation. Missing/unsupported configuration must refuse generation. There is
no automatic model/provider fallback. Do not enable a route by guessing a large
context budget. No credentials, paid calls or real financial data are needed for
local verification. A real-provider synthetic smoke requires separate operator
approval for credentials and cost; it cannot substitute for these attestations.

## User contract and data

Individual opt-in is required. Household reads additionally require current
membership and administrator-enabled household consent. Conversations are private
to their owner, not shared with household members. Scope, currency and locale are
pinned; a scope change starts a new conversation. Account identifiers/labels are
pseudonymized for the model. Relevant descriptions, categories, dates, amounts,
balances, commitments, forecast evidence and submitted free text can be disclosed
to OpenRouter. Pseudonymization cannot remove identifying material from free text.
There are no financial mutation tools, arbitrary SQL, external plugins or browsing.

SQLite retains transcripts, evidence and partial/failed attempts until owner
deletion. Replay events are retained for 24 hours after termination, independently
of materialized history. Deletion cancels work, revokes delivery, removes associated
content and blocks late writes; non-content idempotency tombstones can remain for
24 hours. **Backups are not immediately purged by deletion.** Apply a documented
backup retention/access policy. No additional provider-side deletion promise is
made. Treat DB, backups, transcripts, journals and evidence as private financial data.
Do not log prompts, provider bodies, SSE, merchant text, amounts, account identifiers,
session tokens, CSRF or keys. Counts/outcomes/timing/model are sufficient diagnostics.

Originating-session logout/expiry cancels its unfinished run, even if another owner
session is viewing it. Subscriber-only logout closes that subscriber, not the
origin's work. Saved history still requires current owner/scope authorization;
loss never converts Household to Personal. Ownership-only deletion remains possible.
Bytes already delivered cannot be revoked. Stop is an authenticated local command;
**local cancellation cannot guarantee provider-side billing cancellation**.

## Bounds, clocks and recovery

One active run per owner/conversation, eight globally; excess work is rejected,
not queued. Limits include 8 KiB UTF-8 input, 64 KiB output, four rounds/eight tools,
16 KiB per tool/64 KiB evidence, twelve completed context turns within a separate
64 KiB conversation budget, 256 KiB frames/subscriber backlog and 1 MiB journal.
Limits fail safely with committed partials, not silently clipped JSON or summaries.
Retry adds an explicit attempt under the same user turn; reconnect/GET/restart never
regenerates. Restart marks unfinished work `Server_restarted` and retains partials.

Use a reliable wall clock for sessions, timestamps and retention, and monotonic time
for run deadlines. Total run limit is 120 s, upstream inactivity 30 s, no-subscriber
grace 30 s; heartbeats are about 15 s. Provider comments do not extend total time.
Session inspection/SSE heartbeats do not renew session expiry. Never hold the SQL
guard across provider/client waits. Keep the supervisor under the server lifetime.

### Explicit unresolved storage/history limits

Native injected failures prove that uncommitted content is not delivered, not
arbitrary storage survival. Shared SQL rollback/release-cleanup failure containment
remains **unverified**. A persistent storage failure cannot guarantee that terminal
status/events are persisted before storage recovers. Do not claim unconditional
terminal durability or successful cleanup under such failures; validate recovery
and failure containment on the deployment storage stack before release.

HTTP transcript responses have a **4 MiB cap with no pagination contract**. Larger
histories are not solved by metadata pagination. Do not silently truncate history
or advertise unbounded transcript recovery. This is an unresolved release limitation
requiring a separately approved contract, not an automatic fallback in this slice.

## HTTP, Caddy and platform gates

Native OCaml 5.5.1/Eio and browser OCaml 5.1.1/Bonsai (`cashflow-web`) are separate
Dune contexts. Shared protocol/reducer code must compile in both; native library
boundaries must remain free of Async. Serve the release browser assets from
`_build/web/web`. Never deploy `test/advisor_fixture_server.exe` or its controls.

Advisor snapshots/history use no-store. SSE is same-origin authenticated,
`text/event-stream`, no-store/no-transform, with security headers and no fixed
Content-Length. Preserve these headers through the proxy; do not cache event routes.
If the deployment's Caddy rules buffer/compress SSE, isolate this route from normal
compression and flush immediately. An illustrative route separation (adapt to the
existing site, not a complete configuration):

```caddyfile
@advisor_events path /api/advisor/runs/*/events
handle @advisor_events {
    reverse_proxy 127.0.0.1:8080 {
        flush_interval -1
    }
}
handle {
    encode zstd gzip
    reverse_proxy 127.0.0.1:8080
}
```

Keep normal compression elsewhere; do not apply an outer `encode` to the event
handler. Validate the actual site configuration, headers, idle/drain timeouts and
error paths, not just this snippet. Through deployed Caddy/TLS/HTTP2, use only a
synthetic gated upstream: observe a browser delta **before** completion release,
then ordered final delta/one terminal, replay, disconnect and session cancellation.
Check concurrent ordinary requests still work. Linux loopback timing does not prove
this. **Deployed Caddy/proxy/HTTP2/TLS timing is a release BLOCKER here.**

**Native FreeBSD compilation, certificate verification/TLS, socket flushing,
cancellation and disconnect/drain behavior are also release BLOCKERS here.** Run
the native suite and synthetic real-socket/browser streaming checks on that host.
Do not weaken certificate verification or patch package/global trust to obtain a
pass. Chromium keyboard/focus/status/viewport checks are not VoiceOver/NVDA or
physical-device attestation; perform those separately before accessibility sign-off.

## Reproducible local synthetic matrix

Prerequisites: installed native switch 5.5.1, configured `cashflow-web`, Python 3,
Node/Chromium and existing Playwright (default `/tmp/cashflow-browser-tools/node_modules`,
overridable with `NODE_PATH`). No package installation is performed by the runner.

```sh
opam exec --switch=5.5.1 -- dune build
opam exec --switch=5.5.1 -- dune test
opam exec --switch=5.5.1 -- dune build @test/check-library-boundaries
opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default
CASHFLOW_TEST_SWITCH=5.5.1 bash web/e2e/run.sh
CASHFLOW_TEST_SWITCH=5.5.1 bash web/e2e/run_advisor.sh
```

The Advisor runner builds its native/web fixtures, checks loopback readiness,
refuses a port collision, uses fresh synthetic state per case, bounds browser
execution, kills/waits its processes on success/failure/signal, and removes its
owned temporary database/assets. Logs/exit receipts/screenshots remain in the printed
external evidence directory (`ADVISOR_EVIDENCE_DIR` may select one). It never starts
the production binary with fixture controls. The normal runner exercises legacy
import/inbox/budget/navigation flows separately.

### Proof map (not a blanket end-to-end claim)

- Existing product/browser suites: first delta before release, continuity, consent,
  scopes, retry/Stop/history/deletion, currencies, session races and Home boundaries.
- Privacy helper, actual product + native authenticated routes + Eio + fake provider
  + durable SSE + Bonsai: simultaneous retries; origin/subscriber logout during true
  silence; two sampled tool results separated by an explicit native fixture import;
  final-boundary disconnect; pruned replay/snapshot recovery; deletion while the
  provider callback is blocked and an injected late callback; malicious merchant,
  user/output text; UTF-8 input and output bounds; eight active owners/ninth rejection;
  disk-backed restart without provider reissue. Only the evidence-import control is
  allowed to change financial state, and its post-import digest becomes the baseline.
- Pruning deliberately holds **one unchanged real authenticated active snapshot**
  using Playwright response scheduling, then completes/prunes natively. The browser
  receives that genuine stale cursor, consumes the native resync event and fetches
  a current authorized snapshot. This is not fabricated protocol/authority data,
  nor a claim about uncontrolled network timing.
- Every new case compares a deterministic digest of 15 named financial tables:
  accounts/details/tags, transfers/tags, budget_versions, savings_goals,
  goal_earmarks, reconciliation_checkpoints, import_batches/rows, inbox_items,
  statement_legs, triage_transfers and categorisation_rules. It excludes legitimate
  Advisor/auth bookkeeping and is not an attestation of every database table.
- Fixture state exposes bounded counts, tool-result hashes and one synthetic-marker
  boolean, not captured financial request bodies. Runner scans actual server log
  captures for synthetic API/user/merchant/output markers and session/CSRF secrets.
  Absence of these markers is not proof about every possible operational log sink.
- Native socket/controller suites remain separately identified. Native runtime/store/
  tool/provider/property tests prove their isolated failure/authority/bounds contracts,
  not full browser journeys. Controller malformed-response/Remote/legacy-readiness
  injections are protocol/lifecycle checks, not native authorization proof.
- A synthetic provider exercises the real adapter but does not attest real provider
  privacy, model judgment, prompt-injection resistance or accounting/compliance.
  Persistent restart DBs require a matching fixture marker and are never reseeded on
  reopen; unmarked existing databases are refused. Controls bind loopback only and
  are absent from `bin/cashflow.exe`.
