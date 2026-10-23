# Task 1 receipt — DONE_WITH_CONCERNS

Implemented the incremental transport proof only. Actual text reaches Bonsai before an explicit server continue gate opens; later text remains ordered; closing EventSource terminates the native connection-owned producer. No later Advisor domain, storage, route, authentication, or provider features were implemented.

## Commits and scope

- Full BASE: `6e3812eb22db669e74922297ec8af7ae082a5cf1`.
- Implementation: `2cccee5295be762bce3ed40d71bb42493bdf336c` — `feat(advisor): establish incremental SSE transport`.
- Verified source HEAD: `0a9180738cb992d19d14916b9503ab710f1f8239` — `fix(advisor): keep streaming payloads out of debug logs`.
- This receipt is committed separately after the verified source HEAD; its commit ID is returned in the completion response. It contains no source changes.
- Source range reviewed with `jj diff --from 6e3812eb22db669e74922297ec8af7ae082a5cf1 --to 0a9180738cb992d19d14916b9503ab710f1f8239 --color=never`. Working change was empty after both source commits. jj has no staging area; no git commands were used.

Changed source/test files:

```
lib/io/dune
lib/io/http_sse.ml
lib/io/http_sse.mli
lib/io/https.ml
lib/io/https.mli
shared/advisor/dune
shared/advisor/sse.ml
shared/advisor/sse.mli
test/advisor_fixture_server.ml
test/dune
test/test_advisor_stream.ml
web/advisor_stream/dune
web/advisor_stream/transport.ml
web/advisor_stream/transport.mli
web/advisor_stream/smoke/dune
web/advisor_stream/smoke/main.ml
web/advisor_stream/smoke/index.html
web/e2e/advisor_stream_smoke.js
```

The only additional changed file is this authoritative receipt. No controller progress ledger or alternate report path was written. No production test routes, test bypass, credentials, real provider requests, installed-package edits, global CA changes, push, merge, deployment, new workspace, or subagents/reviewers were used.

## Baseline before source edits

All three required baseline commands passed:

| Exact command | Wall time |
| --- | ---: |
| `opam exec --switch=5.5.1 -- dune build` | 6.062 s |
| `opam exec --switch=5.5.1 -- dune test` | 5.869 s |
| `opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default` | 16.707 s |

High-output logs are ephemeral files under `/tmp/advisor-task1-logs/`, including `baseline-{build,test,web}.log`. OCaml and system-design skills were read from the local dotagent skill directory; verification used fresh command output, explicit gates, and jj diff inspection. No separate verification skill file was available there.

## TDD / exact RED and GREEN evidence

1. Added the framing test and ran `opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe` before the protocol library existed. Expected RED: exit 1, `Library "cashflow_advisor_protocol" not found.` Compiler/toolchain was present. Log: `red-native.log`. An earlier test registration attempt also caught the not-yet-created fixture module; registration was corrected before the intended interface RED.
2. The initial browser attempt reported missing local Playwright module resolution. Existing ephemeral tooling was found at `/tmp/cashflow-browser-tools/node_modules`; no package install or repository dependency changes were needed. An occupied loopback port was avoided by selecting `127.0.0.1:18197`; its existing owner was not changed or killed.
3. Real browser REDs during implementation: missing Bonsai mount element prevented visible text; subsequently `first` became visible before release, but the browser assertion failed with state `released open` after close. This exposed connection lifetime ownership rather than being replaced with a mock or buffered demonstration.
4. The installed cohttp-eio adapter defect is retained as a deterministic fake-reader regression. A single 24,000-byte non-repeating `Cohttp.Transfer.Chunk` is read repeatedly into 4096-byte destinations. Byte count matches, but bytes differ. The replacement streaming path receives the *same* payload over trusted loopback TLS and asserts exact equality for chunked and Content-Length responses.
5. Separate security RED: `opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe` exited 1 with `FAIL not logged: advisor-auth-unique`, expected `false`, received `true`. Log: `red-no-payload-logs.log`. A capturing reporter with Debug enabled exposed plaintext authorization logging from the inherited IO adapter.
6. Security GREEN after the minimal non-logging IO primitive overrides: same command exited 0; six tests passed in 1.028 s (`green-no-payload-logs.log`). The reporter regression exercises successful, rejected, untrusted, truncated and cancelled TLS paths, checking synthetic authorization/header/request/response markers are absent. Existing log levels/reporters are restored after the test.

Framing coverage includes the exact required CRLF/UTF-8 sample at every byte split, including the split inside `å`; comments; CR, LF, CRLF; multiline data; incomplete finish; rejection above 256 KiB; and encode/decode roundtrip. TLS tests verify first bytes before a continue gate, ordered completion, non-2xx response suppression, default rejection of the untrusted fixture, successful fixture trust, Host authority including port, POST target/query, bounded HTTP fragments, cancellation propagation and server-side resource cleanup, exact large payloads, and truncated chunked/Content-Length bodies. Each TLS case has a five-second test safety deadline; success is not inferred from a timeout.

## Approved implementation adjustments

These decisions were explicitly escalated and approved by the supervisor:

- Construction-only `Https.stream_post ?tls` injection for a fixture-specific `Tls.Config.client`. The returned function retains the required signature. Production defaults retain system CA chain verification and hostname/SNI checking through the existing connector. The test override uses `X509.Authenticator.chain_of_trust` for an ephemeral self-signed localhost certificate; there is no insecure authenticator or runtime/config/user-controlled trust override.
- Use installed Cohttp request serialization and HTTP body-framing functors directly over verified TLS to avoid the installed **cohttp-eio 6.3.0** `Utils.Reader_flow.single_read` defect: in its partial-buffer branch it copies from source offset 0 instead of `pos`. This can duplicate bytes when a transfer chunk spans multiple partial reads. No installed library was patched. Cohttp's transfer reader consumes at most 32 KiB per fragment, independent of the declared HTTP chunk size; the Eio input parser is capped at 64 KiB. Strict EOF wrappers reject truncated framed bodies instead of silently accepting the stock reader's early Done. EOF-delimited responses retain ordinary EOF completion semantics.
- The synthetic Httpun server owns subscriber producer work in an explicit per-connection Eio cancellation scope. When its connection handler returns, that scope is explicitly cancelled, releasing pending flush waits; writer close runs in `Fun.protect` cleanup. Ordinary `Switch.run` exit alone would wait for children and is insufficient.
- A separate follow-up security commit overrides only IO read/read_line/write primitives to avoid inherited Debug payload logging. Installed HTTP serialization/framing and non-logging refill/input-buffer/flush operations remain in use. Existing `Https.post` implementation remains byte-for-byte unchanged, and its new interface preserves the connector/tls_config API needed by OIDC.

## Final gates (after the security follow-up)

| Exact command | Result | Wall time |
| --- | --- | ---: |
| `opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe` | PASS, six tests; security GREEN | 1.028 s test runtime |
| `opam exec --switch=5.5.1 -- dune build` | PASS | 2.139 s |
| `opam exec --switch=5.5.1 -- dune test` | PASS, including Advisor and boundary checks | 4.666 s |
| `opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default` | PASS | 0.277 s |
| `opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/advisor_stream/smoke/default` | PASS | 0.237 s |

These final build times are incremental/warm, not clean-build benchmarks. An attempted `/usr/bin/time` invocation failed because that utility is not installed; all final gates were rerun successfully using Bash `time`. Intermediate compiler/test failures were fixed only within this task (record-label disambiguation, sequential mutable-parser test evaluation, Dune alias syntax, IO optional argument eta-expansion, timeout API spelling).

Exact final browser reproduction, with native fixture and compiled Bonsai assets:

```sh
_build/default/test/advisor_fixture_server.exe _build/web/web/advisor_stream/smoke > /tmp/advisor-task1-logs/fixture-final.log 2>&1 &
fixture=$!
sleep .2
timeout 15 env NODE_PATH=/tmp/cashflow-browser-tools/node_modules node web/e2e/advisor_stream_smoke.js > /tmp/advisor-task1-logs/final-browser.log 2>&1
result=$?
kill "$fixture"
wait "$fixture" 2>/dev/null
# result = 0
```

Final browser output:

```
first visible before release: 352ms
ordered second and cancellation: 594ms
```

Three earlier fresh-fixture repetitions also passed: first/complete-and-cleanup timings 302/576 ms, 339/577 ms, 330/600 ms. The proof is the DOM assertion **before** `/release`, with fixture state still waiting, not timing alone. `/state` acquires/releases the synthetic Db_guard while the stream waits; no guard is held across gates, upstream waits, or client writes. The second exact DOM assertion is `first second`, followed by EventSource close and observed server producer cleanup. There is no fixture cancellation route or timeout-based success. The 50 ms loop polls cleanup state; it does not cause cancellation. The startup sleep is readiness allowance only. No screenshot was needed.

## Self-review and concerns

No unresolved blocker found in the implemented seam. Scope remained Task 1; source interfaces precede new native implementations. Shared framing is Stdlib-only and compiles in native and web contexts; native IO is not imported into the browser library. The smoke executable is isolated from production `web/main`; the loopback-only fixture and controls are test executables, not production handlers. Buffered clients/OIDC, global trust, database/accounting behavior and production app routes remain unchanged.

Important integration/review concerns:

1. **Task 9 lifetime requirement:** `Http_sse` callers must supply a connection-owned subscriber cancellation scope and exception-safe writer cleanup. A flush acknowledgement alone is not enough to release producer work after disconnect in the installed Httpun/Gluten stack. The eventual generation run must remain separately owned with its approved disconnected-subscriber policy; do not bind generation to a single HTTP connection. Production `http_server.ml` integration was deliberately not attempted in Task 1. This requirement is also documented in `http_sse.mli`.
2. The streaming implementation uses installed Cohttp `Private` framing functors, and the deterministic adapter-defect regression refers to installed `Cohttp_eio__Utils` from tests only. Dependency upgrades need to revisit that compatibility seam and the defect-pinning assertion. The production code does not call the defective adapter.
3. Native framing enforces the configured raw-frame cap. EventSource itself buffers browser-side wire events before dispatch; the adapter additionally rejects dispatched data over 256 KiB, but cannot impose a pre-dispatch browser-internal byte cap. Production output must enforce bounded frames/backlogs at the server; broader admission/replay policies are later tasks.
4. Pre-existing buffered `Https.post`/OIDC transport still inherits Cohttp Debug logging behavior. This task does not silently alter existing clients; the new streaming path's no-payload-logging regression is green. This existing-path concern should be considered separately.
5. Browser regression requires available Playwright/Chromium tooling; this environment resolves it using the existing ephemeral NODE_PATH above. No repository dependency setup was widened. Native TLS tests require the locally available openssl executable to generate ephemeral fixture material.
6. Independent acceptance review is still required and is owned by the parent. No reviewer/subagent was launched by this worker.

Recommended next step: independent review of the complete BASE-to-source-HEAD range and this receipt before proceeding beyond Task 1.

```acceptance-report
{
  "criteriaSatisfied": [
    {"id":"criterion-1","status":"satisfied","evidence":"Only Task 1 transport libraries, native/browser fixtures and tests changed; four narrow platform/security decisions explicitly approved."},
    {"id":"criterion-2","status":"satisfied","evidence":"Exact RED/GREEN outputs, full source commits, reproducible local TLS/browser commands, fresh final native/web gates and integration concerns recorded above."}
  ],
  "changedFiles": ["lib/io/dune","lib/io/http_sse.ml","lib/io/http_sse.mli","lib/io/https.ml","lib/io/https.mli","shared/advisor/dune","shared/advisor/sse.ml","shared/advisor/sse.mli","test/advisor_fixture_server.ml","test/dune","test/test_advisor_stream.ml","web/advisor_stream/dune","web/advisor_stream/transport.ml","web/advisor_stream/transport.mli","web/advisor_stream/smoke/dune","web/advisor_stream/smoke/main.ml","web/advisor_stream/smoke/index.html","web/e2e/advisor_stream_smoke.js",".superpowers/sdd/2026-10-22-financial-advisor-first-slice/task-1-receipt.md"],
  "testsAddedOrUpdated": ["test/test_advisor_stream.ml","test/advisor_fixture_server.ml","web/e2e/advisor_stream_smoke.js","web/advisor_stream/smoke/main.ml"],
  "commandsRun": [
    {"command":"opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe (initial interface RED)","result":"failed","summary":"Expected: protocol library absent, not compiler absent."},
    {"command":"opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe (logging RED)","result":"failed","summary":"Expected regression: synthetic authorization marker was logged."},
    {"command":"opam exec --switch=5.5.1 -- dune exec test/test_advisor_stream.exe","result":"passed","summary":"Six tests, including trusted/untrusted TLS, fragmentation, cancellation, truncation and Debug log capture."},
    {"command":"opam exec --switch=5.5.1 -- dune build","result":"passed","summary":"Baseline and final native build."},
    {"command":"opam exec --switch=5.5.1 -- dune test","result":"passed","summary":"Baseline and final native tests/boundaries."},
    {"command":"opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default","result":"passed","summary":"Baseline and final production web build."},
    {"command":"opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/advisor_stream/smoke/default","result":"passed","summary":"Isolated Bonsai smoke executable built."},
    {"command":"NODE_PATH=/tmp/cashflow-browser-tools/node_modules node web/e2e/advisor_stream_smoke.js (with native fixture)","result":"passed","summary":"352 ms first before release, 594 ms ordered completion and resource cleanup."},
    {"command":"jj diff --from 6e3812eb22db669e74922297ec8af7ae082a5cf1 --to 0a9180738cb992d19d14916b9503ab710f1f8239 --color=never","result":"passed","summary":"Complete source range inspected; buffered post unchanged."},
    {"command":"jj status","result":"passed","summary":"Empty working change after source commits; no staging area."}
  ],
  "validationOutput": ["Native focused: 6 tests passed.","All final native and web gates passed.","Real gated browser proof and disconnect cleanup passed repeatedly."],
  "residualRisks": ["Connection-owned subscriber scope required at later production integration.","Cohttp private API/defect regression requires dependency-upgrade review.","Browser EventSource pre-dispatch buffering is browser-owned.","Existing buffered transport Debug logging remains outside scope.","Independent parent-owned review pending."],
  "noStagedFiles": true,
  "diffSummary": "Incremental verified-TLS POST, bounded shared SSE framing, flush-backed native SSE writer, browser EventSource transport, isolated Bonsai proof and test-only loopback fixtures.",
  "reviewFindings": ["No unresolved Task 1 blocker found in self-review; integration and dependency concerns documented."],
  "manualNotes": "Full verified source HEAD is 0a9180738cb992d19d14916b9503ab710f1f8239; receipt is committed separately. This is self-review, not independent acceptance."
}
```
