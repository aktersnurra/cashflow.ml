#!/usr/bin/env bash
# Synthetic only: requires the existing Playwright installation; never installs packages.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
SWITCH=${CASHFLOW_TEST_SWITCH:-5.5.1}
export NODE_PATH=${NODE_PATH:-/tmp/cashflow-browser-tools/node_modules}
export ADVISOR_EVIDENCE_DIR=${ADVISOR_EVIDENCE_DIR:-$(mktemp -d /tmp/advisor-evidence.XXXXXX)}
mkdir -p "$ADVISOR_EVIDENCE_DIR"
WORK=$(mktemp -d /tmp/advisor-fixture.XXXXXX)
SERVER=
CLIENT=
SERVER_LOG=
check_logs() {
  [[ -n "$SERVER_LOG" ]] || return 0
  python3 - "$SERVER_LOG" "$WORK/session.json" <<'PY'
import json, pathlib, sys
log = pathlib.Path(sys.argv[1]).read_bytes()
secrets = [b'SYNTHETIC_API_SECRET', b'SYNTHETIC_USER_SECRET', b'SYNTHETIC_OUTPUT_SECRET', b'SYNTHETIC_MERCHANT_SECRET']
path = pathlib.Path(sys.argv[2])
if path.exists():
    for session in json.loads(path.read_text()).values():
        secrets.extend(session[k].encode() for k in ('token', 'csrf'))
if any(s and s in log for s in secrets):
    raise SystemExit('synthetic secret found in actual fixture log')
PY
}
stop_server() {
  if [[ -n "$SERVER" ]]; then
    kill "$SERVER" 2>/dev/null || true
    for _ in {1..50}; do kill -0 "$SERVER" 2>/dev/null || break; sleep .1; done
    kill -KILL "$SERVER" 2>/dev/null || true
    wait "$SERVER" 2>/dev/null || true
    SERVER=
    check_logs || return 1
    printf 'secret-scan %s exit=0\n' "$SERVER_LOG" >>"$ADVISOR_EVIDENCE_DIR/exits.txt"
    SERVER_LOG=
  fi
}
stop_client() {
  if [[ -n "$CLIENT" ]]; then
    # The leader may already be reaped; retain its private group until descendants
    # have exited or bounded TERM/KILL escalation has completed.
    if ! kill -TERM -- "-$CLIENT" 2>/dev/null; then
      # A signal can arrive before the launcher calls setsid. Only address a
      # still-running shell-owned job here, never a recycled leader PID.
      if jobs -pr | grep -qx "$CLIENT"; then kill "$CLIENT" 2>/dev/null || true; fi
    fi
    for _ in {1..50}; do kill -0 -- "-$CLIENT" 2>/dev/null || break; sleep .1; done
    if kill -0 -- "-$CLIENT" 2>/dev/null; then
      kill -KILL -- "-$CLIENT" 2>/dev/null || true
      for _ in {1..10}; do kill -0 -- "-$CLIENT" 2>/dev/null || break; sleep .01; done
    fi
    wait "$CLIENT" 2>/dev/null || true
    CLIENT=
  fi
}
cleanup() {
  local status=$?
  stop_client
  stop_server || status=1
  rm -rf -- "$WORK"
  exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
# Refuse collisions rather than connecting tests to someone else's server.
python3 - <<'PY'
import socket
with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(('127.0.0.1', 18197))
PY
run() {
  local label=$1; shift
  local status=0
  # A private process group lets signal cleanup include Python-owned fixture children.
  python3 -c 'import os, sys; os.setsid(); os.execvp(sys.argv[1], sys.argv[1:])' "$@" >"$ADVISOR_EVIDENCE_DIR/$label.log" 2>&1 &
  CLIENT=$!
  wait "$CLIENT" || status=$?
  stop_client
  printf '%s exit=%s\n' "$label" "$status" >>"$ADVISOR_EVIDENCE_DIR/exits.txt"
  if [[ $status != 0 ]]; then cat "$ADVISOR_EVIDENCE_DIR/$label.log" >&2; return "$status"; fi
}
start_server() {
  local label=$1; shift
  _build/default/test/advisor_fixture_server.exe _build/web/web --product "$@" >"$ADVISOR_EVIDENCE_DIR/$label-server.log" 2>&1 &
  SERVER=$!
  SERVER_LOG="$ADVISOR_EVIDENCE_DIR/$label-server.log"
  for _ in {1..100}; do
    kill -0 "$SERVER" 2>/dev/null || { cat "$ADVISOR_EVIDENCE_DIR/$label-server.log" >&2; return 1; }
    if curl --fail --silent --max-time 1 http://127.0.0.1:18197/fixture/session >"$WORK/session.json"; then return; fi
    sleep .05
  done
  echo "fixture readiness failed: $label" >&2; return 1
}
product() {
  local label=$1 script=$2; shift 2
  start_server "$label" "$@"
  run "$label" timeout 100 node "web/e2e/$script"
  stop_server
}
run native-fixture-build opam exec --switch="$SWITCH" -- dune build test/advisor_fixture_server.exe
run web-build opam exec --switch="$SWITCH" -- dune build --workspace dune-workspace.web --profile release @web/default @test/advisor_browser/default
run playwright node -e 'require("playwright")'
run asset-cache node web/e2e/asset_cache.js
run harness-safety python3 test/test_advisor_harness_safety.py
run socket python3 test/test_advisor_http_socket.py
run controller python3 test/test_advisor_browser.py
product product advisor.js
product navigation advisor_navigation.js
product command-bar advisor_command_bar.js
product proposal advisor_proposal.js --privacy-mode propose
product categorize advisor_categorize.js --privacy-mode propose-categorize
product goal advisor_goal.js --privacy-mode propose-goal
product weekly-brief weekly_brief.js --weekly-brief
product entry advisor_entry.js --privacy-mode propose-entry
product presentation advisor_presentation.js
ADVISOR_CHAT_PAUSED=1 product presentation-paused advisor_presentation.js
product smoothness advisor_smoothness.js
product boundaries advisor_boundaries.js --browser-text
product session-race advisor_session_race.js
product currency advisor_currency.js --currency-scales
ADVISOR_UNAVAILABLE=1 product unavailable advisor.js --unavailable
for mode in name route route-history enable enable-pending mobile revoke leave disable enable-shared personal-leave personal-leave-error personal-change; do
  flags=()
  case "$mode" in revoke|leave|disable|enable-shared|personal-*) flags=(--household-finding);; esac
  ADVISOR_REVIEW_CASE=$mode product "review-$mode" advisor_review.js "${flags[@]}"
done
for mode in 0 1; do
  ADVISOR_BASELINE_PENDING=$mode product "baseline-$mode" advisor_membership_baseline.js --household-finding
done
# Standalone production Remote module harness: deliberately injected lifecycle proof.
mkdir "$WORK/remote"
printf '%s' '<!doctype html><html><body><div id="app"></div><script src="/main.bc.js"></script></body></html>' >"$WORK/remote/index.html"
cp _build/web/test/advisor_browser/fixture.bc.js "$WORK/remote/main.bc.js"
_build/default/test/advisor_fixture_server.exe "$WORK/remote" >"$ADVISOR_EVIDENCE_DIR/remote-server.log" 2>&1 &
SERVER=$!
SERVER_LOG="$ADVISOR_EVIDENCE_DIR/remote-server.log"
for _ in {1..100}; do
  kill -0 "$SERVER" 2>/dev/null || exit 1
  if curl --fail --silent --max-time 1 http://127.0.0.1:18197/fixture/session >"$WORK/session.json"; then break; fi
  sleep .05
done
curl --fail --silent --max-time 1 http://127.0.0.1:18197/fixture/session >"$WORK/session.json"
run remote timeout 100 node web/e2e/advisor_remote.js
stop_server
for mode in retry terminal prune delete malicious input output silent-origin silent-subscriber invalid evidence capacity; do
  ADVISOR_PRIVACY_CASE=$mode product "privacy-$mode" advisor.js --privacy-mode "$mode"
done
# Restart is process-level, preserving only this marked temporary synthetic DB.
start_server restart --privacy-mode restart --database "$WORK/restart.sqlite3"
ADVISOR_PRIVACY_CASE=restart-before run restart-before timeout 100 node web/e2e/advisor.js
stop_server
start_server restart-after --privacy-mode restart --database "$WORK/restart.sqlite3"
ADVISOR_PRIVACY_CASE=restart-after run restart-after timeout 100 node web/e2e/advisor.js
stop_server

# Same marked restart database contract, with generation disabled only after persistence.
start_server unavailable-history-before --privacy-mode restart --database "$WORK/unavailable.sqlite3"
ADVISOR_HISTORY_PHASE=before run unavailable-history-before timeout 100 node web/e2e/advisor_unavailable_history.js
stop_server
start_server unavailable-history-after --privacy-mode restart --database "$WORK/unavailable.sqlite3" --unavailable
ADVISOR_HISTORY_PHASE=after run unavailable-history-after timeout 100 node web/e2e/advisor_unavailable_history.js
stop_server
echo "Advisor evidence: $ADVISOR_EVIDENCE_DIR (synthetic DB removed on exit)"
