#!/bin/bash
# Browser test: starts the server on a fresh database with the built UI and drives Chromium
# through opening an account, importing a CSV, triaging the inbox, every page, and switching
# to Swedish. Needs Node with Playwright. Build the UI first:
#   dune build --workspace dune-workspace.web --profile release @web/default
set -e
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(mktemp -d)
PORT=${CASHFLOW_E2E_PORT:-18090}
export BASE_URL="http://127.0.0.1:$PORT"
cd "$ROOT"
eval $(opam env --switch="${CASHFLOW_TEST_SWITCH:-5.5.1}" 2>/dev/null)
rm -f $WORK/db.sqlite3*
cat > $WORK/cashflow.sexp <<CONFIG
((database $WORK/db.sqlite3) (listen 127.0.0.1:$PORT) (public_url $BASE_URL)
 (oidc ((issuer https://id.invalid) (audience $BASE_URL)))
 (static_dir $PWD/_build/web/web))
CONFIG
export CASHFLOW_CONFIG=$WORK/cashflow.sexp CASHFLOW_ALLOW_DEV_SESSIONS=1
BIN=./_build/default/bin/cashflow.exe
$BIN migrate >/dev/null 2>&1
TOKEN=$($BIN create-session alice --admin 2>/dev/null)
$BIN serve > $WORK/server.log 2>&1 &
SERVER=$!
trap 'kill "$SERVER" 2>/dev/null || true' EXIT
for i in $(seq 1 30); do curl -s -o /dev/null "$BASE_URL/health" && break; sleep 0.2; done
cd "$ROOT/web/e2e"
OUT=$WORK node signed_out.js
OUT=$WORK TOKEN=$TOKEN node budget_rollover.js
OUT=$WORK TOKEN=$TOKEN node test.js
OUT=$WORK TOKEN=$TOKEN node theme_palette.js
kill $SERVER
wait $SERVER 2>/dev/null || true
# Restart on the same database using only the test fixture's injected outbound network.
cd "$ROOT"
./_build/default/test/logo_browser_server.exe > $WORK/logo-server.log 2>&1 &
SERVER=$!
cd "$ROOT/web/e2e"
for i in $(seq 1 30); do curl -s -o /dev/null "$BASE_URL/health" && break; sleep 0.2; done
OUT=$WORK TOKEN=$TOKEN node merchant_logos.js
echo "screenshots in $WORK"
