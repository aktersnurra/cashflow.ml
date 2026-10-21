#!/bin/bash
# Browser test: starts the server on a fresh database with the built UI and drives Chromium
# through opening an account, importing a CSV, triaging the inbox, every page, and switching
# to Swedish. Needs Node with Playwright. Build the UI first:
#   dune build --workspace dune-workspace.web --profile release @web/default
set -e
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(mktemp -d)
cd "$ROOT"
eval $(opam env --switch=cashflow 2>/dev/null)
rm -f $WORK/db.sqlite3*
cat > $WORK/cashflow.sexp <<CONFIG
((database $WORK/db.sqlite3) (listen 127.0.0.1:18090) (public_url http://127.0.0.1:18090)
 (oidc ((issuer https://id.invalid) (audience http://127.0.0.1:18090)))
 (static_dir $PWD/_build/web/web))
CONFIG
export CASHFLOW_CONFIG=$WORK/cashflow.sexp CASHFLOW_ALLOW_DEV_SESSIONS=1
BIN=./_build/default/bin/cashflow.exe
$BIN migrate >/dev/null 2>&1
TOKEN=$($BIN create-session alice --admin 2>/dev/null)
$BIN serve > $WORK/server.log 2>&1 &
SERVER=$!
trap "kill $SERVER" EXIT
for i in $(seq 1 30); do curl -s -o /dev/null http://127.0.0.1:18090/health && break; sleep 0.2; done
cd "$ROOT/web/e2e"
OUT=$WORK TOKEN=$TOKEN node budget_rollover.js
OUT=$WORK TOKEN=$TOKEN node test.js
echo "screenshots in $WORK"
