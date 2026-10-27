#!/usr/bin/env bash
# Builds the web UI bundle and writes exactly its deployable files, flat, into <out-dir>, which
# must be missing or empty. The server serves them from static_dir. Needs the opam switches
# 5.5.1 (whose dune drives the build) and cashflow-web (see dune-workspace.web). CI calls this
# too, so a deploy builds the bundle the same way.
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <out-dir>" >&2
  exit 2
fi

out=$1
mkdir -p "$out"
if [ -n "$(ls -A "$out")" ]; then
  echo "$0: $out is not empty" >&2
  exit 1
fi
out=$(cd "$out" && pwd)

cd "$(dirname "$0")/.."

# Everything index.html and manifest.webmanifest reference, plus icon.svg.
files=(index.html main.bc.js style.css manifest.webmanifest icon.svg
  apple-touch-icon.png favicon-32.png icon-192.png icon-512.png)

targets=()
for f in "${files[@]}"; do targets+=("_build/web/web/$f"); done

opam exec --switch=5.5.1 -- dune build --workspace dune-workspace.web --profile release @web/default "${targets[@]}"
install -m 0644 "${targets[@]}" "$out"
