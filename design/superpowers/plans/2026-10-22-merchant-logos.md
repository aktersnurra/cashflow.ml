# Merchant Logos Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans for native execution or superpowers:subagent-driven-development if the user chooses delegation. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Resolve merchant logos privately after imports and let users override them from Transactions without losing today's initial-tile fallback.

**Architecture:** A small, typed fetcher accepts an injected DNS/HTTPS boundary and returns verified image bytes. Owner-scoped merchant records reference content-addressed image blobs; a use-case module owns suggestion, cache and override transitions. Import completion schedules background work outside database transactions, while commands, authenticated image routes and a Bonsai editor expose the results.

**Tech Stack:** Existing OCaml 5.5/Eio/SQLite server, Cohttp_eio/TLS, Yojson, Alcotest, Bonsai v0.17 on OCaml 5.1, Playwright. No new runtime dependencies.

**Spec:** `docs/merchant-logos.md`, `docs/cashflow-ui-agent-brief.md`, and `design/design.md` sections 6 and 12. The ignored working briefs have been copied into this workspace unchanged.

## Approved decisions

- Choices belong to each owner's books, including separate personal choices for household members. Only identical image bytes are shared by hash.
- Automatic resolution runs in the background after a successful import. Manual Save fetches synchronously.
- Implementation call: refuse SVG instead of introducing a rasterizer or attempting incomplete sanitisation, as expressly permitted by the brief.
- Implementation call: preserve the existing server's Stdlib/typed-SQL conventions rather than changing dependencies or adopting a new IO abstraction across unrelated modules.

## Global Constraints

- `https` only, port 443 only; reject credentials, IP literals, non-public hostnames and every non-public DNS result. Connect directly to an already checked address with the original hostname for TLS and Host.
- At most 3 redirects, rechecking each destination. 5 s timeout per request; HTML at most 512 KB; images at most 256 KB. Image bytes must identify PNG, ICO, WebP or JPEG, with dimensions at least 32×32.
- Prefer the largest apple-touch-icon, then the largest square icon at least 64 px, then `/favicon.ico`. Failed candidates may fall through to the next safe candidate.
- User choice wins. Unknown automatic results are cached; failed fetches retry only after 30 days. Never automatically replace an intervening user choice.
- With the assistant off, no automatic LLM or merchant network access; manual links still work. LLM prompts contain merchant display names only, batches are bounded, confidence must be 70–100 and domains must be bare public hostnames.
- Record every logo LLM call through `Service.log_llm_call` with purpose `logos`.
- Browser images are same-origin `/api/logos/<id>`; signed-in access is checked against current owner associations even when bytes are deduplicated. Keep CSP `img-src 'self' data:` and use private caching.
- Messages exist in both English and Swedish; keep the existing dense table and coloured initials when any image fails.

## Review Focus

- An automatic response arriving after Remove/Save must not resurrect or overwrite a user's choice: exercise an intervening manual edit in the resolver tests.
- IPv4-mapped IPv6, mixed public/private DNS answers, DNS rebinding and redirects must not reach internal services: assert rejection before the injected connection is called and assert address pinning.
- Deduplication must not disclose another owner's logo: test shared bytes, unassociated users and membership loss at the HTTP route.
- A failed manual fetch must preserve an existing logo, not just leave an empty database empty: assert before/after query equality.
- Broken images, narrow screens, keyboard dismissal and repeated Save must keep the editor usable and the initial visible: browser tests cover these states.

---

### Task 1: Verified logo fetching

**Files:** Create `lib/logo_fetch.ml`, `lib/logo_fetch.mli`, `test/test_logo_fetch.ml`; extend `lib/https.ml` and `test/dune`.

**Interfaces:**
- `Logo_fetch.error` is a named ADT for invalid link, unsafe address, unreachable site, redirect limit, oversized body, unsupported image and too-small image; `error_code` and `error_message` project stable machine and user-facing descriptions.
- `Logo_fetch.network` injects polymorphic `with_request` around each DNS-validation-plus-GET operation, `resolve : host:string -> (string list, error) result` and `get : uri:Uri.t -> address:string -> max_bytes:int -> (response, error) result`; `response` contains status, headers and bounded body. Per-hop timeout scope is concurrency-safe without mutable shared deadlines.
- `Logo_fetch.image` is abstract outside the fetcher, exposing bytes, detected content type, dimensions and final source URL via accessors.
- `Logo_fetch.fetch : network -> string -> (image, error) result` accepts a website or direct image link.
- `Https.logo_network env` resolves with Eio, wraps the entire DNS/request operation in the required timeout, and uses `Cohttp_eio.Client.make_generic` with a pinned TCP connection and the existing TLS authenticator.

- [x] Write table-driven tests for link parsing, public-hostname policy, IPv4/IPv6 forbidden ranges (including mapped addresses), all DNS answers checked, redirect safety and the fourth redirect.
- [x] Add image fixtures with independently known dimensions, incorrect content types, unsupported/truncated bytes, size boundaries and SVG rejection. Assert icon selection order and relative URL resolution against a fetched page.
- [x] Run `dune exec test/test_logo_fetch.exe`; observe the absent API/behaviour fail before adding implementations.
- [x] Implement the interface, pure URL/address/image/icon decisions and bounded injected fetch chain. Add the production Eio transport without automatic redirects or second DNS resolution.
- [x] Run the new executable and `dune test`; inspect the fetcher's trust boundary and pinning calls.
- [x] `jj diff` and `jj commit -m "feat(logos): add bounded SSRF-safe logo fetching"`.

### Task 2: Owner-scoped logo states and automatic suggestions

**Files:** Append migration 6 in `lib/schema.ml`; create `lib/merchant_logo_store.ml`, `lib/merchant_logos.ml`, `lib/merchant_logos.mli`, `test/test_merchant_logos.ml`; extend `lib/sql.ml`, `lib/service.ml`, `test/dune`.

**Interfaces:**
- Add a typed `Sql.blob` field codec for genuine SQLite BLOB values.
- Store tables follow the brief: `logos` stores SHA-256-deduplicated validated blobs; `merchant_logos` uses an owner identity plus merchant key primary key, source/status enums, website, nullable logo id/confidence and timestamp.
- `Service.make` gains optional injected logo network and a background scheduling function; defaults are offline/no scheduling, keeping existing tests deterministic.
- `Merchant_logos.set : Service.t -> Access.t -> owner:Owner.t -> merchant_key:string -> link:string -> (view, error) result` checks access before IO and atomically stores only success.
- `Merchant_logos.clear ... ~mode:(None_selected | Automatic)` stores user-none or deletes the override and runs the chain for that merchant.
- `Merchant_logos.resolve : Service.t -> Access.t -> owner:Owner.t -> (string * string) list -> unit` consumes `(key, display_name)` pairs, deduplicates, checks cache, batches at 60 names, verifies the LLM reply and stores only verified results. It rechecks state before writing after external IO.
- `Merchant_logos.view ...` projects `{key, logo}` without IO; owner-scoped blob lookup is reusable by HTTP.

- [x] Add failing integration tests against real migrated SQLite for manual website/image Save, refused Save preserving prior state, owner isolation and image deduplication.
- [x] Add failing chain tests for user/none precedence, resolved/unknown cache hits, retry just before/at 30 days, LLM off, malformed/low-confidence/path/IP proposals, bounded batching, name-only prompt and logged purpose.
- [x] Add an intervening user-edit test using the injected completion or network boundary; verify automatic work cannot overwrite it.
- [x] Run the focused executable to prove red, then implement the migration, codecs, state transitions and resolver with network outside transactions.
- [x] Run focused and full tests, then `jj commit -m "feat(logos): persist owner-scoped choices and suggestions"`.

### Task 3: Import integration, commands and private image serving

**Files:** Modify `lib/import.ml`, `lib/commands.ml`, `lib/http_server.ml`, `bin/cashflow.ml`, `test/test_import.ml`, `test/test_http.ml`, `test/test_merchant_logos.ml`.

**Interfaces:**
- Commands `set_merchant_logo {merchant_key, owner?, link}` and `clear_merchant_logo {merchant_key, owner?, mode}` require `ledger:write` and use existing personal/household owner argument parsing.
- `query_transactions`, including `fold_triage`, adds `merchant: {key, logo}`. Prefer the import's existing merchant key; otherwise use `Analytics.merchant_key` over transfer memo. Determine owner from visible books rather than exposing hidden transfer scope.
- `/api/logos/<id>` supports GET/HEAD for a signed-in user who currently owns a referring merchant record. Return verified bytes, detected type, private long-lived cache headers and unchanged security headers; inaccessible/missing ids return 404.
- After successful committed import, schedule merchant names from the imported account's owner once; do not schedule previews or failed imports. A server-lifetime Eio switch owns background fibers; expected failures never fail an import.

- [x] Add failing command tests for schemas, successful writes, invalid modes/links, read-only scope and invalid owner access.
- [x] Add query assertions on folded imports and manual entries, proving merchant identity and same-origin URL without network on query.
- [x] Add HTTP tests for unsigned, allowed, wrong owner, shared hash, removed association, left household and HEAD responses/cache/CSP.
- [x] Add import scheduling tests proving successful import does not execute network inline, no work before commit or after failure, and no automatic network with LLM off.
- [x] Run focused tests for red, implement the integration and server wiring, then run `dune build` and `dune test`.
- [x] `jj commit -m "feat(logos): expose commands and authenticated merchant images"`.

### Task 4: Transactions logo editor and browser acceptance

**Files:** Create `web/merchant_logo.ml` and `web/e2e/merchant_logos.js`; modify `web/transactions_page.ml`, `web/style.css`, `locales/en.mf2`, `locales/sv.mf2`, `web/e2e/run.sh`.

**Interfaces:**
- A small Bonsai component owns selected merchant, draft link, pending/error/result state; it exposes the row tile renderer and editor view without leaking mutable state into table rendering.
- Tile is a keyboard-accessible button with a same-origin image over the existing initial fallback; on image load error the fallback remains. It opens a single modal sheet showing current logo, labelled Website or image link input, primary Save, quiet Remove logo/Use automatic and Close.
- Disable actions during a request, preserve draft/error on refused Save, refresh rows after success, manage initial focus, Escape, focus containment and return focus to the opener. Use existing scope/account ownership for write args.
- Map stable fetch failure codes to translated messages; do not translate by comparing arbitrary server exception text.

- [x] Add a browser test using the real server for command persistence and injected same-origin image responses for visual fixtures (no external merchant network). Cover Save, Remove, automatic, a failed Save and image failure fallback.
- [ ] Run it against the old UI and observe the missing tile/editor fail.
- [x] Implement the minimal component and localized copy, maintaining the table's current dimensions and quiet visual language.
- [x] Build server and UI, run `web/e2e/run.sh`, and capture Transactions at desktop and phone widths in light/dark with and without logos.
- [x] Inspect screenshots and browser network requests; prove no merchant requests leave the browser and LLM-off import triggers none from the server.
- [x] Run `dune build`, `dune test`, and `dune build --workspace dune-workspace.web --profile release @web/default` one final time. Review the complete feature diff and report any unverified environment constraint explicitly.
- [x] `jj commit -m "feat(web): add merchant logo tiles and editing sheet"`.

## Baseline evidence

Workspace: `/home/aktersnurra/projects/vibe/cashflow.ml.workspaces/merchant-logos`, workspace name `merchant-logos`.
Base: `99edc4f7b4d1f9368a2394359892e6599fca3390` (the existing current work, including Transactions and fold_triage).
Before product changes: `dune build`, `dune test`, and the release web build all exit 0. The server switch is `5.5.1`; the web switch is `cashflow-web`. `web/e2e/run.sh` names a nonexistent `cashflow` switch but already inherits the valid server environment if that lookup fails; change it only if verification proves necessary.
