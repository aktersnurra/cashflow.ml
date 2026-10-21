# cashflow

A double-entry household cashflow ledger following TigerBeetle's rules, with an MCP server and a Bonsai web UI planned on top. The design is in [`docs/design.md`](docs/design.md).

## Status

M1, the ledger core, is in place. It has accounts, transfers, linked chains, pending transfers (post, partial post, void, expiry), reversals, idempotent writes and balance verification, over an in-memory store. There is no server, database or UI yet.

## Building

Requires OCaml 5.5 and [opam](https://opam.ocaml.org).

```sh
opam install --deps-only --with-test .
dune build
dune test
```

Property tests use [Hegel](https://ocaml.org/p/hegel/latest/README.html), which needs a native `libhegel` (published for Linux and macOS on Apple Silicon), so the tests run on a development machine or in CI rather than on FreeBSD.

## Layout

- `lib/ledger_core.ml`: the rules, as pure functions over a read-only view of the store
- `lib/ledger.ml`: `Ledger.Make (Store)`, which runs the core inside a store transaction
- `lib/mem_store.ml`: the in-memory store used by tests
- `test/test_ledger.ml`: one Alcotest case per rule
- `test/test_ledger_properties.ml`: the Hegel state-machine test
