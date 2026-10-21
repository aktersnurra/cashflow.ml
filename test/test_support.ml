module Account = Cashflow_core.Account
module Amount = Cashflow_core.Amount
module Currency = Cashflow_core.Currency
module Date = Cashflow_core.Date
module Idempotency_key = Cashflow_core.Idempotency_key
module Ledger_core = Cashflow_core.Ledger_core
module Ledger_error = Cashflow_core.Ledger_error
module Ledger_event = Cashflow_core.Ledger_event
module Owner = Cashflow_core.Owner
module Transfer = Cashflow_core.Transfer

let sek = Result.get_ok (Currency.make ~code:"SEK" ~scale:2)
let eur = Result.get_ok (Currency.make ~code:"EUR" ~scale:2)
let me = Owner.User (Id_generator.user ())
let household = Owner.Household (Id_generator.household ())

(* Deterministic clock and ids, so tests never depend on real time. *)
let env () : Ledger_core.env =
  let ticks = ref 0 in
  { now = (fun () -> incr ticks; Option.get (Ptime.of_float_s (1_760_000_000. +. float !ticks)));
    fresh_account_id = Id_generator.account; fresh_transfer_id = Id_generator.transfer }

let key value = Result.get_ok (Idempotency_key.of_string value)
let amount value = Option.get (Amount.of_int64 (Int64.of_int value))
let date value = Result.get_ok (Date.of_string value)
let today = date "2026-10-21"

module type STORE = sig
  include Ledger.STORE

  val name : string
  val create : unit -> t
end

module Mem : STORE with type t = Mem_store.t = struct
  include Mem_store

  let name = "memory"
end

module Sqlite : STORE with type t = Store_sqlite.t = struct
  include Store_sqlite

  let name = "sqlite"
  let create = open_memory
end

let stores : (module STORE) list = [ (module Mem); (module Sqlite) ]

let result_label = function
  | Ok (Ledger_core.Created _) -> "created"
  | Ok (Exists _) -> "exists"
  | Error error -> Ledger_error.transfer_to_string error

let created = "created"
let exists = "exists"
let failed error = Ledger_error.transfer_to_string error
let expect expected results = Alcotest.(check (list string)) "results" expected (List.map result_label results)
let int64 = Alcotest.int64

module Make (S : STORE) = struct
  module L = Ledger.Make (S)

  let open_account store env ?(owner = me) ?(currency = sek) ?(flags = Account.no_flags) name kind =
    match L.open_account store env (key ("account:" ^ name)) { owner; kind; currency; name; flags } with
    | Ok (Created account | Exists account) -> account.Account.id
    | Error _ -> failwith ("could not open " ^ name)

  let transfer ?(linked = false) ?(phase = Ledger_event.Posted) ?(code = Transfer.Purchase) ?(on = today) key_name ~debit ~credit
      value =
    { Ledger_event.key = key key_name; linked;
      body =
        Transfer
          { debit_account_id = debit; credit_account_id = credit; amount = amount value; phase; code;
            memo = { Transfer.empty_memo with description = key_name }; value_date = on } }

  let account store id = Option.get ((S.view store).account id)
  let balance store id = Account.posted_balance (account store id)
  let pending store id = Account.pending_balance (account store id)

  let submit_ok store env ?(scope = me) events =
    match L.submit store env scope events with
    | Ok results -> results
    | Error error -> Alcotest.fail (Ledger_error.batch_to_string error)

  let created_id = function
    | Ok (Ledger_core.Created (transfer : Transfer.t)) -> transfer.id
    | Ok (Exists _) -> Alcotest.fail "expected a new transfer, got an existing one"
    | Error error -> Alcotest.fail (Ledger_error.transfer_to_string error)
end
