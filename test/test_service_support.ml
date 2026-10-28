module Import = Cashflow_io.Import
module Service = Cashflow_io.Service
module Store_sqlite = Cashflow_io.Store_sqlite
module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Date = Cashflow_core.Date
module Idempotency_key = Cashflow_core.Idempotency_key
module Ledger_core = Cashflow_core.Ledger_core
module Ledger_error = Cashflow_core.Ledger_error
module Transfer = Cashflow_core.Transfer

let now = Option.get (Ptime.of_date_time ((2026, 10, 21), ((10, 0, 0), 0)))
let make_service () = Service.make ~now:(fun () -> now) (Store_sqlite.open_memory ())
let key value = Result.get_ok (Idempotency_key.of_string value)
let amount value = Option.get (Amount.of_int64 (Int64.of_int value))
let date value = Result.get_ok (Date.of_string value)

let sign_in service name =
  Service.sign_in service
    { issuer = "https://id.example"; subject = name; display_name = Some name; email = Some (name ^ "@example.com"); locale = None }

let ok = function Ok value -> value | Error error -> Alcotest.fail (Service.error_to_string error)

let account_named service access name =
  match List.find_opt (fun (i : Account_info.t) -> i.account.name = name) (Service.accounts service access) with
  | Some info -> info.account.id
  | None -> Alcotest.fail ("no account named " ^ name)

let open_checking service access ?(owner = Service.(Access.personal access)) ?(statement_backed = false) name =
  match
    Service.open_account service access ~key:(key ("open:" ^ name)) ~owner ~kind:Asset ~name ~currency:"SEK"
      ~flags:Account.no_flags ~details:{ Account_info.no_details with statement_backed }
  with
  | Ok (`Created info | `Exists info) -> info.account.id
  | Error error -> Alcotest.fail (Service.error_to_string error)

let entry ?scope key_name ~from_account ~to_account value : Service.entry =
  { key = key key_name; from_account; to_account; amount = amount value; value_date = date "2026-10-20";
    memo = { Transfer.empty_memo with description = key_name }; code = None; scope }

let label = function
  | Ok (Ledger_core.Created _) -> "created"
  | Ok (Exists _) -> "exists"
  | Error error -> Ledger_error.transfer_to_string error

let balance service access id = Account.posted_balance (ok (Service.find_account service access id)).account


let ok_import = function Ok value -> value | Error error -> Alcotest.fail (Import.error_to_string error)
