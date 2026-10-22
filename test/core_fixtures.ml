(* Accounts and transfers built directly, without a ledger or database, for testing the pure
   analytics in cashflow_core. *)
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Date = Cashflow_core.Date
module Household_id = Cashflow_core.Household_id
module Idempotency_key = Cashflow_core.Idempotency_key
module Owner = Cashflow_core.Owner
module Pay_cycle_setting = Cashflow_core.Pay_cycle_setting
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id
module User_id = Cashflow_core.User_id

let uuid n = Printf.sprintf "01890f3e-7c00-7000-8000-%012d" n
let sek = Result.get_ok (Currency.make ~code:"SEK" ~scale:2)
let me = Owner.User (Option.get (User_id.of_string (uuid 999_001)))
let household = Owner.Household (Option.get (Household_id.of_string (uuid 999_002)))
let date text = Result.get_ok (Date.of_string text)
let key text = Result.get_ok (Idempotency_key.of_string text)
let setting = Pay_cycle_setting.default

let account ?(owner = me) ?(tags = []) ?(balance = 0L) n kind name : Account_info.t =
  let balances =
    if Int64.compare balance 0L >= 0 then { Account.zero_balances with debits_posted = balance }
    else { Account.zero_balances with credits_posted = Int64.neg balance }
  in
  { account =
      { id = Option.get (Account_id.of_string (uuid n)); key = key ("account-" ^ string_of_int n); owner; kind; currency = sek; name;
        flags = Account.no_flags; closed = false; balances; created_at = Ptime.epoch };
    details = { Account_info.no_details with tags } }

let id (info : Account_info.t) = info.account.id
let book accounts = Analytics.make_book ~accounts ~owners:[ me ] ~currency:sek

let next_transfer = ref 0

(* Money moving [from] one account [to_] another: the destination is debited. *)
let transfer ?(code = Transfer.Purchase) ?counterparty ~(from : Account_info.t) ~(to_ : Account_info.t) on minor description : Transfer.t =
  incr next_transfer;
  { id = Option.get (Transfer_id.of_string (uuid (500_000 + !next_transfer))); scope = me; key = key ("transfer-" ^ string_of_int !next_transfer);
    debit_account_id = id to_; credit_account_id = id from; amount = Option.get (Amount.of_int64 minor); kind = Single; code;
    memo = { description; counterparty; tags = [] }; value_date = date on; reverses = None; recorded_at = Ptime.epoch }
