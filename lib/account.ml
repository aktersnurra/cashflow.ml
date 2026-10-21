type kind = Asset | Liability | Equity | Income | Expense

(* TigerBeetle's balance constraints. Both checks include pending amounts, so a reservation can't
   push an account past its limit either. *)
type flags = { debits_must_not_exceed_credits : bool; credits_must_not_exceed_debits : bool }

let no_flags = { debits_must_not_exceed_credits = false; credits_must_not_exceed_debits = false }

type balances = { debits_posted : int64; credits_posted : int64; debits_pending : int64; credits_pending : int64 }

let zero_balances = { debits_posted = 0L; credits_posted = 0L; debits_pending = 0L; credits_pending = 0L }

(* What a caller asks for when opening an account; everything else is assigned by the ledger. *)
type spec = { owner : Owner.t; kind : kind; currency : Currency.t; name : string; flags : flags }

type t = {
  id : Account_id.t;
  key : Idempotency_key.t;
  owner : Owner.t;
  kind : kind;
  currency : Currency.t;
  name : string;
  flags : flags;
  closed : bool;
  balances : balances;
  created_at : Ptime.t;
}

let max_name_length = 200

let validate_spec (spec : spec) =
  let name = String.trim spec.name in
  if name = "" || String.length name > max_name_length then
    Error (`Invalid_account (Printf.sprintf "account name must be 1 to %d bytes" max_name_length))
  else if spec.flags.debits_must_not_exceed_credits && spec.flags.credits_must_not_exceed_debits then
    Error (`Invalid_account "an account can't limit both its debits and its credits")
  else Ok { spec with name }

let matches_spec (account : t) (spec : spec) =
  Owner.equal account.owner spec.owner && account.kind = spec.kind && Currency.equal account.currency spec.currency
  && String.equal account.name spec.name && account.flags = spec.flags

(* Assets and expenses grow with debits; liabilities, equity and income grow with credits. *)
let debit_normal = function Asset | Expense -> true | Liability | Equity | Income -> false

let signed kind ~debits ~credits = if debit_normal kind then Int64.sub debits credits else Int64.sub credits debits

let posted_balance account =
  signed account.kind ~debits:account.balances.debits_posted ~credits:account.balances.credits_posted

let pending_balance account =
  signed account.kind ~debits:account.balances.debits_pending ~credits:account.balances.credits_pending

let has_pending account =
  not (Int64.equal account.balances.debits_pending 0L && Int64.equal account.balances.credits_pending 0L)

let kind_to_string = function
  | Asset -> "asset"
  | Liability -> "liability"
  | Equity -> "equity"
  | Income -> "income"
  | Expense -> "expense"
