(* TigerBeetle's result codes, as far as this ledger has the corresponding features. *)
type transfer =
  [ `Linked_event_failed
  | `Linked_event_chain_open
  | `Exists_with_different_fields
  | `Accounts_must_be_different
  | `Debit_account_not_found
  | `Credit_account_not_found
  | `Accounts_must_have_same_currency
  | `Debit_account_closed
  | `Credit_account_closed
  | `Exceeds_credits
  | `Exceeds_debits
  | `Overflow
  | `Pending_transfer_not_found
  | `Pending_transfer_not_pending
  | `Pending_transfer_already_posted
  | `Pending_transfer_already_voided
  | `Exceeds_pending_transfer_amount
  | `Transfer_not_found
  | `Transfer_not_reversible
  | `Transfer_already_reversed ]

type batch = [ `Batch_empty | `Batch_too_large of int ]

type account =
  [ `Exists_with_different_fields
  | `Invalid_account of string
  | `Account_not_found
  | `Account_has_pending_transfers
  | `Account_balance_not_zero ]

let transfer_to_string : transfer -> string = function
  | `Linked_event_failed -> "another transfer in this linked chain failed"
  | `Linked_event_chain_open -> "the last transfer in the batch is marked linked"
  | `Exists_with_different_fields -> "this idempotency key was already used for a different transfer"
  | `Accounts_must_be_different -> "debit and credit accounts must be different"
  | `Debit_account_not_found -> "debit account not found"
  | `Credit_account_not_found -> "credit account not found"
  | `Accounts_must_have_same_currency -> "debit and credit accounts have different currencies"
  | `Debit_account_closed -> "debit account is closed"
  | `Credit_account_closed -> "credit account is closed"
  | `Exceeds_credits -> "transfer would make the debit account's debits exceed its credits"
  | `Exceeds_debits -> "transfer would make the credit account's credits exceed its debits"
  | `Overflow -> "transfer would overflow an account balance"
  | `Pending_transfer_not_found -> "pending transfer not found"
  | `Pending_transfer_not_pending -> "the referenced transfer is not a pending transfer"
  | `Pending_transfer_already_posted -> "pending transfer was already posted"
  | `Pending_transfer_already_voided -> "pending transfer was already voided"
  | `Exceeds_pending_transfer_amount -> "amount exceeds the pending transfer's amount"
  | `Transfer_not_found -> "transfer not found"
  | `Transfer_not_reversible -> "only posted transfers that are not themselves reversals can be reversed"
  | `Transfer_already_reversed -> "transfer was already reversed"

let batch_to_string : batch -> string = function
  | `Batch_empty -> "batch contains no transfers"
  | `Batch_too_large max -> Printf.sprintf "batch contains more than %d transfers" max

let account_to_string : account -> string = function
  | `Exists_with_different_fields -> "this idempotency key was already used for a different account"
  | `Invalid_account reason -> reason
  | `Account_not_found -> "account not found"
  | `Account_has_pending_transfers -> "account has pending transfers"
  | `Account_balance_not_zero -> "asset and liability accounts can only be closed with a zero balance"
