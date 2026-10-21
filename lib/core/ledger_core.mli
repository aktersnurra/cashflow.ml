(** The ledger's rules over a read-only view of stored state. This module performs no direct
    I/O, but the legacy {!view} and {!env} callbacks can be backed by I/O in callers; this
    interface does not guarantee referential transparency for production writes. Deterministic
    callbacks make core-only calculations reproducible. The store writes back an {!applied} result, which
    only this module can construct, so nothing unvalidated ever reaches storage. *)

type view = {
  account : Account_id.t -> Account.t option;
  account_by_key : Owner.t -> Idempotency_key.t -> Account.t option;
  transfer : Transfer_id.t -> Transfer.t option;
  transfer_by_key : Owner.t -> Idempotency_key.t -> Transfer.t option;
  resolution : Transfer_id.t -> Transfer.t option;  (** the post or void that resolved a pending transfer *)
  reversal : Transfer_id.t -> Transfer.t option;  (** the transfer that reversed this one *)
}

type env = { now : unit -> Ptime.t; fresh_account_id : unit -> Account_id.t; fresh_transfer_id : unit -> Transfer_id.t }

type 'a outcome =
  | Created of 'a
  | Exists of 'a  (** the key was used before for the same request: a safe retry *)

type applied
(** Transfers to insert, in order, and the final state of every account they changed. *)

val transfers : applied -> Transfer.t list
val accounts : applied -> Account.t list
val max_batch_size : int

val apply :
  env -> view -> Owner.t -> Ledger_event.t list ->
  (applied * (Transfer.t outcome, Ledger_error.transfer) result list, Ledger_error.batch) result
(** Applies a batch in order, with one result per event. Idempotency keys are looked up within
    the given scope. A chain of linked events is applied in full or not at all. *)

val open_account :
  env -> view -> Idempotency_key.t -> Account.spec -> (Account.t outcome, [> Ledger_error.account ]) result

val close_account : view -> Account_id.t -> (Account.t, [> Ledger_error.account ]) result
(** Accounts with pending transfers can't be closed, nor can asset and liability accounts with a
    non-zero balance. Closing a closed account succeeds. *)

type mismatch = { account_id : Account_id.t; stored : Account.balances; expected : Account.balances }

val verify : accounts:Account.t list -> transfers:Transfer.t list -> mismatch list
(** Accounts whose stored balances differ from those recomputed from the transfers. *)
