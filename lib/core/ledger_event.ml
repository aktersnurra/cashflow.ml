(* One write in a batch. [linked] chains it to the next event: the chain is applied in full or
   not at all, like TigerBeetle's [linked] flag. *)
type phase = Posted | Pending of { expires_on : Date.t option }

type transfer = {
  debit_account_id : Account_id.t;
  credit_account_id : Account_id.t;
  amount : Amount.t;
  phase : phase;
  code : Transfer.code;
  memo : Transfer.memo;
  value_date : Date.t;
}

type body =
  | Transfer of transfer
  | Post_pending of { pending_id : Transfer_id.t; amount : Amount.t option; value_date : Date.t }
      (** [amount] defaults to the full pending amount; a smaller one releases the rest. *)
  | Void_pending of { pending_id : Transfer_id.t; value_date : Date.t }
  | Reverse of { transfer_id : Transfer_id.t; value_date : Date.t option; memo : Transfer.memo option }
      (** [value_date] and [memo] default to the reversed transfer's. *)

type t = { key : Idempotency_key.t; linked : bool; body : body }
