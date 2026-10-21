type code =
  | Purchase
  | Income
  | Internal  (** between two of the same owner's accounts *)
  | Contribution  (** from a member's personal account to a household account *)
  | Reclassify
  | Reversal
  | Revaluation
  | Opening
  | Settlement
  | Adjustment

(* A pending transfer reserves its amount until a later transfer posts or voids it. *)
type kind =
  | Single
  | Pending of { expires_on : Date.t option }
  | Post_pending of { pending_id : Transfer_id.t }
  | Void_pending of { pending_id : Transfer_id.t }

type memo = { description : string; counterparty : string option; tags : string list }

let empty_memo = { description = ""; counterparty = None; tags = [] }

(* Tags are a set: stored sorted and without duplicates, so a retried request compares equal
   however its tags were ordered. *)
let normalize_memo memo = { memo with tags = List.sort_uniq String.compare memo.tags }

(* Transfers are immutable once recorded: corrections are new transfers that reverse them. A
   void's amount is the pending amount it released; a post's is the amount it posted. *)
type t = {
  id : Transfer_id.t;
  scope : Owner.t;
  key : Idempotency_key.t;
  debit_account_id : Account_id.t;
  credit_account_id : Account_id.t;
  amount : Amount.t;
  kind : kind;
  code : code;
  memo : memo;
  value_date : Date.t;
  reverses : Transfer_id.t option;
  recorded_at : Ptime.t;
}

let code_to_string = function
  | Purchase -> "purchase"
  | Income -> "income"
  | Internal -> "internal"
  | Contribution -> "contribution"
  | Reclassify -> "reclassify"
  | Reversal -> "reversal"
  | Revaluation -> "revaluation"
  | Opening -> "opening"
  | Settlement -> "settlement"
  | Adjustment -> "adjustment"
