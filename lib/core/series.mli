(** Every regular way money enters or leaves a cash account: bills and subscriptions paid from
    it, income landing in it, and transfers between it and any other account (a savings sweep,
    the card bill, an amortisation, the household contribution). The forecast repeats these. *)

type kind = Bill | Income | Transfer

type t = {
  id : string;  (** stable across requests: "bill:<merchant>", "income:<merchant>", "transfer:<from>:<to>" *)
  kind : kind;
  name : string;  (** as the latest payment wrote it *)
  cadence : Analytics.cadence;
  account : Account_id.t;  (** the cash account money moves in or out of *)
  counter : Account_id.t;  (** the account on the other side *)
  amount : int64;  (** what one payment is expected to be; negative when money leaves [account] *)
  last_amount : int64;  (** signed like [amount] *)
  category : Account_info.t option;  (** the expense or income category, for bills and income *)
  first_date : Date.t;
  last_date : Date.t;
  next_expected : Date.t;
  occurrences : int;
}

val kind_to_string : kind -> string

val detect : Analytics.book -> cash:(Account_id.t -> bool) -> setting:Pay_cycle_setting.t -> Transfer.t list -> t list
(** Posted transfers only; reclassifications and reversals are never part of a series. Sorted by
    id. *)
