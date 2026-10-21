(** A strictly positive amount of money in a currency's minor unit (öre, cent).
    Amounts are never floats; signed balances are plain [int64]s. *)
type t = private int64

val of_int64 : int64 -> t option
(** [None] unless the value is greater than zero. *)

val to_int64 : t -> int64
val equal : t -> t -> bool
val compare : t -> t -> int
val to_string : t -> string

val checked_add : int64 -> int64 -> int64 option
(** Adds two non-negative balance columns; [None] on overflow. *)
