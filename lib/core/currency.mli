(** A currency: its ISO 4217 code and the number of decimals in its minor unit. *)
type t

val make : code:string -> scale:int -> (t, [> `Invalid_currency of string ]) result
(** [code] is three uppercase ASCII letters; [scale] is between 0 and 4. *)

val code : t -> string
val scale : t -> int
val equal : t -> t -> bool
(** Currencies are equal when their codes are. *)
