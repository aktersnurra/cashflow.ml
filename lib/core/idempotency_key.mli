(** A client-chosen key that makes a write safe to retry. Keys are unique within an owner scope. *)
type t = private string

val of_string : string -> (t, [> `Invalid_idempotency_key of string ]) result
(** Between 1 and 256 bytes, with no control characters. *)

val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
