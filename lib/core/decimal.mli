type error =
  | Invalid_scale of int
  | Invalid_format
  | Excess_precision of { scale : int }
  | Out_of_range

val parse : scale:int -> string -> (int64, error) result
(** Scales must be in 0..4. *)

val format : scale:int -> int64 -> string
(** Asserts that the programmer-supplied scale is in 0..4. *)
