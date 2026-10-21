(* copied from kcal: lib/date.mli; errors are polymorphic variants here. *)
(** A calendar date, with no time of day and no zone. *)
type t

val of_string : string -> (t, [> `Invalid_date of string ]) result
(** Parses [YYYY-MM-DD]. *)

val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int

val of_instant : utc_offset_s:int -> Ptime.t -> t
(** The date the instant falls on at [utc_offset_s] seconds east of UTC. *)

val add_days : t -> int -> t
val days_between : first:t -> last:t -> int
(** [last] minus [first], in days; negative when [last] is earlier. *)

val midnight_utc : t -> Ptime.t
