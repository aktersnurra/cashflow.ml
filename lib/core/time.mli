(* copied from kcal: lib/time.mli; errors are polymorphic variants here. *)
val parse_offset_datetime : string -> (Ptime.t, [> `Invalid_timestamp ]) result
val to_utc_string : Ptime.t -> string
val seconds_per_day : int

(** Where calendar days begin and end. *)
type zone

val utc : zone

val parse_zone : string -> (zone, [> `Unsupported_timezone of string ]) result
(** A name from [supported_zone_names], or a fixed offset such as [+02:00] or [Z]. *)

val supported_zone_names : string list
val zone_name : zone -> string
val utc_offset_s_at : zone -> Ptime.t -> int
val local_date : zone -> Ptime.t -> Date.t

val local_day_bounds : zone -> Date.t -> Ptime.t * Ptime.t
(** [start, stop) of the local calendar day, as UTC instants. *)
