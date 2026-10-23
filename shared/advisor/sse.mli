type frame = { id : string option; event : string option; data : string }
type error = Frame_too_large | Incomplete_frame
type t
val create : max_frame_bytes:int -> t
val feed : t -> string -> (frame list, error) result
val finish : t -> (unit, error) result
(** Encodes data lines; rejects CR/LF in metadata and CR in data. *)
val encode : frame -> string
