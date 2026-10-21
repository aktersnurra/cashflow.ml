(* copied from kcal: lib/id.ml; generates UUIDv7, which sort by creation time, and adds equal and
   compare so ids can key maps. *)
module type S = sig
  type t

  val fresh : unit -> t
  val of_string : string -> t option
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

let now_ms () =
  let days, picoseconds = Ptime.Span.to_d_ps (Ptime.to_span (Ptime_clock.now ())) in
  Int64.add (Int64.mul (Int64.of_int days) 86_400_000L) (Int64.div picoseconds 1_000_000_000L)

module Make () : S = struct
  type t = string

  let generator = Uuidm.v7_non_monotonic_gen ~now_ms (Random.State.make_self_init ())

  let fresh () = Uuidm.to_string (generator ())

  let of_string value =
    match Uuidm.of_string value with
    | None -> None
    | Some uuid -> Some (Uuidm.to_string uuid)

  let to_string value = value
  let equal = String.equal
  let compare = String.compare
end
