module type S = sig
  type t

  val of_string : string -> t option
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make () : S = struct
  type t = string

  let of_string value =
    match Uuidm.of_string value with
    | None -> None
    | Some uuid -> Some (Uuidm.to_string uuid)

  let to_string value = value
  let equal = String.equal
  let compare = String.compare
end
