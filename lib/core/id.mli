module type S = sig
  type t

  val of_string : string -> t option
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make () : S
