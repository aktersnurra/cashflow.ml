type t = int64

let of_int64 value = if Int64.compare value 0L > 0 then Some value else None
let to_int64 value = value
let equal = Int64.equal
let compare = Int64.compare
let to_string = Int64.to_string

(* Both operands are non-negative, so the sum overflowed exactly when it wrapped below zero. *)
let checked_add a b =
  let sum = Int64.add a b in
  if Int64.compare sum 0L < 0 then None else Some sum
