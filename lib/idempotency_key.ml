type t = string

let max_length = 256

let of_string value =
  let length = String.length value in
  if length = 0 || length > max_length then
    Error (`Invalid_idempotency_key (Printf.sprintf "idempotency key must be 1 to %d bytes" max_length))
  else if String.exists (fun c -> Char.code c < 0x20 || Char.code c = 0x7f) value then
    Error (`Invalid_idempotency_key "idempotency key must not contain control characters")
  else Ok value

let to_string key = key
let equal = String.equal
let compare = String.compare
