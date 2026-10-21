(* Substring replacement without pulling in Str. *)
let replace_all ~sub ~by s =
  if sub = "" then s
  else
    let buffer = Buffer.create (String.length s) in
    let n = String.length sub in
    let rec go i =
      if i > String.length s - n then Buffer.add_string buffer (String.sub s i (String.length s - i))
      else if String.sub s i n = sub then (Buffer.add_string buffer by; go (i + n))
      else (Buffer.add_char buffer s.[i]; go (i + 1))
    in
    go 0;
    Buffer.contents buffer

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0
