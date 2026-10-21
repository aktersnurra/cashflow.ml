open Base

type error =
  | Invalid_scale of int
  | Invalid_format
  | Excess_precision of { scale : int }
  | Out_of_range

let format ~scale minor =
  assert (scale >= 0 && scale <= 4);
  if scale = 0 then Int64.to_string minor
  else
    let negative = Int64.compare minor 0L < 0 in
    let digits = Int64.to_string (Int64.abs minor) in
    let digits = if String.length digits <= scale then String.make (scale - String.length digits + 1) '0' ^ digits else digits in
    let split = String.length digits - scale in
    (if negative then "-" else "") ^ String.sub digits ~pos:0 ~len:split ^ "." ^ String.sub digits ~pos:split ~len:scale

let parse ~scale text =
  if scale < 0 || scale > 4 then Error (Invalid_scale scale)
  else
    let text = Stdlib.String.trim text in
    let negative = String.is_prefix text ~prefix:"-" in
    let body = if negative then String.sub text ~pos:1 ~len:(String.length text - 1) else text in
    let whole, fraction = match String.lfindi body ~f:(fun _ c -> Char.equal c '.') with
      | Some i -> String.sub body ~pos:0 ~len:i, String.sub body ~pos:(i + 1) ~len:(String.length body - i - 1)
      | None -> body, ""
    in
    let digits s = not (String.is_empty s) && String.for_all s ~f:(fun c -> Char.(c >= '0' && c <= '9')) in
    if not (digits whole && (String.is_empty fraction || digits fraction)) then Error Invalid_format
    else if String.length fraction > scale then Error (Excess_precision { scale })
    else
      let fraction = fraction ^ String.make (scale - String.length fraction) '0' in
      match Stdlib.Int64.of_string_opt (whole ^ fraction) with
      | Some value -> Ok (if negative then Int64.neg value else value)
      | None -> Error Out_of_range
