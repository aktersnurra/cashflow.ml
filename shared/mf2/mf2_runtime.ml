(* Formatting for compiled MessageFormat 2 messages (see tools/mf2c). Plain OCaml with no
   dependencies, so the same code runs on the server and in the browser. *)
type money = { minor : int64; scale : int; code : string }
type date = { year : int; month : int; day : int }

(* CLDR plural rules for the supported languages: English and Swedish both have "one" for the
   integer 1 and "other" for everything else. *)
let plural_category _locale n = if n = 1 then "one" else "other"

let group ~separator digits =
  let n = String.length digits in
  let buffer = Buffer.create (n + (n / 3)) in
  String.iteri (fun i c -> if i > 0 && (n - i) mod 3 = 0 then Buffer.add_string buffer separator; Buffer.add_char buffer c) digits;
  Buffer.contents buffer

let nbsp = "\xC2\xA0"
let is_swedish locale = String.length locale >= 2 && String.sub locale 0 2 = "sv"
let thousands locale = if is_swedish locale then nbsp else ","
let decimal_mark locale = if is_swedish locale then "," else "."
let minus = "\xE2\x88\x92"

let format_int64 locale n =
  let digits = Int64.to_string (Int64.abs n) in
  (if Int64.compare n 0L < 0 then minus else "") ^ group ~separator:(thousands locale) digits

let format_integer locale n = format_int64 locale (Int64.of_int n)

let format_decimal locale ~scale minor =
  let negative = Int64.compare minor 0L < 0 in
  let digits = Int64.to_string (Int64.abs minor) in
  let digits = if String.length digits <= scale then String.make (scale - String.length digits + 1) '0' ^ digits else digits in
  let whole = String.sub digits 0 (String.length digits - scale) and fraction = String.sub digits (String.length digits - scale) scale in
  (if negative then minus else "") ^ group ~separator:(thousands locale) whole ^ if scale = 0 then "" else decimal_mark locale ^ fraction

let format_number locale f = format_decimal locale ~scale:2 (Int64.of_float (Float.round (f *. 100.)))

(* Swedish writes "1 240,50 kr"; English "SEK 1,240.50". *)
let format_money locale m =
  let amount = format_decimal locale ~scale:m.scale m.minor in
  if is_swedish locale then amount ^ nbsp ^ if m.code = "SEK" then "kr" else m.code else m.code ^ nbsp ^ amount

let months_en = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let months_sv = [| "jan."; "feb."; "mars"; "apr."; "maj"; "juni"; "juli"; "aug."; "sep."; "okt."; "nov."; "dec." |]

let format_date locale d =
  if is_swedish locale then Printf.sprintf "%d %s" d.day months_sv.(d.month - 1) else Printf.sprintf "%s %d" months_en.(d.month - 1) d.day

let month_name locale month = if is_swedish locale then months_sv.(month - 1) else months_en.(month - 1)
