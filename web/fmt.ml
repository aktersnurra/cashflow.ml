(* Turning the server's values into what the messages need. Money arrives as an exact decimal
   string ("1240.50"); its number of decimals is the currency's scale. *)
open! Core

let money ~code text : Mf2_runtime.money =
  let text = String.strip text in
  let negative = String.is_prefix text ~prefix:"-" in
  let body = if negative then String.drop_prefix text 1 else text in
  let whole, fraction = match String.lsplit2 body ~on:'.' with Some (w, f) -> (w, f) | None -> (body, "") in
  let minor = Option.value (Int64.of_string_opt (whole ^ fraction)) ~default:0L in
  { minor = (if negative then Int64.neg minor else minor); scale = String.length fraction; code }

let money_json ~code json = money ~code (J.str ~default:"0" json)

(* An amount to approve, or on the record: whole when it is, exact when it isn't. "SEK 4,000", "SEK 123.45". *)
let amount locale ~code text =
  let m = money ~code text in
  let unit = Int64.pow 10L (Int64.of_int m.scale) in
  Mf2_runtime.format_money locale (if Int64.(rem m.minor unit = 0L) then { m with minor = Int64.(m.minor / unit); scale = 0 } else m)
let format_money locale ~code json = Mf2_runtime.format_money locale (money_json ~code json)

let date text : Mf2_runtime.date =
  match String.split text ~on:'-' with
  | [ y; m; d ] -> { year = Int.of_string y; month = Int.of_string m; day = Int.of_string (String.prefix d 2) }
  | _ -> { year = 1970; month = 1; day = 1 }

let date_json json = date (J.str ~default:"1970-01-01" json)
let format_date locale json = Mf2_runtime.format_date locale (date_json json)

(* What an amount field suggests: "0.00", "0,00". *)
let amount_placeholder locale = Mf2_runtime.format_decimal locale ~scale:2 0L

(* An amount as typed, as the server's decimal. Spaces group thousands ("1 234,50"); with both marks, the commas
   group thousands ("1,234.50"); otherwise a comma is the decimal mark. Anything ambiguous ("1,234") reads as three
   decimals, which the server refuses rather than taking a thousand times less. *)
let typed_amount text =
  let text = String.substr_replace_all text ~pattern:"\xC2\xA0" ~with_:"" |> String.filter ~f:(fun c -> not (Char.is_whitespace c)) in
  if String.mem text '.' then String.filter text ~f:(fun c -> not (Char.equal c ',')) else String.tr text ~target:',' ~replacement:'.'

(* A date that may be in another year, with its year: "Jun 1, 2027", "1 juni 2027". *)
let full_date locale text =
  let d = date text in
  let day = Mf2_runtime.format_date locale d in
  if String.is_prefix locale ~prefix:"sv" then sprintf "%s %d" day d.year else sprintf "%s, %d" day d.year
let to_float json = Option.value (Float.of_string_opt (J.str ~default:"0" json)) ~default:0.

(* "2026-11" as "November 2026" in the user's language. *)
let month_label locale cycle =
  match String.split cycle ~on:'-' with
  | [ y; m ] -> (
      let month = Int.of_string m in
      if String.is_prefix locale ~prefix:"sv" then
        let names = [| "januari"; "februari"; "mars"; "april"; "maj"; "juni"; "juli"; "augusti"; "september"; "oktober"; "november"; "december" |] in
        names.(month - 1) ^ " " ^ y
      else
        let names = [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December" |] in
        names.(month - 1) ^ " " ^ y)
  | _ -> cycle

(* Saving is good news; dipping into savings isn't. *)
let sign_tone amount = if Float.is_negative (to_float amount) then "negative" else "positive"

(* Money in a sentence or a summary is whole: "18 420 kr", not "18 420,00 kr". Exact figures belong where they are
   checked against a statement: the ledger, an account's balance. *)
let whole_money ~code json : Mf2_runtime.money = { minor = Int64.of_float (Float.round_nearest (to_float json)); scale = 0; code }

let whole locale ~code json = Mf2_runtime.format_money locale (whole_money ~code json)

let signed_whole locale ~code json = (if Float.is_positive (to_float json) then "+" else "") ^ whole locale ~code json

(* Whole percent, spaced the Swedish way where that applies: "28 %" / "28%". *)
let percent ?(signed = false) locale x =
  let n = Float.iround_nearest_exn x in
  (if signed && n > 0 then "+" else "") ^ Int.to_string n ^ if String.is_prefix locale ~prefix:"sv" then " %" else "%"
