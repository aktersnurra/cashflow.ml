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
let format_money locale ~code json = Mf2_runtime.format_money locale (money_json ~code json)

let date text : Mf2_runtime.date =
  match String.split text ~on:'-' with
  | [ y; m; d ] -> { year = Int.of_string y; month = Int.of_string m; day = Int.of_string (String.prefix d 2) }
  | _ -> { year = 1970; month = 1; day = 1 }

let date_json json = date (J.str ~default:"1970-01-01" json)
let format_date locale json = Mf2_runtime.format_date locale (date_json json)
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
