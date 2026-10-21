(* copied from kcal: lib/date.ml; errors are polymorphic variants here. *)
(* Represented as the date's UTC midnight, so day arithmetic is Ptime arithmetic. *)
type t = Ptime.t

let of_ymd (year, month, day) = Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0))

let of_string value =
  let invalid = Error (`Invalid_date value) in
  match String.length value, String.split_on_char '-' value with
  | 10, [ year; month; day ]
    when String.length year = 4 && String.length month = 2 && String.length day = 2
         && String.for_all (fun c -> c >= '0' && c <= '9') (year ^ month ^ day) -> (
      match int_of_string_opt year, int_of_string_opt month, int_of_string_opt day with
      | Some year, Some month, Some day -> (match of_ymd (year, month, day) with Some date -> Ok date | None -> invalid)
      | _ -> invalid)
  | _ -> invalid

let to_string date =
  let year, month, day = Ptime.to_date date in
  Printf.sprintf "%04d-%02d-%02d" year month day

let equal = Ptime.equal
let compare = Ptime.compare

let of_instant ~utc_offset_s instant =
  let date, _ = Ptime.to_date_time ~tz_offset_s:utc_offset_s instant in
  match of_ymd date with
  | Some date -> date
  | None -> invalid_arg "Date.of_instant: Ptime produced a date it cannot represent"

let add_days date days =
  match Option.bind (Ptime.Span.of_d_ps (days, 0L)) (Ptime.add_span date) with
  | Some date -> date
  | None -> invalid_arg "Date.add_days: result is outside Ptime's calendar (years 0000-9999)"

(* Both are midnights, so the span is a whole number of days. *)
let days_between ~first ~last = fst (Ptime.Span.to_d_ps (Ptime.diff last first))

let midnight_utc date = date

let of_ymd year month day = of_ymd (year, month, day)
let to_ymd date = Ptime.to_date date
let weekday date = Ptime.weekday date

let days_in_month year month =
  match month with
  | 2 -> if (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0 then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

let max a b = if compare a b >= 0 then a else b
let min a b = if compare a b <= 0 then a else b
