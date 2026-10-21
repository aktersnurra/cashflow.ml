(* Public holidays, for working out banking days. Each calendar is a first-class module, so a
   pay cycle can name its calendar in settings and Cycle can be applied to it. *)
module type CALENDAR = sig
  val code : string

  val is_holiday : Date.t -> bool
  (** Days banks don't settle payments on, besides weekends. *)
end

let date year month day = Option.get (Date.of_ymd year month day)

(* The anonymous Gregorian algorithm (Meeus/Jones/Butcher). *)
let easter_sunday year =
  let a = year mod 19 and b = year / 100 and c = year mod 100 in
  let d = b / 4 and e = b mod 4 in
  let f = (b + 8) / 25 in
  let g = (b - f + 1) / 3 in
  let h = ((19 * a) + b - d - g + 15) mod 30 in
  let i = c / 4 and k = c mod 4 in
  let l = (32 + (2 * e) + (2 * i) - h - k) mod 7 in
  let m = (a + (11 * h) + (22 * l)) / 451 in
  let month = (h + l - (7 * m) + 114) / 31 and day = ((h + l - (7 * m) + 114) mod 31) + 1 in
  date year month day

(* The first date from [first] onwards that falls on [weekday]. *)
let next_weekday first weekday =
  let rec go date = if Date.weekday date = weekday then date else go (Date.add_days date 1) in
  go first

module Sweden : CALENDAR = struct
  let code = "SE"

  (* Bank holidays: the public holidays that fall on weekdays, plus midsummer eve, christmas eve
     and new year's eve, which banks treat as holidays too. *)
  let holidays year =
    let easter = easter_sunday year in
    [ date year 1 1; date year 1 6; Date.add_days easter (-2); Date.add_days easter 1; date year 5 1;
      Date.add_days easter 39; date year 6 6; next_weekday (date year 6 19) `Fri; date year 12 24; date year 12 25;
      date year 12 26; date year 12 31 ]

  let cache = Hashtbl.create 8

  let is_holiday day =
    let year, _, _ = Date.to_ymd day in
    let days =
      match Hashtbl.find_opt cache year with
      | Some days -> days
      | None ->
          let days = holidays year in
          Hashtbl.replace cache year days;
          days
    in
    List.exists (Date.equal day) days
end

let calendars : (module CALENDAR) list = [ (module Sweden) ]
let supported_calendars = List.map (fun (module C : CALENDAR) -> C.code) calendars
let find code = List.find_opt (fun (module C : CALENDAR) -> C.code = code) calendars

let is_banking_day (module C : CALENDAR) day =
  match Date.weekday day with `Sat | `Sun -> false | _ -> not (C.is_holiday day)
