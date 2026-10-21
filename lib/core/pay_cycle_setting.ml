(* How an owner's month (pay cycle) is defined. Stored as a compact string, e.g. "25;previous;SE;covered". *)
type start = Day of int | Last_banking_day
type adjustment = Previous_banking_day | Next_banking_day | No_adjustment
type naming = Mostly_covered_month | Starting_month
type t = { start : start; adjustment : adjustment; calendar : string; naming : naming }

let default = { start = Day 25; adjustment = Previous_banking_day; calendar = "SE"; naming = Mostly_covered_month }
let calendar_month = { start = Day 1; adjustment = No_adjustment; calendar = "SE"; naming = Starting_month }

let to_string setting =
  String.concat ";"
    [ (match setting.start with Day day -> string_of_int day | Last_banking_day -> "last");
      (match setting.adjustment with Previous_banking_day -> "previous" | Next_banking_day -> "next" | No_adjustment -> "none");
      setting.calendar;
      (match setting.naming with Mostly_covered_month -> "covered" | Starting_month -> "starting") ]

let make ~start ~adjustment ~calendar ~naming =
  match start with
  | Day day when day < 1 || day > 28 -> Error (`Invalid_pay_cycle "the cycle start day must be between 1 and 28")
  | _ when not (List.mem calendar Holidays.supported_calendars) ->
      Error (`Invalid_pay_cycle ("unsupported holiday calendar " ^ calendar))
  | _ -> Ok { start; adjustment; calendar; naming }

let of_string value =
  match String.split_on_char ';' value with
  | [ start; adjustment; calendar; naming ] -> (
      let start = if start = "last" then Some Last_banking_day else Option.map (fun day -> Day day) (int_of_string_opt start) in
      let adjustment =
        match adjustment with
        | "previous" -> Some Previous_banking_day
        | "next" -> Some Next_banking_day
        | "none" -> Some No_adjustment
        | _ -> None
      in
      let naming = match naming with "covered" -> Some Mostly_covered_month | "starting" -> Some Starting_month | _ -> None in
      match start, adjustment, naming with
      | Some start, Some adjustment, Some naming -> Result.to_option (make ~start ~adjustment ~calendar ~naming)
      | _ -> None)
  | _ -> None
