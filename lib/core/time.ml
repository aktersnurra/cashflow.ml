(* copied from kcal: lib/time.ml; errors are polymorphic variants here. *)
let invalid_timestamp = `Invalid_timestamp

let parse_offset_datetime value =
  match Ptime.of_rfc3339 ~strict:true value with
  | Ok (time, Some _, _) -> Ok time
  | Ok (_, None, _) -> Error invalid_timestamp
  | Error _ -> Error invalid_timestamp

let to_utc_string time = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 time

let seconds_per_day = 86400

(* No tz database is linked in, so a zone is either a fixed UTC offset or one
   of the three EU standard times with EU summer time. *)
type eu_standard_time = Wet | Cet | Eet
type rule = Fixed of { utc_offset_s : int } | Eu_summer_time of eu_standard_time
type zone = { name : string; rule : rule }

let utc = { name = "UTC"; rule = Fixed { utc_offset_s = 0 } }

let named_rules =
  [ ("UTC", Fixed { utc_offset_s = 0 });
    ("Europe/London", Eu_summer_time Wet); ("Europe/Dublin", Eu_summer_time Wet); ("Europe/Lisbon", Eu_summer_time Wet);
    ("Europe/Stockholm", Eu_summer_time Cet); ("Europe/Oslo", Eu_summer_time Cet); ("Europe/Copenhagen", Eu_summer_time Cet);
    ("Europe/Berlin", Eu_summer_time Cet); ("Europe/Amsterdam", Eu_summer_time Cet); ("Europe/Brussels", Eu_summer_time Cet);
    ("Europe/Paris", Eu_summer_time Cet); ("Europe/Madrid", Eu_summer_time Cet); ("Europe/Rome", Eu_summer_time Cet);
    ("Europe/Vienna", Eu_summer_time Cet); ("Europe/Zurich", Eu_summer_time Cet); ("Europe/Prague", Eu_summer_time Cet);
    ("Europe/Warsaw", Eu_summer_time Cet);
    ("Europe/Helsinki", Eu_summer_time Eet); ("Europe/Athens", Eu_summer_time Eet); ("Europe/Kyiv", Eu_summer_time Eet) ]

let supported_zone_names = List.map fst named_rules
let max_fixed_offset_s = 14 * 3600

let parse_zone value =
  match List.assoc_opt value named_rules with
  | Some rule -> Ok { name = value; rule }
  | None -> (
      match Ptime.of_rfc3339 ~strict:true ("2000-01-01T00:00:00" ^ value) with
      | Ok (_, Some utc_offset_s, _) when abs utc_offset_s <= max_fixed_offset_s -> Ok { name = value; rule = Fixed { utc_offset_s } }
      | _ -> Error (`Unsupported_timezone value))

let zone_name zone = zone.name

let add_seconds time ~seconds =
  match Ptime.add_span time (Ptime.Span.of_int_s seconds) with
  | Some time -> time
  | None -> invalid_arg "Time.add_seconds: result is outside Ptime's range"

let standard_offset_s = function Wet -> 0 | Cet -> 3600 | Eet -> 7200

type eu_switch = Spring_forward | Fall_back

(* Directive 2000/84/EC: summer time begins and ends at 01:00 UTC on the last
   Sunday of March and of October respectively. *)
let eu_summer_time_switch year switch =
  let month = match switch with Spring_forward -> 3 | Fall_back -> 10 in
  let at_0100_utc day =
    match Ptime.of_date_time ((year, month, day), ((1, 0, 0), 0)) with
    | Some time -> time
    | None -> invalid_arg "Time.eu_summer_time_switch: year is outside Ptime's calendar"
  in
  (* March and October both have 31 days. *)
  let days_after_last_sunday =
    match Ptime.weekday (at_0100_utc 31) with
    | `Sun -> 0 | `Mon -> 1 | `Tue -> 2 | `Wed -> 3 | `Thu -> 4 | `Fri -> 5 | `Sat -> 6
  in
  at_0100_utc (31 - days_after_last_sunday)

let utc_offset_s_at zone time =
  match zone.rule with
  | Fixed { utc_offset_s } -> utc_offset_s
  | Eu_summer_time standard ->
      let (year, _, _), _ = Ptime.to_date_time time in
      let in_summer_time =
        Ptime.compare time (eu_summer_time_switch year Spring_forward) >= 0
        && Ptime.compare time (eu_summer_time_switch year Fall_back) < 0
      in
      standard_offset_s standard + if in_summer_time then 3600 else 0

let local_date zone time = Date.of_instant ~utc_offset_s:(utc_offset_s_at zone time) time

(* The offset in effect at the date's UTC midnight is the offset in effect at
   its local midnight: fixed offsets never change, and EU local midnight falls
   between 21:00 and 00:00 UTC while the switches happen at 01:00 UTC. *)
let local_midnight zone date =
  let midnight_utc = Date.midnight_utc date in
  add_seconds midnight_utc ~seconds:(-utc_offset_s_at zone midnight_utc)

let local_day_bounds zone date = (local_midnight zone date, local_midnight zone (Date.add_days date 1))
