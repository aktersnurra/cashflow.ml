(* Pay cycles: the "months" budgets and monthly overviews are measured in. *)
type t = { first : Date.t; last : Date.t; year : int; month : int  (** the month the cycle is named after *) }

let label cycle = Printf.sprintf "%04d-%02d" cycle.year cycle.month
let contains cycle date = Date.compare cycle.first date <= 0 && Date.compare date cycle.last <= 0
let day_count cycle = Date.days_between ~first:cycle.first ~last:cycle.last + 1

let shift_month year month offset =
  let index = (year * 12) + (month - 1) + offset in
  (index / 12, (index mod 12) + 1)

module Make (C : Holidays.CALENDAR) = struct
  let banking_day = Holidays.is_banking_day (module C)

  let rec step_until_banking_day date direction =
    if banking_day date then date else step_until_banking_day (Date.add_days date direction) direction

  (* When the cycle that nominally begins in [year]-[month] actually starts. *)
  let start (setting : Pay_cycle_setting.t) year month =
    match setting.start with
    | Last_banking_day -> step_until_banking_day (Option.get (Date.of_ymd year month (Date.days_in_month year month))) (-1)
    | Day day -> (
        let nominal = Option.get (Date.of_ymd year month day) in
        match setting.adjustment with
        | No_adjustment -> nominal
        | Previous_banking_day -> step_until_banking_day nominal (-1)
        | Next_banking_day -> step_until_banking_day nominal 1)

  (* The month with the most days in the range; ties go to the later month. *)
  let mostly_covered first last =
    let rec go date best counts =
      if Date.compare date last > 0 then best counts
      else
        let year, month, _ = Date.to_ymd date in
        let counts = match counts with (y, m, n) :: rest when y = year && m = month -> (y, m, n + 1) :: rest | _ -> (year, month, 1) :: counts in
        go (Date.add_days date 1) best counts
    in
    go first
      (fun counts ->
        let y, m, _ = List.fold_left (fun (by, bm, bn) (y, m, n) -> if n > bn then (y, m, n) else (by, bm, bn)) (0, 0, 0) counts in
        (y, m))
      []

  let cycle setting year month =
    let first = start setting year month in
    let next_year, next_month = shift_month year month 1 in
    let last = Date.add_days (start setting next_year next_month) (-1) in
    let year, month =
      match setting.naming with Starting_month -> (year, month) | Mostly_covered_month -> mostly_covered first last
    in
    { first; last; year; month }

  (* Adjustments can move a start into the previous month, so check the neighbouring cycles. *)
  let cycle_of setting date =
    let year, month, _ = Date.to_ymd date in
    List.find (fun cycle -> contains cycle date)
      (List.map (fun offset -> let y, m = shift_month year month offset in cycle setting y m) [ 0; 1; -1 ])

  let next setting cycle = cycle_of setting (Date.add_days cycle.last 1)
  let previous setting cycle = cycle_of setting (Date.add_days cycle.first (-1))

  (* Cycles overlapping [first, last], oldest first. *)
  let between setting ~first ~last =
    let rec go cycle acc = if Date.compare cycle.first last > 0 then List.rev acc else go (next setting cycle) (cycle :: acc) in
    go (cycle_of setting first) []
end

module type S = sig
  val cycle_of : Pay_cycle_setting.t -> Date.t -> t
  val next : Pay_cycle_setting.t -> t -> t
  val previous : Pay_cycle_setting.t -> t -> t
  val between : Pay_cycle_setting.t -> first:Date.t -> last:Date.t -> t list
end

let for_setting (setting : Pay_cycle_setting.t) : (module S) =
  match Holidays.find setting.calendar with
  | Some calendar -> (module Make ((val calendar : Holidays.CALENDAR)))
  | None -> invalid_arg ("unsupported holiday calendar " ^ setting.calendar)

let cycle_of setting date = let module C = (val for_setting setting) in C.cycle_of setting date
let between setting ~first ~last = let module C = (val for_setting setting) in C.between setting ~first ~last
let previous setting cycle = let module C = (val for_setting setting) in C.previous setting cycle
let next setting cycle = let module C = (val for_setting setting) in C.next setting cycle
