(* Where an unfinished pay cycle is heading, from what the statements show so far.

   Statements arrive irregularly, so a cycle's figures run only through the day the imports cover
   ([covered_through]), and right after payday they show the salary and not the bills. Comparing
   those with whole cycles says nothing. So the cycle so far is set beside the same days of earlier
   cycles ([usual_by_now]), and the rest of it is projected: the income, bills and household
   contributions still due (the forecast's recurring payments dated after the covered day, up to
   the cycle's end) and usual day-to-day spending for the days left. Moves between the scope's own
   accounts are left out, as the cycle's savings leaves them out. The range is the forecast's own,
   measured by backtesting, around its line at the cycle's end. *)

let ( +: ) = Int64.add
let ( -: ) = Int64.sub

type t = {
  cycle : Cycle.t;
  covered_through : Date.t option;  (** the last day the cycle's figures include; [None] before its first *)
  days_covered : int;
  days_total : int;
  so_far : Analytics.flows;
  so_far_savings : int64;
  usual_by_now : usual option;
  still_due : Forecast.item list;  (** income, bills and contributions after [covered_through], to the cycle's end *)
  usual_rest : int64;  (** usual day-to-day spending for the days left, positive *)
  expected : int64;  (** the cycle's savings if the rest goes as usual *)
  expected_range : (int64 * int64) option;  (** 8 in 10 past forecasts would have landed between *)
  safe_to_spend : int64;
      (** what day-to-day spending can still take until payday and leave the bills still due and the goals'
          monthly amounts covered; negative when they already aren't *)
  goals_set_aside : int64;  (** the goals' monthly amounts *)
  unseen_spending : int64;  (** usual spending assumed for the days since the statements end, up to today *)
  days_to_payday : int;  (** today included *)
}

and usual = {
  spending : int64;
  income : int64;
  savings : int64;
  cycles : int;  (** how many earlier cycles this is the average of *)
}

let days ~first ~last = Date.days_between ~first ~last + 1

(* The same first [n] days of up to six earlier cycles that had income (one without is before the
   books began), averaged; at least two of them, or nothing to compare with. *)
let usual_by_now book ~setting (cycle : Cycle.t) ~n transfers =
  let rec earlier (c : Cycle.t) k acc = if k = 0 then List.rev acc else let p = Cycle.previous setting c in earlier p (k - 1) (p :: acc) in
  let samples =
    List.filter_map
      (fun (p : Cycle.t) ->
        let whole = Analytics.flows_between book ~first:p.first ~last:p.last transfers in
        if Int64.compare whole.income 0L <= 0 then None
        else
          let last = Date.min p.last (Date.add_days p.first (n - 1)) in
          Some (Analytics.flows_between book ~first:p.first ~last transfers))
      (earlier cycle 6 [])
  in
  match samples with
  | [] | [ _ ] -> None
  | _ ->
      let k = Int64.of_int (List.length samples) in
      let avg f = Int64.div (List.fold_left (fun sum x -> sum +: f x) 0L samples) k in
      Some { spending = avg (fun (f : Analytics.flows) -> f.spending); income = avg (fun f -> f.income); savings = avg Analytics.savings; cycles = List.length samples }

let counts (item : Forecast.item) = match item.kind with Bill | Income | Contribution -> true | Transfer | One_off -> false

let make ?(goals : Forecast.goal list = []) book ~setting ~today ~covered_through ~(forecast : Forecast.t) transfers =
  let cycle = Cycle.cycle_of setting today in
  let through = Date.min today (Option.value covered_through ~default:today) in
  let days_total = days ~first:cycle.first ~last:cycle.last in
  let days_covered = if Date.compare through cycle.first < 0 then 0 else min days_total (days ~first:cycle.first ~last:through) in
  let so_far =
    if days_covered = 0 then Analytics.zero_flows else Analytics.flows_between book ~first:cycle.first ~last:(Date.min through cycle.last) transfers
  in
  let after = Date.add_days cycle.first (days_covered - 1) in
  let still_due =
    List.filter
      (fun (i : Forecast.item) -> counts i && Date.compare i.date after > 0 && Date.compare i.date cycle.last <= 0)
      forecast.items
  in
  let per_cycle =
    List.find_map (function Forecast.Usual_spending { per_cycle; _ } -> Some per_cycle | _ -> None) forecast.assumptions
    |> Option.value ~default:0L
  in
  let usual_rest = Int64.div (Int64.mul per_cycle (Int64.of_int (days_total - days_covered))) (Int64.of_int days_total) in
  let so_far_savings = Analytics.savings so_far in
  let expected = so_far_savings +: List.fold_left (fun sum (i : Forecast.item) -> sum +: i.amount) 0L still_due -: usual_rest in
  (* The forecast's spread at the cycle's end (or the last day it reaches before it), moved onto
     this cycle's expected result. *)
  let expected_range =
    let upto = List.filter (fun (d, _, _) -> Date.compare d cycle.last <= 0) forecast.range in
    match List.rev upto with
    | (d, low, high) :: _ -> (
        match List.find_opt (fun (p : Forecast.point) -> Date.equal p.date d) forecast.baseline with
        | Some p when days_covered < days_total -> Some (expected +: (low -: p.cash), expected +: (high -: p.cash))
        | _ -> None)
    | [] -> None
  in
  (* Safe to spend: what is left of the cycle for day-to-day spending once the bills still due are paid and
     the goals have their monthly amounts. Days since the statements end are not free: they are counted at
     usual spending, so an upload a week old doesn't make the money look unspent. *)
  let goals_set_aside = List.fold_left (fun sum (g : Forecast.goal) -> sum +: Option.value g.monthly ~default:0L) 0L goals in
  (* The days after the last covered one, today included. *)
  let unseen_days = max 0 (Date.days_between ~first:(Date.max after (Date.add_days cycle.first (-1))) ~last:today) in
  let unseen_days = min unseen_days (days_total - days_covered) in
  let unseen_spending = Int64.div (Int64.mul per_cycle (Int64.of_int unseen_days)) (Int64.of_int days_total) in
  let due = List.fold_left (fun sum (i : Forecast.item) -> sum +: i.amount) 0L still_due in
  let safe_to_spend = so_far_savings +: due -: goals_set_aside -: unseen_spending in
  { cycle; covered_through = (if days_covered = 0 then None else Some after); days_covered; days_total; so_far; so_far_savings;
    usual_by_now = (if days_covered = 0 || days_covered >= days_total then None else usual_by_now book ~setting cycle ~n:days_covered transfers);
    still_due; usual_rest; expected; expected_range; safe_to_spend; goals_set_aside; unseen_spending;
    days_to_payday = max 1 (days ~first:today ~last:cycle.last) }
