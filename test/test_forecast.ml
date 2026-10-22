module Cycle = Cashflow_core.Cycle
module Forecast = Cashflow_core.Forecast
module Series = Cashflow_core.Series
open Core_fixtures

(* Thursday 22 Oct 2026, the last day of the cycle that began on Friday 25 Sep; the next payday
   is Friday 23 Oct, since the 25th is a Sunday. *)
let today = date "2026-10-22"
let checking ?(balance = 0L) () = account 1 Asset "Checking" ~balance
let savings ?(balance = 0L) () = account 2 Asset "Savings" ~balance
let card = account 4 Liability "Card"
let groceries = account 5 Expense "Groceries"
let rent = account 6 Expense "Rent"
let salary = account 7 Income "Salary"
let shared = account 8 Asset "Shared" ~owner:household
let euros = { (account 9 Asset "Euro account") with account = { (account 9 Asset "Euro account").account with currency = Result.get_ok (Currency.make ~code:"EUR" ~scale:2) } }

let series ?(kind = Series.Bill) ?(cadence = Analytics.Monthly) ?counter ~(account : Account_info.t) series_id amount next : Series.t =
  { id = series_id; kind; name = series_id; cadence; account = id account; counter = id (Option.value counter ~default:account); amount;
    last_amount = amount; category = None; first_date = date "2026-01-01"; last_date = Date.add_days (date next) (-30);
    next_expected = date next; occurrences = 5 }

let input ?(horizon = 1) ?(imported = []) ?(transfers = []) ?(series = []) ?(budgets = []) ?(goals = []) ?contribution accounts : Forecast.input =
  { today; setting; horizon; book = book accounts; owners = [ me ]; accounts; imported_through = imported; transfers; series; budgets; goals;
    contribution }

let project ?(scenario = Forecast.no_scenario) input = Forecast.project input scenario
let point (f : Forecast.t) on = List.find (fun (p : Forecast.point) -> Date.equal p.date (date on)) f.baseline
let cash_on f on = (point f on).cash
let last_cash (points : Forecast.point list) = (List.nth points (List.length points - 1)).cash
let items_of (f : Forecast.t) item_id = List.filter (fun (i : Forecast.item) -> i.item_id = item_id) f.items
let dates items = List.map (fun (i : Forecast.item) -> Date.to_string i.date) items
let outlook (f : Forecast.t) (a : Account_info.t) = List.find (fun (o : Forecast.outlook) -> Account_id.equal o.account.account.id (id a)) f.accounts
let check_cash label expected actual = Alcotest.(check int64) label expected actual

let salary_then_rent () =
  let checking = checking ~balance:500_000L () in
  let f =
    project
      (input [ checking; salary; rent ]
         ~series:
           [ series ~kind:Income ~account:checking ~counter:salary "income:acme" 3_200_000L "2026-10-23";
             series ~account:checking ~counter:rent "bill:hyra" (-1_200_000L) "2026-11-01" ])
  in
  Alcotest.(check string) "a month ahead" "2026-11-22" (Date.to_string f.horizon_end);
  Alcotest.(check (list string)) "salary" [ "2026-10-23" ] (dates (items_of f "income:acme"));
  Alcotest.(check (list string)) "rent" [ "2026-11-01" ] (dates (items_of f "bill:hyra"));
  check_cash "today" 500_000L (cash_on f "2026-10-22");
  check_cash "after payday" 3_700_000L (cash_on f "2026-10-23");
  check_cash "after rent" 2_500_000L (cash_on f "2026-11-01");
  check_cash "at the end" 2_500_000L (last_cash f.baseline);
  Alcotest.(check string) "lowest is today" "2026-10-22" (Date.to_string (fst f.lowest))

(* Card purchases stay on the card; the card bill brings them into cash. *)
let card_bill () =
  let checking = checking ~balance:1_000_000L () in
  let history = List.map (fun on -> transfer ~from:card ~to_:groceries on 150_000L "ICA") [ "2026-08-10"; "2026-09-10"; "2026-10-10" ] in
  let f =
    project
      (input [ checking; card; groceries ] ~transfers:history
         ~series:[ series ~kind:Transfer ~account:checking ~counter:card "transfer:card" (-453_333L) "2026-10-28" ])
  in
  check_cash "no usual spending from checking" 1_000_000L (cash_on f "2026-10-27");
  check_cash "the card bill" 546_667L (last_cash f.baseline)

let savings_sweep () =
  let checking = checking ~balance:1_000_000L () and savings = savings () in
  let f = project (input [ checking; savings ] ~series:[ series ~kind:Transfer ~account:checking ~counter:savings "transfer:sweep" (-200_000L) "2026-10-26" ]) in
  List.iter (fun (p : Forecast.point) -> check_cash "total unchanged" 1_000_000L p.cash) f.baseline;
  check_cash "checking after" 800_000L (snd (outlook f checking).lowest);
  Alcotest.(check (option string)) "savings never short" None (Option.map Date.to_string (outlook f savings).overdraw)

let stopped_subscription () =
  let checking = checking ~balance:100_000L () in
  let f = project (input [ checking ] ~series:[ series ~account:checking "bill:netflix" (-14_900L) "2026-08-05" ]) in
  Alcotest.(check (list string)) "no payments" [] (dates (items_of f "bill:netflix"));
  Alcotest.(check bool) "said so" true (List.exists (function Forecast.Stopped s -> s.id = "bill:netflix" | _ -> false) f.assumptions)

(* A payment due since the last import may just not be imported yet; one the import already
   covers was skipped this time. *)
let missing_import () =
  let checking = checking ~balance:100_000L () in
  let with_import on = project (input [ checking ] ~imported:[ (id checking, date on) ] ~series:[ series ~account:checking "bill:el" (-50_000L) "2026-10-20" ]) in
  Alcotest.(check (list string)) "not imported yet: due today" [ "2026-10-22"; "2026-11-20" ] (dates (items_of (with_import "2026-10-18") "bill:el"));
  Alcotest.(check (list string)) "imported without it: skipped" [ "2026-11-20" ] (dates (items_of (with_import "2026-10-21") "bill:el"));
  (* Without imports, as the Overview does: a payment from the last five days may not be entered yet. *)
  let without_import next = dates (items_of (project (input [ checking ] ~series:[ series ~account:checking "bill:el" (-50_000L) next ])) "bill:el") in
  Alcotest.(check (list string)) "no imports, three days late: due today" [ "2026-10-22"; "2026-11-19" ] (without_import "2026-10-19");
  Alcotest.(check (list string)) "no imports, ten days late: skipped" [ "2026-11-12" ] (without_import "2026-10-12")

let plan_replaces_household_transfer () =
  let checking = checking ~balance:2_000_000L () in
  let f =
    project
      (input [ checking; shared ] ~contribution:(Outgoing { due_now = 300_000L; per_cycle = 900_000L })
         ~series:[ series ~kind:Transfer ~account:checking ~counter:shared "transfer:household" (-800_000L) "2026-10-25" ])
  in
  Alcotest.(check (list string)) "the detected transfer is replaced" [] (dates (items_of f "transfer:household"));
  let planned = items_of f "contribution" in
  Alcotest.(check (list string)) "what's left now, then the usual day" [ "2026-10-22"; "2026-10-25" ] (dates planned);
  Alcotest.(check (list int64)) "amounts" [ -300_000L; -900_000L ] (List.map (fun (i : Forecast.item) -> i.amount) planned)

let overdraw () =
  let checking = checking ~balance:100_000L () in
  let f = project (input [ checking; rent ] ~series:[ series ~account:checking ~counter:rent "bill:hyra" (-150_000L) "2026-10-30" ]) in
  Alcotest.(check (option string)) "first day below zero" (Some "2026-10-30") (Option.map Date.to_string (outlook f checking).overdraw);
  Alcotest.(check string) "lowest" "2026-10-30" (Date.to_string (fst f.lowest));
  check_cash "how low" (-50_000L) (snd f.lowest)

(* Goals set money aside on payday without changing cash, and stop at their target. *)
let goal_band () =
  let checking = checking ~balance:2_000_000L () in
  let f = project (input [ checking ] ~horizon:3 ~goals:[ { goal_name = "Car"; earmarked = 1_000_000L; monthly = Some 300_000L; target = 1_500_000L } ]) in
  Alcotest.(check int64) "today" 1_000_000L (point f "2026-10-22").earmarked;
  Alcotest.(check int64) "payday" 1_300_000L (point f "2026-10-23").earmarked;
  Alcotest.(check int64) "capped at the target" 1_500_000L (List.nth f.baseline (List.length f.baseline - 1)).earmarked;
  check_cash "cash unchanged" 2_000_000L (last_cash f.baseline)

(* Three complete cycles of spending, spread evenly and in whole öre that add up exactly. *)
let usual_spending () =
  let checking = checking ~balance:5_000_000L () in
  let current = Cycle.cycle_of setting today in
  let c1 = Cycle.previous setting current in
  let c3 = Cycle.previous setting (Cycle.previous setting c1) in
  let window = Date.days_between ~first:c3.first ~last:c1.last + 1 in
  (* The history begins on the window's first day, so all three cycles count. *)
  let history = List.map (fun n -> transfer ~from:checking ~to_:groceries (Date.to_string (Date.add_days c3.first (31 * n))) 300_000L "ICA") [ 0; 1; 2 ] in
  let f = project (input [ checking; groceries ] ~transfers:history) in
  let days = Date.days_between ~first:today ~last:f.horizon_end in
  check_cash "spent by the end" (Int64.sub 5_000_000L (Int64.div (Int64.mul 900_000L (Int64.of_int days)) (Int64.of_int window))) (last_cash f.baseline);
  match List.find_map (function Forecast.Usual_spending { per_cycle; days; _ } -> Some (per_cycle, days) | _ -> None) f.assumptions with
  | Some (per_cycle, days) ->
      Alcotest.(check int) "over three cycles" window days;
      check_cash "per cycle" (Int64.div (Int64.mul 900_000L (Int64.of_int (Cycle.day_count current))) (Int64.of_int window)) per_cycle
  | None -> Alcotest.fail "no usual spending assumption"

let short_history () =
  let checking = checking ~balance:5_000_000L () in
  let f = project (input [ checking; groceries ] ~transfers:[ transfer ~from:checking ~to_:groceries "2026-09-01" 240_000L "ICA" ]) in
  Alcotest.(check (option int)) "24 days, 1–24 Sep" (Some 24) (List.find_map (function Forecast.Short_history d -> Some d | _ -> None) f.assumptions)

(* A few days of history aren't a month's spending squeezed into a few days: what they cost is
   spread over at least a whole cycle. *)
let shorter_than_a_cycle () =
  let checking = checking ~balance:5_000_000L () in
  let f = project (input [ checking; groceries ] ~transfers:[ transfer ~from:checking ~to_:groceries "2026-09-24" 900_000L "HYRA" ]) in
  let cycle_days = Cycle.day_count (Cycle.cycle_of setting today) in
  let days = Date.days_between ~first:today ~last:f.horizon_end in
  check_cash "spread over a cycle" (Int64.sub 5_000_000L (Int64.div (Int64.mul 900_000L (Int64.of_int days)) (Int64.of_int cycle_days))) (last_cash f.baseline);
  Alcotest.(check (option int)) "and says so" (Some 1) (List.find_map (function Forecast.Short_history d -> Some d | _ -> None) f.assumptions)

let assumptions () =
  let checking = checking ~balance:100_000L () in
  let f = project (input [ checking; euros ] ~imported:[ (id checking, date "2026-10-01") ]) in
  let has p = List.exists p f.assumptions in
  Alcotest.(check bool) "stale" true (has (function Forecast.Stale_import d -> Date.to_string d = "2026-10-01" | _ -> false));
  Alcotest.(check bool) "euros left out" true (has (function Forecast.Other_currency [ a ] -> a.account.name = "Euro account" | _ -> false));
  Alcotest.(check bool) "no income" true (has (function Forecast.No_income_detected -> true | _ -> false));
  Alcotest.(check (option string)) "imported through" (Some "2026-10-01") (Option.map Date.to_string f.imported_through)

let one_off () =
  let checking = checking ~balance:2_000_000L () in
  let i = input [ checking ] in
  let baseline = project i in
  Alcotest.(check bool) "no scenario" true (Option.is_none baseline.scenario);
  let f = project i ~scenario:{ Forecast.no_scenario with one_off = [ { date = date "2026-11-15"; amount = -1_200_000L; description = "Soffa" } ] } in
  let scenario = Option.get f.scenario in
  check_cash "before" 2_000_000L (List.find (fun (p : Forecast.point) -> Date.equal p.date (date "2026-11-14")) scenario).cash;
  check_cash "after" 800_000L (last_cash scenario);
  check_cash "baseline untouched" 2_000_000L (last_cash f.baseline)

let follow_budgets () =
  let checking = checking ~balance:5_000_000L () in
  let history = List.map (fun on -> transfer ~from:checking ~to_:groceries on 300_000L "ICA") [ "2026-07-01"; "2026-08-01"; "2026-09-01" ] in
  let f = project (input [ checking; groceries ] ~transfers:history ~budgets:[ (id groceries, 150_000L) ]) ~scenario:{ Forecast.no_scenario with follow_budgets = true } in
  let days = Date.days_between ~first:today ~last:f.horizon_end in
  let cycle_days = Cycle.day_count (Cycle.cycle_of setting today) in
  check_cash "the budget instead of the history"
    (Int64.sub 5_000_000L (Int64.div (Int64.mul 150_000L (Int64.of_int days)) (Int64.of_int cycle_days)))
    (last_cash (Option.get f.scenario))

(* History is worked back from today's balance by undoing later transfers. *)
let history () =
  let checking = checking ~balance:1_000_000L () in
  let f = project (input [ checking; groceries ] ~transfers:[ transfer ~from:checking ~to_:groceries "2026-10-20" 200_000L "ICA" ]) in
  let on d = List.assoc_opt (date d) (List.map (fun (d, v) -> (d, v)) f.history) in
  Alcotest.(check (option int64)) "before the purchase" (Some 1_200_000L) (on "2026-10-19");
  Alcotest.(check (option int64)) "after it" (Some 1_000_000L) (on "2026-10-21");
  Alcotest.(check string) "from the start of the cycle before" (Date.to_string (Cycle.previous setting (Cycle.cycle_of setting today)).first)
    (Date.to_string (fst (List.hd f.history)));
  Alcotest.(check (option int64)) "until yesterday" None (on "2026-10-22")

(* The Overview's card and the forecast agree: an account the card says the bills before payday
   would overdraw also goes below zero in the forecast by payday, unless income is projected into
   it first. The forecast may warn about more, since it also counts usual spending. *)
let agree ~today (i : Forecast.input) =
  let i = { i with today } in
  let cycle = Cycle.cycle_of setting today in
  let payday = Date.add_days cycle.last 1 in
  let flagged = Analytics.shortfalls i.book ~today ~until:cycle.last (Analytics.recurring_payments i.book i.transfers) in
  let f = Forecast.project { i with series = Series.detect i.book ~cash:(fun a -> List.exists (fun (o : Account_info.t) -> Account_id.equal (id o) a && o.account.kind = Asset) i.accounts) ~setting i.transfers } Forecast.no_scenario in
  List.for_all
    (fun (s : Analytics.shortfall) ->
      let a = s.short_account.account.id in
      let o = List.find (fun (o : Forecast.outlook) -> Account_id.equal o.account.account.id a) f.accounts in
      (match o.overdraw with Some d -> Date.compare d payday <= 0 | None -> false)
      || List.exists (fun (it : Forecast.item) -> it.kind = Income && Account_id.equal it.account a && Date.compare it.date payday <= 0) f.items)
    flagged,
  flagged, f

let monthly_history ~from ~to_ ?counterparty name day minor months =
  List.map (fun m -> transfer ~from ~to_ ?counterparty (Printf.sprintf "2026-%02d-%02d" m day) minor name) months

let card_and_forecast () =
  let today = date "2026-10-10" in
  let rent_due balance = input [ checking ~balance (); rent; groceries; salary ] ~transfers:(monthly_history ~from:(checking ()) ~to_:rent "HYRA" 15 150_000L [ 7; 8; 9 ]) in
  let ok, flagged, f = agree ~today (rent_due 100_000L) in
  Alcotest.(check bool) "both warn" true (ok && flagged <> [] && Option.is_some (List.hd f.accounts).overdraw);
  let spending = List.map2 (fun on v -> transfer ~from:(checking ()) ~to_:groceries on v "ICA") [ "2026-07-01"; "2026-07-20"; "2026-09-10" ] [ 500_000L; 700_000L; 600_000L ] in
  let i = rent_due 200_000L in
  let ok, flagged, f = agree ~today { i with transfers = i.transfers @ spending } in
  Alcotest.(check bool) "only the forecast warns, from usual spending" true (ok && flagged = [] && Option.is_some (List.hd f.accounts).overdraw);
  let i = rent_due 100_000L in
  let pay = monthly_history ~from:salary ~to_:(checking ()) ~counterparty:"ACME" "LÖN" 12 3_000_000L [ 7; 8; 9 ] in
  let ok, flagged, f = agree ~today { i with transfers = i.transfers @ pay } in
  Alcotest.(check bool) "salary first: the card warns, the forecast doesn't need to" true (ok && flagged <> [] && Option.is_none (List.hd f.accounts).overdraw)

(* The range: backtests of the same projection over the last year. An account whose balance is
   what its own history adds up to, so the actual line the backtest compares with is right. *)
let opening = account 10 Equity "Opening"
let settled n name transfers =
  let a = account n Asset name in
  let balance =
    List.fold_left
      (fun sum (t : Transfer.t) ->
        if Account_id.equal t.debit_account_id (id a) then Int64.add sum (Analytics.amount t)
        else if Account_id.equal t.credit_account_id (id a) then Int64.sub sum (Analytics.amount t)
        else sum)
      0L transfers
  in
  account n Asset name ~balance

let months_from (y, m) count = List.init count (fun i -> let y, m = Cycle.shift_month y m i in (y, m))
let on (y, m) d = Printf.sprintf "%04d-%02d-%02d" y m d
let before_today = List.filter (fun (t : Transfer.t) -> Date.compare t.value_date today < 0)

(* Salary on payday, rent on the 1st, Spotify on the 15th and nothing else: every past forecast
   was exactly right, so the range has no width. *)
let exact_history () =
  let months = months_from (2026, 1) 10 in
  let probe = account 1 Asset "Checking" in
  let history =
    before_today
      (transfer ~from:opening ~to_:probe "2026-01-01" 5_000_000L "Opening"
      :: List.concat_map
           (fun ym ->
             [ transfer ~code:Income ~counterparty:"ACME" ~from:salary ~to_:probe (Date.to_string (Cycle.cycle_of setting (date (on ym 25))).first) 3_000_000L "LÖN";
               transfer ~from:probe ~to_:rent (on ym 1) 900_000L "HYRA"; transfer ~from:probe ~to_:groceries (on ym 15) 12_900L "SPOTIFY" ])
           months)
  in
  let checking = settled 1 "Checking" history in
  let i = input [ checking; salary; rent; groceries; opening ] ~transfers:history in
  let f = Forecast.project { i with series = Series.detect i.book ~cash:(Account_id.equal (id checking)) ~setting i.transfers } Forecast.no_scenario in
  Alcotest.(check bool) "a range" true (f.range <> []);
  List.iter2
    (fun (p : Forecast.point) (d, low, high) ->
      Alcotest.(check string) "aligned" (Date.to_string p.date) (Date.to_string d);
      Alcotest.(check (pair int64 int64)) ("no width on " ^ Date.to_string d) (p.cash, p.cash) (low, high))
    (List.filteri (fun k _ -> k < List.length f.range) f.baseline) f.range;
  Alcotest.(check (option int)) "always above zero" (Some 100) f.chance_positive;
  Alcotest.(check bool) "says what it rests on" true (List.exists (function Forecast.Range_from { forecasts; _ } -> forecasts >= 10 | _ -> false) f.assumptions);
  Alcotest.(check bool) "no season under a year" true (not (List.exists (function Forecast.Seasonal _ -> true | _ -> false) f.assumptions))

(* Groceries every third day, twice as often last December: December is expected to cost more,
   by half the difference, and the range has width. *)
let seasons () =
  let probe = account 1 Asset "Checking" in
  let days first last = let rec go d = if Date.compare d last > 0 then [] else d :: go (Date.add_days d 3) in go first in
  let shop d = transfer ~from:probe ~to_:groceries (Date.to_string d) 20_000L "ICA" in
  let december = days (date "2025-12-02") (date "2025-12-31") in
  let history =
    transfer ~from:opening ~to_:probe "2025-07-01" 50_000_000L "Opening"
    :: List.map shop (days (date "2025-07-01") (Date.add_days today (-1)))
    @ List.map (fun d -> shop (Date.add_days d 1)) december
  in
  let checking = settled 1 "Checking" history in
  let f = project (input [ checking; groceries; opening ] ~horizon:3 ~transfers:history) in
  let percent = List.find_map (function Forecast.Seasonal { month = 12; year = 2025; percent } -> Some percent | _ -> None) f.assumptions in
  Alcotest.(check bool) "December, from last December" true (match percent with Some p -> p >= 30 && p <= 50 | None -> false);
  let spent first last = Int64.sub (cash_on f first) (cash_on f last) in
  Alcotest.(check bool) "a December week costs more than a November week" true (Int64.compare (spent "2026-12-01" "2026-12-08") (spent "2026-11-10" "2026-11-17") > 0);
  Alcotest.(check bool) "a range" true (f.range <> []);
  Alcotest.(check bool) "low at most high" true (List.for_all (fun (_, low, high) -> Int64.compare low high <= 0) f.range)

let no_range_yet () =
  let checking = checking ~balance:100_000L () in
  let f = project (input [ checking; groceries ] ~transfers:[ transfer ~from:checking ~to_:groceries "2026-09-01" 10_000L "ICA" ]) in
  Alcotest.(check bool) "no range" true (f.range = [] && f.chance_positive = None);
  Alcotest.(check bool) "and says so" true (List.mem Forecast.No_range_yet f.assumptions)

(* Properties over random accounts, series and spending. *)
open Hegel

(* Random steady bills and sometimes a salary, on a random day of the cycle. *)
let agreement tc =
  let int lo hi = draw tc (integers ~min_value:lo ~max_value:hi ()) in
  let checking = checking ~balance:(Int64.of_int (int 0 2_000_000)) () in
  let bills =
    List.concat
      (List.init (int 1 4) (fun n ->
           let months = if draw tc (booleans ()) then [ 7; 8; 9 ] else [ 6; 7; 8; 9 ] in
           monthly_history ~from:checking ~to_:rent ~counterparty:(Printf.sprintf "Bill %d" n) "X" (int 1 28) (Int64.of_int (int 1_000 900_000)) months))
  in
  let pay = if draw tc (booleans ()) then monthly_history ~from:salary ~to_:checking ~counterparty:"ACME" "LÖN" (int 1 28) 2_000_000L [ 7; 8; 9 ] else [] in
  let today = Date.add_days (date "2026-09-25") (int 0 27) in
  let ok, _, _ = agree ~today (input [ checking; rent; salary ] ~transfers:(bills @ pay)) in
  if not ok then failwith "the card warns but the forecast doesn't"

let%hegel_test card_and_forecast_agree tc = agreement tc [@@settings Settings.create ~test_cases:300 ()]

let draw_input tc =
  let int lo hi = draw tc (integers ~min_value:lo ~max_value:hi ()) in
  let checking = checking ~balance:(Int64.of_int (int (-100_000) 3_000_000)) () and savings = savings ~balance:(Int64.of_int (int 0 3_000_000)) () in
  let cash = [| checking; savings |] in
  let series =
    List.init (int 0 5) (fun n ->
        let account = cash.(int 0 1) in
        let next = Date.to_string (Date.add_days today (int (-10) 40)) in
        let cadence = if draw tc (booleans ()) then Analytics.Monthly else Weekly in
        match int 0 2 with
        | 0 -> series ~cadence ~account ~counter:rent (Printf.sprintf "bill:%d" n) (Int64.of_int (-int 1 900_000)) next
        | 1 -> series ~kind:Income ~cadence ~account ~counter:salary (Printf.sprintf "income:%d" n) (Int64.of_int (int 1 900_000)) next
        | _ -> series ~kind:Transfer ~cadence ~account:checking ~counter:savings (Printf.sprintf "transfer:%d" n) (Int64.of_int (-int 1 900_000)) next)
  in
  let transfers =
    if draw tc (booleans ()) then []
    else List.init (int 0 20) (fun _ -> transfer ~from:cash.(int 0 1) ~to_:groceries (Date.to_string (Date.add_days today (-int 1 120))) (Int64.of_int (int 1 50_000)) "ICA")
  in
  let imported = if draw tc (booleans ()) then [] else [ (id checking, Date.add_days today (-int 0 20)) ] in
  input [ checking; savings; groceries; rent; salary ] ~horizon:[| 1; 3; 6 |].(int 0 2) ~series ~transfers ~imported

let laws tc =
  let i = draw_input tc in
  let f = project i in
  if project i <> f then failwith "not deterministic";
  let starts = List.fold_left (fun sum (o : Forecast.outlook) -> Int64.add sum o.start) 0L f.accounts in
  if not (List.exists (fun (p : Forecast.point) -> Date.equal p.date (fst f.lowest) && p.cash = snd f.lowest) f.baseline) then failwith "lowest is not on the line";
  if List.exists (fun (p : Forecast.point) -> Int64.compare p.cash (snd f.lowest) < 0) f.baseline then failwith "a point below the lowest";
  if i.transfers = [] then begin
    let cash id = List.exists (fun (o : Forecast.outlook) -> Account_id.equal o.account.account.id id) f.accounts in
    let moved =
      List.fold_left
        (fun sum (item : Forecast.item) ->
          match item.series with Some s when cash s.counter -> sum | _ -> Int64.add sum item.amount)
        0L f.items
    in
    if last_cash f.baseline <> Int64.add starts moved then failwith "the end is not the start plus the items";
    if i.series = [] && List.exists (fun (p : Forecast.point) -> p.cash <> starts) f.baseline then failwith "not flat"
  end;
  let without = project { i with series = [] } in
  let left_out = project i ~scenario:{ Forecast.no_scenario with leave_out = List.map (fun (s : Series.t) -> s.id) i.series } in
  (* With no series to leave out, there is no what-if: the baseline is the answer. *)
  if List.map (fun (p : Forecast.point) -> p.cash) (Option.value left_out.scenario ~default:left_out.baseline) <> List.map (fun (p : Forecast.point) -> p.cash) without.baseline then
    failwith "leaving out every series is not usual spending alone"

let%hegel_test forecast_laws tc = laws tc [@@settings Settings.create ~test_cases:300 ()]

let () =
  Alcotest.run "forecast"
    [ ( "project",
        [ Alcotest.test_case "salary then rent" `Quick salary_then_rent; Alcotest.test_case "the card bill brings card spending in" `Quick card_bill;
          Alcotest.test_case "a savings sweep keeps the total" `Quick savings_sweep; Alcotest.test_case "a stopped subscription" `Quick stopped_subscription;
          Alcotest.test_case "a payment missing from the import" `Quick missing_import;
          Alcotest.test_case "the plan replaces the household transfer" `Quick plan_replaces_household_transfer;
          Alcotest.test_case "the first day below zero" `Quick overdraw; Alcotest.test_case "goals" `Quick goal_band;
          Alcotest.test_case "usual spending" `Quick usual_spending; Alcotest.test_case "short history" `Quick short_history;
          Alcotest.test_case "shorter than a cycle" `Quick shorter_than_a_cycle;
          Alcotest.test_case "assumptions" `Quick assumptions; Alcotest.test_case "a one-off" `Quick one_off;
          Alcotest.test_case "keep to my budgets" `Quick follow_budgets; Alcotest.test_case "history" `Quick history ] );
      ("agreement", [ Alcotest.test_case "the Overview card and the forecast" `Quick card_and_forecast ]);
      ( "range",
        [ Alcotest.test_case "exact history, no width" `Quick exact_history; Alcotest.test_case "seasons" `Quick seasons;
          Alcotest.test_case "no range yet" `Quick no_range_yet ] );
      ( "properties",
        [ Alcotest.test_case "deterministic, lowest, items add up, flat, leaving out" `Slow forecast_laws;
          Alcotest.test_case "the card never warns alone" `Slow card_and_forecast_agree ] ) ]
