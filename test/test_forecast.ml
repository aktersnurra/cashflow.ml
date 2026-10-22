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
  Alcotest.(check (list string)) "imported without it: skipped" [ "2026-11-20" ] (dates (items_of (with_import "2026-10-21") "bill:el"))

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

(* Properties over random accounts, series and spending. *)
open Hegel

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
      ("properties", [ Alcotest.test_case "deterministic, lowest, items add up, flat, leaving out" `Slow forecast_laws ]) ]
