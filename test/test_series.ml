module Series = Cashflow_core.Series
open Core_fixtures

let checking = account 1 Asset "Checking"
let savings = account 2 Asset "Savings"
let broker = account 3 Asset "Broker" ~tags:[ "investment" ]
let card = account 4 Liability "Card"
let subscriptions = account 5 Expense "Subscriptions"
let eating_out = account 6 Expense "Eating out"
let salary = account 7 Income "Salary"
let shared = account 8 Asset "Shared" ~owner:household
let accounts = [ checking; savings; broker; card; subscriptions; eating_out; salary; shared ]
let cash account_id = List.exists (fun a -> Account_id.equal (id a) account_id) [ checking; savings ]
let detect transfers = Series.detect (book accounts) ~cash ~setting transfers
let ids series = List.map (fun (s : Series.t) -> s.id) series
let find series series_id = List.find (fun (s : Series.t) -> s.id = series_id) series
let monthly ~from ~to_ ?code ?counterparty description days = List.map (fun (on, minor) -> transfer ?code ?counterparty ~from ~to_ on minor description) days
let check_date label expected actual = Alcotest.(check string) label expected (Date.to_string actual)
let pair from to_ = Printf.sprintf "transfer:%s:%s" (Account_id.to_string (id from)) (Account_id.to_string (id to_))

(* Salary follows the pay cycle: 25 Oct 2026 is a Sunday, so it comes on Friday the 23rd, not
   a month after the last one. *)
let salary_on_payday () =
  let series =
    detect
      (monthly ~from:salary ~to_:checking ~counterparty:"ACME AB" "LÖN" ~code:Income
         [ ("2026-07-24", 3_200_000L); ("2026-08-25", 3_200_000L); ("2026-09-25", 3_250_000L) ])
  in
  let s = find series "income:acme ab" in
  Alcotest.(check string) "kind" "income" (Series.kind_to_string s.kind);
  Alcotest.(check int64) "money comes in" 3_200_000L s.amount;
  Alcotest.(check int64) "latest" 3_250_000L s.last_amount;
  Alcotest.(check bool) "into checking" true (Account_id.equal (id checking) s.account);
  check_date "the next payday" "2026-10-23" s.next_expected;
  check_date "first" "2026-07-24" s.first_date;
  Alcotest.(check (option string)) "category" (Some "Salary") (Option.map (fun (c : Account_info.t) -> c.account.name) s.category)

let monthly_bill () =
  let series =
    detect
      (monthly ~from:checking ~to_:subscriptions ~counterparty:"Spotify AB" "SPOTIFY"
         [ ("2026-07-05", 12_900L); ("2026-08-05", 12_900L); ("2026-09-05", 12_900L); ("2026-10-05", 12_900L) ])
  in
  let s = find series "bill:spotify ab" in
  Alcotest.(check int64) "money goes out" (-12_900L) s.amount;
  Alcotest.(check int) "occurrences" 4 s.occurrences;
  Alcotest.(check bool) "against the category" true (Account_id.equal (id subscriptions) s.counter);
  check_date "next" "2026-11-05" s.next_expected

(* A card bill follows what was bought on the card, so only its timing has to be regular. *)
let varying_card_bill () =
  let series =
    detect
      (monthly ~from:checking ~to_:card ~code:Internal "KORTBETALNING"
         [ ("2026-07-28", 400_000L); ("2026-08-28", 750_000L); ("2026-09-28", 210_000L) ])
  in
  let s = find series (pair checking card) in
  Alcotest.(check string) "kind" "transfer" (Series.kind_to_string s.kind);
  Alcotest.(check int64) "the average of the last three" (-453_333L) s.amount;
  check_date "next" "2026-10-28" s.next_expected

(* A sweep between two cash accounts is one series, seen from the account it leaves. *)
let savings_sweep_and_household_transfer () =
  let series =
    detect
      (monthly ~from:checking ~to_:savings ~code:Internal "SPAR" [ ("2026-07-26", 200_000L); ("2026-08-26", 200_000L); ("2026-09-26", 200_000L) ]
      @ monthly ~from:checking ~to_:shared ~code:Contribution "HUSHÅLL" [ ("2026-07-27", 800_000L); ("2026-08-25", 800_000L); ("2026-09-25", 800_000L) ])
  in
  Alcotest.(check (list string)) "two series" (List.sort compare [ pair checking savings; pair checking shared ]) (ids series);
  let sweep = find series (pair checking savings) in
  Alcotest.(check int64) "leaves checking" (-200_000L) sweep.amount;
  Alcotest.(check bool) "for savings" true (Account_id.equal (id savings) sweep.counter);
  Alcotest.(check bool) "household account on the other side" true (Account_id.equal (id shared) (find series (pair checking shared)).counter)

let not_series () =
  let series =
    detect
      (monthly ~from:checking ~to_:eating_out ~counterparty:"Espresso House" "CAFE"
         [ ("2026-07-05", 6_500L); ("2026-08-04", 41_000L); ("2026-09-06", 12_000L); ("2026-10-05", 30_000L) ]
      @ monthly ~from:card ~to_:subscriptions ~counterparty:"Netflix" "NETFLIX" [ ("2026-07-12", 14_900L); ("2026-08-12", 14_900L); ("2026-09-12", 14_900L) ]
      @ monthly ~from:checking ~to_:broker ~code:Reclassify "FLYTT" [ ("2026-07-02", 10_000L); ("2026-08-02", 10_000L); ("2026-09-02", 10_000L) ]
      @ monthly ~from:checking ~to_:subscriptions ~counterparty:"Viaplay" "VIAPLAY" [ ("2026-08-09", 9_900L); ("2026-09-09", 9_900L) ])
  in
  Alcotest.(check (list string)) "café varies, Netflix is on the card, a reclassification and two payments are not series" [] (ids series)

(* Properties over random histories. *)
open Hegel

let pool = [| checking; savings; broker; card; subscriptions; eating_out; salary; shared |]
let names = [| "Spotify"; "ICA"; "ACME"; "Hyra"; "SL" |]
let codes = [| Transfer.Purchase; Internal; Income; Contribution; Reclassify |]

let draw_history tc =
  let pick array = array.(draw tc (integers ~min_value:0 ~max_value:(Array.length array - 1) ())) in
  List.init (draw tc (integers ~min_value:0 ~max_value:40 ())) (fun _ ->
      let from = pick pool and to_ = pick pool in
      let on = Date.add_days (date "2025-10-01") (draw tc (integers ~min_value:0 ~max_value:390 ())) in
      let minor = Int64.of_int (draw tc (integers ~min_value:1 ~max_value:50_000 ())) in
      transfer ~code:(pick codes) ~counterparty:(pick names) ~from ~to_ (Date.to_string on) minor "X")
  (* Planted runs, so that series are actually found: monthly-ish payments of about one amount. *)
  @ List.concat
      (List.init (draw tc (integers ~min_value:0 ~max_value:4 ())) (fun _ ->
           let from = pick pool and to_ = pick pool and code = pick codes and counterparty = pick names in
           let start = draw tc (integers ~min_value:0 ~max_value:200 ()) and base = draw tc (integers ~min_value:100 ~max_value:50_000 ()) in
           List.init (draw tc (integers ~min_value:2 ~max_value:6 ())) (fun i ->
               let on = Date.add_days (date "2025-10-01") (start + (i * 30) + draw tc (integers ~min_value:(-2) ~max_value:2 ())) in
               let minor = Int64.of_int (base + draw tc (integers ~min_value:0 ~max_value:(base / 10) ())) in
               transfer ~code ~counterparty ~from ~to_ (Date.to_string on) minor "X")))
  |> List.filter (fun (t : Transfer.t) -> not (Account_id.equal t.debit_account_id t.credit_account_id))

let laws tc =
  let history = draw_history tc in
  let series = detect history in
  if detect (List.rev history) <> series then failwith "depends on the order of the transfers";
  let unique = List.sort_uniq compare (ids series) in
  if List.length unique <> List.length series then failwith "duplicate ids";
  List.iter
    (fun (s : Series.t) ->
      if s.occurrences < 3 then failwith (s.id ^ ": fewer than three payments");
      if Date.compare s.next_expected s.last_date <= 0 then failwith (s.id ^ ": next is not after the last");
      if Date.compare s.first_date s.last_date > 0 then failwith (s.id ^ ": first after last");
      if not (cash s.account) then failwith (s.id ^ ": not a cash account"))
    series

let%hegel_test series_laws tc = laws tc [@@settings Settings.create ~test_cases:300 ()]

let () =
  Alcotest.run "series"
    [ ( "detect",
        [ Alcotest.test_case "salary on the adjusted payday" `Quick salary_on_payday; Alcotest.test_case "a monthly bill" `Quick monthly_bill;
          Alcotest.test_case "a varying card bill" `Quick varying_card_bill;
          Alcotest.test_case "a savings sweep and the household transfer" `Quick savings_sweep_and_household_transfer;
          Alcotest.test_case "what is not a series" `Quick not_series ] );
      ("properties", [ Alcotest.test_case "deterministic, unique, at least three, next after last" `Slow series_laws ]) ]
