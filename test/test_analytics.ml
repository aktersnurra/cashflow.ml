module Commands = Cashflow_io.Commands
module Insights = Cashflow_io.Insights
module Json = Cashflow_io.Json
module Planning_store = Cashflow_io.Planning_store
module Query = Cashflow_io.Query
module Service = Cashflow_io.Service
module Account_id = Cashflow_core.Account_id
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Household = Cashflow_core.Household
module Owner = Cashflow_core.Owner
module User_id = Cashflow_core.User_id

open Test_service_support

let field path json =
  List.fold_left (fun json name -> match json with `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null | _ -> `Null) json path

let str path json = match field path json with `String s -> s | `Null -> "null" | other -> Yojson.Safe.to_string other

let query service access name args =
  match Query.find name with
  | None -> Alcotest.fail ("no query " ^ name)
  | Some q -> ok (Query.execute service access q args)

(* Alice is paid on 25 September; the current pay cycle runs 25 September to 22 October. *)
let scenario () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let checking = open_checking service alice "Checking" in
  let shared = open_checking service alice ~owner:(Household household.id) "Shared" in
  let salary = account_named service alice "Salary" in
  let groceries =
    List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && i.account.owner = Access.personal alice) (Service.accounts service alice)
  in
  let housing =
    List.find (fun (i : Account_info.t) -> i.account.name = "Housing" && i.account.owner = Owner.Household household.id) (Service.accounts service alice)
  in
  let subscriptions = account_named service alice "Subscriptions" in
  let add key_name ~from_account ~to_account value on =
    match ok (Service.add_entry service alice { (entry key_name ~from_account ~to_account value) with value_date = date on }) with
    | Ok _ -> ()
    | Error error -> Alcotest.fail (Ledger_error.transfer_to_string error)
  in
  add "salary" ~from_account:salary ~to_account:checking 30_000_00 "2026-09-25";
  add "ica" ~from_account:checking ~to_account:groceries.account.id 4_000_00 "2026-10-01";
  add "to household" ~from_account:checking ~to_account:shared 10_000_00 "2026-09-26";
  add "rent" ~from_account:shared ~to_account:housing.account.id 9_000_00 "2026-09-30";
  List.iteri
    (fun i on ->
      let e = entry (Printf.sprintf "spotify %d" i) ~from_account:checking ~to_account:subscriptions 119_00 in
      ignore (ok (Service.add_entry service alice { e with value_date = date on; memo = { e.memo with counterparty = Some "Spotify AB" } })))
    [ "2026-07-02"; "2026-08-02"; "2026-09-02"; "2026-10-02" ];
  (service, alice, groceries)

let test_cashflow_by_scope () =
  let service, alice, _ = scenario () in
  let current scope = field [ "cycles" ] (query service alice "cashflow_summary" [ ("scope", `String scope); ("cycles", `Int 1) ]) |> function
    | `List [ cycle ] -> cycle
    | _ -> Alcotest.fail "expected one cycle"
  in
  let me = current "me" in
  Alcotest.(check string) "cycle" "2026-10" (str [ "cycle" ] me);
  Alcotest.(check string) "income" "30000.00" (str [ "income" ] me);
  Alcotest.(check string) "spending" "4119.00" (str [ "spending" ] me);
  Alcotest.(check string) "to the household" "10000.00" (str [ "transfers_out" ] me);
  Alcotest.(check string) "savings" "15881.00" (str [ "savings" ] me);
  Alcotest.(check string) "rate" "52.93%" (str [ "savings_rate" ] me);
  Alcotest.(check string) "not reconciled" "false" (str [ "reconciled" ] me);
  let household = current "household" in
  Alcotest.(check string) "household in" "10000.00" (str [ "transfers_in" ] household);
  Alcotest.(check string) "household spending" "9000.00" (str [ "spending" ] household);
  let family = current "family" in
  Alcotest.(check string) "family spending" "13119.00" (str [ "spending" ] family);
  Alcotest.(check string) "contributions cancel out" "0.00" (str [ "transfers_out" ] family)

let test_categories_recurring_and_budgets () =
  let service, alice, groceries = scenario () in
  (match field [ "categories" ] (query service alice "spending_by_category" []) with
  | `List (top :: _) -> Alcotest.(check string) "largest category" "Groceries" (str [ "category"; "name" ] top)
  | _ -> Alcotest.fail "no categories");
  (match field [ "payments" ] (query service alice "recurring_payments" []) with
  | `List [ spotify ] ->
      Alcotest.(check string) "merchant, as written" "Spotify AB" (str [ "merchant" ] spotify);
      Alcotest.(check string) "cadence" "monthly" (str [ "cadence" ] spotify);
      Alcotest.(check string) "next" "2026-11-02" (str [ "next_expected" ] spotify)
  | other -> Alcotest.failf "expected spotify, got %s" (Yojson.Safe.to_string other));
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  ignore (ok (Insights.set_budget ctx ~category_id:groceries.account.id ~monthly_amount:6_000_00L ~rollover:false ~effective_from:None));
  match field [ "lines" ] (query service alice "budget_status" []) with
  | `List [ line ] ->
      Alcotest.(check string) "budget" "6000.00" (str [ "budget" ] line);
      Alcotest.(check string) "spent" "4000.00" (str [ "spent" ] line);
      (* 27 of 28 days have passed, so pace expects 5785.71. *)
      Alcotest.(check string) "status" "on_pace" (str [ "status" ] line)
  | other -> Alcotest.failf "expected one budget line, got %s" (Yojson.Safe.to_string other)

(* Regular visits to the same café are not a subscription: the amounts vary too much. *)
let test_irregular_spending_is_not_recurring () =
  let service, alice, groceries = scenario () in
  let checking = account_named service alice "Checking" in
  List.iteri
    (fun i (on, value) ->
      let e = entry (Printf.sprintf "cafe %d" i) ~from_account:checking ~to_account:groceries.account.id value in
      ignore (ok (Service.add_entry service alice { e with value_date = date on; memo = { e.memo with counterparty = Some "Espresso House" } })))
    [ ("2026-07-05", 65_00); ("2026-08-04", 410_00); ("2026-09-06", 120_00); ("2026-10-05", 300_00) ];
  match field [ "payments" ] (query service alice "recurring_payments" []) with
  | `List [ only ] -> Alcotest.(check string) "only the subscription" "Spotify AB" (str [ "merchant" ] only)
  | other -> Alcotest.failf "expected only spotify, got %s" (Yojson.Safe.to_string other)


(* The forecast query: Spotify is charged on the 2nd of every month, so it repeats. *)
let test_forecast_query () =
  let service, alice, _ = scenario () in
  let forecast args = query service alice "forecast" args in
  let f = forecast [] in
  Alcotest.(check string) "today" "2026-10-21" (str [ "today" ] f);
  Alcotest.(check string) "three months ahead by default" "2027-01-21" (str [ "horizon_end" ] f);
  Alcotest.(check string) "no what-if" "null" (str [ "scenario" ] f);
  let items json = match field [ "items" ] json with `List items -> items | _ -> [] in
  let spotify = List.filter (fun i -> str [ "id" ] i = "bill:spotify ab") (items f) in
  Alcotest.(check (list string)) "every month" [ "2026-11-02"; "2026-12-02"; "2027-01-02" ] (List.map (str [ "date" ]) spotify);
  let first = List.hd spotify in
  Alcotest.(check string) "kind" "bill" (str [ "kind" ] first);
  Alcotest.(check string) "amount" "-119.00" (str [ "amount" ] first);
  Alcotest.(check string) "account" "Checking" (str [ "account"; "name" ] first);
  Alcotest.(check string) "category" "Subscriptions" (str [ "category"; "name" ] first);
  Alcotest.(check string) "basis" "monthly 4 2026-07-02" (String.concat " " [ str [ "basis"; "cadence" ] first; str [ "basis"; "occurrences" ] first; str [ "basis"; "since" ] first ]);
  Alcotest.(check bool) "says no income was found" true
    (match field [ "assumptions" ] f with `List a -> List.exists (fun a -> str [ "kind" ] a = "no_income_detected") a | _ -> false);
  Alcotest.(check string) "too little history for a range" "[] null" (str [ "range" ] f ^ " " ^ str [ "chance_positive" ] f);
  Alcotest.(check bool) "and it says so" true
    (match field [ "assumptions" ] f with `List a -> List.exists (fun a -> str [ "kind" ] a = "no_range_yet") a | _ -> false);
  Alcotest.(check (list string)) "stable ids" (List.map (str [ "id" ]) (items f)) (List.map (str [ "id" ]) (items (forecast [])));
  let last key json = match field [ key ] json with `List points -> str [ "cash" ] (List.nth points (List.length points - 1)) | _ -> Alcotest.fail key in
  let minor text = Option.get (Json.parse_decimal ~scale:2 text) in
  let left_out = forecast [ ("scenario", `Assoc [ ("leave_out", `List [ `String "bill:spotify ab" ]) ]) ] in
  Alcotest.(check int64) "without Spotify" (Int64.add (minor (last "baseline" left_out)) 357_00L) (minor (last "scenario" left_out));
  let refused args = match Query.execute service alice (Option.get (Query.find "forecast")) args with Error (`Invalid m) -> m | _ -> Alcotest.fail "expected a refusal" in
  Alcotest.(check string) "horizon" "cycles must be 1, 3 or 6" (refused [ ("cycles", `Int 2) ]);
  Alcotest.(check string) "amount" "amount must be a decimal like 1240.50"
    (refused [ ("scenario", `Assoc [ ("one_off", `List [ `Assoc [ ("date", `String "2026-11-01"); ("amount", `String "12.345") ] ]) ]) ]);
  Alcotest.(check string) "shape" "scenario.leave_out must be a list of series ids" (refused [ ("scenario", `Assoc [ ("leave_out", `String "x") ]) ])

let test_contribution_plan () =
  let service, alice, _ = scenario () in
  let bob = sign_in service "bob" in
  let household = Option.get (Access.household alice) in
  ignore (ok (Service.invite_member service alice ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  let plan =
    query service alice "contribution_plan"
      [ ("target", `String "24000");
        ("incomes", `Assoc [ (User_id.to_string (Access.user alice).id, `String "36000"); (User_id.to_string (Access.user bob).id, `String "24000") ]) ]
  in
  match field [ "members" ] plan with
  | `List [ a; b ] ->
      Alcotest.(check string) "alice pays 60%" "14400.00" (str [ "contribution" ] a);
      Alcotest.(check string) "and has paid" "10000.00" (str [ "transferred" ] a);
      Alcotest.(check string) "bob pays 40%" "9600.00" (str [ "contribution" ] b);
      Alcotest.(check string) "bob's income is not shown to alice" "null" (str [ "income" ] b)
  | other -> Alcotest.failf "expected two members, got %s" (Yojson.Safe.to_string other)

let test_rounding_keeps_the_total () =
  let unit = 100L in
  match Analytics.plan_contributions ~unit ~target:1_000_00L [ ("a", 1L); ("b", 1L); ("c", 1L) ] with
  | Some shares ->
      Alcotest.(check int64) "sums to target" 1_000_00L (List.fold_left (fun s (_, _, x) -> Int64.add s x) 0L shares);
      List.iter (fun (_, _, x) -> Alcotest.(check int64) "whole units" 0L (Int64.rem x unit)) shares
  | None -> Alcotest.fail "no plan"

let test_decimals () =
  Alcotest.(check string) "format" "1240.50" (Json.decimal ~scale:2 124050L);
  Alcotest.(check string) "small" "0.05" (Json.decimal ~scale:2 5L);
  Alcotest.(check string) "negative" "-3.25" (Json.decimal ~scale:2 (-325L));
  Alcotest.(check string) "no decimals" "1240" (Json.decimal ~scale:0 1240L);
  Alcotest.(check (option int64)) "parse" (Some 124050L) (Json.parse_decimal ~scale:2 "1240.5");
  Alcotest.(check (option int64)) "too precise" None (Json.parse_decimal ~scale:2 "1.234");
  Alcotest.(check (option int64)) "junk" None (Json.parse_decimal ~scale:2 "12,50")

(* Borrowing and repaying move money between an asset and a liability: net worth doesn't change. *)
let test_net_worth_with_a_loan () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Checking" in
  let loan =
    match
      Service.open_account service alice ~key:(key "open:loan") ~owner:(Access.personal alice) ~kind:Liability ~name:"Car loan" ~currency:"SEK"
        ~flags:Account.no_flags ~details:Account_info.no_details
    with
    | Ok (`Created info | `Exists info) -> info.account.id
    | Error e -> Alcotest.fail (Service.error_to_string e)
  in
  let salary = account_named service alice "Salary" and groceries = account_named service alice "Groceries" in
  let add key_name ~from_account ~to_account value on =
    ignore (ok (Service.add_entry service alice { (entry key_name ~from_account ~to_account value) with value_date = date on }))
  in
  add "salary" ~from_account:salary ~to_account:checking 10_000_00 "2026-08-01";
  add "drawdown" ~from_account:loan ~to_account:checking 1_000_00 "2026-09-01";
  add "repayment" ~from_account:checking ~to_account:loan 400_00 "2026-10-01";
  add "food" ~from_account:checking ~to_account:groceries 100_00 "2026-10-02";
  let points = match field [ "points" ] (query service alice "net_worth_series" [ ("cycles", `Int 3) ]) with `List p -> p | _ -> [] in
  Alcotest.(check (list string)) "after pay, the loan and its repayment" [ "10000.00"; "10000.00"; "9900.00" ] (List.map (str [ "net_worth" ]) points)

(* JSON numbers are read exactly or refused, never rounded. *)
let test_json_amounts () =
  let sek = Result.get_ok (Currency.make ~code:"SEK" ~scale:2) in
  let read v = Commands.signed_amount sek "amount" [ ("amount", v) ] in
  Alcotest.(check (option int64)) "seven digits" (Some 1_234_567L) (Result.get_ok (read (`Float 12345.67)));
  Alcotest.(check (option int64)) "whole" (Some 10_000L) (Result.get_ok (read (`Float 100.)));
  Alcotest.(check (option int64)) "big" (Some 98_765_432_101L) (Result.get_ok (read (`Float 987654321.01)));
  Alcotest.(check bool) "too many decimals is refused" true (Result.is_error (read (`Float (0.1 +. 0.2))));
  Alcotest.(check bool) "so is an exponent" true (Result.is_error (read (`Float 1e25)))

(* A goal is funded from accounts in its own currency. *)
let test_earmark_currency () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let euros =
    match
      Service.open_account service alice ~key:(key "open:eur") ~owner:(Access.personal alice) ~kind:Asset ~name:"Euros" ~currency:"EUR"
        ~flags:Account.no_flags ~details:Account_info.no_details
    with
    | Ok (`Created info | `Exists info) -> info.account.id
    | Error e -> Alcotest.fail (Service.error_to_string e)
  in
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  let goal = ok (Insights.save_goal ctx ~name:"Car" ~target_amount:100_000_00L ~target_date:None ~monthly_amount:None ()) in
  Alcotest.(check bool) "SEK goal, EUR account" true (Result.is_error (Insights.earmark ctx ~goal_id:goal.goal.goal_id ~account_id:euros ~amount:0L))

let test_goal_report_currency () =
  let service, alice, _ = scenario () in
  let save scope currency name target =
    let ctx = ok (Insights.context service alice ~scope ~currency ()) in
    ignore (ok (Insights.save_goal ctx ~name ~target_amount:target ~target_date:None ~monthly_amount:None ()))
  in
  save Me "SEK" "Personal SEK" 1_000_00L;
  save Me "USD" "Personal USD" 2_000_00L;
  save Household "SEK" "Household SEK" 3_000_00L;
  save Household "USD" "Household USD" 4_000_00L;
  List.iter
    (fun (scope, currency, expected) ->
      let report = query service alice "savings_goals" [ ("scope", `String scope); ("currency", `String currency) ] in
      Alcotest.(check string) "report currency" currency (str [ "currency" ] report);
      let goals = match field [ "goals" ] report with `List rows -> rows | _ -> Alcotest.fail "no goals list" in
      let actual = List.map (fun goal -> (str [ "name" ] goal, str [ "target" ] goal)) goals in
      Alcotest.(check (list (pair string string))) (scope ^ " / " ^ currency)
        (List.sort compare expected) (List.sort compare actual))
    [ ("me", "SEK", [ ("Personal SEK", "1000.00") ]);
      ("me", "USD", [ ("Personal USD", "2000.00") ]);
      ("household", "SEK", [ ("Household SEK", "3000.00") ]);
      ("household", "USD", [ ("Household USD", "4000.00") ]);
      ("family", "SEK", [ ("Personal SEK", "1000.00"); ("Household SEK", "3000.00") ]);
      ("family", "USD", [ ("Personal USD", "2000.00"); ("Household USD", "4000.00") ]);
      ("family", "EUR", []) ]

let test_contribution_target_currency () =
  let service, alice, _ = scenario () in
  let household = Option.get (Access.household alice) in
  let set_planning currency budget monthly =
    let ctx = ok (Insights.context service alice ~scope:Household ~currency ()) in
    let category =
      match ok (Service.open_account service alice ~key:(key ("category:" ^ currency)) ~owner:(Household household.id)
                  ~kind:Expense ~name:("Bills " ^ currency) ~currency ~flags:Account.no_flags ~details:Account_info.no_details) with
      | `Created info | `Exists info -> info.account.id
    in
    ignore (ok (Insights.set_budget ctx ~category_id:category ~monthly_amount:budget ~rollover:false ~effective_from:None));
    ignore (ok (Insights.save_goal ctx ~name:("Goal " ^ currency) ~target_amount:100_000_00L ~target_date:None ~monthly_amount:(Some monthly) ()))
  in
  set_planning "SEK" 300_00L 20_00L;
  set_planning "USD" 700_00L 50_00L;
  let personal = ok (Insights.context service alice ~scope:Me ()) in
  ignore (ok (Insights.save_goal personal ~name:"Personal" ~target_amount:100_000_00L ~target_date:None ~monthly_amount:(Some 999_00L) ()));
  List.iter
    (fun (currency, expected) ->
      let plan = query service alice "contribution_plan"
          [ ("currency", `String currency); ("incomes", `Assoc [ (User_id.to_string (Access.user alice).id, `String "1000") ]) ] in
      Alcotest.(check string) currency expected (str [ "target" ] plan))
    [ ("SEK", "320.00"); ("USD", "750.00"); ("EUR", "0.00") ]

let test_goal_update_currency () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Savings" in
  ignore (ok (Service.set_opening_balance service alice ~account_id:checking ~balance:2_000_00L ~value_date:(date "2026-10-01")));
  let save fields = Commands.execute service alice ~scopes:[ "ledger:write" ] Commands.save_goal (`Assoc fields) in
  let created = ok_import (save [ ("name", `String "Car"); ("target_amount", `String "5000.00"); ("monthly_amount", `String "100.00") ]) in
  let id = str [ "id" ] created in
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  ignore (ok (Insights.earmark ctx ~goal_id:id ~account_id:checking ~amount:500_00L));
  let result = save [ ("goal_id", `String id); ("currency", `String "USD"); ("name", `String "Changed");
                      ("target_amount", `String "1000.00"); ("monthly_amount", `String "10.00") ] in
  (match result with Error (`Invalid _) -> () | _ -> Alcotest.fail "accepted a cross-currency goal update");
  let stored = Option.get (Planning_store.find_goal service.db id) in
  Alcotest.(check string) "currency unchanged" "SEK" stored.currency;
  Alcotest.(check string) "name unchanged" "Car" stored.name;
  Alcotest.(check int64) "target unchanged" 5_000_00L stored.target_amount;
  Alcotest.(check (option int64)) "monthly unchanged" (Some 100_00L) stored.monthly_amount;
  Alcotest.(check (list int64)) "earmark unchanged" [ 500_00L ] (List.map snd stored.earmarks);
  ignore (ok_import (save [ ("goal_id", `String id); ("name", `String "New car"); ("target_amount", `String "6000.00") ]));
  let stored = Option.get (Planning_store.find_goal service.db id) in
  Alcotest.(check string) "same-currency update" "New car" stored.name;
  Alcotest.(check int64) "updated target" 6_000_00L stored.target_amount;
  Alcotest.(check (list int64)) "earmark retained" [ 500_00L ] (List.map snd stored.earmarks);
  let usd = ok_import (save [ ("currency", `String "USD"); ("name", `String "Trip"); ("target_amount", `String "1000.00") ]) in
  let usd_id = str [ "id" ] usd in
  (match save [ ("goal_id", `String usd_id); ("name", `String "Trip"); ("target_amount", `String "2000.00") ] with
  | Error (`Invalid _) -> ()
  | _ -> Alcotest.fail "accepted default SEK currency for a USD goal");
  ignore (ok_import (save [ ("goal_id", `String usd_id); ("currency", `String "USD"); ("name", `String "Trip"); ("target_amount", `String "2000.00") ]));
  Alcotest.(check int64) "explicit USD update" 2_000_00L (Option.get (Planning_store.find_goal service.db usd_id)).target_amount

(* Editing a household goal from the personal scope keeps it the household's (the store never
   changes a goal's owner or currency on update). *)
let test_goal_keeps_its_owner () =
  let service, alice, _ = scenario () in
  let household = ok (Insights.context service alice ~scope:Household ()) in
  let goal = ok (Insights.save_goal household ~name:"Sofa" ~target_amount:10_000_00L ~target_date:None ~monthly_amount:None ()) in
  let me = ok (Insights.context service alice ~scope:Me ()) in
  ignore (ok (Insights.save_goal me ~goal_id:goal.goal.goal_id ~name:"New sofa" ~target_amount:12_000_00L ~target_date:None ~monthly_amount:None ()));
  let stored = Option.get (Planning_store.find_goal service.db goal.goal.goal_id) in
  Alcotest.(check string) "renamed" "New sofa" stored.name;
  Alcotest.(check bool) "still the household's" true (Owner.equal stored.goal_owner (List.hd household.owners))

let test_contribution_reversal () =
  let service, alice, _ = scenario () in
  let ctx = ok (Insights.context service alice ~scope:Household ()) in
  let cycle = Insights.current_cycle ctx in
  let contribution = List.find (fun (t : Transfer.t) -> t.memo.description = "to household")
      (Insights.transfers ctx ~first:cycle.first ~last:cycle.last) in
  let plan () =
    let report = query service alice "contribution_plan"
        [ ("target", `String "20000"); ("incomes", `Assoc [ (User_id.to_string (Access.user alice).id, `String "30000") ]) ] in
    match field [ "members" ] report with `List [ member ] -> member | _ -> Alcotest.fail "expected one member"
  in
  Alcotest.(check string) "contributed" "10000.00" (str [ "transferred" ] (plan ()));
  Alcotest.(check string) "reversed" "created" (label (ok (Service.reverse service alice ~key:(key "reverse contribution") ~transfer_id:contribution.id ())));
  let member = plan () in
  Alcotest.(check string) "reversal nets out" "0.00" (str [ "transferred" ] member);
  Alcotest.(check string) "still due" "20000.00" (str [ "remaining" ] member)

let test_income_category_trend () =
  let service, alice, groceries = scenario () in
  let salary = account_named service alice "Salary" in
  let total category =
    let report = query service alice "category_trend" [ ("category_id", `String (Account_id.to_string category)); ("cycles", `Int 1) ] in
    match field [ "cycles" ] report with `List [ cycle ] -> str [ "total" ] cycle | _ -> Alcotest.fail "expected one cycle"
  in
  Alcotest.(check string) "income is credit-normal" "30000.00" (total salary);
  Alcotest.(check string) "expense stays debit-normal" "4000.00" (total groceries.account.id);
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  let cycle = Insights.current_cycle ctx in
  let pay = List.find (fun (t : Transfer.t) -> t.memo.description = "salary")
      (Insights.transfers ctx ~first:cycle.first ~last:cycle.last) in
  Alcotest.(check string) "reversed" "created" (label (ok (Service.reverse service alice ~key:(key "reverse salary") ~transfer_id:pay.id ())));
  Alcotest.(check string) "income reversal cancels out" "0.00" (total salary);
  let checking = account_named service alice "Checking" in
  Alcotest.(check string) "income returned" "created"
    (label (ok (Service.add_entry service alice (entry "return salary" ~from_account:checking ~to_account:salary 1_000_00))));
  Alcotest.(check string) "returned income is negative" "-1000.00" (total salary);
  Alcotest.(check string) "expenses unaffected" "4000.00" (total groceries.account.id)

let test_budget_rollover_report () =
  let service, alice, groceries = scenario () in
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  List.iter
    (fun (effective_from, rollover) ->
      ignore (ok (Insights.set_budget ctx ~category_id:groceries.account.id ~monthly_amount:6_000_00L ~rollover ~effective_from:(Some effective_from))))
    [ ("2026-09", false); ("2026-10", true); ("2026-11", false) ];
  List.iter
    (fun (offset, expected) ->
      let report = query service alice "budget_status" [ ("cycle_offset", `Int offset) ] in
      match field [ "lines" ] report with
      | `List [ line ] -> Alcotest.(check string) "effective rollover" expected (str [ "rollover" ] line)
      | _ -> Alcotest.fail "expected one budget line")
    [ (0, "true"); (-1, "false") ]

let test_cashflow_query_boundary () =
  let service, alice, _ = scenario () in
  List.iter (fun scope ->
    let legacy = ok (Insights.context service alice ~scope ()) in
    let expected = Query.cashflow_summary.encode legacy (Insights.cashflow_summary legacy ~cycles:2) in
    let module W = Cashflow_workflow in
    let first, last = Insights.span (Insights.recent_cycles legacy 2) in
    let facts : W.Cashflow_facts.t = { accounts=Insights.accounts legacy;
      transfers=Insights.transfers legacy ~first ~last; reconciled_through=Insights.reconciled_through legacy } in
    let scripted = W.Effects.run { W.Effects.handle = (fun (type a) (op : a W.Effect_ops.t) : a -> match op with
      | Now -> now
      | Resolve_currency code -> Result.map_error (fun _ -> W.Reporting_error.Unknown_currency code) (Service.currency service code)
      | Load_cashflow _ -> Ok facts
      | Load_forecast _ -> Alcotest.fail "no forecast here") } (fun () -> W.Reporting.cashflow_summary ~access:alice ~scope ~cycles:2 ()) in
    let scripted = match scripted with Ok report -> report | Error _ -> Alcotest.fail "scripted report failed" in
    Alcotest.(check string) "live/scripted parity" (Yojson.Safe.to_string expected)
      (Yojson.Safe.to_string (Query.cashflow_summary.encode legacy scripted.cycles));
    let calls = ref 0 in
    let service = { service with now = (fun () -> incr calls; now) } in
    let actual = query service alice "cashflow_summary" ["scope", `String (Analytics.scope_to_string scope); "cycles", `Int 2] in
    Alcotest.(check string) "complete legacy parity" (Yojson.Safe.to_string expected) (Yojson.Safe.to_string actual);
    Alcotest.(check int) "one captured clock sample" 1 !calls) [Analytics.Me; Household; Family];
  let alone = sign_in service "alone" in
  let common : Query.common = { scope=Me; currency=Some "BAD" } in
  Alcotest.(check bool) "unsupported currency" true
    (Query.run service alone (Query.Cashflow_summary (common, 1)) = Error (`Invalid "unknown currency BAD"));
  let common = { common with scope=Household } in
  Alcotest.(check bool) "household before currency" true
    (Query.run service alone (Query.Cashflow_summary (common, 1)) = Error (`Invalid "you don't belong to a household, so use scope \"me\""))

let test_legacy_query_dispatch () =
  let service, access, category = scenario () in
  let check : type a. a Query.descriptor -> a Query.t -> (Insights.ctx -> a) -> unit =
    fun descriptor query calculate ->
      let scope = if String.equal descriptor.name "contribution_plan" || String.equal descriptor.name "settle_up_status" then Analytics.Household else Me in
      let ctx = ok (Insights.context service access ~scope ()) in
      let expected = descriptor.encode ctx (calculate ctx) in
      let calls = ref 0 in
      let service = {service with now=(fun () -> incr calls; now)} in
      let value, ctx = ok (Query.run service access query) in
      Alcotest.(check string) descriptor.name (Yojson.Safe.to_string expected) (Yojson.Safe.to_string (descriptor.encode ctx value));
      Alcotest.(check int) "legacy clock sampled once" 1 !calls
  in
  let common : Query.common = {scope=Me; currency=None} in
  let household = {common with scope=Household} in
  check Query.spending_by_category (Spending_by_category (common, Cycle_offset 0, true)) (fun ctx -> Insights.spending_by_category ctx ~period:(Cycle_offset 0) ~compare_previous:true);
  check Query.category_trend (Category_trend (common, category.account.id, 2)) (fun ctx -> ok (Insights.category_trend ctx ~category:category.account.id ~cycles:2));
  check Query.net_worth (Net_worth (common, 2)) (fun ctx -> Insights.net_worth ctx ~cycles:2);
  check Query.largest_expenses (Largest_expenses (common, Cycle_offset 0, 3)) (fun ctx -> Insights.largest_expenses ctx ~period:(Cycle_offset 0) ~limit:3);
  check Query.runway (Runway common) Insights.runway;
  check Query.recurring (Recurring common) Insights.recurring;
  check Query.budget_status (Budget_status (common, 0)) (fun ctx -> Insights.budget_status ctx ~cycle_offset:0);
  check Query.contribution_plan (Contribution_plan (household, Some "1000.00", [User_id.to_string (Access.user access).id, "2000.00"])) (fun ctx -> ok (Insights.contribution_plan ctx ~target:(Some 100000L) ~incomes:[User_id.to_string (Access.user access).id, 200000L]));
  check Query.settle_up (Settle_up household) (fun ctx -> ok (Insights.settle_up ctx));
  check Query.goals (Goals common) Insights.goals

let () =
  Alcotest.run "analytics"
    [ ( "insights",
        [ Alcotest.test_case "legacy query dispatch" `Quick test_legacy_query_dispatch;
          Alcotest.test_case "cashflow query boundary" `Quick test_cashflow_query_boundary;
          Alcotest.test_case "cashflow by scope" `Quick test_cashflow_by_scope;
          Alcotest.test_case "categories, recurring and budgets" `Quick test_categories_recurring_and_budgets;
          Alcotest.test_case "irregular spending is not recurring" `Quick test_irregular_spending_is_not_recurring;
          Alcotest.test_case "forecast" `Quick test_forecast_query;
          Alcotest.test_case "contribution plan" `Quick test_contribution_plan;
          Alcotest.test_case "net worth with a loan" `Quick test_net_worth_with_a_loan;
          Alcotest.test_case "goal report currency" `Quick test_goal_report_currency;
          Alcotest.test_case "contribution target currency" `Quick test_contribution_target_currency;
          Alcotest.test_case "goal update currency" `Quick test_goal_update_currency;
          Alcotest.test_case "a goal keeps its owner" `Quick test_goal_keeps_its_owner;
          Alcotest.test_case "contribution reversal" `Quick test_contribution_reversal;
          Alcotest.test_case "income category trend" `Quick test_income_category_trend;
          Alcotest.test_case "budget rollover report" `Quick test_budget_rollover_report ] );
      ( "arithmetic",
        [ Alcotest.test_case "rounding keeps the total" `Quick test_rounding_keeps_the_total;
          Alcotest.test_case "decimals" `Quick test_decimals; Alcotest.test_case "json amounts" `Quick test_json_amounts;
          Alcotest.test_case "earmark currency" `Quick test_earmark_currency ] ) ]
