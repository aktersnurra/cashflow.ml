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
      Alcotest.(check string) "merchant" "spotify ab" (str [ "merchant" ] spotify);
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

let () =
  Alcotest.run "analytics"
    [ ( "insights",
        [ Alcotest.test_case "cashflow by scope" `Quick test_cashflow_by_scope;
          Alcotest.test_case "categories, recurring and budgets" `Quick test_categories_recurring_and_budgets;
          Alcotest.test_case "contribution plan" `Quick test_contribution_plan;
          Alcotest.test_case "net worth with a loan" `Quick test_net_worth_with_a_loan ] );
      ( "arithmetic",
        [ Alcotest.test_case "rounding keeps the total" `Quick test_rounding_keeps_the_total;
          Alcotest.test_case "decimals" `Quick test_decimals ] ) ]
