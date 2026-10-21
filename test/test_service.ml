open Test_service_support

let test_sign_in_seeds_the_chart_once () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let count = List.length (Service.accounts service alice) in
  Alcotest.(check bool) "seeded" true (count > 10);
  let alice = sign_in service "alice" in
  Alcotest.(check int) "not seeded twice" count (List.length (Service.accounts service alice))

let test_users_are_isolated () =
  let service = make_service () in
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
  let alices = open_checking service alice "Alice checking" in
  let bobs_groceries = account_named service bob "Groceries" in
  Alcotest.(check bool) "bob can't see alice's account" true (Result.is_error (Service.find_account service bob alices));
  (match Service.add_entry service alice (entry "steal" ~from_account:alices ~to_account:bobs_groceries 100) with
  | Error (`Not_found _) -> ()
  | _ -> Alcotest.fail "alice could post to bob's account");
  (* The same holds below the service layer: the ledger only sees accounts through Access. *)
  let raw : Ledger_event.t =
    { key = key "raw"; linked = false;
      body =
        Transfer
          { debit_account_id = bobs_groceries; credit_account_id = alices; amount = amount 100; phase = Posted; code = Purchase;
            memo = Transfer.empty_memo; value_date = date "2026-10-20" } }
  in
  Alcotest.(check (list string)) "bob's account doesn't exist for alice"
    [ Ledger_error.transfer_to_string `Debit_account_not_found ]
    (List.map label (ok (Service.submit service alice ~scope:(Access.personal alice) [ raw ])));
  Alcotest.(check bool) "nor can alice write in bob's key space" true
    (Result.is_error (Service.submit service alice ~scope:(Access.personal bob) [ raw ]))

let test_household_sharing_and_masking () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" and bob = sign_in service "bob" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let shared = open_checking service alice ~owner:(Household household.id) "Shared" in
  let alices = open_checking service alice "Alice checking" in
  Alcotest.(check bool) "bob can't see the shared account yet" true (Result.is_error (Service.find_account service bob shared));
  ignore (ok (Service.invite_member service alice ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  Alcotest.(check bool) "bob sees it after accepting" true (Result.is_ok (Service.find_account service bob shared));
  ignore (ok (Service.add_entry service alice (entry "contribute" ~from_account:alices ~to_account:shared 5_000_00)));
  (match Service.query_transfers service bob Transfer_query.default_filter with
  | [ view ] ->
      Alcotest.(check string) "code" "contribution" (Transfer.code_to_string view.transfer.code);
      (match view.credit with
      | Hidden { owner_name } -> Alcotest.(check string) "alice's side is masked" "alice" owner_name
      | Own _ -> Alcotest.fail "bob sees alice's personal account")
  | views -> Alcotest.failf "expected one visible transfer, got %d" (List.length views));
  let groceries = account_named service bob "Groceries" in
  Alcotest.(check bool) "bob can't move alice's money" true
    (Result.is_error (Service.add_entry service bob (entry "bob takes" ~from_account:alices ~to_account:groceries 1)))

let test_only_admins_add_household_bank_accounts () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" and bob = sign_in service "bob" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  ignore (ok (Service.invite_member service (Service.access_for service (Access.user alice)) ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  let attempt kind =
    Service.open_account service bob ~key:(key "bob-opens") ~owner:(Household household.id) ~kind ~name:"Bob's" ~currency:"SEK"
      ~flags:Account.no_flags ~details:Account_info.no_details
  in
  Alcotest.(check bool) "no asset" true (Result.is_error (attempt Asset));
  Alcotest.(check bool) "but a category" true (Result.is_ok (attempt Expense));
  Alcotest.(check bool) "non-admins can't create households" true
    (Result.is_error (Service.create_household service (sign_in service "carol") ~name:"x" ~default_currency:"SEK"))

let test_manual_entry_on_statement_account_is_pending () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let groceries = account_named service alice "Groceries" in
  (match ok (Service.add_entry service alice (entry "ica" ~from_account:checking ~to_account:groceries 250_00)) with
  | Ok (Created view) -> Alcotest.(check bool) "pending" true (match view.transfer.kind with Pending _ -> true | _ -> false)
  | _ -> Alcotest.fail "expected a created transfer");
  Alcotest.(check int64) "posted balance still matches the bank" 0L (balance service alice checking)

let test_opening_balance_and_correction () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Checking" in
  let groceries = account_named service alice "Groceries" in
  ignore (ok (Service.set_opening_balance service alice ~account_id:checking ~balance:10_000_00L ~value_date:(date "2026-10-01")));
  Alcotest.(check int64) "opening" 10_000_00L (balance service alice checking);
  let id =
    match ok (Service.add_entry service alice (entry "ica" ~from_account:checking ~to_account:groceries 250_00)) with
    | Ok (Created view) -> view.transfer.id
    | _ -> Alcotest.fail "expected created"
  in
  let results =
    ok
      (Service.correct service alice ~key:(key "fix-ica") ~transfer_id:id
         { amount = Some (amount 205_00); from_account = None; to_account = None; value_date = None; memo = None })
  in
  Alcotest.(check (list string)) "linked pair" [ "created"; "created" ] (List.map label results);
  Alcotest.(check int64) "corrected" 205_00L (balance service alice groceries);
  Alcotest.(check (list string)) "retry" [ "exists"; "exists" ]
    (List.map label
       (ok
          (Service.correct service alice ~key:(key "fix-ica") ~transfer_id:id
             { amount = Some (amount 205_00); from_account = None; to_account = None; value_date = None; memo = None })))

let test_swedish_holidays_and_cycles () =
  let is_holiday d = Holidays.Sweden.is_holiday (date d) in
  Alcotest.(check bool) "good friday 2026" true (is_holiday "2026-04-03");
  Alcotest.(check bool) "easter monday 2026" true (is_holiday "2026-04-06");
  Alcotest.(check bool) "ascension 2026" true (is_holiday "2026-05-14");
  Alcotest.(check bool) "midsummer eve 2026" true (is_holiday "2026-06-19");
  Alcotest.(check bool) "an ordinary day" false (is_holiday "2026-10-21");
  let cycle d = Cycle.cycle_of Pay_cycle_setting.default (date d) in
  let show (c : Cycle.t) = Printf.sprintf "%s..%s %s" (Date.to_string c.first) (Date.to_string c.last) (Cycle.label c) in
  (* The 25th of October 2026 is a Sunday, so pay arrives on Friday the 23rd. *)
  Alcotest.(check string) "october" "2026-10-23..2026-11-24 2026-11" (show (cycle "2026-10-23"));
  Alcotest.(check string) "the day before" "2026-09-25..2026-10-22 2026-10" (show (cycle "2026-10-22"));
  (* Christmas: the 24th and 25th are bank holidays, so December's pay comes on the 23rd. *)
  Alcotest.(check string) "december" "2026-12-23..2027-01-24 2027-01" (show (cycle "2026-12-23"));
  let calendar = Cycle.cycle_of Pay_cycle_setting.calendar_month (date "2026-02-14") in
  Alcotest.(check string) "calendar month" "2026-02-01..2026-02-28 2026-02" (show calendar);
  let cycles = Cycle.between Pay_cycle_setting.default ~first:(date "2026-01-01") ~last:(date "2026-12-31") in
  List.iteri
    (fun i (c : Cycle.t) ->
      match List.nth_opt cycles (i + 1) with
      | Some (next : Cycle.t) -> Alcotest.(check string) "cycles tile" (Date.to_string (Date.add_days c.last 1)) (Date.to_string next.first)
      | None -> ())
    cycles

let () =
  Alcotest.run "service"
    [ ( "access",
        [ Alcotest.test_case "sign-in seeds the chart once" `Quick test_sign_in_seeds_the_chart_once;
          Alcotest.test_case "users are isolated" `Quick test_users_are_isolated;
          Alcotest.test_case "household sharing and masking" `Quick test_household_sharing_and_masking;
          Alcotest.test_case "only admins add household bank accounts" `Quick test_only_admins_add_household_bank_accounts ] );
      ( "entries",
        [ Alcotest.test_case "manual entry on a statement account is pending" `Quick test_manual_entry_on_statement_account_is_pending;
          Alcotest.test_case "opening balance and correction" `Quick test_opening_balance_and_correction ] );
      ("cycles", [ Alcotest.test_case "swedish holidays and pay cycles" `Quick test_swedish_holidays_and_cycles ]) ]
