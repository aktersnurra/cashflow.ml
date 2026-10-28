module Json = Cashflow_io.Json
module Service = Cashflow_io.Service
module Starter = Cashflow_io.Starter
module Transfer_query = Cashflow_io.Transfer_query
module Chart = Cashflow_core.Chart
module Cycle = Cashflow_core.Cycle
module Holidays = Cashflow_core.Holidays
module Household = Cashflow_core.Household
module Ledger_event = Cashflow_core.Ledger_event
module Pay_cycle_setting = Cashflow_core.Pay_cycle_setting
module Owner = Cashflow_core.Owner
module User = Cashflow_core.User

open Test_service_support

let test_sign_in_seeds_the_chart_once () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let count = List.length (Service.accounts service alice) in
  Alcotest.(check bool) "seeded" true (count > 10);
  let alice = sign_in service "alice" in
  Alcotest.(check int) "not seeded twice" count (List.length (Service.accounts service alice))

(* Changing language is a display setting: it writes nothing to anyone's accounts, the household's
   included, and a renamed category keeps its name. *)
let test_changing_language_writes_no_accounts () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  ignore (ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK"));
  let alice = Service.access_for service (Access.user alice) in
  let other = account_named service alice "Other" in
  ignore (ok (Service.update_account service alice other ~name:(Some "Misc") ~details:Account_info.no_details));
  let before = Service.accounts service alice in
  Alcotest.(check bool) "the household's accounts are there" true
    (List.exists (fun (i : Account_info.t) -> match i.account.owner with Owner.Household _ -> true | User _ -> false) before);
  let user = Access.user alice in
  let alice =
    ok (Service.update_settings service alice { User.locale = "sv"; timezone = user.timezone; default_currency = user.default_currency; pay_cycle = user.pay_cycle })
  in
  Alcotest.(check string) "the language changed" "sv" (Access.user alice).locale;
  Alcotest.(check bool) "every account as it was" true (Service.accounts service alice = before)

(* Books are seeded in English whoever creates them; viewers see them in their own language. *)
let test_new_books_are_seeded_in_english () =
  let service = make_service () in
  let alice =
    Service.sign_in service
      { issuer = "https://id.example"; subject = "alice"; display_name = Some "alice"; email = None; locale = Some "sv" }
  in
  Alcotest.(check string) "a Swedish user" "sv" (Access.user alice).locale;
  ignore (account_named service alice "Groceries");
  ignore (account_named service alice "Uncategorized");
  let household = ok (Service.create_household service alice ~name:"Hem" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  Alcotest.(check bool) "the household's too" true
    (List.exists (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service alice))

(* Every starter category has a name in every locale, and a key leads back to its category. *)
let test_starter_keys_and_names () =
  List.iter
    (fun (entry : Chart.entry) -> List.iter (fun locale -> ignore (Category_name.name ~slug:entry.slug ~currency:None locale)) Msg.locales)
    Chart.entries;
  let starter k = Option.map (fun (s : Starter.t) -> (s.entry.slug, s.currency)) (Starter.of_key (key k)) in
  let check = Alcotest.(check (option (pair string (option string)))) in
  check "a slug of its own" (Some ("other-income", None)) (starter "seed:other-income");
  check "the base form" (Some ("other", None)) (starter "seed:other");
  check "another currency" (Some ("groceries", Some "EUR")) (starter "seed:groceries-EUR");
  check "a slug with a dash, in a currency" (Some ("other-income", Some "EUR")) (starter "seed:other-income-EUR");
  check "not a currency" None (starter "seed:groceries-eur");
  check "not a starter" None (starter "open:Alice checking")

(* The API marks an untouched starter category and names it in English, whatever was stored, so
   the browser can show it in the viewer's language and MCP reads English. *)
let test_account_json_marks_starters () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let open_category ~key:k name =
    match
      Service.open_account service alice ~key:(key k) ~owner:(Access.personal alice) ~kind:Expense ~name ~currency:"EUR" ~flags:Account.no_flags
        ~details:Account_info.no_details
    with
    | Ok (`Created info | `Exists info) -> info.account.id
    | Error error -> Alcotest.fail (Service.error_to_string error)
  in
  ignore (open_category ~key:"open:pets" "Pets");
  ignore (open_category ~key:"seed:groceries-EUR" "Groceries (EUR)");
  ignore (ok (Service.update_account service alice (account_named service alice "Other") ~name:(Some "Misc") ~details:Account_info.no_details));
  let groceries = List.find (fun (i : Account_info.t) -> i.account.name = "Groceries") (Service.accounts service alice) in
  Store_sqlite.save_account service.db { groceries.account with name = "Mat & hushåll" };
  let json name =
    let info = List.find (fun (i : Account_info.t) -> i.account.name = name) (Service.accounts service alice) in
    let field f = function `Assoc fields -> Option.value (List.assoc_opt f fields) ~default:`Null | _ -> `Null in
    let pick json = Yojson.Safe.to_string (`List [ field "name" json; field "starter" json ]) in
    (pick (Json.account info), pick (Json.account_ref info))
  in
  let check label name expected =
    let account, account_ref = json name in
    Alcotest.(check string) label expected account;
    Alcotest.(check string) (label ^ ", as a reference") expected account_ref
  in
  check "a starter stored in Swedish" "Mat & hushåll" {|["Groceries",{"slug":"groceries","currency":null}]|};
  check "a starter" "Uncategorized" {|["Uncategorized",{"slug":"uncategorized-expense","currency":null}]|};
  check "in another currency" "Groceries (EUR)" {|["Groceries (EUR)",{"slug":"groceries","currency":"EUR"}]|};
  check "renamed" "Misc" {|["Misc",null]|};
  check "someone's own" "Pets" {|["Pets",null]|}

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
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
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

(* Whoever creates a household owns it: only they invite, rename, add its bank accounts and close them. A member
   uses it (categories, entries, uploads) but doesn't run it. Anyone may start a household of their own. *)
let test_the_creator_owns_the_household () =
  let service = make_service () in
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  Alcotest.(check bool) "the creator is the owner" true (Access.is_owner alice);
  ignore (ok (Service.invite_member service alice ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  Alcotest.(check bool) "a member is not" false (Access.is_owner bob);
  let attempt access kind =
    Service.open_account service access ~key:(key (Printf.sprintf "opens-%b" (Access.is_owner access))) ~owner:(Household household.id) ~kind ~name:"Bank"
      ~currency:"SEK" ~flags:Account.no_flags ~details:Account_info.no_details
  in
  Alcotest.(check bool) "a member can't add a bank account" true (Result.is_error (attempt bob Asset));
  Alcotest.(check bool) "but a category" true (Result.is_ok (attempt bob Expense));
  let shared = match attempt alice Asset with Ok (`Created info | `Exists info) -> info.account.id | Error e -> Alcotest.fail (Service.error_to_string e) in
  Alcotest.(check bool) "the owner can" true (Result.is_ok (Service.find_account service alice shared));
  Alcotest.(check bool) "a member can't close it" true (Result.is_error (Service.close_account service bob shared));
  Alcotest.(check bool) "a member can't invite" true (Result.is_error (Service.invite_member service bob ~email:"alice@example.com"));
  Alcotest.(check bool) "or change the household" true
    (Result.is_error (Service.update_household service bob ~name:(Some "Mine") ~report_split_bp:5000 ~pay_cycle:household.pay_cycle));
  Alcotest.(check bool) "the owner can close it" true (Result.is_ok (Service.close_account service alice shared));
  Alcotest.(check bool) "anyone may start a household" true
    (Result.is_ok (Service.create_household service (sign_in service "carol") ~name:"Carol's" ~default_currency:"SEK"))

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
          Alcotest.test_case "changing language writes no accounts" `Quick test_changing_language_writes_no_accounts;
          Alcotest.test_case "new books are seeded in english" `Quick test_new_books_are_seeded_in_english;
          Alcotest.test_case "starter keys and names" `Quick test_starter_keys_and_names;
          Alcotest.test_case "account json marks starters" `Quick test_account_json_marks_starters;
          Alcotest.test_case "users are isolated" `Quick test_users_are_isolated;
          Alcotest.test_case "household sharing and masking" `Quick test_household_sharing_and_masking;
          Alcotest.test_case "the creator owns the household" `Quick test_the_creator_owns_the_household ] );
      ( "entries",
        [ Alcotest.test_case "manual entry on a statement account is pending" `Quick test_manual_entry_on_statement_account_is_pending;
          Alcotest.test_case "opening balance and correction" `Quick test_opening_balance_and_correction ] );
      ("cycles", [ Alcotest.test_case "swedish holidays and pay cycles" `Quick test_swedish_holidays_and_cycles ]) ]
