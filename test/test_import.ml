open Test_service_support

let october =
  {|Kontoutdrag;Lönekonto ****1234
Period;2026-10-01 - 2026-10-20

Bokföringsdag;Transaktionsdatum;Text;Belopp;Saldo
2026-10-20;2026-10-19;ICA NARA;-250,00;9 512,00
2026-10-15;2026-10-15;SPOTIFY;-119,00;9 762,00
2026-10-15;2026-10-15;SPOTIFY;-119,00;9 881,00
2026-10-01;2026-10-01;LÖN;10 000,00;10 000,00
|}

(* Overlaps October by two rows and adds one. *)
let late_october =
  {|Bokföringsdag;Transaktionsdatum;Text;Belopp;Saldo
2026-10-21;2026-10-21;ICA NARA;-100,00;9 412,00
2026-10-20;2026-10-19;ICA NARA;-250,00;9 512,00
2026-10-15;2026-10-15;SPOTIFY;-119,00;9 762,00
|}

let request ?(decisions = []) account_id content : Import.request = { account_id; file_name = Some "export.csv"; content; decisions }

let imported = function
  | Ok (`Imported (s : Import.summary)) -> s
  | Ok (`Already_imported _) -> Alcotest.fail "already imported"
  | Error e -> Alcotest.fail (Import.error_to_string e)

let test_parsing () =
  Alcotest.(check (option int64)) "swedish" (Some 123456L) (Statement_format.parse_amount ~scale:2 "1 234,56");
  Alcotest.(check (option int64)) "english" (Some (-123456L)) (Statement_format.parse_amount ~scale:2 "-1,234.56");
  Alcotest.(check (option int64)) "dotted thousands" (Some 123456L) (Statement_format.parse_amount ~scale:2 "1.234,56");
  Alcotest.(check (option int64)) "unicode minus" (Some (-1200L)) (Statement_format.parse_amount ~scale:2 "\xE2\x88\x9212,00");
  Alcotest.(check (option int64)) "whole" (Some 10000L) (Statement_format.parse_amount ~scale:2 "100");
  let table = Csv.parse "a;\"b;c\";\"say \"\"hi\"\"\"\r\n1;2;3\r\n" in
  Alcotest.(check (list (list string))) "quoted" [ [ "a"; "b;c"; "say \"hi\"" ]; [ "1"; "2"; "3" ] ] table;
  (* "Lön" in Latin-1. *)
  Alcotest.(check (list (list string))) "latin-1" [ [ "L\xC3\xB6n" ] ] (Csv.parse "L\xF6n\n")

let setup () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  (service, alice, checking)

let test_import_matches_dedupes_and_checks_the_balance () =
  let service, alice, checking = setup () in
  let groceries = account_named service alice "Groceries" in
  (* Alice logged the ICA purchase herself on the day; the bank booked it a day later. *)
  ignore (ok (Service.add_entry service alice { (entry "ica by hand" ~from_account:checking ~to_account:groceries 250_00) with value_date = date "2026-10-19" }));
  let ctx = Import.context service alice in
  let preview = ok_import (Import.preview ctx (request checking october)) in
  let statuses = List.map (fun (p : Import.planned) -> match p.status with New -> "new" | Duplicate _ | Decided_duplicate _ -> "duplicate" | Matched _ -> "matched" | Ambiguous _ -> "ambiguous") preview.rows in
  Alcotest.(check (list string)) "statuses" [ "matched"; "new"; "new"; "new" ] statuses;
  (match preview.balance with
  | Some b -> Alcotest.(check int64) "the ledger will agree with the bank" b.statement_balance b.ledger_balance
  | None -> Alcotest.fail "no balance check");
  let summary = imported (Import.confirm ctx (request checking october)) in
  Alcotest.(check int) "inbox" 3 summary.inbox_count;
  Alcotest.(check int64) "checking" 9_512_00L (balance service alice checking);
  Alcotest.(check int64) "the manual entry kept its category" 250_00L (balance service alice groceries);
  Alcotest.(check int) "inbox lists them" 3 (Inbox.count service alice);
  (match Import.confirm ctx (request checking october) with Ok (`Already_imported _) -> () | _ -> Alcotest.fail "imported twice");
  let summary = imported (Import.confirm ctx (request checking late_october)) in
  Alcotest.(check (list int)) "overlap: new, duplicate" [ 1; 2 ] [ summary.batch.new_count; summary.batch.duplicate_count ];
  Alcotest.(check int64) "checking" 9_412_00L (balance service alice checking);
  match summary.balance with
  | Some b -> Alcotest.(check int64) "reconciled" b.statement_balance b.ledger_balance
  | None -> Alcotest.fail "no checkpoint"

(* Without balances or references, two exports can hold different identical rows of the same day
   (§6.3): the second coffee must not be dropped as a duplicate of the first. *)
let test_identical_rows_without_balances () =
  let service, alice, checking = setup () in
  let ctx = Import.context service alice in
  let statuses csv ?decisions () =
    List.map (fun (p : Import.planned) -> match p.status with New -> "new" | Duplicate _ | Decided_duplicate _ -> "duplicate" | Matched _ -> "matched" | Ambiguous _ -> "ambiguous")
      (ok_import (Import.preview ctx (request ?decisions checking csv))).rows
  in
  let coffee = "Datum;Text;Belopp\n2026-10-20;KAFFE;-35,00\n" in
  ignore (imported (Import.confirm ctx (request checking coffee)));
  (* The same row in a different file: it may be the same coffee or another one. *)
  let second = "Datum;Text;Belopp\n2026-10-20;KAFFE;-35,00\n2026-10-21;SL;-39,00\n" in
  Alcotest.(check (list string)) "can't tell" [ "ambiguous"; "new" ] (statuses second ());
  (match Import.confirm ctx (request checking second) with
  | Error (`Invalid _) -> ()
  | _ -> Alcotest.fail "confirmed with an undecided row");
  let summary = imported (Import.confirm ctx (request ~decisions:[ (0, Import.Is_new) ] checking second)) in
  Alcotest.(check int) "both new" 2 summary.batch.new_count;
  Alcotest.(check int64) "two coffees" (-109_00L) (balance service alice checking);
  (* A later export that covers the 20th and 21st in full lines up with what is there, with no questions. *)
  let month = "Datum;Text;Belopp\n2026-10-19;ICA;-100,00\n2026-10-20;KAFFE;-35,00\n2026-10-20;KAFFE;-35,00\n2026-10-21;SL;-39,00\n2026-10-22;ICA;-100,00\n" in
  Alcotest.(check (list string)) "complete days" [ "new"; "duplicate"; "duplicate"; "duplicate"; "new" ] (statuses month ());
  let summary = imported (Import.confirm ctx (request checking month)) in
  Alcotest.(check (list int)) "new, duplicates" [ 2; 3 ] [ summary.batch.new_count; summary.batch.duplicate_count ];
  Alcotest.(check int64) "every row once" (-309_00L) (balance service alice checking)

let test_categorize_learn_and_undo () =
  let service, alice, checking = setup () in
  let ctx = Import.context service alice in
  ignore (imported (Import.confirm ctx (request checking october)));
  let salary = account_named service alice "Salary" and subscriptions = account_named service alice "Subscriptions" in
  let item text = List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = text) (Inbox.list service alice ~limit:50) in
  ignore (ok (Inbox.categorize service alice ~transfer_id:(item "LÖN").item.transfer_id ~category_id:salary ()));
  ignore (ok (Inbox.categorize service alice ~transfer_id:(item "SPOTIFY").item.transfer_id ~category_id:subscriptions ()));
  Alcotest.(check int64) "salary" 10_000_00L (balance service alice salary);
  Alcotest.(check int64) "uncategorized income is empty" 0L (balance service alice (account_named service alice "Uncategorized income"));
  Alcotest.(check int64) "checking is untouched" 9_512_00L (balance service alice checking);
  let november = "Bokföringsdag;Text;Belopp;Saldo\n2026-11-15;SPOTIFY;-119,00;9 393,00\n" in
  let preview = ok_import (Import.preview (Import.context service alice) (request checking november)) in
  (match (List.hd preview.rows).suggestion with
  | Some s -> Alcotest.(check bool) "learned from history" true (Account_id.equal s.category_id subscriptions)
  | None -> Alcotest.fail "no suggestion");
  let summary = imported (Import.confirm ctx (request checking november)) in
  ignore (ok_import (Import.undo ctx ~batch_id:summary.batch.batch_id));
  Alcotest.(check int64) "undone" 9_512_00L (balance service alice checking);
  let again = imported (Import.confirm ctx (request checking november)) in
  Alcotest.(check int) "re-imported as new after undo" 1 again.batch.new_count;
  Alcotest.(check int64) "and posted again" 9_393_00L (balance service alice checking)

let test_rules_auto_apply () =
  let service, alice, checking = setup () in
  let subscriptions = account_named service alice "Subscriptions" in
  ignore
    (ok
       (Inbox.add_rule service alice ~owner:(Access.personal alice) ~pattern:"spotify" ~direction:(Some `Out) ~min_amount:None ~max_amount:None
          ~bank_account_id:None ~category_id:subscriptions ~tags:[ "music" ] ~shared:false ~auto_apply:true));
  let summary = imported (Import.confirm (Import.context service alice) (request checking october)) in
  Alcotest.(check int) "spotify skipped the inbox" 2 summary.inbox_count;
  Alcotest.(check int64) "and was categorised" 238_00L (balance service alice subscriptions)

let test_pair_and_share () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let savings = open_checking service alice ~statement_backed:true "Savings" in
  let ctx = Import.context service alice in
  ignore (imported (Import.confirm ctx (request checking "Datum;Text;Belopp\n2026-10-02;TILL SPARKONTO;-1000,00\n2026-10-03;COOP;-300,00\n")));
  ignore (imported (Import.confirm ctx (request savings "Datum;Text;Belopp\n2026-10-03;FRÅN LÖNEKONTO;1000,00\n")));
  let items = Inbox.list service alice ~limit:50 in
  let find text = (List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = text) items).item.transfer_id in
  let candidates = ok (Inbox.pair_candidates service alice ~transfer_id:(find "TILL SPARKONTO")) in
  Alcotest.(check int) "one candidate" 1 (List.length candidates);
  ignore (ok (Inbox.pair service alice ~out_id:(find "TILL SPARKONTO") ~in_id:(find "FRÅN LÖNEKONTO")));
  Alcotest.(check int64) "uncategorized cancelled out" 300_00L (balance service alice (account_named service alice "Uncategorized"));
  let household_groceries =
    (List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service alice)).account.id
  in
  let results = ok (Inbox.share service alice ~transfer_id:(find "COOP") ~household_category_id:household_groceries) in
  Alcotest.(check int) "linked pair" 2 (List.length (List.filter Result.is_ok results));
  Alcotest.(check int64) "household spent it" 300_00L (balance service alice household_groceries);
  Alcotest.(check int64) "alice's books no longer count it" 0L (balance service alice (account_named service alice "Uncategorized"));
  Alcotest.(check int) "inbox is clear" 0 (Inbox.count service alice);
  match ok (Query.execute service alice (Option.get (Query.find "settle_up_status")) []) with
  | `Assoc fields -> (
      match List.assoc "owed" fields with
      | `List [ `Assoc owed ] -> Alcotest.(check string) "the household owes alice" "\"300.00\"" (Yojson.Safe.to_string (List.assoc "amount" owed))
      | other -> Alcotest.failf "unexpected %s" (Yojson.Safe.to_string other))
  | _ -> Alcotest.fail "unexpected settle-up"

let test_reconcile () =
  let service, alice, checking = setup () in
  ignore (imported (Import.confirm (Import.context service alice) (request checking october)));
  let `Difference difference, `Adjusted adjusted, `Ledger_balance after =
    ok (Inbox.reconcile service alice ~account_id:checking ~on_date:(date "2026-10-20") ~statement_balance:9_500_00L ~adjust:true)
  in
  Alcotest.(check int64) "difference" (-12_00L) difference;
  Alcotest.(check (option int64)) "adjusted" (Some (-12_00L)) adjusted;
  Alcotest.(check int64) "now agrees" 9_500_00L after

let () =
  Alcotest.run "import"
    [ ("parsing", [ Alcotest.test_case "amounts and csv" `Quick test_parsing ]);
      ( "pipeline",
        [ Alcotest.test_case "matches, dedupes and checks the balance" `Quick test_import_matches_dedupes_and_checks_the_balance;
          Alcotest.test_case "identical rows without balances" `Quick test_identical_rows_without_balances;
          Alcotest.test_case "categorise, learn and undo" `Quick test_categorize_learn_and_undo;
          Alcotest.test_case "rules auto-apply" `Quick test_rules_auto_apply ] );
      ( "inbox",
        [ Alcotest.test_case "pair and share" `Quick test_pair_and_share; Alcotest.test_case "reconcile" `Quick test_reconcile ] ) ]
