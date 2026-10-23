module Commands = Cashflow_io.Commands
module Import = Cashflow_io.Import
module Import_store = Cashflow_io.Import_store
module Inbox = Cashflow_io.Inbox
module Planning_store = Cashflow_io.Planning_store
module Query = Cashflow_io.Query
module Service = Cashflow_io.Service
module Account_id = Cashflow_core.Account_id
module Csv = Cashflow_core.Csv
module Household = Cashflow_core.Household
module Owner = Cashflow_core.Owner
module Statement_format = Cashflow_core.Statement_format
module Str_compat = Cashflow_core.Str_compat
module Transfer_id = Cashflow_core.Transfer_id
module User_id = Cashflow_core.User_id

open Test_service_support

let field path json = List.fold_left (fun json name -> match json with `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null | _ -> `Null) json path

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

let status_name (p : Import.planned) =
  match p.status with
  | New -> "new"
  | Duplicate _ | Decided_duplicate _ -> "duplicate"
  | Matched _ -> "matched"
  | Confirms_leg _ -> "confirms_leg"
  | Ambiguous _ -> "ambiguous"

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
  let statuses = List.map (fun (p : Import.planned) -> status_name p) preview.rows in
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
  let first = (List.nth (Import_store.batches service.db ~account_id:checking ~limit:5) 1).batch_id in
  Alcotest.(check bool) "the earlier import can't be undone under the later one" true (Result.is_error (Import.undo ctx ~batch_id:first));
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
    List.map (fun (p : Import.planned) -> status_name p)
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
  Alcotest.(check int64) "every row once" (-309_00L) (balance service alice checking);
  (* Both coffees again, in a file that ends on the 20th: this time they are the ones already there. *)
  let again = "Datum;Text;Belopp\n2026-10-18;ICA;-100,00\n2026-10-20;KAFFE;-35,00\n2026-10-20;KAFFE;-35,00\n" in
  Alcotest.(check (list string)) "edge day again" [ "new"; "ambiguous"; "ambiguous" ] (statuses again ());
  let summary = imported (Import.confirm ctx (request ~decisions:[ (1, Import.Is_duplicate); (2, Import.Is_duplicate) ] checking again)) in
  Alcotest.(check (list int)) "said to be already imported" [ 1; 2 ] [ summary.batch.new_count; summary.batch.duplicate_count ];
  Alcotest.(check int64) "so only the ICA is added" (-409_00L) (balance service alice checking)

(* A one-day file listed oldest first: the running balances, not the dates, say which row closes it. *)
let test_closing_balance_oldest_first () =
  let service, alice, checking = setup () in
  let csv = "Datum;Text;Belopp;Saldo\n2026-10-20;KAFFE;-35,00;-35,00\n2026-10-20;KAFFE;-35,00;-70,00\n" in
  match (imported (Import.confirm (Import.context service alice) (request checking csv))).balance with
  | Some b -> Alcotest.(check (list int64)) "agrees" [ -70_00L; -70_00L ] [ b.statement_balance; b.ledger_balance ]
  | None -> Alcotest.fail "no balance check"

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

(* Listing for people: each imported row once, with the category it has now, and none of the
   transfers triage posted to get it there. *)
let test_query_folds_triage () =
  let service, alice, checking = setup () in
  ignore (imported (Import.confirm (Import.context service alice) (request checking october)));
  let spotify = List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = "SPOTIFY") (Inbox.list service alice ~limit:50) in
  let id = spotify.item.transfer_id in
  ignore (ok (Inbox.categorize service alice ~transfer_id:id ~category_id:(account_named service alice "Subscriptions") ()));
  ignore (ok (Inbox.categorize service alice ~transfer_id:id ~category_id:(account_named service alice "Entertainment") ()));
  let rows =
    match field [ "transactions" ] (ok_import (Commands.execute service alice ~scopes:[ "ledger:read" ] Commands.query_transactions (`Assoc [ ("fold_triage", `Bool true) ]))) with
    | `List rows -> rows
    | _ -> Alcotest.fail "no transactions"
  in
  let text path row = match field path row with `String s -> s | other -> Yojson.Safe.to_string other in
  Alcotest.(check int) "each statement row once" 4 (List.length rows);
  Alcotest.(check bool) "no triage transfers" false (List.exists (fun r -> text [ "code" ] r = "reclassify") rows);
  let row text_id = List.find (fun r -> text [ "id" ] r = text_id) rows in
  let spotify = row (Transfer_id.to_string id) in
  Alcotest.(check string) "the latest category" "Entertainment" (text [ "category"; "name" ] spotify);
  Alcotest.(check string) "triaged" "done" (text [ "triage" ] spotify);
  let salary = List.find (fun r -> text [ "description" ] r = "LÖN") rows in
  Alcotest.(check string) "still uncategorised" "Uncategorized income" (text [ "category"; "name" ] salary);
  Alcotest.(check string) "waiting in the inbox" "open" (text [ "triage" ] salary)

(* The row limit counts visible rows: triage's hidden transfers don't use it up. *)
let test_fold_triage_limit () =
  let service, alice, checking = setup () in
  ignore (imported (Import.confirm (Import.context service alice) (request checking "Datum;Text;Belopp\n2026-10-03;COOP;-300,00\n")));
  let coop = List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = "COOP") (Inbox.list service alice ~limit:50) in
  ignore (ok (Inbox.categorize service alice ~transfer_id:coop.item.transfer_id ~category_id:(account_named service alice "Groceries") ()));
  match
    field [ "transactions" ]
      (ok_import (Commands.execute service alice ~scopes:[ "ledger:read" ] Commands.query_transactions (`Assoc [ ("fold_triage", `Bool true); ("limit", `Int 1) ])))
  with
  | `List [ row ] -> Alcotest.(check string) "the purchase" "COOP" (match field [ "description" ] row with `String s -> s | _ -> "")
  | other -> Alcotest.failf "expected the one purchase, got %s" (Yojson.Safe.to_string other)

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

let household_setup () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let shared = open_checking service alice ~owner:(Household household.id) ~statement_backed:true "Shared" in
  (service, alice, household, checking, shared)

let find_item service alice text =
  (List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = text) (Inbox.list service alice ~limit:50)).item.transfer_id

let sum_of service alice owner =
  List.fold_left (fun acc (i : Account_info.t) -> if Owner.equal i.account.owner owner then acc @ [ (i.account.name, Account.posted_balance i.account) ] else acc) [] (Service.accounts service alice)
  |> List.filter (fun (_, b) -> not (Int64.equal b 0L))

(* Undoing an import takes back both sides of a shared cost, the household's included. *)
let test_undo_after_share () =
  let service, alice, household, checking, _ = household_setup () in
  let ctx = Import.context service alice in
  let summary = imported (Import.confirm ctx (request checking "Datum;Text;Belopp\n2026-10-03;COOP;-300,00\n")) in
  let groceries =
    (List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service alice)).account.id
  in
  ignore (ok (Inbox.share service alice ~transfer_id:(find_item service alice "COOP") ~household_category_id:groceries));
  Alcotest.(check int64) "household spent it" 300_00L (balance service alice groceries);
  ignore (ok_import (Import.undo ctx ~batch_id:summary.batch.batch_id));
  Alcotest.(check (list (pair string int64))) "nothing left in the household's books" [] (sum_of service alice (Household household.id));
  Alcotest.(check (list (pair string int64))) "or alice's" [] (sum_of service alice (Access.personal alice))

(* Undoing both imports of a paired move: the first undo puts the other row back in the inbox; the
   second must not bring back the first row, whose own import is gone. Found by the service
   property test: the revived row could be shared, posting household transfers for nothing. *)
let test_undo_does_not_revive_a_row () =
  let service, alice, household, checking, _ = household_setup () in
  let savings = open_checking service alice ~statement_backed:true "Savings" in
  let ctx = Import.context service alice in
  let first = imported (Import.confirm ctx (request checking "Datum;Text;Belopp\n2026-10-01;TILL SPAR;-100,00\n")) in
  let second = imported (Import.confirm ctx (request savings "Datum;Text;Belopp\n2026-10-01;FRÅN LÖN;100,00\n")) in
  let out_id = find_item service alice "TILL SPAR" and in_id = find_item service alice "FRÅN LÖN" in
  ignore (ok (Inbox.pair service alice ~out_id ~in_id));
  ignore (ok_import (Import.undo ctx ~batch_id:first.batch.batch_id));
  let in_inbox id = List.exists (fun (v : Inbox.item_view) -> Transfer_id.equal v.item.transfer_id id) (Inbox.list service alice ~limit:50) in
  Alcotest.(check bool) "the savings row is back in the inbox" true (in_inbox in_id);
  ignore (ok_import (Import.undo ctx ~batch_id:second.batch.batch_id));
  Alcotest.(check bool) "the checking row stays gone" false (in_inbox out_id);
  let groceries =
    (List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service alice)).account.id
  in
  Alcotest.(check bool) "and can't be shared" true (Result.is_error (Inbox.share service alice ~transfer_id:out_id ~household_category_id:groceries));
  Alcotest.(check (list (pair string int64))) "nothing left in alice's books" [] (sum_of service alice (Access.personal alice))

(* A contribution that came in as two imported rows: pairing it is idempotent, counts as the
   contribution in both scopes, and undoing either import puts the other row back in the inbox. *)
let test_pairing_a_contribution () =
  let service, alice, _, checking, shared = household_setup () in
  let ctx = Import.context service alice in
  ignore (imported (Import.confirm ctx (request checking "Datum;Text;Belopp\n2026-10-02;TILL HUSHÅLL;-1000,00\n2026-10-02;SL;-1000,00\n")));
  let in_batch = imported (Import.confirm ctx (request shared "Datum;Text;Belopp\n2026-10-02;FRÅN ALICE;1000,00\n")) in
  let out_id = find_item service alice "TILL HUSHÅLL" and in_id = find_item service alice "FRÅN ALICE" in
  let other = find_item service alice "SL" in
  let first = ok (Inbox.pair service alice ~out_id ~in_id) in
  let again = ok (Inbox.pair service alice ~out_id ~in_id) in
  (match first, again with
  | Ok (Created a), Ok (Exists b) -> Alcotest.(check bool) "a retry finds the same transfer" true (Transfer_id.equal a.transfer.id b.transfer.id)
  | _ -> Alcotest.fail "expected created, then exists");
  Alcotest.(check bool) "neither row pairs with another" true (Result.is_error (Inbox.pair service alice ~out_id:other ~in_id));
  let flows scope =
    match field [ "cycles" ] (ok (Query.execute service alice (Option.get (Query.find "cashflow_summary")) [ ("scope", `String scope); ("cycles", `Int 1) ])) with
    | `List [ c ] -> List.map (fun f -> match field [ f ] c with `String s -> s | _ -> "?") [ "spending"; "income"; "transfers_out"; "transfers_in" ]
    | _ -> Alcotest.fail "expected one cycle"
  in
  Alcotest.(check (list string)) "alice: SL spent, 1000 to the household" [ "1000.00"; "0.00"; "1000.00"; "0.00" ] (flows "me");
  Alcotest.(check (list string)) "household: 1000 in from alice" [ "0.00"; "0.00"; "0.00"; "1000.00" ] (flows "household");
  let contributed () =
    let report = ok (Query.execute service alice (Option.get (Query.find "contribution_plan"))
        [ ("target", `String "1000"); ("incomes", `Assoc [ (User_id.to_string (Access.user alice).id, `String "30000") ]) ]) in
    match field [ "members" ] report with
    | `List [ member ] -> field [ "transferred" ] member
    | _ -> Alcotest.fail "expected one member"
  in
  Alcotest.(check string) "paired contribution counts" {|"1000.00"|} (Yojson.Safe.to_string (contributed ()));
  ignore (ok_import (Import.undo ctx ~batch_id:in_batch.batch.batch_id));
  Alcotest.(check bool) "the other side is back in the inbox" true
    (List.exists (fun (v : Inbox.item_view) -> Transfer_id.equal v.item.transfer_id out_id) (Inbox.list service alice ~limit:50));
  (* Gross flows keep both moves, as for any reversed contribution; what she saved is as after the import. *)
  Alcotest.(check (list string)) "and alice's books net out" [ "2000.00"; "0.00"; "1000.00"; "1000.00" ] (flows "me");
  Alcotest.(check string) "undo takes back the contribution" {|"0.00"|} (Yojson.Safe.to_string (contributed ()))

(* A move entered by hand between two accounts whose statements are both imported: the first
   statement posts it, the second only confirms its own side. *)
let test_manual_move_confirmed_by_both_statements () =
  let service, alice, checking = setup () in
  let savings = open_checking service alice ~statement_backed:true "Savings" in
  ignore (ok (Service.add_entry service alice { (entry "to savings" ~from_account:checking ~to_account:savings 1_000_00) with value_date = date "2026-10-02" }));
  let ctx = Import.context service alice in
  let statuses account csv = List.map status_name (ok_import (Import.preview ctx (request account csv))).rows in
  let checking_csv = "Datum;Text;Belopp\n2026-10-02;TILL SPARKONTO;-1000,00\n" in
  let savings_csv = "Datum;Text;Belopp\n2026-10-02;FRÅN LÖNEKONTO;1000,00\n" in
  Alcotest.(check (list string)) "checking posts it" [ "matched" ] (statuses checking checking_csv);
  ignore (imported (Import.confirm ctx (request checking checking_csv)));
  Alcotest.(check (list string)) "savings confirms it" [ "confirms_leg" ] (statuses savings savings_csv);
  let summary = imported (Import.confirm ctx (request savings savings_csv)) in
  Alcotest.(check (list int64)) "moved once" [ -1_000_00L; 1_000_00L ] [ balance service alice checking; balance service alice savings ];
  Alcotest.(check int) "nothing to triage" 0 (Inbox.count service alice);
  Alcotest.(check bool) "the checking import can't be undone under it" true
    (Result.is_error (Import.undo ctx ~batch_id:(List.nth (Import_store.batches service.db ~account_id:checking ~limit:1) 0).batch_id));
  (* Undoing the savings import only forgets the confirmation. *)
  ignore (ok_import (Import.undo ctx ~batch_id:summary.batch.batch_id));
  Alcotest.(check (list int64)) "still moved once" [ -1_000_00L; 1_000_00L ] [ balance service alice checking; balance service alice savings ];
  Alcotest.(check (list string)) "and savings can confirm it again" [ "confirms_leg" ] (statuses savings (savings_csv ^ "\n"))

(* A member who isn't an admin shares their first purchase: the household's "owed to" account for
   them is opened on the way. *)
let test_member_shares_a_purchase () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" and bob = sign_in service "bob" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  ignore (ok (Service.invite_member service (Service.access_for service (Access.user alice)) ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  let checking = open_checking service bob ~statement_backed:true "Bob checking" in
  ignore (imported (Import.confirm (Import.context service bob) (request checking "Datum;Text;Belopp\n2026-10-03;COOP;-300,00\n")));
  let groceries =
    (List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service bob)).account.id
  in
  ignore (ok (Inbox.share service bob ~transfer_id:(find_item service bob "COOP") ~household_category_id:groceries));
  Alcotest.(check int64) "the household spent it" 300_00L (balance service bob groceries);
  (* But a member still can't open other household liabilities. *)
  Alcotest.(check bool) "no other liabilities" true
    (Result.is_error
       (Service.open_account service bob ~key:(key "loan") ~owner:(Household household.id) ~kind:Liability ~name:"Loan" ~currency:"SEK"
          ~flags:Account.no_flags ~details:Account_info.no_details))

(* The reconciled flag follows the ledger as it is now: reversing an entry before a checkpoint, or
   undoing the import that made it, takes it back. *)
let test_reconciliation_follows_the_ledger () =
  let service, alice, checking = setup () in
  let ctx = Import.context service alice in
  let through () =
    let info = ok (Service.find_account service alice checking) in
    Planning_store.reconciled_through service.db checking ~ledger_balance_on:(Import_store.posted_balance_on service.db info.account)
  in
  let summary = imported (Import.confirm ctx (request checking october)) in
  Alcotest.(check (option string)) "reconciled" (Some "2026-10-20") (Option.map Date.to_string (through ()));
  let spotify = (List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = "SPOTIFY") (Inbox.list service alice ~limit:10)).item.transfer_id in
  ignore (ok (Service.reverse service alice ~key:(key "oops") ~transfer_id:spotify ()));
  Alcotest.(check (option string)) "not after a reversal" None (Option.map Date.to_string (through ()));
  ignore (ok_import (Import.undo ctx ~batch_id:summary.batch.batch_id));
  Alcotest.(check (option string)) "nor after an undo" None (Option.map Date.to_string (through ()));
  Alcotest.(check int) "the undone import's checkpoint is gone" 0 (List.length (Planning_store.checkpoints service.db checking))

(* A Handelsbanken export as an .xlsx, built here the way LibreOffice saves one: text in shared
   strings, amounts and balances as numbers, newest first, under the bank's preamble. *)
let handelsbanken_xlsx ?(number = "Lönekonto 123 456 789") rows =
  let strings = ref [] in
  let shared text =
    match List.find_index (String.equal text) (List.rev !strings) with
    | Some i -> i
    | None -> strings := text :: !strings; List.length !strings - 1
  in
  let text col row t = Printf.sprintf {|<c r="%c%d" t="s"><v>%d</v></c>|} col row (shared t) in
  let num col row v = Printf.sprintf {|<c r="%c%d" t="n"><v>%s</v></c>|} col row v in
  let row r cells = Printf.sprintf {|<row r="%d">%s</row>|} r (String.concat "" cells) in
  let preamble =
    [ row 1 [ text 'A' 1 "Handelsbanken" ]; row 2 [ text 'A' 2 "2026-10-21, 19:50" ]; row 4 [ text 'A' 4 number ];
      row 6 [ text 'A' 6 "Kontoform: Allkonto"; text 'B' 6 "Clearingnummer: 6000"; text 'C' 6 "Saldo: 0" ];
      row 7 [ text 'A' 7 "Period: 2026-09-21 - 2026-10-21"; text 'B' 7 "Transaktionstyp: Alla" ];
      row 9 [ text 'A' 9 "Reskontradatum"; text 'B' 9 "Transaktionsdatum"; text 'C' 9 "Text"; text 'D' 9 "Belopp"; text 'E' 9 "Saldo" ] ]
  in
  let body = List.mapi (fun i (d, t, amount, balance) -> let r = 10 + i in row r [ text 'A' r d; text 'B' r d; text 'C' r t; num 'D' r amount; num 'E' r balance ]) rows in
  let sheet = {|<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>|} ^ String.concat "" (preamble @ body) ^ "</sheetData></worksheet>" in
  let sst =
    {|<?xml version="1.0" encoding="UTF-8"?><sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">|}
    ^ String.concat "" (List.rev_map (fun t -> {|<si><t xml:space="preserve">|} ^ t ^ "</t></si>") !strings) ^ "</sst>"
  in
  let workbook =
    {|<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="Ark1" sheetId="1" r:id="rId3"/></sheets></workbook>|}
  in
  let rels =
    {|<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>|}
  in
  let add zip (path, content) =
    let file = Result.get_ok (Zipc.File.deflate_of_binary_string content) in
    Zipc.add (Result.get_ok (Zipc.Member.make ~path (Zipc.Member.File file))) zip
  in
  List.fold_left add Zipc.empty [ ("xl/workbook.xml", workbook); ("xl/_rels/workbook.xml.rels", rels); ("xl/worksheets/sheet1.xml", sheet); ("xl/sharedStrings.xml", sst) ]
  |> Zipc.to_binary_string |> Result.get_ok

let test_handelsbanken_xlsx () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking =
    match
      Service.open_account service alice ~key:(key "open:lönekonto") ~owner:(Access.personal alice) ~kind:Asset ~name:"Lönekonto" ~currency:"SEK"
        ~flags:Account.no_flags ~details:{ Account_info.no_details with statement_backed = true; number_last4 = Some "6789" }
    with
    | Ok (`Created info | `Exists info) -> info.account.id
    | Error e -> Alcotest.fail (Service.error_to_string e)
  in
  let ctx = Import.context service alice in
  let rows = [ ("2026-10-06", "Överf Mobil", "12345.67", "19945.67"); ("2026-10-06", "ICA NARA", "-2400", "7600.00"); ("2026-09-30", "LÖN", "8000", "10000") ] in
  let file = handelsbanken_xlsx rows in
  let preview = ok_import (Import.preview ctx (request checking file)) in
  Alcotest.(check string) "format" "handelsbanken" preview.decoded_file.format;
  Alcotest.(check (list int64)) "exact amounts, newest first" [ 12_345_67L; -2_400_00L; 8_000_00L ] (List.map (fun (p : Import.planned) -> p.row.amount) preview.rows);
  (* Nothing before the file: the bank's balance says what the account held the day before. *)
  Alcotest.(check (option (pair string int64))) "opening suggested" (Some ("2026-09-29", 2_000_00L))
    (Option.map (fun (d, b) -> (Date.to_string d, b)) preview.opening);
  ignore (ok (Service.set_opening_balance service alice ~account_id:checking ~balance:2_000_00L ~value_date:(date "2026-09-29")));
  Alcotest.(check bool) "and not again" true (Option.is_none (ok_import (Import.preview ctx (request checking file))).opening);
  let summary = imported (Import.confirm ctx (request checking file)) in
  (match summary.balance with
  | Some b -> Alcotest.(check (list int64)) "agrees with the bank" [ 19_945_67L; 19_945_67L ] [ b.statement_balance; b.ledger_balance ]
  | None -> Alcotest.fail "no balance check");
  Alcotest.(check int64) "balance" 19_945_67L (balance service alice checking);
  (* A statement for another account is refused. *)
  match Import.preview ctx (request checking (handelsbanken_xlsx ~number:"Sparkonto 987 654 321" rows)) with
  | Error (`Invalid message) -> Alcotest.(check bool) "names both" true (Str_compat.contains ~sub:"****4321" message && Str_compat.contains ~sub:"****6789" message)
  | _ -> Alcotest.fail "imported into the wrong account"

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
          Alcotest.test_case "closing balance, oldest first" `Quick test_closing_balance_oldest_first;
          Alcotest.test_case "handelsbanken xlsx" `Quick test_handelsbanken_xlsx;
          Alcotest.test_case "categorise, learn and undo" `Quick test_categorize_learn_and_undo;
          Alcotest.test_case "query folds triage" `Quick test_query_folds_triage;
          Alcotest.test_case "fold triage counts visible rows" `Quick test_fold_triage_limit;
          Alcotest.test_case "rules auto-apply" `Quick test_rules_auto_apply ] );
      ( "inbox",
        [ Alcotest.test_case "pair and share" `Quick test_pair_and_share; Alcotest.test_case "undo after share" `Quick test_undo_after_share;
          Alcotest.test_case "pairing a contribution" `Quick test_pairing_a_contribution;
          Alcotest.test_case "undo doesn't revive a row" `Quick test_undo_does_not_revive_a_row;
          Alcotest.test_case "a manual move confirmed by both statements" `Quick test_manual_move_confirmed_by_both_statements;
          Alcotest.test_case "a member shares a purchase" `Quick test_member_shares_a_purchase;
          Alcotest.test_case "reconciliation follows the ledger" `Quick test_reconciliation_follows_the_ledger; Alcotest.test_case "reconcile" `Quick test_reconcile ] ) ]
