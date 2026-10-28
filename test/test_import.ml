module Commands = Cashflow_io.Commands
module Import = Cashflow_io.Import
module Import_store = Cashflow_io.Import_store
module Inbox = Cashflow_io.Inbox
module Llm = Cashflow_io.Llm
module Logo_fetch = Cashflow_io.Logo_fetch
module Merchant_logos = Cashflow_io.Merchant_logos
module Sql = Cashflow_io.Sql
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
module User = Cashflow_core.User
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

let request ?(decisions = []) account_id content : Import.request = { target = Existing account_id; file_name = Some "export.csv"; content; decisions }

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
  Alcotest.(check string) "import merchant key" "spotify" (text ["merchant"; "key"] spotify);
  Alcotest.(check string) "visible owner" "personal" (text ["merchant"; "owner"] spotify);
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


let test_manual_merchant_query () =
  let db = Store_sqlite.open_memory () in
  let calls = ref 0 in
  let service = Service.make ~logo_network:Logo_fetch.{with_request=(fun f -> f ());
    resolve=(fun ~host:_ -> Ok ["8.8.8.8"]);
    get=(fun ~uri:_ ~address:_ ~max_bytes:_ -> incr calls; Ok {status=200; headers=[]; body=(let path = if Sys.file_exists "test/fixtures/logo_64x48.png" then "test/fixtures/logo_64x48.png" else "fixtures/logo_64x48.png" in In_channel.with_open_bin path In_channel.input_all)})} db in
  let access = sign_in service "alice" in
  let checking = open_checking service access "Checking" in
  let groceries = account_named service access "Groceries" in
  ignore (ok (Service.add_entry service access { (entry "ICA purchase" ~from_account:checking ~to_account:groceries 100) with
    memo={Transfer.empty_memo with counterparty=Some "ICA"} }));
  let logo = Result.get_ok (Merchant_logos.set service access ~owner:(Access.personal access) ~merchant_key:"ica" ~link:"shop.com/pic.png") in
  let count = !calls in
  let result = ok_import (Commands.execute service access ~scopes:["ledger:read"] Commands.query_transactions (`Assoc [])) in
  let rows = match field ["transactions"] result with `List rows -> rows | _ -> Alcotest.fail "rows" in
  let row = List.find (fun row -> field ["merchant";"key"] row = `String "ica") rows in
  Alcotest.(check bool) "same-origin URL" true (field ["merchant";"logo"] row = `String (Option.get logo.logo));
  Alcotest.(check string) "personal owner" {|"personal"|} (Yojson.Safe.to_string (field ["merchant";"owner"] row));
  Alcotest.(check int) "query does not fetch" count !calls

let test_logo_scheduling () =
  let db = Store_sqlite.open_memory () in
  let pending = ref [] and calls = ref 0 in
  let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system ~user:_ () -> if Str_compat.contains ~sub:"website hostname" system then incr calls; Ok {|{"merchants":[]}|})} in
  let service = Service.make ~assistant ~logo_network:(Logo_fetch.{ with_request=(fun f -> f ());
    resolve=(fun ~host:_ -> Alcotest.fail "unexpected DNS"); get=(fun ~uri:_ ~address:_ ~max_bytes:_ -> Alcotest.fail "unexpected fetch") })
    ~schedule:(fun work -> Alcotest.(check bool) "after commit" false (Sql.in_transaction db); pending := work :: !pending) db in
  let access = sign_in service "alice" in
  let checking = open_checking service access "Checking" in
  let ctx = Import.context service access in
  ignore (Import.preview ctx (request checking october) |> ok_import);
  Alcotest.(check int) "preview no schedule" 0 (List.length !pending);
  let bad = request checking "Datum;Text;Belopp\nnot-a-date;INVALID;-100,00\n" in
  Alcotest.(check bool) "failed confirmation" true (Result.is_error (Import.confirm ctx bad));
  Alcotest.(check int) "failed confirmation no job" 0 (List.length !pending);
  ignore (imported (Import.confirm ctx (request checking october)));
  Alcotest.(check int) "one job" 1 (List.length !pending);
  Alcotest.(check int) "not inline" 0 !calls;
  List.iter (fun f -> f ()) !pending;
  Alcotest.(check bool) "resolved later" true (!calls > 0);
  pending := [];
  ignore (Import.confirm ctx (request checking october));
  Alcotest.(check int) "duplicate no schedule" 0 (List.length !pending);
  let offline_jobs = ref 0 and offline_dns = ref 0 and offline_get = ref 0 in
  let offline = Service.make ~logo_network:(Logo_fetch.{ with_request=(fun f -> f ());
    resolve=(fun ~host:_ -> incr offline_dns; Ok ["8.8.8.8"]);
    get=(fun ~uri:_ ~address:_ ~max_bytes:_ -> incr offline_get; Error Unreachable_site) })
    ~schedule:(fun _ -> incr offline_jobs) db in
  let fresh_checking = open_checking offline access "Offline checking" in
  ignore (imported (Import.confirm (Import.context offline access) (request fresh_checking october)));
  Alcotest.(check int) "fresh import schedules no logo job without assistant" 0 !offline_jobs;
  Alcotest.(check int) "offline logo DNS" 0 !offline_dns;
  Alcotest.(check int) "offline logo GET" 0 !offline_get

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

(* History suggestions wait in the inbox until the bank account asks for the confident ones to be applied. *)
let test_auto_apply_threshold () =
  let service, alice, checking = setup () in
  let ctx = Import.context service alice in
  ignore (imported (Import.confirm ctx (request checking october)));
  let subscriptions = account_named service alice "Subscriptions" in
  let spotify = (List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = "SPOTIFY") (Inbox.list service alice ~limit:50)).item.transfer_id in
  ignore (ok (Inbox.categorize service alice ~transfer_id:spotify ~category_id:subscriptions ()));
  let file day = Printf.sprintf "Bokföringsdag;Text;Belopp;Saldo\n2026-11-%02d;SPOTIFY;-119,00;9 393,00\n" day in
  let import day = imported (Import.confirm (Import.context service alice) (request checking (file day))) in
  let set minimum = ok (Inbox.set_auto_apply service alice ~account_id:checking ~min_confidence:minimum) in
  Alcotest.(check (list (pair string int))) "off by default" [] (List.map (fun (id, n) -> (Account_id.to_string id, n)) (Inbox.auto_apply_settings service alice));
  Alcotest.(check int) "history waits in the inbox" 1 (import 10).inbox_count;
  set (Some 90);
  Alcotest.(check int) "a threshold above its confidence still waits" 1 (import 11).inbox_count;
  let before = balance service alice subscriptions in
  set (Some 60);
  Alcotest.(check int) "a met threshold skips the inbox" 0 (import 12).inbox_count;
  Alcotest.(check int64) "and categorises it" (Int64.add before 119_00L) (balance service alice subscriptions);
  Alcotest.(check int) "the setting is listed" 1 (List.length (Inbox.auto_apply_settings service alice));
  set None;
  Alcotest.(check int) "turned off, it waits again" 1 (import 13).inbox_count;
  let bob = sign_in service "bob" in
  Alcotest.(check bool) "another user cannot change it" true (Result.is_error (Inbox.set_auto_apply service bob ~account_id:checking ~min_confidence:(Some 60)));
  Alcotest.(check (list string)) "nor see it" [] (List.map (fun (id, _) -> Account_id.to_string id) (Inbox.auto_apply_settings service bob));
  Alcotest.(check bool) "only a bank account can opt in" true
    (Result.is_error (Inbox.set_auto_apply service alice ~account_id:subscriptions ~min_confidence:(Some 60)))

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
  Alcotest.(check (list string)) "the move paired itself on the second import" [ "COOP" ]
    (List.map (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description) items);
  let find text = (List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = text) items).item.transfer_id in
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

(* Two equal outflows could each be the contribution: the model, asked after the import has committed,
   picks the one whose text says so, and a low-confidence answer pairs nothing. *)
let test_llm_pairs_unsure_moves () =
  let run ~confidence =
    let asked = ref 0 in
    let complete ?schema:_ ~system ~user () =
      if not (Str_compat.contains ~sub:"one transfer of the user's own money" system) then Ok {|{"suggestions":[]}|}
      else (
        incr asked;
        match Yojson.Safe.from_string user with
        | `List cases ->
            let pairs =
              List.filter_map
                (fun case ->
                  let options = match field [ "options" ] case with `List o -> o | _ -> [] in
                  List.find_opt (fun o -> field [ "text" ] o = `String "FRÅN ALICE") options
                  |> Option.map (fun chosen ->
                         if field [ "text" ] case = `String "TILL HUSHÅLL" then `Assoc [ ("id", field [ "id" ] case); ("option", field [ "id" ] chosen); ("confidence", `Int confidence) ]
                         else `Assoc [ ("id", field [ "id" ] case); ("option", field [ "id" ] chosen); ("confidence", `Int 10) ]))
                cases
            in
            Ok (Yojson.Safe.to_string (`Assoc [ ("pairs", `List pairs) ]))
        | _ -> Alcotest.fail "expected cases")
    in
    let service = Service.make ~assistant:Llm.{ model = "fake"; complete } ~now:(fun () -> now) (Store_sqlite.open_memory ()) in
    let alice = sign_in service ~admin:true "alice" in
    let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
    let alice = Service.access_for service (Access.user alice) in
    let checking = open_checking service alice ~statement_backed:true "Checking" in
    let shared = open_checking service alice ~owner:(Household household.id) ~statement_backed:true "Shared" in
    let ctx = Import.context service alice in
    ignore (imported (Import.confirm ctx (request checking "Datum;Text;Belopp\n2026-10-02;TILL HUSHÅLL;-1000,00\n2026-10-02;SL;-1000,00\n")));
    let summary = imported (Import.confirm ctx (request shared "Datum;Text;Belopp\n2026-10-02;FRÅN ALICE;1000,00\n")) in
    (!asked, summary.inbox_count, List.map (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description) (Inbox.list service alice ~limit:50))
  in
  let asked, left, open_rows = run ~confidence:95 in
  Alcotest.(check int) "asked once" 1 asked;
  Alcotest.(check int) "the new row is no longer open" 0 left;
  Alcotest.(check (list string)) "only SL is left to triage" [ "SL" ] open_rows;
  let _, left, open_rows = run ~confidence:40 in
  Alcotest.(check int) "unsure answer leaves the row" 1 left;
  Alcotest.(check int) "all three stay open" 3 (List.length open_rows)

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

(* Rows already in the inbox, however many: two years from two accounts is far more than a page of 500, and a move
   that was ambiguous when imported pairs once its rival is gone. *)
let test_pair_all_moves_retroactively () =
  let service, alice, _, checking, shared = household_setup () in
  let ctx = Import.context service alice in
  ignore (imported (Import.confirm ctx (request checking "Datum;Text;Belopp\n2026-10-02;TILL HUSHÅLL;-1000,00\n2026-10-02;SL;-1000,00\n")));
  ignore (imported (Import.confirm ctx (request shared "Datum;Text;Belopp\n2026-10-02;FRÅN ALICE;1000,00\n")));
  Alcotest.(check int) "ambiguous at import, all open" 3 (Inbox.count service alice);
  ignore (ok (Inbox.dismiss service alice ~transfer_id:(find_item service alice "SL")));
  Alcotest.(check (triple int int int)) "paired by rule" (1, 0, 0) (Inbox.pair_all service alice);
  Alcotest.(check int) "inbox is clear" 0 (Inbox.count service alice);
  let rows sign text = String.concat "" (List.init 700 (fun i -> Printf.sprintf "%s;%s;%s%d,00\n" (Date.to_string (Date.add_days (date "2024-01-01") i)) text sign (1000 + i))) in
  ignore (imported (Import.confirm ctx (request checking ("Datum;Text;Belopp\n" ^ rows "-" "TILL HUSHÅLL")))) ;
  ignore (imported (Import.confirm ctx (request shared ("Datum;Text;Belopp\n" ^ rows "" "FRÅN ALICE"))));
  Alcotest.(check int) "a long history pairs completely" 0 (Inbox.count service alice)

(* Undoing an import reverses its rows in the ledger, but a bank account's list shows what the statement
   says: after undoing and uploading the same file again, each row once. *)
let test_list_after_undo_and_reupload () =
  let service, alice, _, checking, _ = household_setup () in
  let ctx = Import.context service alice in
  let csv = "Datum;Text;Belopp\n2026-10-03;COOP;-300,00\n2026-10-04;ICA;-100,00\n" in
  let listed () =
    let result = ok_import (Commands.execute service alice ~scopes:[ "ledger:read" ] Commands.query_transactions
      (`Assoc [ ("account_id", `String (Account_id.to_string checking)); ("fold_triage", `Bool true) ])) in
    List.map (fun t -> field [ "description" ] t |> Yojson.Safe.Util.to_string) (Yojson.Safe.Util.to_list (field [ "transactions" ] result))
  in
  let first = imported (Import.confirm ctx (request checking csv)) in
  ignore (ok_import (Import.undo ctx ~batch_id:first.batch.batch_id));
  Alcotest.(check (list string)) "nothing left after the undo" [] (listed ());
  ignore (imported (Import.confirm ctx (request checking csv)));
  Alcotest.(check (list string)) "each row once" [ "ICA"; "COOP" ] (listed ());
  Alcotest.(check int64) "and the balance counts them once" (-400_00L) (balance service alice checking)

(* Differential: a ledger that imported a file, undid it and imported it again must report what one that
   imported it once reports. *)
let test_reports_after_undo_and_reupload () =
  let month i = let m = 5 + i in Printf.sprintf "2026-%02d" m in
  let rows =
    String.concat ""
      (List.concat_map
         (fun i -> [ Printf.sprintf "%s-15;SPOTIFY;-119,00\n" (month i); Printf.sprintf "%s-%02d;ICA NARA;-%d,00\n" (month i) (3 + i) (200 + (37 * i));
                     Printf.sprintf "%s-25;LÖN;30000,00\n" (month i); Printf.sprintf "%s-26;TILL HUSHÅLL;-5000,00\n" (month i) ])
         [ 0; 1; 2; 3; 4; 5 ])
  in
  let csv = "Datum;Text;Belopp\n" ^ rows in
  let shared_csv = "Datum;Text;Belopp\n" ^ String.concat "" (List.init 6 (fun i -> Printf.sprintf "%s-26;FRÅN ALICE;5000,00\n" (month i))) in
  let build ~churn =
    let service, alice, _, checking, shared = household_setup () in
    let ctx = Import.context service alice in
    let both () =
      let a = imported (Import.confirm ctx (request checking csv)) in
      let b = imported (Import.confirm ctx (request shared shared_csv)) in
      (a, b)
    in
    if churn then begin
      let a, b = both () in
      (* The newest first, as the page requires. *)
      ignore (ok_import (Import.undo ctx ~batch_id:b.batch.batch_id));
      ignore (ok_import (Import.undo ctx ~batch_id:a.batch.batch_id))
    end;
    ignore (both ());
    (service, alice)
  in
  let clean = build ~churn:false and churned = build ~churn:true in
  let report (service, alice) name args =
    match Query.execute service alice (Option.get (Query.find name)) args with
    | Ok json -> Str.global_replace (Str.regexp "[0-9a-f]+-[0-9a-f]+-[0-9a-f]+-[0-9a-f]+-[0-9a-f]+") "ID" (Yojson.Safe.pretty_to_string json)
    | Error e -> "ERROR " ^ Service.error_to_string e
  in
  let differing = ref [] in
  List.iter
    (fun (name, args) ->
      let a = report clean name args and b = report churned name args in
      if not (String.equal a b) then differing := Printf.sprintf "%s\nclean:\n%s\nafter undo and reupload:\n%s" name a b :: !differing)
    [ ("cashflow_summary", [ ("cycles", `Int 6) ]); ("spending_by_category", []); ("net_worth_series", [ ("cycles", `Int 6) ]);
      ("largest_transactions", []); ("runway", []); ("recurring_payments", []); ("forecast", []) ];
  Alcotest.(check (list string)) "reports that differ" [] (List.rev !differing)

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
  Alcotest.(check int) "the move paired itself" 0 (Inbox.count service alice);
  let out_id = List.hd (Import_store.batch_transfers service.db first.batch.batch_id) and in_id = List.hd (Import_store.batch_transfers service.db second.batch.batch_id) in
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
  Alcotest.(check int) "both equal outflows are candidates to choose between by hand" 2 (List.length (ok (Inbox.pair_candidates service alice ~transfer_id:in_id)));
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
  (* The contribution and its reversal cancel out: a move that was undone is not a move out and back. *)
  Alcotest.(check (list string)) "and alice's books net out" [ "2000.00"; "0.00"; "0.00"; "0.00" ] (flows "me");
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

(* A file for an account that isn't here yet: it says which one (the bank, the account's name and
   number), the preview opens nothing, and the import opens the account with the bank's balance. *)
let test_new_account_from_the_file () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let ctx = Import.context service alice in
  let rows = [ ("2026-10-06", "Överf Mobil", "12345.67", "19945.67"); ("2026-10-06", "ICA NARA", "-2400", "7600.00"); ("2026-09-30", "LÖN", "8000", "10000") ] in
  let file = handelsbanken_xlsx rows in
  let identified = ok_import (Import.identify ctx file) in
  Alcotest.(check (list (option string))) "what the file says" [ Some "Handelsbanken"; Some "Lönekonto"; Some "6789" ]
    [ identified.identity.institution; identified.identity.account_name; identified.identity.last4 ];
  Alcotest.(check int) "no account has those digits" 0 (List.length identified.matches);
  let request : Import.request =
    { target = Create { name = "Lönekonto"; kind = Asset; owner = Access.personal alice; institution = Some "Handelsbanken"; number_last4 = Some "6789" };
      file_name = Some "export.xlsx"; content = file; decisions = [] }
  in
  let preview = ok_import (Import.preview ctx request) in
  Alcotest.(check (list string)) "all new" [ "new"; "new"; "new" ] (List.map status_name preview.rows);
  (match preview.balance with
  | Some b -> Alcotest.(check int64) "counts the opening balance it will set" b.statement_balance b.ledger_balance
  | None -> Alcotest.fail "no balance check");
  let banks () = List.filter (fun (i : Account_info.t) -> i.account.kind = Asset && Option.is_none i.details.role) (Service.accounts service alice) in
  Alcotest.(check int) "a preview opens nothing" 0 (List.length (banks ()));
  let summary = imported (Import.confirm ctx request) in
  Alcotest.(check string) "named" "Lönekonto" summary.account.account.name;
  Alcotest.(check (option string)) "numbered" (Some "6789") summary.account.details.number_last4;
  Alcotest.(check int) "one account" 1 (List.length (banks ()));
  Alcotest.(check int64) "opened with the bank's balance" 19_945_67L (balance service alice summary.account.account.id);
  (match summary.balance with
  | Some b -> Alcotest.(check int64) "agrees with the bank" b.statement_balance b.ledger_balance
  | None -> Alcotest.fail "no balance check");
  let again = ok_import (Import.identify ctx file) in
  Alcotest.(check (list string)) "the next file finds it" [ Account_id.to_string summary.account.account.id ]
    (List.map (fun (i : Account_info.t) -> Account_id.to_string i.account.id) again.matches)

(* Handelsbanken hands its export out zipped: the zip is read as the statement inside it. *)
let test_zipped_statement () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let ctx = Import.context service alice in
  let rows = [ ("2026-10-06", "ICA NARA", "-2400", "7600.00"); ("2026-09-30", "LÖN", "8000", "10000") ] in
  let xlsx = handelsbanken_xlsx rows in
  let zip members =
    List.fold_left
      (fun zip (path, content) ->
        let file = Result.get_ok (Zipc.File.deflate_of_binary_string content) in
        Zipc.add (Result.get_ok (Zipc.Member.make ~path (Zipc.Member.File file))) zip)
      Zipc.empty members
    |> Zipc.to_binary_string |> Result.get_ok
  in
  let zipped = zip [ ("Transaktioner.xlsx", xlsx); ("__MACOSX/._Transaktioner.xlsx", "junk") ] in
  let identified = ok_import (Import.identify ctx zipped) in
  Alcotest.(check string) "the spreadsheet inside" "handelsbanken" identified.format;
  Alcotest.(check (option string)) "its account" (Some "6789") identified.identity.last4;
  let request content : Import.request =
    { target = Create { name = "Lönekonto"; kind = Asset; owner = Access.personal alice; institution = Some "Handelsbanken"; number_last4 = Some "6789" };
      file_name = Some "Transaktioner.zip"; content; decisions = [] }
  in
  let summary = imported (Import.confirm ctx (request zipped)) in
  Alcotest.(check int) "rows" 2 summary.batch.new_count;
  (* The same statement unzipped, or zipped again, is the same file. *)
  (match Import.preview ctx { (request xlsx) with target = Existing summary.account.account.id } with
  | Ok p -> Alcotest.(check bool) "already imported" true (Option.is_some p.already_imported)
  | Error e -> Alcotest.fail (Import.error_to_string e));
  (match Import.identify ctx (zip [ ("a.csv", october); ("b.csv", october) ]) with
  | Error (`Unreadable message) -> Alcotest.(check bool) "names them" true (Str_compat.contains ~sub:"a.csv" message)
  | _ -> Alcotest.fail "guessed among several files");
  match Import.identify ctx (zip [ ("readme.pdf", "%PDF") ]) with
  | Error (`Unreadable _) -> ()
  | _ -> Alcotest.fail "read a zip with no statement"

(* A card's statement writes the debt as negative; the card opens owing it. *)
let test_new_card_opens_owing () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let ctx = Import.context service alice in
  let request : Import.request =
    { target = Create { name = "Kreditkort"; kind = Liability; owner = Access.personal alice; institution = None; number_last4 = None };
      file_name = Some "card.csv"; content = "Datum;Text;Belopp;Saldo\n2026-10-20;Purchase;-100,00;-1000,00\n"; decisions = [] }
  in
  let preview = ok_import (Import.preview ctx request) in
  Alcotest.(check (option (pair string int64))) "owed the day before" (Some ("2026-10-19", 900_00L))
    (Option.map (fun (d, b) -> (Date.to_string d, b)) preview.opening);
  (match preview.balance with
  | Some b -> Alcotest.(check (list int64)) "agrees with the bank" [ 1_000_00L; 1_000_00L ] [ b.statement_balance; b.ledger_balance ]
  | None -> Alcotest.fail "no balance check");
  let summary = imported (Import.confirm ctx request) in
  Alcotest.(check int64) "owes 1000" 1_000_00L (balance service alice summary.account.account.id);
  match summary.balance with
  | Some b -> Alcotest.(check int64) "reconciled" b.statement_balance b.ledger_balance
  | None -> Alcotest.fail "no checkpoint"

(* An account opened by hand learns its number from the first file, so the next one finds it. *)
let test_an_account_learns_its_number () =
  let service, alice, checking = setup () in
  let ctx = Import.context service alice in
  let identified = ok_import (Import.identify ctx october) in
  Alcotest.(check (list (option string))) "a masked number in the preamble" [ Some "Lönekonto"; Some "1234" ]
    [ identified.identity.account_name; identified.identity.last4 ];
  Alcotest.(check int) "nothing matches yet" 0 (List.length identified.matches);
  ignore (imported (Import.confirm ctx (request checking october)));
  Alcotest.(check (list string)) "now it does" [ Account_id.to_string checking ]
    (List.map (fun (i : Account_info.t) -> Account_id.to_string i.account.id) (ok_import (Import.identify ctx october)).matches)

let test_reconcile () =
  let service, alice, checking = setup () in
  ignore (imported (Import.confirm (Import.context service alice) (request checking october)));
  let `Difference difference, `Adjusted adjusted, `Ledger_balance after =
    ok (Inbox.reconcile service alice ~account_id:checking ~on_date:(date "2026-10-20") ~statement_balance:9_500_00L ~adjust:true)
  in
  Alcotest.(check int64) "difference" (-12_00L) difference;
  Alcotest.(check (option int64)) "adjusted" (Some (-12_00L)) adjusted;
  Alcotest.(check int64) "now agrees" 9_500_00L after

(* History looks across all of an owner's categories, so a confident suggestion may be one the inbox would refuse:
   another owner's or another currency's. Such a row must wait rather than be filed. *)
let test_auto_apply_respects_owner_and_currency () =
  let service, alice, _household, checking, shared = household_setup () in
  let ctx = Import.context service alice in
  ignore (imported (Import.confirm ctx (request checking october)));
  let subscriptions = account_named service alice "Subscriptions" in
  let spotify = find_item service alice "SPOTIFY" in
  ignore (ok (Inbox.categorize service alice ~transfer_id:spotify ~category_id:subscriptions ()));
  let euro =
    match
      Service.open_account service alice ~key:(Test_service_support.key "open:euro") ~owner:(Service.(Access.personal alice)) ~kind:Asset ~name:"Euro card"
        ~currency:"EUR" ~flags:Account.no_flags ~details:{ Account_info.no_details with statement_backed = true }
    with
    | Ok (`Created info | `Exists info) -> info.account.id
    | Error error -> Alcotest.fail (Service.error_to_string error)
  in
  let file = "Bokföringsdag;Text;Belopp;Saldo\n2026-11-10;SPOTIFY;-119,00;9 393,00\n" in
  List.iter
    (fun (label, account) ->
      ok (Inbox.set_auto_apply service alice ~account_id:account ~min_confidence:(Some 60));
      let before = balance service alice subscriptions in
      let summary = imported (Import.confirm (Import.context service alice) (request account file)) in
      Alcotest.(check int) (label ^ " waits in the inbox") 1 summary.inbox_count;
      Alcotest.(check int64) (label ^ " leaves the category alone") before (balance service alice subscriptions))
    [ ("a household account", shared); ("a euro account", euro) ]

(* A row in a new currency lands in an Uncategorized account for that currency, stored in English
   like the rest of the starter chart, whatever the importer's language. *)
let test_new_currency_uncategorized_is_stored_in_english () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let user = Access.user alice in
  let alice =
    ok (Service.update_settings service alice { User.locale = "sv"; timezone = user.timezone; default_currency = user.default_currency; pay_cycle = user.pay_cycle })
  in
  let euro =
    match
      Service.open_account service alice ~key:(key "open:euro") ~owner:(Access.personal alice) ~kind:Asset ~name:"Euro card" ~currency:"EUR"
        ~flags:Account.no_flags ~details:{ Account_info.no_details with statement_backed = true }
    with
    | Ok (`Created info | `Exists info) -> info.account.id
    | Error error -> Alcotest.fail (Service.error_to_string error)
  in
  ignore (imported (Import.confirm (Import.context service alice) (request euro "Bokföringsdag;Text;Belopp;Saldo\n2026-11-10;SPOTIFY;-119,00;9 393,00\n")));
  ignore (account_named service alice "Uncategorized (EUR)")

(* Moves between a person's own accounts, imported every way a person might: income is the salary, spending is the
   purchases, and each move nets out, in every scope. Alice is paid into checking, keeps some in savings and pays her
   part of the household's costs into the shared account, every month. *)
let months = [ "2026-05"; "2026-06"; "2026-07"; "2026-08"; "2026-09"; "2026-10" ]
let statement rows = "Datum;Text;Belopp\n" ^ String.concat "" (List.concat_map rows months)

let checking_csv =
  statement (fun m -> [ m ^ "-05;LÖN;30000,00\n"; m ^ "-05;TILL SPARKONTO;-3000,00\n"; m ^ "-06;TILL HUSHÅLL;-8000,00\n"; m ^ "-10;ICA;-450,00\n" ])

let savings_csv = statement (fun m -> [ m ^ "-05;FRÅN LÖNEKONTO;3000,00\n" ])
let shared_csv = statement (fun m -> [ m ^ "-07;FRÅN ALICE;8000,00\n"; m ^ "-12;COOP;-2000,00\n" ])

let moves_setup ?assistant () =
  let service = Service.make ?assistant ~now:(fun () -> now) (Store_sqlite.open_memory ()) in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let savings = open_checking service alice ~statement_backed:true "Savings" in
  let shared = open_checking service alice ~owner:(Household household.id) ~statement_backed:true "Shared" in
  (service, alice, checking, savings, shared)

let import_into service alice account csv = ignore (imported (Import.confirm (Import.context service alice) (request account csv)))

let personal service alice name =
  (List.find (fun (i : Account_info.t) -> i.account.name = name && Owner.equal i.account.owner (Access.personal alice)) (Service.accounts service alice)).account.id

(* What a person files every row with [text] in the inbox as. *)
let file_as service alice text category =
  List.iter
    (fun (v : Inbox.item_view) ->
      if v.transfer.transfer.memo.description = text then
        ignore (ok (Inbox.categorize service alice ~transfer_id:v.item.transfer_id ~category_id:(personal service alice category) ())))
    (Inbox.list service alice ~limit:max_int)

let cycle_flows service alice ~cycles scope =
  match field [ "cycles" ] (ok (Query.execute service alice (Option.get (Query.find "cashflow_summary")) [ ("scope", `String scope); ("cycles", `Int cycles) ])) with
  | `List rows ->
      List.map
        (fun c ->
          let text name = match field [ name ] c with `String s -> s | other -> Yojson.Safe.to_string other in
          Printf.sprintf "%s income=%s spending=%s in=%s out=%s" (text "cycle") (text "income") (text "spending") (text "transfers_in") (text "transfers_out"))
        rows
  | _ -> Alcotest.fail "expected cycles"

let check_moves_net_out service alice =
  let each_month income spending in_ out = List.map (fun m -> Printf.sprintf "%s income=%s spending=%s in=%s out=%s" m income spending in_ out) months in
  Alcotest.(check (list string)) "me: the salary in, ICA spent, the contribution out" (each_month "30000.00" "450.00" "0.00" "8000.00") (cycle_flows service alice ~cycles:6 "me");
  Alcotest.(check (list string)) "household: COOP spent, alice's contribution in" (each_month "0.00" "2000.00" "8000.00" "0.00") (cycle_flows service alice ~cycles:6 "household");
  Alcotest.(check (list string)) "family: the salary in, ICA and COOP spent" (each_month "30000.00" "2450.00" "0.00" "0.00") (cycle_flows service alice ~cycles:6 "family")

let test_moves_net_out_in_any_order () =
  List.iter
    (fun order ->
      let service, alice, checking, savings, shared = moves_setup () in
      List.iter
        (function
          | `Checking -> import_into service alice checking checking_csv
          | `Savings -> import_into service alice savings savings_csv
          | `Shared -> import_into service alice shared shared_csv)
        order;
      check_moves_net_out service alice)
    [ [ `Checking; `Savings; `Shared ]; [ `Shared; `Savings; `Checking ]; [ `Savings; `Checking; `Shared ] ]

(* Checking came first, and alice filed its rows before the other statements were in: the moves out as spending.
   When the other halves arrive, the move is still one move. Undoing the other half's import files the row where she
   had put it again, rather than back in the inbox. *)
let test_moves_filed_before_the_other_half () =
  let service, alice, checking, savings, shared = moves_setup () in
  import_into service alice checking checking_csv;
  file_as service alice "LÖN" "Salary";
  file_as service alice "ICA" "Groceries";
  file_as service alice "TILL SPARKONTO" "Other";
  file_as service alice "TILL HUSHÅLL" "Housing";
  let savings_batch = imported (Import.confirm (Import.context service alice) (request savings savings_csv)) in
  import_into service alice shared shared_csv;
  check_moves_net_out service alice;
  Alcotest.(check int) "only COOP waits in the inbox" 6 (Inbox.count service alice);
  ignore (ok_import (Import.undo (Import.context service alice) ~batch_id:savings_batch.batch.batch_id));
  Alcotest.(check int) "the moves to savings are filed again, not back in the inbox" 6 (Inbox.count service alice);
  Alcotest.(check string) "as spending, as alice filed them" "2026-10 income=30000.00 spending=3450.00 in=0.00 out=8000.00"
    (List.hd (cycle_flows service alice ~cycles:1 "me"))

(* Both halves filed by hand, as when pairing didn't exist yet, or when another outflow of the same amount made the move
   ambiguous and alice filed the rows herself: finding the moves puts them right once the rival is skipped. *)
let test_moves_with_both_halves_filed () =
  let service, alice, checking, savings, _ = moves_setup () in
  import_into service alice checking "Datum;Text;Belopp\n2026-10-05;TILL SPARKONTO;-3000,00\n2026-10-05;SL;-3000,00\n";
  import_into service alice savings "Datum;Text;Belopp\n2026-10-05;FRÅN LÖNEKONTO;3000,00\n";
  file_as service alice "TILL SPARKONTO" "Other";
  file_as service alice "FRÅN LÖNEKONTO" "Other income";
  ignore (ok (Inbox.dismiss service alice ~transfer_id:(find_item service alice "SL")));
  Alcotest.(check string) "filed, the move is income and spending, besides SL" "2026-10 income=3000.00 spending=6000.00 in=0.00 out=0.00"
    (List.hd (cycle_flows service alice ~cycles:1 "me"));
  Alcotest.(check (triple int int int)) "paired by rule" (1, 0, 0) (Inbox.pair_all service alice);
  Alcotest.(check string) "paired, it is neither" "2026-10 income=0.00 spending=3000.00 in=0.00 out=0.00" (List.hd (cycle_flows service alice ~cycles:1 "me"))

(* The assistant files rows it is confident about on an account that opted in, the moves out included: a model that
   is only asked for a category calls a transfer to savings "Other". *)
let test_moves_filed_by_the_assistant () =
  let complete ?schema:_ ~system ~user () =
    if Str_compat.contains ~sub:"You categorise bank transactions" system then
      let json = Yojson.Safe.from_string user in
      let list path = match field path json with `List l -> l | _ -> [] in
      let id_of name = List.find_map (fun c -> if field [ "name" ] c = `String name then Some (field [ "id" ] c) else None) (list [ "categories" ]) in
      let suggestions =
        List.filter_map
          (fun r ->
            let spending = match field [ "amount" ] r with `String s -> String.starts_with ~prefix:"-" s | _ -> false in
            Option.map (fun id -> `Assoc [ ("row", field [ "row" ] r); ("category_id", id); ("confidence", `Int 80) ]) (id_of (if spending then "Other" else "Other income")))
          (list [ "rows" ])
      in
      Ok (Yojson.Safe.to_string (`Assoc [ ("suggestions", `List suggestions) ]))
    else Ok {|{"pairs":[]}|}
  in
  let service, alice, checking, savings, shared = moves_setup ~assistant:Llm.{ model = "fake"; complete } () in
  ok (Inbox.set_auto_apply service alice ~account_id:checking ~min_confidence:(Some 80));
  import_into service alice checking checking_csv;
  import_into service alice savings savings_csv;
  import_into service alice shared shared_csv;
  (* The salary, ICA and COOP the model filed or left as other income and other spending: only the moves are checked. *)
  let each_month f = List.map f months in
  Alcotest.(check (list string)) "me: only the salary in, only ICA spent, the contribution out"
    (each_month (fun m -> Printf.sprintf "%s income=30000.00 spending=450.00 in=0.00 out=8000.00" m)) (cycle_flows service alice ~cycles:6 "me")

(* History files a row when the account asks for it: once alice has called the move to savings "Other", the next
   ones are filed without asking, and must still pair with savings' side. *)
let test_moves_filed_from_history () =
  let service, alice, checking, savings, shared = moves_setup () in
  let first, rest = match String.split_on_char '\n' checking_csv with header :: rows -> (header, rows) | [] -> assert false in
  let may, later = List.partition (fun row -> String.starts_with ~prefix:"2026-05" row) rest in
  import_into service alice checking (String.concat "\n" (first :: may) ^ "\n");
  file_as service alice "TILL SPARKONTO" "Other";
  ok (Inbox.set_auto_apply service alice ~account_id:checking ~min_confidence:(Some 60));
  import_into service alice checking (String.concat "\n" (first :: later));
  import_into service alice savings savings_csv;
  import_into service alice shared shared_csv;
  check_moves_net_out service alice

(* Undoing one side's import and uploading it again pairs the move again. *)
let test_moves_after_undo_and_reupload_of_one_side () =
  let service, alice, checking, savings, shared = moves_setup () in
  import_into service alice checking checking_csv;
  import_into service alice savings savings_csv;
  let shared_batch = imported (Import.confirm (Import.context service alice) (request shared shared_csv)) in
  ignore (ok_import (Import.undo (Import.context service alice) ~batch_id:shared_batch.batch.batch_id));
  import_into service alice shared shared_csv;
  check_moves_net_out service alice

let () =
  Alcotest.run "import"
    [ ("parsing", [ Alcotest.test_case "amounts and csv" `Quick test_parsing ]);
      ( "pipeline",
        [ Alcotest.test_case "matches, dedupes and checks the balance" `Quick test_import_matches_dedupes_and_checks_the_balance;
          Alcotest.test_case "identical rows without balances" `Quick test_identical_rows_without_balances;
          Alcotest.test_case "closing balance, oldest first" `Quick test_closing_balance_oldest_first;
          Alcotest.test_case "handelsbanken xlsx" `Quick test_handelsbanken_xlsx;
          Alcotest.test_case "a new currency's uncategorized is stored in english" `Quick test_new_currency_uncategorized_is_stored_in_english;
          Alcotest.test_case "a new account from the file" `Quick test_new_account_from_the_file;
          Alcotest.test_case "a new card opens owing" `Quick test_new_card_opens_owing;
          Alcotest.test_case "an account learns its number" `Quick test_an_account_learns_its_number;
          Alcotest.test_case "a zipped statement" `Quick test_zipped_statement;
          Alcotest.test_case "categorise, learn and undo" `Quick test_categorize_learn_and_undo;
          Alcotest.test_case "query folds triage" `Quick test_query_folds_triage;
          Alcotest.test_case "fold triage counts visible rows" `Quick test_fold_triage_limit;

          Alcotest.test_case "logo scheduling" `Quick test_logo_scheduling;
          Alcotest.test_case "manual merchant query" `Quick test_manual_merchant_query;
          Alcotest.test_case "rules auto-apply" `Quick test_rules_auto_apply;
          Alcotest.test_case "auto-apply threshold" `Quick test_auto_apply_threshold;
          Alcotest.test_case "auto-apply respects owner and currency" `Quick test_auto_apply_respects_owner_and_currency ] );
      ( "inbox",
        [ Alcotest.test_case "reports after undo and reupload" `Quick test_reports_after_undo_and_reupload; Alcotest.test_case "list after undo and reupload" `Quick test_list_after_undo_and_reupload; Alcotest.test_case "pair and share" `Quick test_pair_and_share; Alcotest.test_case "undo after share" `Quick test_undo_after_share;
          Alcotest.test_case "pairing a contribution" `Quick test_pairing_a_contribution;
          Alcotest.test_case "llm pairs unsure moves" `Quick test_llm_pairs_unsure_moves;
          Alcotest.test_case "pair all moves retroactively" `Quick test_pair_all_moves_retroactively;
          Alcotest.test_case "undo doesn't revive a row" `Quick test_undo_does_not_revive_a_row;
          Alcotest.test_case "a manual move confirmed by both statements" `Quick test_manual_move_confirmed_by_both_statements;
          Alcotest.test_case "a member shares a purchase" `Quick test_member_shares_a_purchase;
          Alcotest.test_case "reconciliation follows the ledger" `Quick test_reconciliation_follows_the_ledger; Alcotest.test_case "reconcile" `Quick test_reconcile ] );
      ( "moves",
        [ Alcotest.test_case "net out in any order" `Quick test_moves_net_out_in_any_order;
          Alcotest.test_case "filed before the other half" `Quick test_moves_filed_before_the_other_half;
          Alcotest.test_case "both halves filed" `Quick test_moves_with_both_halves_filed;
          Alcotest.test_case "filed by the assistant" `Quick test_moves_filed_by_the_assistant;
          Alcotest.test_case "filed from history" `Quick test_moves_filed_from_history;
          Alcotest.test_case "undo and reupload of one side" `Quick test_moves_after_undo_and_reupload_of_one_side ] ) ]
