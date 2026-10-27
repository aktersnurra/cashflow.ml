module Db_guard = Cashflow_io.Db_guard
module Id_generator = Cashflow_io.Id_generator
module Import = Cashflow_io.Import
module Import_store = Cashflow_io.Import_store
module Inbox = Cashflow_io.Inbox
module Planning_store = Cashflow_io.Planning_store
module Reporting_live = Cashflow_io.Reporting_live
module Service = Cashflow_io.Service
module Sql = Cashflow_io.Sql
open Cashflow_core
open Cashflow_workflow
open Test_service_support
let get = function Ok x -> x | Error _ -> Alcotest.fail "unexpected reporting error"
let request access =
  let context = get (Reporting_context.make ~access ~scope:Analytics.Me ~currency:(get (Currency.make ~code:"SEK" ~scale:2)) ~now) in
  get (Cashflow_request.make ~context ~first:(date "2026-09-25") ~last:(date "2026-10-22"))
let load service access req = Reporting_live.run ~service ~access (fun () -> Effects.perform (Effect_ops.Load_cashflow req))
let with_wal_service f =
  let path = Filename.temp_file "reporting-wal-" ".sqlite3" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun suffix -> let file = path ^ suffix in if Sys.file_exists file then Sys.remove file) [""; "-wal"; "-shm"])
    (fun () ->
      let db = Sql.open_db path in
      Fun.protect ~finally:(fun () -> Sql.close db) (fun () ->
        Cashflow_io.Schema.migrate db;
        let service = Service.make ~now:(fun () -> now) db in
        let access = sign_in service "alice" in
        let writer = Sql.open_db path in
        Fun.protect ~finally:(fun () -> Sql.close writer) (fun () ->
          Sql.exec_script db "PRAGMA busy_timeout = 0";
          Sql.exec_script writer "PRAGMA busy_timeout = 0";
          f service access writer)))

let report_reads_during_writer () =
  with_wal_service (fun service access writer ->
    Sql.transaction writer (fun () ->
      Sql.exec_script writer "UPDATE accounts SET name = 'Uncommitted' WHERE name = 'Salary'";
      let lock = try Sql.transaction service.db (fun () -> ()); "no conflict" with Sql.Sql_error message -> message in
      Alcotest.(check string) "existing immediate writer conflict" "exec: BUSY (database is locked)" lock;
      let result = Cashflow_io.Query.execute service access (Cashflow_io.Query.Any Cashflow_io.Query.cashflow_summary) ["cycles", `Int 1] in
      Alcotest.(check (result string string)) "public report succeeds during WAL writer"
        (Ok "me")
        (Result.map (fun json -> Yojson.Safe.Util.(json |> member "scope" |> to_string))
          (Result.map_error Service.error_to_string result))));
  ()

let invalid_stored_currency () =
  with_wal_service (fun service access _ ->
    Sql.exec_script service.db "INSERT INTO currencies (code, scale) VALUES ('BAD!', 2)";
    let reason = "currency code \"BAD!\" must be three uppercase letters" in
    let invalid_reason = function Error (`Invalid message) -> message | _ -> Alcotest.fail "expected Invalid" in
    Alcotest.(check string) "legacy currency reason" reason (invalid_reason (Service.currency service "BAD!"));
    let result = Cashflow_io.Query.execute service access (Cashflow_io.Query.Any Cashflow_io.Query.cashflow_summary) ["currency", `String "BAD!"] in
    Alcotest.(check string) "query currency reason" reason (invalid_reason result))

let authority () =
  let service = make_service () in
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
  Alcotest.(check bool) "foreign principal" true (load service alice (request bob) = Error Reporting_error.Access_denied);
  let fake_household : Household.t = { id=Id_generator.household (); name="foreign"; default_currency="SEK"; report_split_bp=5000; pay_cycle=Pay_cycle_setting.default; created_by=(Access.user alice).id; created_at=now } in
  let forged = Access.make ~user:(Access.user alice) ~household:(Some fake_household) in
  let context = get (Reporting_context.make ~access:forged ~scope:Household ~currency:(get (Currency.make ~code:"SEK" ~scale:2)) ~now) in
  let req = get (Cashflow_request.make ~context ~first:(date "2026-09-25") ~last:(date "2026-10-22")) in
  Alcotest.(check bool) "foreign owner" true (load service alice req = Error Reporting_error.Access_denied)
let detached () =
  let service = make_service () in
  let access = sign_in service "alice" in
  let facts = get (load service access (request access)) in
  let count = List.length facts.accounts in
  let checking = open_checking service access "Later" in
  let salary = account_named service access "Salary" in
  ignore (ok (Service.add_entry service access (entry "later salary" ~from_account:salary ~to_account:checking 10000)));
  Alcotest.(check int) "detached accounts" count (List.length facts.accounts);
  Alcotest.(check int) "detached transfers" 0 (List.length facts.transfers);
  let later = get (load service access (request access)) in
  Alcotest.(check int) "new accounts" (count+1) (List.length later.accounts);
  Alcotest.(check int) "new transfers" 1 (List.length later.transfers)
let storage () =
  let service = make_service () in
  let access = sign_in service "alice" in
  Sql.exec_script service.db "ALTER TABLE currencies RENAME TO unavailable_currencies";
  let result = Reporting_live.run ~service ~access (fun () -> Reporting.cashflow_summary ~access ~scope:Me ~cycles:1 ()) in
  Alcotest.(check bool) "typed SQL failure" true (result = Error (Reporting_error.Storage_unavailable Resolve_currency))
let reconciliation () =
  let service = make_service () in
  let access = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service access ~name:"Home" ~default_currency:"SEK") in
  let access = Service.access_for service (Access.user access) in
  let bank = open_checking service access ~statement_backed:true "Bank" in
  ignore (open_checking service access ~owner:(Owner.Household household.id) ~statement_backed:true "Shared unreconciled");
  ignore (ok (Service.open_account service access ~key:(key "eur-bank") ~owner:(Access.personal access) ~kind:Asset ~name:"EUR unreconciled" ~currency:"EUR"
    ~flags:Account.no_flags ~details:{Account_info.no_details with statement_backed=true}));
  let checkpoint : Planning_store.checkpoint = { checkpoint_id="bank"; account_id=bank; on_date=date "2026-10-22"; statement_balance=0L; ledger_balance=0L; source="manual" } in
  Planning_store.add_checkpoint service.db ~now ~user_id:(Access.user access).id ~source:"manual" checkpoint;
  let req = request access in
  let facts = get (load service access req) in
  Alcotest.(check (option string)) "scope/currency reconciliation filter" (Some "2026-10-22") (Option.map Date.to_string facts.reconciled_through);
  Sql.exec_script service.db "DELETE FROM reconciliation_checkpoints";
  Alcotest.(check (option string)) "detached reconciliation" (Some "2026-10-22") (Option.map Date.to_string facts.reconciled_through);
  Alcotest.(check bool) "later reconciliation" true ((get (load service access req)).reconciled_through = None);
  Sql.exec_script service.db "ALTER TABLE reconciliation_checkpoints RENAME TO unavailable_checkpoints";
  Alcotest.(check bool) "typed snapshot storage failure" true (load service access req = Error (Reporting_error.Storage_unavailable Load_cashflow));
  Alcotest.(check bool) "failed snapshot rolled back" false (Sql.in_transaction service.db)

let guarded_snapshot () =
  Eio_main.run @@ fun _ ->
  let service = make_service () in
  let access = sign_in service "alice" in
  let req = request access in
  let guard = Db_guard.make () in
  let acquisitions = ref 0 in
  let entered, signal_entered = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  Sql.set_guard service.db { guard with exclusive = (fun f -> guard.exclusive (fun () ->
    incr acquisitions;
    if !acquisitions = 1 then (Eio.Promise.resolve signal_entered (); Eio.Promise.await release);
    (* Yield while the outer mark is held; nested statements must not reacquire. *)
    Eio.Fiber.yield (); f ())) };
  let snapshot = ref None in
  Eio.Fiber.both
    (fun () -> snapshot := Some (get (load service access req)))
    (fun () -> Eio.Promise.await entered;
      Eio.Promise.resolve signal_release ();
      Sql.(run service.db (stmt "UPDATE accounts SET name = 'Competing' WHERE name = 'Salary'" nil exec)));
  Alcotest.(check int) "report plus competing statement" 2 !acquisitions;
  let snapshot = Option.get !snapshot in
  Alcotest.(check bool) "no competing account in snapshot" false
    (List.exists (fun (i : Account_info.t) -> i.account.name = "Competing") snapshot.accounts);
  Alcotest.(check bool) "later snapshot sees mutation" true
    (List.exists (fun (i : Account_info.t) -> i.account.name = "Competing") (get (load service access req)).accounts)

let cancellation () =
  Eio_main.run @@ fun _ ->
  let service = make_service () in
  let access = sign_in service "alice" in
  let req = request access in
  let guard = Db_guard.make () in
  let owner_ready, owner_signal = Eio.Promise.create () in
  let release, release_signal = Eio.Promise.create () in
  let waiting, waiting_signal = Eio.Promise.create () in
  let cancelled = ref false in
  let acquisitions = ref 0 in
  Sql.set_guard service.db { guard with exclusive = (fun f ->
    incr acquisitions;
    if !acquisitions = 2 then Eio.Promise.resolve waiting_signal ();
    guard.exclusive f) };
  Eio.Fiber.both
    (fun () -> Sql.transaction service.db (fun () ->
      Eio.Promise.resolve owner_signal (); Eio.Promise.await release))
    (fun () ->
      Eio.Promise.await owner_ready;
      (try Eio.Switch.run (fun sw ->
        Eio.Fiber.fork ~sw (fun () ->
          try ignore (load service access req) with Eio.Cancel.Cancelled _ -> cancelled := true);
        Eio.Promise.await waiting;
        Eio.Switch.fail sw Exit)
       with Exit -> ());
      Eio.Promise.resolve release_signal ());
  Alcotest.(check bool) "cancelled while waiting" true !cancelled;
  ignore (get (load service access req));
  Alcotest.(check bool) "transaction unwound" false (Sql.in_transaction service.db)

let captured_visibility () =
  let service = make_service () in
  let alice = sign_in service "alice" and bob = sign_in service ~admin:true "bob" in
  let household = ok (Service.create_household service bob ~name:"Foreign" ~default_currency:"SEK") in
  let bob = Service.access_for service (Access.user bob) in
  let checking = open_checking service alice "Visible bank" in
  let salary = account_named service alice "Salary" in
  let visible = match ok (Service.add_entry service alice (entry "visible salary" ~from_account:salary ~to_account:checking 10000)) with
    | Ok (Created view) -> view.transfer.id
    | _ -> Alcotest.fail "expected visible transfer created" in
  let owner = Owner.Household household.id in
  let foreign_from = open_checking service bob ~owner "Foreign source" in
  let foreign_to = open_checking service bob ~owner "Foreign destination" in
  let foreign = match ok (Service.add_entry service bob (entry ~scope:owner "foreign transfer" ~from_account:foreign_from ~to_account:foreign_to 5000)) with
    | Ok (Created view) -> view.transfer.id
    | _ -> Alcotest.fail "expected foreign transfer created" in
  let account_ids accounts = List.sort String.compare (List.map (fun (i : Account_info.t) -> Account_id.to_string i.account.id) accounts) in
  let transfer_ids transfers = List.sort String.compare (List.map (fun (t : Transfer.t) -> Transfer_id.to_string t.id) transfers) in
  let legitimate = get (load service bob (request bob)) in
  List.iter (fun id -> Alcotest.(check bool) "foreign account visible to owner" true
    (List.mem (Account_id.to_string id) (account_ids legitimate.accounts))) [foreign_from; foreign_to];
  Alcotest.(check (list string)) "foreign transfer visible to owner" [Transfer_id.to_string foreign] (transfer_ids legitimate.transfers);
  let forged = Access.make ~user:(Access.user alice) ~household:(Some household) in
  let facts = get (load service alice (request forged)) in
  Alcotest.(check (list string)) "accepted Me request uses captured account visibility"
    (account_ids (Service.accounts service alice)) (account_ids facts.accounts);
  Alcotest.(check (list string)) "accepted Me request uses captured transfer visibility"
    [Transfer_id.to_string visible] (transfer_ids facts.transfers);
  List.iter (fun id -> Alcotest.(check bool) "foreign account excluded" false
    (List.mem (Account_id.to_string id) (account_ids facts.accounts))) [foreign_from; foreign_to];
  Alcotest.(check bool) "foreign transfer excluded" false
    (List.mem (Transfer_id.to_string foreign) (transfer_ids facts.transfers))

exception Loading_sentinel of int ref
let unexpected_loading_exception () =
  Eio_main.run @@ fun _ ->
  let service = make_service () in
  let access = sign_in service "alice" in
  let req = request access in
  let guard = Db_guard.make () in
  let original = Loading_sentinel (ref 0) in
  let acquisitions = ref 0 and cleanup = ref 0 in
  Sql.set_guard service.db { guard with exclusive = (fun f -> guard.exclusive (fun () ->
    incr acquisitions;
    if !acquisitions = 1 then raise original;
    f ())) };
  let caught = try
    Reporting_live.run ~service ~access (fun () ->
      Fun.protect ~finally:(fun () -> incr cleanup) (fun () ->
        ignore (Effects.perform (Effect_ops.Load_cashflow req))));
    false
    with exn -> exn == original in
  Alcotest.(check bool) "original loading exception identity" true caught;
  Alcotest.(check int) "suspended workflow cleanup exactly once" 1 !cleanup;
  Alcotest.(check int) "sentinel came from loading guard" 1 !acquisitions;
  Alcotest.(check bool) "failed load has no active transaction" false (Sql.in_transaction service.db);
  Alcotest.(check bool) "guard released after exception" false (guard.held ());
  let later = get (load service access req) in
  Alcotest.(check bool) "later load returns accounts" true (later.accounts <> []);
  Alcotest.(check int) "loading guard reused" 2 !acquisitions;
  Alcotest.(check int) "cleanup remains exactly once" 1 !cleanup;
  Alcotest.(check bool) "later load has no active transaction" false (Sql.in_transaction service.db);
  Alcotest.(check bool) "guard released after reuse" false (guard.held ())


(* Forecast loading: the same authority as the cash-flow report, the latest imported date per
   account, goals in the report's currency, and the household contribution plan. *)
let forecast_request ?(scope = Analytics.Me) access =
  let context = get (Reporting_context.make ~access ~scope ~currency:(get (Currency.make ~code:"SEK" ~scale:2)) ~now) in
  get (Forecast_request.make ~context ~horizon:1)
let load_forecast service access req = Reporting_live.run ~service ~access (fun () -> Effects.perform (Effect_ops.Load_forecast req))
let goal ?(currency = "SEK") owner name monthly : Planning_store.goal =
  { goal_id = Id_generator.household () |> Household_id.to_string; goal_owner = owner; name; currency; target_amount = 5_000_000L; target_date = None;
    monthly_amount = Some monthly; earmarks = [] }
let forecast_facts () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" and bob = sign_in service "bob" in
  Alcotest.(check bool) "foreign principal" true (load_forecast service alice (forecast_request bob) = Error Reporting_error.Access_denied);
  let checking = open_checking service alice "Checking" in
  let batch n last undone : Import_store.batch =
    { batch_id = "batch-" ^ string_of_int n; account_id = checking; file_sha256 = "sha-" ^ string_of_int n; file_name = None; format = "csv"; row_count = 1;
      new_count = 1; duplicate_count = 0; matched_count = 0; first_date = Some (date "2026-10-01"); last_date = Some (date last); created_at = now; undone }
  in
  Import_store.save_batch service.db ~user_id:(Access.user alice).id (batch 1 "2026-10-18" false);
  Import_store.save_batch service.db ~user_id:(Access.user alice).id (batch 2 "2026-10-20" false);
  Import_store.undo_batch service.db ~now "batch-2";
  Planning_store.save_goal service.db ~now (goal (Access.personal alice) "Car" 300_000L);
  Planning_store.save_goal service.db ~now (goal ~currency:"EUR" (Access.personal alice) "Euro trip" 10_000L);
  let facts = get (load_forecast service alice (forecast_request alice)) in
  Alcotest.(check (option string)) "an undone import doesn't count" (Some "2026-10-18")
    (Option.map Date.to_string (List.assoc_opt checking facts.imported_through));
  Alcotest.(check (list string)) "goals in SEK" [ "Car" ] (List.map (fun (g : Forecast.goal) -> g.goal_name) facts.goals);
  Alcotest.(check bool) "no household, no plan" true (facts.contribution = None)

let forecast_contribution () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let checking = open_checking service alice "Checking" in
  let salary = account_named service alice "Salary" in
  ignore (ok (Service.add_entry service alice { (entry "salary" ~from_account:salary ~to_account:checking 3_000_000) with value_date = date "2026-09-01" }));
  Planning_store.save_goal service.db ~now (goal (Owner.Household household.id) "Buffer" 1_000_000L);
  (match (get (load_forecast service alice (forecast_request alice))).contribution with
   | Some (Forecast.Outgoing { due_now; per_cycle }) ->
       Alcotest.(check int64) "the whole target, alone" 1_000_000L per_cycle;
       Alcotest.(check int64) "nothing paid yet" 1_000_000L due_now
   | _ -> Alcotest.fail "expected my planned transfer");
  (match (get (load_forecast service alice (forecast_request ~scope:Household alice))).contribution with
   | Some (Forecast.Incoming [ m ]) -> Alcotest.(check int64) "alice's transfer, to the household" 1_000_000L m.per_cycle
   | _ -> Alcotest.fail "expected each member's transfer");
  Alcotest.(check bool) "family: inside the scope" true ((get (load_forecast service alice (forecast_request ~scope:Family alice))).contribution = None)
(* A household transfer that came in as imported rows, paired in the inbox: the forecast sees it
   as the transfer it is, so the contribution plan replaces it instead of adding a second one. *)
let forecast_paired_imports () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let shared = open_checking service alice ~owner:(Household household.id) ~statement_backed:true "Shared" in
  let ctx = Import.context service alice in
  let import account rows =
    match Import.confirm ctx { target = Import.Existing account; file_name = Some "export.csv"; content = "Datum;Text;Belopp\n" ^ rows; decisions = [] } with
    | Ok (`Imported _) -> ()
    | _ -> Alcotest.fail "import"
  in
  import checking "2026-07-24;HUSHÅLL;-8000,00\n2026-08-25;HUSHÅLL;-8000,00\n2026-09-01;LÖN;30000,00\n2026-09-25;HUSHÅLL;-8000,00\n";
  import shared "2026-07-24;FRÅN ALICE;8000,00\n2026-08-25;FRÅN ALICE;8000,00\n2026-09-25;FRÅN ALICE;8000,00\n";
  let row text on =
    (List.find (fun (v : Inbox.item_view) -> v.transfer.transfer.memo.description = text && Date.to_string v.transfer.transfer.value_date = on)
       (Inbox.list service alice ~limit:50)).item.transfer_id
  in
  List.iter (fun on -> ignore (ok (Inbox.pair service alice ~out_id:(row "HUSHÅLL" on) ~in_id:(row "FRÅN ALICE" on)))) [ "2026-07-24"; "2026-08-25"; "2026-09-25" ];
  Planning_store.save_goal service.db ~now (goal (Owner.Household household.id) "Shared costs" 800_000L);
  let report =
    get (Reporting_live.run ~service ~access:alice (fun () ->
             Reporting.forecast ~access:alice ~scope:Me ~horizon:1 ~scenario:Forecast_request.no_scenario ()))
  in
  let out = List.filter (fun (i : Forecast.item) -> Int64.compare i.amount 0L < 0) report.forecast.items in
  Alcotest.(check (list string)) "once a month, as the planned transfer"
    [ "contribution 2026-10-25 -800000" ]
    (List.map (fun (i : Forecast.item) -> Printf.sprintf "%s %s %Ld" (Forecast.kind_to_string i.kind) (Date.to_string i.date) i.amount) out)

(* Categorised imported rows count as their category, so keeping to a budget replaces them. (A
   zero budget is no budget, so this one is 1 kr.) *)
let forecast_categorised_imports () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let ctx = Import.context service alice in
  (match Import.confirm ctx { target = Import.Existing checking; file_name = Some "export.csv"; decisions = [];
                              content = "Datum;Text;Belopp\n2026-07-03;ICA;-2000,00\n2026-07-29;ICA;-4000,00\n2026-09-14;ICA;-3000,00\n" } with
   | Ok (`Imported _) -> ()
   | _ -> Alcotest.fail "import");
  let groceries = account_named service alice "Groceries" in
  List.iter
    (fun (v : Inbox.item_view) -> ignore (ok (Inbox.categorize service alice ~transfer_id:v.item.transfer_id ~category_id:groceries ())))
    (Inbox.list service alice ~limit:50);
  Planning_store.set_budget service.db ~now (Access.personal alice) ~category_id:groceries ~effective_from:"2026-10" ~monthly_amount:100L ~rollover:false;
  let report =
    get (Reporting_live.run ~service ~access:alice (fun () ->
             Reporting.forecast ~access:alice ~scope:Me ~horizon:1 ~scenario:{ Forecast_request.no_scenario with follow_budgets = true } ()))
  in
  let last points = (List.nth points (List.length points - 1) : Forecast.point).cash in
  let first = (List.hd report.forecast.baseline).cash in
  Alcotest.(check bool) "groceries are usual spending" true (Int64.compare (last report.forecast.baseline) first < 0);
  let spent = Int64.sub first (last (Option.get report.forecast.scenario)) in
  Alcotest.(check bool) "a 1 kr grocery budget leaves about 1 kr a month" true (Int64.compare spent 0L > 0 && Int64.compare spent 200L <= 0)

let () = Alcotest.run "Reporting_live" ["live", List.map (fun (n,f) -> Alcotest.test_case n `Quick f) ["invalid_stored_currency",invalid_stored_currency;"report_reads_during_writer",report_reads_during_writer;"authority",authority;"detached",detached;"storage",storage;"reconciliation",reconciliation;"guarded snapshot",guarded_snapshot;"cancellation",cancellation;"captured visibility",captured_visibility;"unexpected loading exception",unexpected_loading_exception;"forecast facts",forecast_facts;"forecast contribution",forecast_contribution;"forecast paired imports",forecast_paired_imports;"forecast categorised imports",forecast_categorised_imports]]
