module Db_guard = Cashflow_io.Db_guard
module Id_generator = Cashflow_io.Id_generator
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
let load service access req = Reporting_live.run ~db:service.Service.db ~access ~now:service.now (fun () -> Effects.perform (Effect_ops.Load_cashflow req))
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
  let result = Reporting_live.run ~db:service.db ~access ~now:service.now (fun () -> Reporting.cashflow_summary ~access ~scope:Me ~cycles:1 ()) in
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

let () = Alcotest.run "Reporting_live" ["live", List.map (fun (n,f) -> Alcotest.test_case n `Quick f) ["authority",authority;"detached",detached;"storage",storage;"reconciliation",reconciliation;"guarded snapshot",guarded_snapshot;"cancellation",cancellation]]
