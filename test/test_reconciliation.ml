module Planning_store = Cashflow_io.Planning_store
module Reconciliation = Cashflow_io.Reconciliation
module Service = Cashflow_io.Service
module Sql = Cashflow_io.Sql
open Cashflow_core
open Test_service_support

let test_filter_and_minimum () =
  let service = make_service () in
  Fun.protect ~finally:(fun () -> Sql.close service.db) @@ fun () ->
  let access = sign_in service "alice" in
  let household = ok (Service.create_household service access ~name:"Home" ~default_currency:"SEK") in
  let access = Service.access_for service (Access.user access) in
  let early = open_checking service access ~statement_backed:true "Early" in
  let late = open_checking service access ~statement_backed:true "Late" in
  ignore (open_checking service access "Not statement-backed");
  ignore (open_checking service access ~owner:(Owner.Household household.id) ~statement_backed:true "Shared");
  ignore (ok (Service.open_account service access ~key:(key "eur") ~owner:(Access.personal access)
    ~kind:Asset ~name:"EUR" ~currency:"EUR" ~flags:Account.no_flags
    ~details:{Account_info.no_details with statement_backed=true}));
  let checkpoint id account_id on_date statement_balance =
    let c : Planning_store.checkpoint = { checkpoint_id=id; account_id; on_date=date on_date;
      statement_balance; ledger_balance=0L; source="manual" } in
    Planning_store.add_checkpoint service.db ~now ~user_id:(Access.user access).id ~source:"manual" c in
  checkpoint "early" early "2026-10-10" 0L;
  checkpoint "late" late "2026-10-20" 0L;
  let currency = ok (Service.currency service "SEK") in
  let through accounts =
    let book = Analytics.make_book ~accounts ~owners:[Access.personal access] ~currency in
    Reconciliation.reconciled_through service.db ~accounts ~book in
  let check label expected accounts = Alcotest.(check (option string)) label expected (Option.map Date.to_string (through accounts)) in
  let accounts = Service.accounts service access in
  check "no statement accounts" None [];
  check "filters owner/currency/non-statement and takes minimum" (Some "2026-10-10") accounts;
  check "minimum independent of order" (Some "2026-10-10") (List.rev accounts);
  let missing = open_checking service access ~statement_backed:true "Missing checkpoint" in
  let missing_info = ok (Service.find_account service access missing) in
  check "missing checkpoint first" None (missing_info :: accounts);
  check "missing checkpoint last" None (accounts @ [missing_info]);
  checkpoint "missing now matched" missing "2026-10-15" 0L;
  check "all matching checkpoints" (Some "2026-10-10") (missing_info :: accounts);
  checkpoint "later mismatch" missing "2026-10-21" 1L;
  check "latest mismatched checkpoint blocks reconciliation" None (accounts @ [missing_info])

let () = Alcotest.run "Reconciliation" ["shared rule", [Alcotest.test_case "filter and minimum" `Quick test_filter_and_minimum]]
