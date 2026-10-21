open Cashflow_core
open Cashflow_workflow
let ok = function Ok x -> x | Error _ -> failwith "fixture"
let date s = ok (Date.of_string s)
let now = ok (Time.parse_offset_datetime "2026-10-21T23:30:00Z")
let uid = Option.get (User_id.of_string "00000000-0000-4000-8000-000000000001")
let hid = Option.get (Household_id.of_string "00000000-0000-4000-8000-000000000002")
let user : User.t = { id=uid; oidc_issuer="test"; oidc_subject="alice"; display_name="Alice"; email=None; locale="sv"; timezone="Europe/Stockholm"; default_currency="SEK"; pay_cycle=Pay_cycle_setting.default; is_admin=false; created_at=now }
let household : Household.t = { id=hid; name="Home"; default_currency="EUR"; report_split_bp=5000; pay_cycle=Pay_cycle_setting.default; created_by=uid; created_at=now }
let access = Access.make ~user ~household:(Some household)
let sek = ok (Currency.make ~code:"SEK" ~scale:2)
let eur = ok (Currency.make ~code:"EUR" ~scale:2)
let id n = Printf.sprintf "00000000-0000-4000-8000-%012d" n
let account n owner kind currency : Account_info.t = { account = { id=Option.get (Account_id.of_string (id n)); key=ok (Idempotency_key.of_string (id n)); owner; kind; currency; name=string_of_int n; flags=Account.no_flags; closed=false; balances=Account.zero_balances; created_at=now }; details=Account_info.no_details }
let checking = account 10 (Owner.User uid) Asset sek
let salary = account 11 (Owner.User uid) Income sek
let groceries = account 12 (Owner.User uid) Expense sek
let shared = account 13 (Owner.Household hid) Asset sek
let housing = account 14 (Owner.Household hid) Expense sek
let foreign_bank = account 15 (Owner.User uid) Asset eur
let foreign_expense = account 16 (Owner.User uid) Expense eur
let transfer n (debit : Account_info.t) (credit : Account_info.t) amount code : Transfer.t =
 { id=Option.get (Transfer_id.of_string (id n)); scope=debit.account.owner; key=ok (Idempotency_key.of_string (id n)); debit_account_id=debit.account.id; credit_account_id=credit.account.id; amount=Option.get (Amount.of_int64 amount); kind=Single; code; memo=Transfer.empty_memo; value_date=date "2026-10-01"; reverses=None; recorded_at=now }
let facts : Cashflow_facts.t = { accounts=[checking;salary;groceries;shared;housing;foreign_bank;foreign_expense]; transfers=[transfer 20 checking salary 3000000L Income; transfer 21 groceries checking 411900L Purchase; transfer 22 shared checking 1000000L Contribution; transfer 23 housing shared 900000L Purchase; transfer 24 foreign_expense foreign_bank 999999L Purchase]; reconciled_through=None }
let run ?(access=access) ?(scope=Analytics.Me) ?currency ?(cycles=1) ?(facts=facts) ?(clock=now) () =
 let events = ref [] in
 let handler = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a -> match op with
 | Resolve_currency code -> events := !events @ [code]; if code="SEK" then Ok sek else if code="EUR" then Ok eur else Error (Reporting_error.Unknown_currency code)
 | Now -> events := !events @ ["now"]; clock
 | Load_cashflow request -> events := !events @ ["load"];
   Alcotest.(check string) "range first" (if cycles > 1 then "2026-08-25" else "2026-09-25") (Date.to_string (Cashflow_request.first request));
   Alcotest.(check string) "range last" "2026-10-22" (Date.to_string (Cashflow_request.last request));
   Alcotest.(check bool) "request access" true (Reporting_context.access (Cashflow_request.context request) == access);
   Ok facts) } in
 let result = Effects.run handler (fun () -> Reporting.cashflow_summary ~access ~scope ?currency ~cycles ()) in
 (result, !events)
let current report = List.hd (ok (fst report)).Reporting.cycles
let money label expected actual = Alcotest.(check int64) label expected actual
let scopes () =
 let me = current (run ()) in
 money "income" 3000000L me.flows.income; money "spending incl currency exclusion" 411900L me.flows.spending;
 money "outgoing" 1000000L me.flows.transfers_out; money "savings" 1588100L me.savings;
 Alcotest.(check (option int)) "rate" (Some 5293) me.savings_rate_bp;
 let h = current (run ~scope:Household ~currency:"SEK" ()) in
 money "household incoming" 1000000L h.flows.transfers_in; money "household spending" 900000L h.flows.spending;
 let f = current (run ~scope:Family ~currency:"SEK" ()) in
 money "family spending" 1311900L f.flows.spending; money "family outgoing" 0L f.flows.transfers_out
let context_and_order () =
 let report, events = run ~cycles:2 () in
 Alcotest.(check (list string)) "effect order" ["SEK";"now";"load"] events;
 let report = ok report in
 Alcotest.(check string) "local today" "2026-10-22" (Date.to_string (Reporting_context.today report.context));
 Alcotest.(check (list string)) "oldest first" ["2026-09";"2026-10"] (List.map (fun (s:Analytics.cycle_summary) -> Cycle.label s.cycle) report.cycles);
 List.iter (fun cycles -> Alcotest.(check int) "clamped" 1 (List.length (ok (fst (run ~cycles ()))).cycles)) [0; -3];
 let _, events = run ~scope:Household () in
 Alcotest.(check (list string)) "household default" ["EUR";"now";"load"] events;
 let _, events = run ~scope:Family () in
 Alcotest.(check (list string)) "family default" ["EUR";"now";"load"] events;
 let alone = Access.make ~user ~household:None in
 List.iter (fun scope -> let r,e = run ~access:alone ~scope ~currency:"BAD" () in
   Alcotest.(check bool) "household before currency" true (r=Error Reporting_error.Household_required);
   Alcotest.(check (list string)) "no effects" [] e) [Household;Family];
 let r,e = run ~currency:"BAD" () in
 Alcotest.(check bool) "unknown currency" true (r=Error (Reporting_error.Unknown_currency "BAD"));
 Alcotest.(check (list string)) "currency before clock" ["BAD"] e;
 let ctx = ok (Reporting_context.make ~access ~scope:Me ~currency:sek ~now) in
 let first=date "2026-10-02" and last=date "2026-10-01" in
 Alcotest.(check bool) "invalid range" true (Cashflow_request.make ~context:ctx ~first ~last = Error (Reporting_error.Invalid_range {first;last}));
 let fallback = Access.make ~user:{user with timezone="invalid"} ~household:None in
 let ctx = ok (Reporting_context.make ~access:fallback ~scope:Me ~currency:sek ~now) in
 Alcotest.(check string) "timezone fallback" "2026-10-21" (Date.to_string (Reporting_context.today ctx))
let edge_cases () =
 let empty = current (run ~facts:{facts with transfers=[]} ()) in
 Alcotest.(check (option int)) "zero income rate" None empty.savings_rate_bp;
 let reversal = transfer 25 checking groceries 11900L Reversal in
 let s = current (run ~facts:{facts with transfers=reversal::facts.transfers; reconciled_through=Some (date "2026-10-22")} ()) in
 money "reversal spending" 400000L s.flows.spending;
 Alcotest.(check bool) "reconciled inclusive" true s.reconciled;
 let s = current (run ~facts:{facts with reconciled_through=Some (date "2026-10-21")} ()) in
 Alcotest.(check bool) "not fully reconciled" false s.reconciled
let storage_errors () =
 List.iter (fun error ->
   let operations = ref [] in
   let handler = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a -> match op with
     | Resolve_currency _ -> operations := "currency" :: !operations; Ok sek
     | Now -> operations := "now" :: !operations; now
     | Load_cashflow _ -> operations := "load" :: !operations; Error error) } in
   let result = Effects.run handler (fun () -> Reporting.cashflow_summary ~access ~scope:Me ~cycles:1 ()) in
   Alcotest.(check bool) "typed load failure" true (result = Error error);
   Alcotest.(check (list string)) "one load" ["load";"now";"currency"] !operations)
   [Reporting_error.Access_denied; Reporting_error.Storage_unavailable Load_cashflow];
 let handler = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a -> match op with
   | Resolve_currency _ -> Error (Reporting_error.Storage_unavailable Resolve_currency)
   | Now | Load_cashflow _ -> Alcotest.fail "must stop at currency") } in
 let result = Effects.run handler (fun () -> Reporting.cashflow_summary ~access ~scope:Me ~cycles:1 ()) in
 Alcotest.(check bool) "typed currency storage failure" true (result = Error (Reporting_error.Storage_unavailable Resolve_currency))
let () = Alcotest.run "Reporting" ["workflow", List.map (fun (n,f) -> Alcotest.test_case n `Quick f) ["literal scopes",scopes;"context and ordering",context_and_order;"edge cases",edge_cases;"storage errors",storage_errors]]
