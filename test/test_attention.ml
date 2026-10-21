open Test_service_support

let kinds service access = List.map (fun (Attention.Card c) -> Attention.kind_name c.kind) (Attention.feed service access)
let key_of service access kind = List.find_map (fun (Attention.Card c) -> if Attention.kind_name c.kind = kind then Some c.key else None) (Attention.feed service access)

let test_feed_follows_the_data () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  Alcotest.(check (list string)) "a new user sees the overview" [ "overview" ] (kinds service alice);
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  Alcotest.(check (list string)) "a statement account asks for an import" [ "import_due"; "overview" ] (kinds service alice);
  let csv = "Datum;Text;Belopp;Saldo\n2026-10-20;ICA;-250,00;-250,00\n" in
  ignore (ok_import (Import.confirm (Import.context service alice) { account_id = checking; file_name = None; content = csv; decisions = [] }));
  Alcotest.(check (list string)) "then the inbox" [ "inbox"; "overview" ] (kinds service alice);
  let item = List.hd (Inbox.list service alice ~limit:10) in
  ignore (ok (Inbox.categorize service alice ~transfer_id:item.item.transfer_id ~category_id:(account_named service alice "Groceries") ()));
  Alcotest.(check (list string)) "and back to the overview" [ "overview" ] (kinds service alice)

let test_snooze_and_budget_acknowledgement () =
  (* Four days into the cycle that began on 25 September, so spending can run ahead of pace. *)
  let service = Service.make ~now:(fun () -> Option.get (Ptime.of_date_time ((2026, 9, 28), ((10, 0, 0), 0)))) (Store_sqlite.open_memory ()) in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Checking" in
  let groceries = account_named service alice "Groceries" in
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  ignore (ok (Insights.set_budget ctx ~category_id:groceries ~monthly_amount:1_000_00L ~rollover:false ~effective_from:None));
  let spend key value = ignore (ok (Service.add_entry service alice { (entry key ~from_account:checking ~to_account:groceries value) with value_date = date "2026-09-27" })) in
  spend "a" 300_00;
  Alcotest.(check bool) "ahead of pace" true (List.mem "budget_alert" (kinds service alice));
  let key = Option.get (key_of service alice "budget_alert") in
  ignore (ok (Attention.act service alice ~key ~action:(`Resolve "ahead_of_pace") ~until:None));
  Alcotest.(check bool) "acknowledged" false (List.mem "budget_alert" (kinds service alice));
  spend "b" 800_00;
  Alcotest.(check bool) "but back once it is over budget" true (List.mem "budget_alert" (kinds service alice));
  ignore (ok (Attention.act service alice ~key ~action:`Snooze ~until:(Some (date "2026-10-01"))));
  Alcotest.(check bool) "snoozed" false (List.mem "budget_alert" (kinds service alice))

let test_snooze_hides_until_the_date () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  ignore (open_checking service alice ~statement_backed:true "Checking");
  let key = Option.get (key_of service alice "import_due") in
  ignore (ok (Attention.act service alice ~key ~action:`Snooze ~until:(Some (date "2026-10-23"))));
  Alcotest.(check (list string)) "snoozed" [ "overview" ] (kinds service alice);
  let later = Service.make ~now:(fun () -> Option.get (Ptime.of_date_time ((2026, 10, 23), ((10, 0, 0), 0)))) service.db in
  Alcotest.(check bool) "back on the date (with a new key for the day)" true (List.mem "import_due" (kinds later alice));
  Alcotest.(check bool) "a snoozed card can't be resolved" true (Result.is_error (Attention.act service alice ~key ~action:(`Resolve "done") ~until:None))

let () =
  Alcotest.run "attention"
    [ ( "feed",
        [ Alcotest.test_case "follows the data" `Quick test_feed_follows_the_data;
          Alcotest.test_case "snooze and budget acknowledgement" `Quick test_snooze_and_budget_acknowledgement;
          Alcotest.test_case "snooze hides until the date" `Quick test_snooze_hides_until_the_date ] ) ]
