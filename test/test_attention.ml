module Attention = Cashflow_io.Attention
module Import = Cashflow_io.Import
module Inbox = Cashflow_io.Inbox
module Insights = Cashflow_io.Insights
module Llm = Cashflow_io.Llm
module Service = Cashflow_io.Service
module Store_sqlite = Cashflow_io.Store_sqlite
module Str_compat = Cashflow_core.Str_compat
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module User = Cashflow_core.User

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
  ignore (ok_import (Import.confirm (Import.context service alice) { target = Import.Existing checking; file_name = None; content = csv; decisions = [] }));
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

let card service access kind = List.find_map (fun (Attention.Card c) -> if Attention.kind_name c.kind = kind then Some (Attention.card_json (Card c)) else None) (Attention.feed service access)
let field path json = List.fold_left (fun json name -> Yojson.Safe.Util.member name json) json path

(* Groceries usually cost 1 000 a cycle; this cycle (from 25 September) it is 2 500 by the 21st. *)
let test_unusual_spending () =
  let prompts = ref [] in
  let assistant : Llm.t = { model = "fake"; complete = (fun ?schema:_ ~system:_ ~user () -> prompts := user :: !prompts; Ok "Mostly the 1 500 at ICA MAXI.") } in
  let service = Service.make ~now:(fun () -> now) ~assistant (Store_sqlite.open_memory ()) in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Checking" in
  let salary = account_named service alice "Salary" and groceries = account_named service alice "Groceries" in
  let post key ~from_account ~to_account value on = ignore (ok (Service.add_entry service alice { (entry key ~from_account ~to_account value) with value_date = date on })) in
  post "salary" ~from_account:salary ~to_account:checking 50_000_00 "2026-06-25";
  List.iter (fun (key, on) -> post key ~from_account:checking ~to_account:groceries 1_000_00 on) [ ("ICA jul", "2026-07-01"); ("ICA aug", "2026-08-01"); ("ICA sep", "2026-09-01") ];
  post "ICA oct" ~from_account:checking ~to_account:groceries 1_000_00 "2026-10-01";
  Alcotest.(check bool) "as usual so far" false (List.mem "unusual_spend" (kinds service alice));
  post "ICA MAXI" ~from_account:checking ~to_account:groceries 1_500_00 "2026-10-10";
  let command = List.find (fun (c : Cashflow_io.Commands.command) -> c.name = "get_attention_feed") Attention.commands in
  ignore (ok_import (command.run service alice [ ("narrative", `Bool false) ]));
  Alcotest.(check int) "deterministic command starts no narrative" 0 (List.length !prompts);
  ignore (ok_import (command.run service alice []));
  Alcotest.(check int) "omitted narrative preserves legacy generation" 1 (List.length !prompts);
  let json = Option.get (card service alice "unusual_spend") in
  Alcotest.(check string) "spent" "\"2500.00\"" (Yojson.Safe.to_string (field [ "data"; "spent" ] json));
  Alcotest.(check string) "usual" "\"1000.00\"" (Yojson.Safe.to_string (field [ "data"; "usual" ] json));
  Alcotest.(check string) "explained" "\"Mostly the 1 500 at ICA MAXI.\"" (Yojson.Safe.to_string (field [ "data"; "text" ] json));
  Alcotest.(check bool) "from this cycle's transactions" true (Str_compat.contains ~sub:"ICA MAXI" (List.hd !prompts));
  Alcotest.(check bool) "but no account names" false (Str_compat.contains ~sub:"Checking" (List.hd !prompts));
  let command = List.find (fun (c : Cashflow_io.Commands.command) -> c.name = "get_attention_feed") Attention.commands in
  ignore (ok_import (command.run service alice [ ("narrative", `Bool false) ]));
  Alcotest.(check bool) "typed narrative flag" true (Result.is_error (command.run service alice [("narrative", `String "false")]));
  ignore (Attention.feed service alice);
  Alcotest.(check int) "written once" 1 (List.length !prompts);
  (* In Swedish it is written again, naming the category as a Swedish reader sees it. *)
  let user = Access.user alice in
  let alice =
    ok (Service.update_settings service alice { User.locale = "sv"; timezone = user.timezone; default_currency = user.default_currency; pay_cycle = user.pay_cycle })
  in
  ignore (Attention.feed service alice);
  Alcotest.(check int) "written again in Swedish" 2 (List.length !prompts);
  Alcotest.(check bool) "naming the category in Swedish" true (Str_compat.contains ~sub:"Mat & hushåll" (List.hd !prompts));
  (* A budget takes over. *)
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  ignore (ok (Insights.set_budget ctx ~category_id:groceries ~monthly_amount:5_000_00L ~rollover:false ~effective_from:None));
  Alcotest.(check bool) "budgeted categories have their own alerts" false (List.mem "unusual_spend" (kinds service alice))

(* Rent of 8 000 has gone out on the 20th for three months; the next one is due before payday on the
   25th, and only 6 000 is left. *)
let test_shortfall_before_payday () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Checking" in
  let salary = account_named service alice "Salary" and rent = account_named service alice "Subscriptions" in
  let post key ~from_account ~to_account value on = ignore (ok (Service.add_entry service alice { (entry key ~from_account ~to_account value) with value_date = date on })) in
  post "salary" ~from_account:salary ~to_account:checking 30_000_00 "2026-07-01";
  List.iter (fun (key, on) -> post key ~from_account:checking ~to_account:rent 8_000_00 on) [ ("RENT 7", "2026-07-20"); ("RENT 8", "2026-08-20"); ("RENT 9", "2026-09-20") ];
  let json = Option.get (card service alice "shortfall") in
  Alcotest.(check string) "short by" "\"2000.00\"" (Yojson.Safe.to_string (field [ "data"; "short_by" ] json));
  Alcotest.(check string) "the rent" "\"RENT 9\"" (Yojson.Safe.to_string (field [ "data"; "upcoming" ] json |> Yojson.Safe.Util.index 0 |> field [ "merchant" ]));
  post "transfer" ~from_account:salary ~to_account:checking 3_000_00 "2026-10-21";
  Alcotest.(check bool) "covered" false (List.mem "shortfall" (kinds service alice))

(* Currency-filtered reports must not hide off-track goals in the owner's other currencies. *)
let test_goal_cards_in_each_currency () =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" in
  ignore (ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK"));
  let alice = Service.access_for service (Access.user alice) in
  let stranger = sign_in service "stranger" in
  let save access scope currency name monthly_amount =
    let ctx = ok (Insights.context service access ~scope ~currency ()) in
    ignore (ok (Insights.save_goal ctx ~name ~target_amount:1_000_00L
                  ~target_date:(Some (date "2027-01-01")) ~monthly_amount:(Some monthly_amount) ()))
  in
  List.iter
    (fun (scope, prefix) ->
      save alice scope "SEK" (prefix ^ " SEK") 1L;
      save alice scope "USD" (prefix ^ " USD") 1L;
      save alice scope "USD" (prefix ^ " on track") 1_000_00L)
    [ (Analytics.Me, "Personal"); (Analytics.Household, "Household") ];
  save stranger Me "USD" "Private" 1L;
  let goals =
    Attention.feed service alice
    |> List.filter_map (fun (Attention.Card c) ->
           match c.kind, c.payload with
           | Attention.Goal_off_track, (g, currency) -> Some (g.goal.name, Currency.code currency)
           | _ -> None)
    |> List.sort compare
  in
  Alcotest.(check (list (pair string string))) "visible off-track goals keep their currency"
    [ ("Household SEK", "SEK"); ("Household USD", "USD"); ("Personal SEK", "SEK"); ("Personal USD", "USD") ] goals

(* A feed reads the ledger once and every calculation filters that in memory: it must give exactly what the
   database would for any range, inside the loaded span, on its edges, or partly outside it. *)
let test_shared_load_matches_the_database () =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice "Checking" in
  let groceries = account_named service alice "Groceries" in
  List.iteri
    (fun n on -> ignore (ok (Service.add_entry service alice { (entry ("e" ^ string_of_int n) ~from_account:checking ~to_account:groceries (100 + n)) with value_date = date on })))
    [ "2026-01-05"; "2026-03-01"; "2026-03-01"; "2026-06-15"; "2026-09-30"; "2026-10-01"; "2026-10-20" ];
  let shared = Insights.new_shared () in
  let ctx = ok (Insights.context service alice ~scope:Me ~shared ()) in
  Insights.preload_range shared service alice ~first:(date "2026-02-01") ~last:(date "2026-10-01");
  let ids range = List.map (fun (t : Cashflow_core.Transfer.t) -> Cashflow_core.Transfer_id.to_string t.id) range in
  List.iter
    (fun (first, last) ->
      let direct = Cashflow_io.Transfer_query.between service.Service.db alice ~first:(date first) ~last:(date last) in
      Alcotest.(check (list string)) (first ^ " to " ^ last) (ids direct) (ids (Insights.transfers ctx ~first:(date first) ~last:(date last))))
    [ ("2026-02-01", "2026-10-01"); ("2026-03-01", "2026-03-01"); ("2026-03-02", "2026-09-29"); ("2026-09-30", "2026-10-01");
      ("2026-01-01", "2026-12-31"); ("2026-10-02", "2026-10-31"); ("2026-04-01", "2026-04-30") ]

let () =
  Alcotest.run "attention"
    [ ( "feed",
        [ Alcotest.test_case "follows the data" `Quick test_feed_follows_the_data;
          Alcotest.test_case "snooze and budget acknowledgement" `Quick test_snooze_and_budget_acknowledgement;
          Alcotest.test_case "snooze hides until the date" `Quick test_snooze_hides_until_the_date;
          Alcotest.test_case "unusual spending" `Quick test_unusual_spending;
          Alcotest.test_case "short before payday" `Quick test_shortfall_before_payday;
          Alcotest.test_case "goal cards in each currency" `Quick test_goal_cards_in_each_currency;
          Alcotest.test_case "shared load matches the database" `Quick test_shared_load_matches_the_database ] ) ]
