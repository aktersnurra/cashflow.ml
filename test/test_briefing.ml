module Briefing = Cashflow_io.Briefing
module Commands = Cashflow_io.Commands
module Import = Cashflow_io.Import
module Inbox = Cashflow_io.Inbox
module Insights = Cashflow_io.Insights
module Analytics = Cashflow_core.Analytics
module Cycle = Cashflow_core.Cycle
module Date = Cashflow_core.Date
module Q = Cashflow_core.Questions

open Test_service_support

(* Nine cycles of a life, imported and filed from the inbox the way statements arrive: salary, rent and a
   streaming subscription; groceries that vary; Wolt six times a cycle (small, and it adds up); clothes that
   tripled four cycles ago and stayed there; interest on a card; one big trip to Bauhaus. *)
let scenario ?assistant () =
  let service = match assistant with None -> make_service () | Some llm -> Service.make ~now:(fun () -> now) ~assistant:llm (Cashflow_io.Store_sqlite.open_memory ()) in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let ctx = Result.get_ok (Insights.context service alice ~scope:Analytics.Me ()) in
  let current = Cycle.cycle_of ctx.setting ctx.today in
  let rec back c n = if n = 0 then c else back (Cycle.previous ctx.setting c) (n - 1) in
  let row d text kr = Printf.sprintf "%s;%s;%d,00" (Date.to_string d) text kr in
  let day (c : Cycle.t) k = Date.add_days c.first k in
  let cycle k =
    let c = back current k in
    let clothes = if k <= 4 then [ (4, 700); (19, 800) ] else [ (4, 250); (19, 250) ] in
    [ row c.first "LON" 30_000; row (day c 2) "HYRA" (-9_000); row (day c 10) "SPOTIFY" (if k <= 2 then -169 else -129); row (day c 26) "KORTRANTA" (-900) ]
    @ List.map (fun d -> row (day c d) "ICA" (-(700 + (((k * 37) + (d * 11)) mod 400)))) [ 3; 10; 17; 24 ]
    @ List.map (fun d -> row (day c d) "WOLT" (-149)) [ 1; 5; 9; 13; 17; 21 ]
    @ List.map (fun (d, kr) -> row (day c d) "HM" (-kr)) clothes
  in
  (* This cycle's salary: the statements reach past the end of the last one. *)
  let rows = List.concat_map cycle [ 9; 8; 7; 6; 5; 4; 3; 2; 1 ] @ [ row (day (back current 1) 12) "BAUHAUS" (-6_000); row current.first "LON" 30_000 ] in
  let csv = String.concat "\n" ("Datum;Text;Belopp" :: rows) ^ "\n" in
  ignore (ok_import (Import.confirm (Import.context service alice) { target = Import.Existing checking; file_name = None; content = csv; decisions = [] }));
  let category = function
    | "LON" -> "Salary" | "HYRA" -> "Housing" | "SPOTIFY" -> "Subscriptions" | "KORTRANTA" -> "Fees & interest"
    | "ICA" -> "Groceries" | "WOLT" -> "Restaurants & cafés" | "HM" -> "Clothes" | _ -> "Shopping"
  in
  let rec file () =
    match Inbox.list service alice ~limit:1 with
    | [] -> ()
    | v :: _ ->
        ignore (ok (Inbox.categorize service alice ~transfer_id:v.item.transfer_id ~category_id:(account_named service alice (category v.transfer.transfer.memo.description)) ()));
        file ()
  in
  file ();
  (service, alice)

let briefing service alice =
  let ctx = Result.get_ok (Insights.context service alice ~scope:Analytics.Me ()) in
  fst (Briefing.questions service alice ctx)

let show qs =
  if Sys.getenv_opt "SHOW_BRIEFING" <> None then
    List.iter
      (fun (q : Q.question) ->
        Printf.eprintf "%-14s %.2f  m%.2f s%.2f r%.2f a%.2f  stake %Ld  %s\n" (Q.family_name q.family) (Q.score q) q.material q.surprising q.reliable q.actionable
          (Int64.div q.stake 100L) q.key)
      qs

let find family qs = List.find_opt (fun (q : Q.question) -> q.family = family) qs

(* The accountant's answers, before being asked: the small purchases that add up, the category that drifted
   up and stayed, and where the money goes; nothing that isn't worth the attention. *)
let test_the_questions_worth_answering () =
  let service, alice = scenario () in
  let qs = briefing service alice in
  show qs;
  Alcotest.(check bool) "at most five" true (List.length qs <= Briefing.limit);
  Alcotest.(check bool) "best first, all above the floor" true
    (List.for_all (fun q -> Q.score q >= Briefing.floor) qs
    && List.for_all2 (fun a b -> Q.score a >= Q.score b) (List.filteri (fun i _ -> i < List.length qs - 1) qs) (List.tl qs));
  (match find Leak qs with
   | Some { answer = Leak { name; count; _ }; _ } ->
       Alcotest.(check string) "Wolt adds up" "WOLT" name;
       Alcotest.(check bool) (Printf.sprintf "many times (%d)" count) true (count >= 30)
   | _ -> Alcotest.fail "no leak");
  (match find Drift qs with
   | Some { answer = Drift { category; usual; recent; cycles }; _ } ->
       Alcotest.(check string) "clothes drifted" "Clothes" category.account.name;
       Alcotest.(check (pair int64 int64)) "from 500 to 1,500 a cycle" (500_00L, 1500_00L) (usual, recent);
       Alcotest.(check bool) "and stayed" true (cycles >= 3)
   | _ -> Alcotest.fail "no drift");
  (match find Where_it_goes qs with
   | Some { answer = Where_it_goes { fixed; one_off; top; _ }; _ } ->
       Alcotest.(check bool) "rent is fixed" true (Int64.compare fixed 9_000_00L >= 0);
       Alcotest.(check int64) "Bauhaus is a one-off, a sixth of it a cycle" 1_000_00L one_off;
       Alcotest.(check string) "groceries first among the flexible" "Groceries" (match top with (c, _) :: _ -> c.account.name | [] -> "")
   | _ -> Alcotest.fail "no where it goes");
  Alcotest.(check bool) "no payday pattern where there is none" true (find Payday_week qs = None);
  (* All of them, not only the best five: Spotify's price rise is in the regular bills. *)
  let ctx = Result.get_ok (Insights.context service alice ~scope:Analytics.Me ()) in
  match List.find_opt (fun (q : Q.question) -> q.family = Regular_bills) (Q.ask (Briefing.input ctx)) with
  | Some { answer = Regular_bills { raised = [ (b, rise) ]; _ }; _ } ->
      Alcotest.(check (pair string int64)) "Spotify went up 40 a month" ("SPOTIFY", 480_00L) (b.bill_name, rise)
  | _ -> Alcotest.fail "no price rise"

(* Got it: that answer is acknowledged and goes. Not useful: that kind of question stops. *)
let test_what_the_person_says () =
  let service, alice = scenario () in
  let act key action =
    let command = List.find (fun (c : Commands.command) -> c.name = "act_on_question") Briefing.commands in
    ignore (ok_import (Commands.execute service alice ~scopes:[ "ledger:write" ] command (`Assoc [ ("key", `String key); ("action", `String action) ])))
  in
  let qs = briefing service alice in
  let leak = Option.get (find Leak qs) and drift = Option.get (find Drift qs) in
  act (Briefing.question_key leak) "got_it";
  act (Briefing.question_key drift) "not_useful";
  let after = briefing service alice in
  Alcotest.(check bool) "the leak, acknowledged, is gone" true (find Leak after = None);
  Alcotest.(check bool) "drift is muted" true (find Drift after = None);
  Alcotest.(check bool) "the rest stays" true (find Where_it_goes after <> None);
  (* Asking the advisor about a card: the server states the question and its figures. *)
  let where = Option.get (find Where_it_goes after) in
  (match Briefing.find service alice ~scope:Analytics.Me ~key:(Briefing.question_key where) with
   | Some (title, summary) ->
       Alcotest.(check string) "the question" "Where the money goes" title;
       Alcotest.(check bool) ("with the figures: " ^ summary) true (Cashflow_core.Str_compat.contains ~sub:"Groceries" summary && Cashflow_core.Str_compat.contains ~sub:"SEK" summary)
   | None -> Alcotest.fail "the advisor can't find the card");
  Alcotest.(check bool) "nor one put away" true (Briefing.find service alice ~scope:Analytics.Me ~key:(Briefing.question_key leak) = None)

(* The model proposes, the engine checks. A fake model reads the digest it is shown and proposes five questions;
   only those whose figures hold up and that the briefing doesn't already answer survive, every figure is the
   engine's, and the model is asked once for the same books. *)
let test_the_model_proposes () =
  let calls = ref 0 and shown = ref `Null in
  (* Only the briefing's question is answered: the import asks it about categories too. *)
  let complete ?schema:_ ~system ~user () =
    if not (String.starts_with ~prefix:"You are a careful personal accountant" system) then Error "not this" else begin
    incr calls;
    shown := Yojson.Safe.from_string user;
    let open Yojson.Safe.Util in
    let category prefix =
      let all = !shown |> member "categories" |> to_list in
      match List.find_opt (fun c -> String.starts_with ~prefix (to_string (member "name" c))) all with
      | Some c -> to_string (member "category_id" c)
      | None -> failwith ("no category " ^ prefix ^ " in " ^ String.concat ", " (List.map (fun c -> to_string (member "name" c)) all))
    in
    let q title check merchant category_id months =
      `Assoc [ ("title", `String title); ("check", `String check); ("merchant", `String merchant); ("category_id", `String category_id); ("months", `Int months) ]
    in
    Ok
      (Yojson.Safe.to_string
         (`Assoc
           [ ( "questions",
               `List
                 [ q "What do groceries at ICA cost you?" "merchant_spend" "ica" "" 6;
                   q "Is 6 months of Wolt too much?" "merchant_spend" "wolt" "" 6;
                   q "Has Wolt become a habit?" "merchant_spend" "wolt" "" 6;
                   q "Have restaurants gone up?" "category_change" "" (category "Restaurants") 0;
                   q "What is this new shop?" "new_merchant" "nowhere" "" 0 ] ) ]))
    end
  in
  let service, alice = scenario ~assistant:{ Cashflow_io.Llm.model = "fake"; complete } () in
  let ctx = Result.get_ok (Insights.context service alice ~scope:Analytics.Me ()) in
  let first, can_propose = Briefing.questions service alice ctx in
  Alcotest.(check (pair bool int)) "not asked while the page loads" (true, 0) (can_propose, !calls);
  Alcotest.(check bool) "nothing proposed yet" true (find Proposed first = None);
  let _, again = Briefing.questions ~write:true service alice ctx in
  Alcotest.(check (pair bool int)) "asked once" (false, 1) (again, !calls);
  Alcotest.(check bool) "it was shown the books, not people" true
    (let open Yojson.Safe.Util in
     List.exists (fun m -> member "merchant" m = `String "wolt") (!shown |> member "merchants" |> to_list)
     && List.mem (`String "leak:wolt:59") (!shown |> member "already_answered" |> to_list));
  let proposed = List.filter_map (Q.verify (Briefing.input ctx) ~existing:(Q.ask (Briefing.input ctx))) (Briefing.proposals_of_json (Yojson.Safe.from_string (Result.get_ok (complete ~system:(Briefing.system ~language:"English") ~user:(Yojson.Safe.to_string !shown) ())))) in
  (match proposed with
   | [ { answer = Proposed { title; figures = Merchant_figures { name; count; total; months } }; _ } ] ->
       Alcotest.(check string) "its words" "What do groceries at ICA cost you?" title;
       Alcotest.(check (pair string int)) "the engine's figures" ("ICA", 6) (name, months);
       Alcotest.(check bool) (Printf.sprintf "weekly shopping (%d, %Ld)" count total) true (count >= 24 && Int64.compare total 0L > 0)
   | qs -> Alcotest.fail (Printf.sprintf "expected only the ICA question to hold up, got %d" (List.length qs)));
  let cached, _ = Briefing.questions service alice ctx in
  Alcotest.(check int) "the books unchanged, not asked again" 2 !calls;
  Alcotest.(check bool) "and the checked question is in the briefing" true
    (List.exists (fun (q : Q.question) -> match q.answer with Proposed { title = "What do groceries at ICA cost you?"; _ } -> true | _ -> false) cached)

let () =
  Alcotest.run "briefing"
    [ ( "briefing",
        [ Alcotest.test_case "the questions worth answering" `Quick test_the_questions_worth_answering;
          Alcotest.test_case "what the person says" `Quick test_what_the_person_says;
          Alcotest.test_case "the model proposes" `Quick test_the_model_proposes ] ) ]
