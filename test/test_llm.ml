module Config = Cashflow_io.Config
module Import = Cashflow_io.Import
module Inbox = Cashflow_io.Inbox
module Llm = Cashflow_io.Llm
module Llm_features = Cashflow_io.Llm_features
module Service = Cashflow_io.Service
module Sql = Cashflow_io.Sql
module Store_sqlite = Cashflow_io.Store_sqlite
module Insights = Cashflow_io.Insights
module Account_id = Cashflow_core.Account_id
module Analytics = Cashflow_core.Analytics
module Household = Cashflow_core.Household
module Owner = Cashflow_core.Owner
module User = Cashflow_core.User
module Str_compat = Cashflow_core.Str_compat

open Test_service_support

let prompts = ref []

(* A fake model that picks Groceries for every row, and invents a category for good measure. *)
let fake : Llm.t =
  { model = "fake";
    complete =
      (fun ?schema:_ ~system:_ ~user () ->
        prompts := user :: !prompts;
        let json = Yojson.Safe.from_string user in
        let open Yojson.Safe.Util in
        let groceries = member "categories" json |> to_list |> List.find (fun c -> member "name" c = `String "Groceries") |> member "id" |> to_string in
        let rows = member "rows" json |> to_list |> List.map (fun r -> member "row" r |> to_int) in
        Ok
          ("```json\n"
          ^ Yojson.Safe.to_string
              (`Assoc
                 [ ( "suggestions",
                     `List
                       (List.concat_map
                          (fun row -> [ `Assoc [ ("row", `Int row); ("category_id", `String groceries); ("confidence", `Int 99) ]; `Assoc [ ("row", `Int row); ("category_id", `String "made-up") ] ])
                          rows) ) ])
          ^ "\n```")) }

let test_suggestions_and_privacy () =
  let service = Service.make ~now:(fun () -> now) ~assistant:fake (Store_sqlite.open_memory ()) in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Alice's secret checking" in
  let csv = "Datum;Text;Belopp;Saldo\n2026-10-20;WILLYS HEMMA;-412,00;87 654,00\n" in
  let preview = ok_import (Import.preview (Import.context service alice) { target = Import.Existing checking; file_name = None; content = csv; decisions = [] }) in
  (match (List.hd preview.rows).suggestion with
  | Some { source = Llm; confidence; category_id; _ } ->
      Alcotest.(check bool) "groceries" true (Account_id.equal category_id (account_named service alice "Groceries"));
      Alcotest.(check int) "confidence is capped below rules" 80 confidence
  | _ -> Alcotest.fail "no LLM suggestion");
  let prompt = List.hd !prompts in
  Alcotest.(check bool) "no account name" false (Str_compat.contains ~sub:"secret" prompt);
  Alcotest.(check bool) "no balance" false (Str_compat.contains ~sub:"87654" prompt);
  Alcotest.(check bool) "but the merchant" true (Str_compat.contains ~sub:"WILLYS" prompt)

let speak_swedish service access =
  let user = Access.user access in
  ok (Service.update_settings service access { User.locale = "sv"; timezone = user.timezone; default_currency = user.default_currency; pay_cycle = user.pay_cycle })

(* Alice reads English and Bob Swedish. Each one's prompt names the household's starter categories
   in their own language, under the same ids, and neither's choice changes the other's. *)
let test_prompts_name_categories_in_the_viewers_language () =
  let asked = ref [] in
  let capturing : Llm.t = { fake with complete = (fun ?schema:_ ~system:_ ~user () -> asked := user :: !asked; Ok {|{"suggestions": []}|}) } in
  let service = Service.make ~now:(fun () -> now) ~assistant:capturing (Store_sqlite.open_memory ()) in
  let alice = sign_in service ~admin:true "alice" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  let bob = sign_in service "bob" in
  ignore (ok (Service.invite_member service alice ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  let bob = speak_swedish service bob in
  let shared = open_checking service alice ~owner:(Household household.id) ~statement_backed:true "Shared" in
  let groceries =
    (List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service alice))
      .account.id
  in
  let named access =
    let csv = "Datum;Text;Belopp\n2026-10-20;WILLYS HEMMA;-412,00\n" in
    ignore (ok_import (Import.preview (Import.context service access) { target = Import.Existing shared; file_name = None; content = csv; decisions = [] }));
    let open Yojson.Safe.Util in
    Yojson.Safe.from_string (List.hd !asked) |> member "categories" |> to_list
    |> List.find_map (fun c -> if member "id" c = `String (Account_id.to_string groceries) then Some (member "name" c |> to_string) else None)
  in
  Alcotest.(check (option string)) "alice reads English" (Some "Groceries") (named alice);
  Alcotest.(check (option string)) "bob reads Swedish" (Some "Mat & hushåll") (named bob);
  Alcotest.(check (option string)) "and alice still English" (Some "Groceries") (named alice)

(* The month review names its top categories in the reader's language. *)
let test_month_review_in_the_readers_language () =
  let asked = ref [] in
  let capturing : Llm.t = { fake with complete = (fun ?schema:_ ~system:_ ~user () -> asked := user :: !asked; Ok "Mostly groceries.") } in
  let service = Service.make ~now:(fun () -> now) ~assistant:capturing (Store_sqlite.open_memory ()) in
  let alice = speak_swedish service (sign_in service "alice") in
  let ctx = ok (Insights.context service alice ~scope:Me ()) in
  let cycle = Insights.current_cycle ctx in
  let summary : Analytics.cycle_summary = { cycle; flows = Analytics.zero_flows; savings = 0L; savings_rate_bp = None; reconciled = false } in
  let groceries = List.find (fun (i : Account_info.t) -> i.account.name = "Groceries") (Service.accounts service alice) in
  ignore (Llm_features.month_summary service alice ~cycle ~currency:ctx.currency ~summary ~previous:None ~top:[ { category = groceries; total = 1_000_00L; previous = None } ]);
  let prompt = List.hd !asked in
  Alcotest.(check bool) "in Swedish" true (Str_compat.contains ~sub:"Mat & hushåll" prompt);
  Alcotest.(check bool) "not in English" false (Str_compat.contains ~sub:"Groceries" prompt)

(* Confirming an import asks the model before it takes the database, never inside the transaction,
   and still applies the suggestion. *)
let test_no_network_inside_the_import_transaction () =
  let db = Store_sqlite.open_memory () in
  let inside = ref [] in
  let watching : Llm.t = { fake with complete = (fun ?schema ~system ~user () -> inside := Sql.in_transaction db :: !inside; fake.complete ?schema ~system ~user ()) } in
  let service = Service.make ~now:(fun () -> now) ~assistant:watching db in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let csv = "Datum;Text;Belopp\n2026-10-20;WILLYS HEMMA;-412,00\n" in
  ignore (ok_import (Import.confirm (Import.context service alice) { target = Import.Existing checking; file_name = None; content = csv; decisions = [] }));
  Alcotest.(check (list bool)) "asked once, outside" [ false ] !inside;
  match Inbox.list service alice ~limit:5 with
  | [ item ] -> Alcotest.(check bool) "suggestion kept" true (Option.is_some item.item.suggested_account_id)
  | _ -> Alcotest.fail "expected one inbox item"

(* A bank no format knows: the assistant reads its layout from the masked rows, and the rows are
   read from the file. *)
let unknown_bank =
  "Example Bank statement\nEveryday account,1234-5678-9012\nWhen,What,Paid out,Paid in,Left\n20/10/2026,COFFEE BAR,4.50,,995.50\n19/10/2026,SALARY,,1000.00,1000.00\n"

let layout_reply =
  {|{"header_row": 2, "columns": {"booking_date": 0, "transaction_date": null, "description": 1, "counterparty": null, "amount": null,
     "money_out": 2, "money_in": 3, "balance": 4, "reference": null}, "date_order": "DMY", "spending_positive": false,
     "institution": "Example Bank", "account_name_cell": {"row": 1, "column": 0}, "account_number_cell": {"row": 1, "column": 1}}|}

let test_unknown_format_read_by_the_assistant () =
  let layouts = ref [] in
  let reading : Llm.t =
    { fake with
      complete =
        (fun ?schema ~system ~user () ->
          match schema with
          | Some (name, _) -> layouts := (name, user) :: !layouts; Ok layout_reply
          | None -> fake.complete ~system ~user ()) }
  in
  let service = Service.make ~now:(fun () -> now) ~assistant:reading (Store_sqlite.open_memory ()) in
  let alice = sign_in service "alice" in
  let ctx = Import.context service alice in
  let identified = ok_import (Import.identify ctx unknown_bank) in
  Alcotest.(check string) "read by the assistant" "llm" identified.format;
  Alcotest.(check (list (option string))) "the account it is for" [ Some "Example Bank"; Some "Everyday account"; Some "9012" ]
    [ identified.identity.institution; identified.identity.account_name; identified.identity.last4 ];
  (match !layouts with
  | [ (name, prompt) ] ->
      Alcotest.(check string) "structured output" "statement_layout" name;
      Alcotest.(check bool) "the layout and the words" true (Str_compat.contains ~sub:"COFFEE BAR" prompt && Str_compat.contains ~sub:"Paid out" prompt);
      List.iter
        (fun figure -> Alcotest.(check bool) ("no figures: " ^ figure) false (Str_compat.contains ~sub:figure prompt))
        [ "4.50"; "995"; "1000"; "1234"; "9012"; "2026" ]
  | _ -> Alcotest.fail "expected one layout request");
  let request : Import.request =
    { target = Create { name = "Everyday"; kind = Asset; owner = Access.personal alice; institution = Some "Example Bank"; number_last4 = Some "9012" };
      file_name = Some "statement.csv"; content = unknown_bank; decisions = [] }
  in
  let preview = ok_import (Import.preview ctx request) in
  Alcotest.(check (list int64)) "amounts from the file" [ -4_50L; 1_000_00L ] (List.map (fun (p : Import.planned) -> p.row.amount) preview.rows);
  Alcotest.(check (list string)) "day, month, year" [ "2026-10-20"; "2026-10-19" ]
    (List.map (fun (p : Import.planned) -> Cashflow_core.Date.to_string p.row.booking_date) preview.rows);
  ignore (ok_import (Import.confirm ctx request));
  Alcotest.(check int) "asked once, then remembered" 1 (List.length !layouts);
  (* Without an assistant the file is refused, as before. *)
  let plain = Service.make ~now:(fun () -> now) (Store_sqlite.open_memory ()) in
  let bob = sign_in plain "bob" in
  match Import.identify (Import.context plain bob) unknown_bank with
  | Error (`Unreadable _) -> ()
  | _ -> Alcotest.fail "read without an assistant"

(* A layout pointing outside the file is refused with a reason, never an exception later. *)
let test_layout_cells_outside_the_file () =
  let table = Cashflow_core.Csv.parse unknown_bank in
  let layout cell = Result.get_ok (Llm_features.layout_of_json (Yojson.Safe.from_string (Str_compat.replace_all ~sub:{|{"row": 1, "column": 0}|} ~by:cell layout_reply))) in
  Alcotest.(check bool) "inside" true (Result.is_ok (Cashflow_core.Statement_format.layout_valid (layout {|{"row": 1, "column": 0}|}) table));
  List.iter
    (fun cell ->
      match Cashflow_core.Statement_format.layout_valid (layout cell) table with
      | Error _ -> ()
      | Ok () -> Alcotest.fail ("accepted " ^ cell))
    [ {|{"row": -1, "column": 0}|}; {|{"row": 0, "column": -1}|}; {|{"row": 99, "column": 0}|}; {|{"row": 1, "column": 9}|} ]

let openrouter_config fields =
  match Config.parse
    (Printf.sprintf
       {|((database /tmp/test.sqlite3) (listen 127.0.0.1:8081) (public_url https://cashflow.example)
          (oidc ((issuer https://id.example) (audience https://cashflow.example)))
          (openrouter ((api_key (Inline test-key)) (model test-model) %s)))|} fields)
  with Ok config -> config | Error e -> Alcotest.fail e

(* Privacy restrictions belong in the HTTP request, not just the configuration. *)
let test_openrouter_privacy_policy () =
  let requests = ref [] in
  let post ~uri:_ ~headers:_ ~body =
    requests := Yojson.Safe.from_string body :: !requests;
    Ok {|{"choices":[{"message":{"content":"Groceries"}}]}|}
  in
  let config = openrouter_config "" in
  let assistant = Llm.openrouter ~post ~api_key:"test-key" ~model:config.openrouter_model ~provider_only:config.openrouter_provider_only in
  Alcotest.(check (result string string)) "completion" (Ok "Groceries") (assistant.complete ~system:"Classify" ~user:"ICA" ());
  Alcotest.(check int) "one request" 1 (List.length !requests);
  let open Yojson.Safe.Util in
  let request = List.hd !requests in
  Alcotest.(check string) "model" "test-model" (member "model" request |> to_string);
  let provider = member "provider" request in
  Alcotest.(check bool) "zero data retention" true (member "zdr" provider = `Bool true);
  Alcotest.(check string) "no data collection" "deny" (member "data_collection" provider |> to_string);
  Alcotest.(check (list string)) "only approved providers"
    [ "google-vertex"; "amazon-bedrock"; "azure" ] (member "only" provider |> to_list |> List.map to_string)

(* With a schema the request asks for structured output, and only from endpoints that honour it. *)
let test_openrouter_structured_output () =
  let requests = ref [] in
  let post ~uri:_ ~headers:_ ~body = requests := Yojson.Safe.from_string body :: !requests; Ok {|{"choices":[{"message":{"content":"{}"}}]}|} in
  let assistant = Llm.openrouter ~post ~api_key:"test-key" ~model:"test-model" ~provider_only:[ "amazon-bedrock" ] in
  let schema = `Assoc [ ("type", `String "object") ] in
  Alcotest.(check (result string string)) "reply" (Ok "{}") (assistant.complete ~schema:("shape", schema) ~system:"Read" ~user:"rows" ());
  let open Yojson.Safe.Util in
  let request = List.hd !requests in
  let format = member "response_format" request in
  Alcotest.(check string) "json schema" "json_schema" (member "type" format |> to_string);
  Alcotest.(check bool) "strict" true (member "strict" (member "json_schema" format) = `Bool true);
  Alcotest.(check bool) "the schema" true (member "schema" (member "json_schema" format) = schema);
  let provider = member "provider" request in
  Alcotest.(check bool) "only endpoints that support it" true (member "require_parameters" provider = `Bool true);
  Alcotest.(check bool) "still zero data retention" true (member "zdr" provider = `Bool true)

let test_openrouter_does_not_relax_policy () =
  let requests = ref [] in
  let post ~uri:_ ~headers:_ ~body = requests := Yojson.Safe.from_string body :: !requests; Error "HTTP 404 Not Found" in
  let config = openrouter_config "(provider_only (amazon-bedrock))" in
  let assistant = Llm.openrouter ~post ~api_key:"test-key" ~model:config.openrouter_model ~provider_only:config.openrouter_provider_only in
  Alcotest.(check (result string string)) "no eligible endpoint is an error" (Error "HTTP 404 Not Found")
    (assistant.complete ~system:"Classify" ~user:"ICA" ());
  Alcotest.(check int) "no relaxed-policy retry" 1 (List.length !requests);
  let open Yojson.Safe.Util in
  let provider = member "provider" (List.hd !requests) in
  Alcotest.(check bool) "failed request still requires ZDR" true (member "zdr" provider = `Bool true);
  Alcotest.(check (list string)) "custom allowlist remains in effect" [ "amazon-bedrock" ]
    (member "only" provider |> to_list |> List.map to_string);
  Alcotest.(check string) "no collection even on failure" "deny" (member "data_collection" provider |> to_string)

let test_openrouter_empty_allowlist_sends_nothing () =
  let sent = ref false in
  let post ~uri:_ ~headers:_ ~body:_ = sent := true; Error "request was sent" in
  let assistant = Llm.openrouter ~post ~api_key:"test-key" ~model:"test-model" ~provider_only:[] in
  Alcotest.(check (result string string)) "empty allowlist refused"
    (Error "OpenRouter provider allowlist must not be empty") (assistant.complete ~system:"Classify" ~user:"ICA" ());
  Alcotest.(check bool) "financial data never sent" false !sent

let () =
  Alcotest.run "llm"
    [ ( "features",
        [ Alcotest.test_case "suggestions and privacy" `Quick test_suggestions_and_privacy;
          Alcotest.test_case "prompts name categories in the viewer's language" `Quick test_prompts_name_categories_in_the_viewers_language;
          Alcotest.test_case "month review in the reader's language" `Quick test_month_review_in_the_readers_language;
          Alcotest.test_case "no network inside the import transaction" `Quick test_no_network_inside_the_import_transaction;
          Alcotest.test_case "an unknown format read by the assistant" `Quick test_unknown_format_read_by_the_assistant;
          Alcotest.test_case "layout cells outside the file" `Quick test_layout_cells_outside_the_file ] );
      ( "openrouter",
        [ Alcotest.test_case "privacy policy in requests" `Quick test_openrouter_privacy_policy;
          Alcotest.test_case "never relax the policy" `Quick test_openrouter_does_not_relax_policy;
          Alcotest.test_case "structured output" `Quick test_openrouter_structured_output;
          Alcotest.test_case "empty allowlist sends nothing" `Quick test_openrouter_empty_allowlist_sends_nothing ] ) ]
