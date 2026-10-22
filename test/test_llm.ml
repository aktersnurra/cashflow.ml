module Config = Cashflow_io.Config
module Import = Cashflow_io.Import
module Inbox = Cashflow_io.Inbox
module Llm = Cashflow_io.Llm
module Service = Cashflow_io.Service
module Sql = Cashflow_io.Sql
module Store_sqlite = Cashflow_io.Store_sqlite
module Account_id = Cashflow_core.Account_id
module Str_compat = Cashflow_core.Str_compat

open Test_service_support

let prompts = ref []

(* A fake model that picks Groceries for every row, and invents a category for good measure. *)
let fake : Llm.t =
  { model = "fake";
    complete =
      (fun ~system:_ ~user ->
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
  let preview = ok_import (Import.preview (Import.context service alice) { account_id = checking; file_name = None; content = csv; decisions = [] }) in
  (match (List.hd preview.rows).suggestion with
  | Some { source = Llm; confidence; category_id; _ } ->
      Alcotest.(check bool) "groceries" true (Account_id.equal category_id (account_named service alice "Groceries"));
      Alcotest.(check int) "confidence is capped below rules" 80 confidence
  | _ -> Alcotest.fail "no LLM suggestion");
  let prompt = List.hd !prompts in
  Alcotest.(check bool) "no account name" false (Str_compat.contains ~sub:"secret" prompt);
  Alcotest.(check bool) "no balance" false (Str_compat.contains ~sub:"87654" prompt);
  Alcotest.(check bool) "but the merchant" true (Str_compat.contains ~sub:"WILLYS" prompt)

(* Confirming an import asks the model before it takes the database, never inside the transaction,
   and still applies the suggestion. *)
let test_no_network_inside_the_import_transaction () =
  let db = Store_sqlite.open_memory () in
  let inside = ref [] in
  let watching : Llm.t = { fake with complete = (fun ~system ~user -> inside := Sql.in_transaction db :: !inside; fake.complete ~system ~user) } in
  let service = Service.make ~now:(fun () -> now) ~assistant:watching db in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let csv = "Datum;Text;Belopp\n2026-10-20;WILLYS HEMMA;-412,00\n" in
  ignore (ok_import (Import.confirm (Import.context service alice) { account_id = checking; file_name = None; content = csv; decisions = [] }));
  Alcotest.(check (list bool)) "asked once, outside" [ false ] !inside;
  match Inbox.list service alice ~limit:5 with
  | [ item ] -> Alcotest.(check bool) "suggestion kept" true (Option.is_some item.item.suggested_account_id)
  | _ -> Alcotest.fail "expected one inbox item"

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
  Alcotest.(check (result string string)) "completion" (Ok "Groceries") (assistant.complete ~system:"Classify" ~user:"ICA");
  Alcotest.(check int) "one request" 1 (List.length !requests);
  let open Yojson.Safe.Util in
  let request = List.hd !requests in
  Alcotest.(check string) "model" "test-model" (member "model" request |> to_string);
  let provider = member "provider" request in
  Alcotest.(check bool) "zero data retention" true (member "zdr" provider = `Bool true);
  Alcotest.(check string) "no data collection" "deny" (member "data_collection" provider |> to_string);
  Alcotest.(check (list string)) "only approved providers"
    [ "google-vertex"; "amazon-bedrock"; "azure" ] (member "only" provider |> to_list |> List.map to_string)

let test_openrouter_does_not_relax_policy () =
  let requests = ref [] in
  let post ~uri:_ ~headers:_ ~body = requests := Yojson.Safe.from_string body :: !requests; Error "HTTP 404 Not Found" in
  let config = openrouter_config "(provider_only (amazon-bedrock))" in
  let assistant = Llm.openrouter ~post ~api_key:"test-key" ~model:config.openrouter_model ~provider_only:config.openrouter_provider_only in
  Alcotest.(check (result string string)) "no eligible endpoint is an error" (Error "HTTP 404 Not Found")
    (assistant.complete ~system:"Classify" ~user:"ICA");
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
    (Error "OpenRouter provider allowlist must not be empty") (assistant.complete ~system:"Classify" ~user:"ICA");
  Alcotest.(check bool) "financial data never sent" false !sent

let () =
  Alcotest.run "llm"
    [ ( "features",
        [ Alcotest.test_case "suggestions and privacy" `Quick test_suggestions_and_privacy;
          Alcotest.test_case "no network inside the import transaction" `Quick test_no_network_inside_the_import_transaction ] );
      ( "openrouter",
        [ Alcotest.test_case "privacy policy in requests" `Quick test_openrouter_privacy_policy;
          Alcotest.test_case "never relax the policy" `Quick test_openrouter_does_not_relax_policy;
          Alcotest.test_case "empty allowlist sends nothing" `Quick test_openrouter_empty_allowlist_sends_nothing ] ) ]
