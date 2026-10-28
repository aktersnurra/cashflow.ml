module Config = Cashflow_io.Config
module Str_compat = Cashflow_core.Str_compat

let minimal =
  {|((database /tmp/db.sqlite3) (listen 127.0.0.1:8081) (public_url https://cashflow.example)
    (oidc ((issuer https://id.example) (audience https://cashflow.example))))|}

let ok text = match Config.parse text with Ok c -> c | Error e -> Alcotest.fail e
let error text = match Config.parse text with Ok _ -> Alcotest.fail "accepted" | Error e -> e

let test_example_file_parses () =
  (* The shipped example must stay in step with the record type. *)
  let sexp = Sexplib.Sexp.load_sexp "../cashflow.example.sexp" in
  let file = Config.File.t_of_sexp sexp in
  Alcotest.(check string) "listen" "127.0.0.1:8081" file.listen;
  Alcotest.(check bool) "web client" true (Option.is_some file.web);
  (* Enable the optional example without resolving deployment secrets. *)
  let text = In_channel.with_open_bin "../cashflow.example.sexp" In_channel.input_all in
  let enabled = Str_compat.replace_all ~sub:"#;(openrouter" ~by:"(openrouter" text in
  let enabled = Config.File.t_of_sexp (Sexplib.Sexp.of_string enabled) in
  let openrouter = Option.get enabled.openrouter in
  Alcotest.(check (list string)) "example provider allowlist"
    [ "google-vertex"; "amazon-bedrock"; "azure" ] openrouter.provider_only

let test_defaults () =
  let c = ok minimal in
  Alcotest.(check string) "static dir" "/usr/local/share/cashflow/web" c.static_dir;
  Alcotest.(check (option string)) "no web client" None c.web_client_id;
  Alcotest.(check (option string)) "no llm" None c.openrouter_api_key;
  Alcotest.(check bool) "info logging" true (c.log_level = Some Logs.Info);
  let with_level level = String.sub minimal 0 (String.length minimal - 1) ^ Printf.sprintf " (log_level %s))" level in
  Alcotest.(check bool) "debug" true ((ok (with_level "Debug")).log_level = Some Logs.Debug);
  Alcotest.(check bool) "quiet" true ((ok (with_level "Quiet")).log_level = None);
  ignore (error (with_level "Verbose"))

let test_secrets () =
  Unix.putenv "CASHFLOW_TEST_SECRET" "from-env";
  let path = Filename.temp_file "secret" "" in
  Out_channel.with_open_bin path (fun oc -> output_string oc "from-file\n");
  let with_secret secret =
    String.sub minimal 0 (String.length minimal - 1) ^ Printf.sprintf " (web ((client_id web) (client_secret %s))))" secret
  in
  Alcotest.(check (option string)) "inline" (Some "s3cret") (ok (with_secret "(Inline s3cret)")).web_client_secret;
  Alcotest.(check (option string)) "env" (Some "from-env") (ok (with_secret "(Env CASHFLOW_TEST_SECRET)")).web_client_secret;
  Alcotest.(check (option string)) "file, newline trimmed" (Some "from-file") (ok (with_secret (Printf.sprintf "(File %s)" path))).web_client_secret;
  ignore (error (with_secret "(Env CASHFLOW_TEST_UNSET_SECRET)"))

let test_errors () =
  let misspelt = {|((database /tmp/db) (listen 127.0.0.1:1) (public_url https://x)
    (oidc ((issuer https://id.example) (audience https://x)))
    (statc_dir /tmp))|} in
  let e = error misspelt in
  Alcotest.(check string) "points at the field" "config:3:5: unknown field statc_dir" e;
  ignore (error {|((database /tmp/db))|});
  ignore (error (Str_compat.replace_all ~sub:"https://id.example" ~by:"http://id.example" minimal));
  ignore (error (Str_compat.replace_all ~sub:"127.0.0.1:8081" ~by:"8081" minimal));
  ignore (error "((database")

let with_openrouter fields =
  String.sub minimal 0 (String.length minimal - 1)
  ^ Printf.sprintf " (openrouter ((api_key (Inline test-key)) %s)))" fields

let test_openrouter_providers () =
  let defaults = ok (with_openrouter "") in
  Alcotest.(check string) "default model" "anthropic/claude-haiku-5.5" defaults.openrouter_model;
  Alcotest.(check (list string)) "default allowlist"
    [ "google-vertex"; "amazon-bedrock"; "azure" ] defaults.openrouter_provider_only;
  let custom = ok (with_openrouter "(provider_only (amazon-bedrock))") in
  Alcotest.(check (option string)) "enabled with a restricted provider" (Some "test-key") custom.openrouter_api_key;
  Alcotest.(check (list string)) "custom allowlist" [ "amazon-bedrock" ] custom.openrouter_provider_only

(* The advisor can run on a bigger model than the cheap calls (categorising, pairing, summaries). *)
let test_advisor_model_apart () =
  let c = ok (with_openrouter "(model anthropic/claude-haiku-5.5) (advisor_model anthropic/claude-sonnet-5.5) (provider_only (amazon-bedrock))") in
  Alcotest.(check string) "cheap calls stay on haiku" "anthropic/claude-haiku-5.5" c.openrouter_model;
  Alcotest.(check string) "advisor on sonnet" "anthropic/claude-sonnet-5.5" c.advisor_model;
  Alcotest.(check bool) "advisor available" true (c.advisor_limits <> None);
  let same = ok (with_openrouter "(model anthropic/claude-sonnet-4.6) (provider_only (amazon-bedrock))") in
  Alcotest.(check string) "without advisor_model it follows model" "anthropic/claude-sonnet-4.6" same.advisor_model;
  let haiku_only = ok (with_openrouter "(provider_only (amazon-bedrock))") in
  Alcotest.(check bool) "haiku alone leaves the advisor unavailable" true (haiku_only.advisor_limits = None)

let test_openrouter_requires_providers () =
  Alcotest.(check string) "empty allowlist rejected" "openrouter.provider_only must not be empty"
    (error (with_openrouter "(provider_only ())"))

let test_openrouter_cannot_disable_privacy () =
  ignore (error (with_openrouter "(zdr false)"));
  ignore (error (with_openrouter "(data_collection allow)"))

let test_advisor_limits () =
  Alcotest.(check bool) "legacy model remains usable, Advisor unavailable" true
    ((ok (with_openrouter "")).advisor_limits = None);
  let profile = "(model anthropic/claude-sonnet-4.6) (provider_only (amazon-bedrock))" in
  let c = ok (with_openrouter profile) in
  Alcotest.(check bool) "ordinary Sonnet+strict-provider config ready" true (Config.advisor_available c);
  Alcotest.(check bool) "empty credential unavailable" false
    (Config.advisor_available { c with openrouter_api_key = Some " " });
  let l = Option.get c.advisor_limits in
  Alcotest.(check int) "supported context policy" 200000 l.context_tokens;
  Alcotest.(check int) "supported completion policy" 8192 l.completion_tokens;
  let limits = "(advisor_limits ((context_tokens 10000) (completion_tokens 1000)))" in
  let configured = with_openrouter (profile ^ " " ^ limits) in
  Alcotest.(check int) "stricter override" 1000
    (Option.get (ok configured).advisor_limits).completion_tokens;
  List.iter (fun (old, by) -> ignore (error (Str_compat.replace_all ~sub:old ~by configured)))
    ["context_tokens 10000", "context_tokens 0";
     "context_tokens 10000", "context_tokens 200001";
     "completion_tokens 1000", "completion_tokens 10001";
     "completion_tokens 1000", "completion_tokens 0";
     "anthropic/claude-sonnet-4.6", "unknown";
     "provider_only (amazon-bedrock)", "provider_only (amazon-bedrock azure)"];
  ignore (error (with_openrouter limits));
  let unsupported = ok (with_openrouter "(model unknown) (provider_only (amazon-bedrock))") in
  Alcotest.(check bool) "unknown model unavailable" true (unsupported.advisor_limits = None);
  let wrong_route = ok (with_openrouter "(model anthropic/claude-sonnet-4.6) (provider_only (azure))") in
  Alcotest.(check bool) "unsupported route unavailable" true (wrong_route.advisor_limits = None)

let () =
  Alcotest.run "config"
    [ ( "file",
        [ Alcotest.test_case "Advisor explicit limits" `Quick test_advisor_limits; Alcotest.test_case "example file parses" `Quick test_example_file_parses; Alcotest.test_case "defaults" `Quick test_defaults;
          Alcotest.test_case "secrets" `Quick test_secrets; Alcotest.test_case "errors" `Quick test_errors;
          Alcotest.test_case "OpenRouter providers" `Quick test_openrouter_providers;
          Alcotest.test_case "OpenRouter requires providers" `Quick test_openrouter_requires_providers;
          Alcotest.test_case "advisor model apart" `Quick test_advisor_model_apart;
          Alcotest.test_case "OpenRouter privacy is mandatory" `Quick test_openrouter_cannot_disable_privacy ] ) ]
