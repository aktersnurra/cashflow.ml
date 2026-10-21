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
  Alcotest.(check string) "admin group" "cashflow-admin" c.admin_group;
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

let test_openrouter_requires_providers () =
  Alcotest.(check string) "empty allowlist rejected" "openrouter.provider_only must not be empty"
    (error (with_openrouter "(provider_only ())"))

let test_openrouter_cannot_disable_privacy () =
  ignore (error (with_openrouter "(zdr false)"));
  ignore (error (with_openrouter "(data_collection allow)"))

let () =
  Alcotest.run "config"
    [ ( "file",
        [ Alcotest.test_case "example file parses" `Quick test_example_file_parses; Alcotest.test_case "defaults" `Quick test_defaults;
          Alcotest.test_case "secrets" `Quick test_secrets; Alcotest.test_case "errors" `Quick test_errors;
          Alcotest.test_case "OpenRouter providers" `Quick test_openrouter_providers;
          Alcotest.test_case "OpenRouter requires providers" `Quick test_openrouter_requires_providers;
          Alcotest.test_case "OpenRouter privacy is mandatory" `Quick test_openrouter_cannot_disable_privacy ] ) ]
