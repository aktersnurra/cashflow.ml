open Test_service_support

let claims ?(scopes = [ "ledger:read"; "ledger:write" ]) ?(groups = None) ?nonce subject : Oidc.claims =
  { issuer = "https://id.example"; subject; audience = [ "https://cashflow.example" ]; expires_at = Ptime.max; scopes; name = Some subject;
    email = Some (subject ^ "@example.com"); groups; nonce }

let config : Config.t =
  { database_path = ":memory:"; listen_address = "127.0.0.1:0"; public_base_url = "https://cashflow.example"; oidc_issuer = "https://id.example";
    oidc_audience = "https://cashflow.example"; web_client_id = Some "web"; web_client_secret = Some "secret"; admin_group = "cashflow-admin";
    static_dir = "/nonexistent"; openrouter_api_key = None; openrouter_model = "x"; log_level = None }

(* A fake identity provider: the nonce of the last authorization request comes back in the ID token. *)
let last_nonce = ref None

let fake_client : Oidc.client =
  { discover =
      (fun ~issuer -> Ok { issuer; jwks_uri = "https://id.example/jwks"; authorization_endpoint = Some "https://id.example/authorize";
                           token_endpoint = Some "https://id.example/token"; max_age_s = None });
    fetch_jwks = (fun ~uri:_ -> Error ());
    post_form = (fun ~uri:_ form -> if List.assoc "code" form = "good-code" then Ok {|{"id_token":"id-token"}|} else Error ()) }

let deps () : Http_server.deps =
  let service = make_service () in
  let verify_bearer = function
    | "alice-token" -> Ok (claims "alice")
    | "reader-token" -> Ok (claims ~scopes:[ "ledger:read" ] "alice")
    | _ -> Error ()
  in
  let verify_id_token = function "id-token" -> Ok (claims ~groups:(Some [ "cashflow-admin" ]) ?nonce:!last_nonce "alice") | _ -> Error () in
  let web_auth =
    Web_auth.make ~client:fake_client ~verify_id_token
      ~config:{ issuer = "https://id.example"; client_id = "web"; client_secret = "secret"; redirect_uri = "https://cashflow.example/auth/callback"; admin_group = "cashflow-admin" }
  in
  { service; verify_bearer; web_auth = Some web_auth; config; now = (fun () -> now) }

let request ?(meth = Http_server.GET) ?(headers = []) ?(body = "") target : Http_server.request = { meth; target; headers; body }
let header (r : Http_server.response) name = List.find_map (fun (k, v) -> if String.lowercase_ascii k = name then Some v else None) r.headers
let headers_named (r : Http_server.response) name = List.filter_map (fun (k, v) -> if k = name then Some v else None) r.headers
let body_json (r : Http_server.response) = try Yojson.Safe.from_string r.body with Yojson.Json_error _ -> `Null
let member name = function `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null | _ -> `Null

let rpc deps ?(token = "alice-token") method_ params =
  Http_server.handle deps
    (request ~meth:POST "/mcp" ~headers:[ ("authorization", "Bearer " ^ token); ("content-type", "application/json") ]
       ~body:(Yojson.Safe.to_string (`Assoc [ ("jsonrpc", `String "2.0"); ("id", `Int 1); ("method", `String method_); ("params", params) ])))

let tool deps ?token name arguments =
  let response = rpc deps ?token "tools/call" (`Assoc [ ("name", `String name); ("arguments", arguments) ]) in
  (response, member "structuredContent" (member "result" (body_json response)))

let test_health_and_metadata () =
  let deps = deps () in
  Alcotest.(check int) "health" 200 (Http_server.handle deps (request "/health")).status;
  let metadata = Http_server.handle deps (request "/.well-known/oauth-protected-resource") in
  Alcotest.(check string) "issuer" {|["https://id.example"]|} (Yojson.Safe.to_string (member "authorization_servers" (body_json metadata)))

let test_mcp_requires_a_token () =
  let deps = deps () in
  let response = rpc deps ~token:"forged" "tools/list" (`Assoc []) in
  Alcotest.(check int) "401" 401 response.status;
  Alcotest.(check bool) "points at the metadata" true (Option.is_some (header response "www-authenticate"))

let test_mcp_tools () =
  let deps = deps () in
  let tools = member "tools" (member "result" (body_json (rpc deps "tools/list" (`Assoc [])))) in
  let names = match tools with `List tools -> List.map (fun t -> Yojson.Safe.Util.to_string (member "name" t)) tools | _ -> [] in
  List.iter (fun name -> Alcotest.(check bool) name true (List.mem name names)) [ "add_transaction"; "cashflow_summary"; "import_statement"; "list_inbox" ];
  Alcotest.(check bool) "web-only commands stay off MCP" false (List.mem "update_settings" names);
  let _, accounts = tool deps "list_accounts" (`Assoc []) in
  let id name =
    match member "accounts" accounts with
    | `List accounts -> Yojson.Safe.Util.to_string (member "id" (List.find (fun a -> member "name" a = `String name) accounts))
    | _ -> Alcotest.fail "no accounts"
  in
  let _, opened = tool deps "open_account" (`Assoc [ ("idempotency_key", `String "open-checking"); ("kind", `String "asset"); ("name", `String "Checking") ]) in
  let checking = Yojson.Safe.Util.to_string (member "id" (member "account" opened)) in
  let add = `Assoc [ ("idempotency_key", `String "ica-1"); ("from_account_id", `String checking); ("to_account_id", `String (id "Groceries")); ("amount", `String "249.90"); ("date", `String "2026-10-20") ] in
  let _, first = tool deps "add_transaction" add in
  Alcotest.(check string) "created" "\"created\"" (Yojson.Safe.to_string (member "status" first));
  Alcotest.(check string) "amount" "\"249.90\"" (Yojson.Safe.to_string (member "amount" (member "transfer" first)));
  let _, again = tool deps "add_transaction" add in
  Alcotest.(check string) "a retry is safe" "\"exists\"" (Yojson.Safe.to_string (member "status" again));
  let response, _ = tool deps "add_transaction" (`Assoc [ ("idempotency_key", `String "x"); ("from_account_id", `String checking); ("to_account_id", `String checking); ("amount", `String "1,5") ]) in
  Alcotest.(check bool) "a mistake the model can fix is a tool error" true (member "isError" (member "result" (body_json response)) = `Bool true);
  let _, summary = tool deps "cashflow_summary" (`Assoc [ ("cycles", `Int 1) ]) in
  match member "cycles" summary with
  | `List [ cycle ] -> Alcotest.(check string) "spending" "\"249.90\"" (Yojson.Safe.to_string (member "spending" cycle))
  | other -> Alcotest.failf "unexpected %s" (Yojson.Safe.to_string other)

let test_mcp_scopes () =
  let deps = deps () in
  let response, _ = tool deps ~token:"reader-token" "add_transaction" (`Assoc []) in
  Alcotest.(check int) "a read-only token can't write" 403 response.status;
  let response, _ = tool deps ~token:"reader-token" "list_accounts" (`Assoc []) in
  Alcotest.(check int) "but can read" 200 response.status

let cookie_header (response : Http_server.response) name =
  List.find_map
    (fun value -> match String.split_on_char ';' value with first :: _ when String.starts_with ~prefix:(name ^ "=") first -> Some first | _ -> None)
    (headers_named response "set-cookie")

let test_web_login_and_csrf () =
  let deps = deps () in
  let login = Http_server.handle deps (request "/auth/login?return_to=/inbox") in
  Alcotest.(check int) "redirects to the identity provider" 302 login.status;
  let location = Uri.of_string (Option.get (header login "location")) in
  Alcotest.(check (option string)) "pkce" (Some "S256") (Uri.get_query_param location "code_challenge_method");
  last_nonce := Uri.get_query_param location "nonce";
  let state = Option.get (Uri.get_query_param location "state") in
  let login_cookie = Option.get (cookie_header login "cashflow_login") in
  let forged = Http_server.handle deps (request ("/auth/callback?code=good-code&state=" ^ state) ~headers:[ ("cookie", "cashflow_login=someone-else") ]) in
  Alcotest.(check int) "a callback from another browser fails" 400 forged.status;
  let login = Http_server.handle deps (request "/auth/login") in
  let location = Uri.of_string (Option.get (header login "location")) in
  last_nonce := Uri.get_query_param location "nonce";
  let state = Option.get (Uri.get_query_param location "state") in
  let login_cookie' = Option.get (cookie_header login "cashflow_login") in
  ignore login_cookie;
  let callback = Http_server.handle deps (request ("/auth/callback?code=good-code&state=" ^ state) ~headers:[ ("cookie", login_cookie') ]) in
  Alcotest.(check int) "signed in" 302 callback.status;
  let session_cookie = Option.get (cookie_header callback "cashflow_session") in
  Alcotest.(check bool) "httponly" true (List.exists (fun v -> String.starts_with ~prefix:session_cookie v && Str_compat.contains ~sub:"HttpOnly" v) (headers_named callback "set-cookie"));
  let session = Http_server.handle deps (request "/api/session" ~headers:[ ("cookie", session_cookie) ]) in
  Alcotest.(check int) "session" 200 session.status;
  Alcotest.(check string) "admin from the groups claim" "true" (Yojson.Safe.to_string (member "is_admin" (member "user" (body_json session))));
  let csrf = Yojson.Safe.Util.to_string (member "csrf_token" (body_json session)) in
  let command ?(csrf_header = []) () =
    Http_server.handle deps
      (request ~meth:POST "/api/commands/list_accounts" ~body:"{}" ~headers:([ ("cookie", session_cookie); ("content-type", "application/json") ] @ csrf_header))
  in
  Alcotest.(check int) "no CSRF token, no write" 403 (command ()).status;
  Alcotest.(check int) "with it" 200 (command ~csrf_header:[ ("x-csrf-token", csrf) ] ()).status;
  Alcotest.(check int) "replaying the login fails" 400 (Http_server.handle deps (request ("/auth/callback?code=good-code&state=" ^ state) ~headers:[ ("cookie", login_cookie') ])).status;
  let logout = Http_server.handle deps (request ~meth:POST "/auth/logout" ~headers:[ ("cookie", session_cookie) ]) in
  Alcotest.(check int) "signed out" 200 logout.status;
  Alcotest.(check int) "session gone" 401 (Http_server.handle deps (request "/api/session" ~headers:[ ("cookie", session_cookie) ])).status

let test_login_return_paths () =
  List.iter
    (fun (return_to, expected) ->
      let deps = deps () in
      let target = Uri.(to_string (add_query_param' (of_string "/auth/login") ("return_to", return_to))) in
      let login = Http_server.handle deps (request target) in
      Alcotest.(check int) "login started" 302 login.status;
      let location = Uri.of_string (Option.get (header login "location")) in
      last_nonce := Uri.get_query_param location "nonce";
      let state = Option.get (Uri.get_query_param location "state") in
      let cookie = Option.get (cookie_header login "cashflow_login") in
      let callback = Http_server.handle deps (request ("/auth/callback?code=good-code&state=" ^ state) ~headers:[ ("cookie", cookie) ]) in
      Alcotest.(check int) "login finished" 302 callback.status;
      Alcotest.(check (option string)) (Printf.sprintf "destination for %S" return_to) (Some expected) (header callback "location"))
    [ ("/inbox", "/inbox"); ("/budgets?scope=household#goals", "/budgets?scope=household#goals");
      ("/", "/"); ("https://example.org", "/"); ("//example.org", "/");
      ("/\\example.org", "/"); ("/\\\\example.org", "/"); ("/\t/example.org", "/");
      ("/inbox\r\nLocation: https://example.org", "/"); ("/inbox\000", "/"); ("/inbox\127", "/") ]

let test_static_paths_stay_inside () =
  let deps = deps () in
  Alcotest.(check int) "traversal" 404 (Http_server.handle deps (request "/assets/../../etc/passwd")).status;
  Alcotest.(check int) "unknown api route" 404 (Http_server.handle deps (request "/api/nothing")).status

let () =
  Alcotest.run "http"
    [ ( "routes",
        [ Alcotest.test_case "health and metadata" `Quick test_health_and_metadata;
          Alcotest.test_case "mcp requires a token" `Quick test_mcp_requires_a_token; Alcotest.test_case "mcp tools" `Quick test_mcp_tools;
          Alcotest.test_case "mcp scopes" `Quick test_mcp_scopes; Alcotest.test_case "web login and csrf" `Quick test_web_login_and_csrf;
          Alcotest.test_case "login return paths" `Quick test_login_return_paths;
          Alcotest.test_case "static paths stay inside" `Quick test_static_paths_stay_inside ] ) ]
