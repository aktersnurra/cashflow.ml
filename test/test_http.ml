module Config = Cashflow_io.Config
module Http_server = Cashflow_io.Http_server
module Logo_fetch = Cashflow_io.Logo_fetch
module Merchant_logo_store = Cashflow_io.Merchant_logo_store
module Owner = Cashflow_core.Owner
module Oidc = Cashflow_io.Oidc
module Sql = Cashflow_io.Sql
module Web_auth = Cashflow_io.Web_auth
module Str_compat = Cashflow_core.Str_compat

open Test_service_support

let claims ?(scopes = [ "ledger:read"; "ledger:write" ]) ?(groups = None) ?nonce subject : Oidc.claims =
  { issuer = "https://id.example"; subject; audience = [ "https://cashflow.example" ]; expires_at = Ptime.max; scopes; name = Some subject;
    email = Some (subject ^ "@example.com"); groups; nonce }

let config : Config.t =
  { database_path = ":memory:"; listen_address = "127.0.0.1:0"; public_base_url = "https://cashflow.example"; oidc_issuer = "https://id.example";
    oidc_audience = "https://cashflow.example"; web_client_id = Some "web"; web_client_secret = Some "secret"; admin_group = "cashflow-admin";
    static_dir = "/nonexistent"; openrouter_api_key = None; openrouter_model = "x";
    openrouter_provider_only = Config.default_openrouter_provider_only;
    advisor_limits = None; log_level = None }

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
  { service; verify_bearer; advisor = None; web_auth = Some web_auth; config; now = (fun () -> now) }

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
  List.iter (fun name -> Alcotest.(check bool) name true (List.mem name names)) [ "add_transaction"; "cashflow_summary"; "forecast"; "import_statement"; "list_inbox" ];
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

(* A Swedish browser signing in, the first time and again, leaves the starter chart in English:
   language is applied when names are shown, not by renaming them. *)
let test_sign_in_keeps_starter_names () =
  let deps = deps () in
  let sign_in_swedish () =
    let login = Http_server.handle deps (request "/auth/login") in
    let location = Uri.of_string (Option.get (header login "location")) in
    last_nonce := Uri.get_query_param location "nonce";
    let state = Option.get (Uri.get_query_param location "state") in
    let cookie = Option.get (cookie_header login "cashflow_login") in
    let callback =
      Http_server.handle deps
        (request ("/auth/callback?code=good-code&state=" ^ state) ~headers:[ ("cookie", cookie); ("accept-language", "sv-SE,sv;q=0.9,en") ])
    in
    Alcotest.(check int) "signed in" 302 callback.status
  in
  sign_in_swedish ();
  let alice = sign_in deps.service "alice" in
  Alcotest.(check string) "a Swedish user" "sv" (Access.user alice).locale;
  let before = Service.accounts deps.service alice in
  ignore (account_named deps.service alice "Groceries");
  sign_in_swedish ();
  Alcotest.(check bool) "signing in again renames nothing" true (Service.accounts deps.service alice = before)

let test_unversioned_assets_revalidate () =
  let dir = Filename.temp_file "cashflow-assets-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let files = ["index.html"; "style.css"; "main.bc.js"] in
  let file name = Filename.concat dir name in
  Fun.protect
    ~finally:(fun () -> List.iter (fun name -> if Sys.file_exists (file name) then Sys.remove (file name)) files; Unix.rmdir dir)
    (fun () ->
      let deps = deps () in
      let deps = {deps with config={deps.config with static_dir=dir}} in
      List.iter (fun version ->
        List.iter (fun name -> Out_channel.with_open_bin (file name) (fun out -> output_string out version)) files;
        List.iter (fun target ->
          let response = Http_server.handle deps (request target) in
          Alcotest.(check int) "asset served" 200 response.status;
          Alcotest.(check string) "same URL serves current build" version response.body;
          Alcotest.(check (option string)) "fixed asset URLs must revalidate" (Some "no-cache") (header response "cache-control"))
          ["/transactions"; "/assets/style.css"; "/assets/main.bc.js"])
        ["build-one"; "build-two"])

(* The bundle is large: content-hashed URLs are cached for good, everything revalidates by ETag, and text is sent
   gzipped to a client that accepts it, so a refresh on a slow link downloads nothing or a third of the bytes. *)
let test_static_caching_and_compression () =
  let dir = Filename.temp_file "cashflow-assets-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let bundle = String.concat "" (List.init 4000 (fun n -> Printf.sprintf "function f%d(){return %d}\n" n (n * 7))) in
  let files = [ ("index.html", "<!doctype html><title>x</title>" ^ String.make 2000 ' '); ("main.bc.js", bundle); ("icon.png", "\x89PNG" ^ String.make 100 'x') ] in
  let file name = Filename.concat dir name in
  Fun.protect
    ~finally:(fun () -> List.iter (fun (name, _) -> if Sys.file_exists (file name) then Sys.remove (file name)) files; Unix.rmdir dir)
    (fun () ->
      List.iter (fun (name, body) -> Out_channel.with_open_bin (file name) (fun out -> output_string out body)) files;
      let deps = deps () in
      let deps = { deps with config = { deps.config with static_dir = dir } } in
      let get ?(headers = []) target = Http_server.handle deps (request ~headers target) in
      let versioned = get "/assets/main.bc.js?v=abc" and plain = get "/assets/main.bc.js" in
      Alcotest.(check (option string)) "a content-hashed URL is cached for good" (Some "public, max-age=31536000, immutable") (header versioned "cache-control");
      Alcotest.(check (option string)) "an unhashed URL revalidates" (Some "no-cache") (header plain "cache-control");
      Alcotest.(check (option string)) "the page itself revalidates, whatever its query" (Some "no-cache") (header (get "/transactions?v=abc") "cache-control");
      Alcotest.(check string) "no Accept-Encoding: the bytes as they are" bundle versioned.body;
      Alcotest.(check (option string)) "and no content-encoding" None (header versioned "content-encoding");
      let zipped = get ~headers:[ ("Accept-Encoding", "br, gzip;q=0.8") ] "/assets/main.bc.js?v=abc" in
      Alcotest.(check (option string)) "gzip when accepted" (Some "gzip") (header zipped "content-encoding");
      Alcotest.(check bool) "a good deal smaller" true (String.length zipped.body * 3 < String.length bundle);
      let raw_deflate = String.sub zipped.body 10 (String.length zipped.body - 18) in
      (match Zipc_deflate.inflate raw_deflate with
       | Ok text -> Alcotest.(check string) "and it decompresses to the file" bundle text
       | Error e -> Alcotest.fail e);
      let tail = String.sub zipped.body (String.length zipped.body - 4) 4 in
      Alcotest.(check int) "with the right length in the trailer" (String.length bundle) (Char.code tail.[0] lor (Char.code tail.[1] lsl 8) lor (Char.code tail.[2] lsl 16) lor (Char.code tail.[3] lsl 24));
      Alcotest.(check (option string)) "responses differ by encoding" (Some "accept-encoding") (header zipped "vary");
      let png = get ~headers:[ ("Accept-Encoding", "gzip") ] "/assets/icon.png" in
      Alcotest.(check (option string)) "an image is not recompressed" None (header png "content-encoding");
      let etag = Option.get (header zipped "etag") in
      let again = get ~headers:[ ("Accept-Encoding", "gzip"); ("If-None-Match", etag) ] "/assets/main.bc.js?v=abc" in
      Alcotest.(check int) "a client that has it is told so" 304 again.status;
      Alcotest.(check string) "with no body" "" again.body;
      Alcotest.(check int) "the identity copy's tag does not match the gzip copy" 200
        (get ~headers:[ ("Accept-Encoding", "gzip"); ("If-None-Match", Option.get (header versioned "etag")) ] "/assets/main.bc.js").status;
      (* A new build under the same URL is never served from the old bytes. *)
      Out_channel.with_open_bin (file "main.bc.js") (fun out -> output_string out (bundle ^ "// next build\n"));
      let next = get ~headers:[ ("Accept-Encoding", "gzip"); ("If-None-Match", etag) ] "/assets/main.bc.js?v=abc" in
      Alcotest.(check int) "so a changed file is sent again" 200 next.status;
      Alcotest.(check bool) "with a new tag" true (header next "etag" <> Some etag))

let test_static_paths_stay_inside () =
  let deps = deps () in
  Alcotest.(check int) "traversal" 404 (Http_server.handle deps (request "/assets/../../etc/passwd")).status;
  Alcotest.(check int) "unknown api route" 404 (Http_server.handle deps (request "/api/nothing")).status

let test_reporting_envelopes () =
  let deps = deps () in
  let access = sign_in deps.service "alice" in
  let token = Web_auth.create_session deps.service.db ~now (Access.user access) in
  let _, csrf = Option.get (Web_auth.session deps.service ~now token) in
  let http args = Http_server.handle deps (request ~meth:POST "/api/commands/cashflow_summary"
    ~headers:["cookie", "cashflow_session=" ^ token; "x-csrf-token", csrf; "content-type", "application/json"]
    ~body:(Yojson.Safe.to_string args)) in
  let check_json label expected actual = Alcotest.(check string) label (Yojson.Safe.to_string expected) (Yojson.Safe.to_string actual) in
  let success = Yojson.Safe.from_string {|{"scope":"me","currency":"SEK","cycles":[{"cycle":"2026-10","first":"2026-09-25","last":"2026-10-22","income":"0.00","spending":"0.00","transfers_in":"0.00","transfers_out":"0.00","savings":"0.00","savings_rate":null,"reconciled":false}]}|} in
  let listed = member "tools" (member "result" (body_json (rpc deps "tools/list" (`Assoc [])))) in
  let descriptor = match listed with
    | `List tools -> List.find (fun t -> member "name" t = `String "cashflow_summary") tools
    | _ -> Alcotest.fail "missing tools" in
  let schema = member "inputSchema" descriptor in
  Alcotest.(check string) "schema type" "\"object\"" (Yojson.Safe.to_string (member "type" schema));
  check_json "schema required" (`List []) (member "required" schema);
  check_json "schema additional properties" (`Bool false) (member "additionalProperties" schema);
  let properties = match member "properties" schema with `Assoc fields -> fields | _ -> Alcotest.fail "missing schema properties" in
  Alcotest.(check (list string)) "cashflow schema names" ["scope"; "currency"; "cycles"] (List.map fst properties);
  List.iter (fun (name,kind) -> check_json ("schema " ^ name) (`String kind) (member "type" (List.assoc name properties)))
    ["scope","string"; "currency","string"; "cycles","integer"];
  let args = `Assoc ["cycles", `Int 1] in
  let response = http args in
  Alcotest.(check int) "HTTP success" 200 response.status;
  check_json "complete HTTP success" success (body_json response);
  let response, _ = tool deps "cashflow_summary" args in
  check_json "complete MCP success"
    (`Assoc ["jsonrpc", `String "2.0"; "id", `Int 1; "result", `Assoc ["resultType", `String "complete";
      "content", `List [`Assoc ["type", `String "text"; "text", `String (Yojson.Safe.to_string success)]];
      "structuredContent", success]]) (body_json response);
  Sql.exec_script deps.service.db "INSERT INTO currencies (code, scale) VALUES ('BAD!', 2)";
  List.iter (fun (args,message) ->
    let response = http args in
    Alcotest.(check int) "HTTP validation" 422 response.status;
    check_json "complete HTTP validation" (`Assoc ["error", `String message]) (body_json response);
    let response, _ = tool deps "cashflow_summary" args in
    check_json "complete MCP validation"
      (`Assoc ["jsonrpc", `String "2.0"; "id", `Int 1; "result", `Assoc ["resultType", `String "complete";
        "content", `List [`Assoc ["type", `String "text"; "text", `String message]];
        "isError", `Bool true]]) (body_json response))
    [`Assoc ["currency", `String "BAD!"], "currency code \"BAD!\" must be three uppercase letters";
     `Assoc ["currency", `String "BAD"], "unknown currency BAD";
     `Assoc ["scope", `String "household"], "you don't belong to a household, so use scope \"me\""];
  (* Only report storage is broken; session and bearer authentication remain intact. *)
  Sql.exec_script deps.service.db "ALTER TABLE currencies RENAME TO unavailable_currencies";
  let response = http args in
  Alcotest.(check int) "HTTP storage status" 500 response.status;
  check_json "complete HTTP storage" (`Assoc ["error", `String "internal error"]) (body_json response);
  let response, _ = tool deps "cashflow_summary" args in
  Alcotest.(check int) "MCP transport" 200 response.status;
  check_json "complete MCP storage" (Yojson.Safe.from_string {|{"jsonrpc":"2.0","id":1,"error":{"code":-32603,"message":"Internal error"}}|}) (body_json response)


let test_private_logos () =
  let deps = deps () in
  let deps = {deps with verify_bearer=(function "bob-token" -> Ok (claims "bob") | token -> deps.verify_bearer token)} in
  let png = let path = if Sys.file_exists "test/fixtures/logo_64x48.png" then "test/fixtures/logo_64x48.png" else "fixtures/logo_64x48.png" in
    In_channel.with_open_bin path In_channel.input_all in
  let image = Logo_fetch.fetch Logo_fetch.{with_request=(fun f -> f ()); resolve=(fun ~host:_ -> Ok ["8.8.8.8"]);
    get=(fun ~uri:_ ~address:_ ~max_bytes:_ -> Ok {status=200; headers=[]; body=png})} "shop.com/pic.png" |> Result.get_ok in
  let alice = Service.sign_in deps.service {issuer="https://id.example"; subject="alice"; display_name=None; email=None; is_admin=None; locale=None} in
  let bob = sign_in deps.service "bob" in
  let owner = Access.personal alice in
  let id = Sql.transaction deps.service.db (fun () ->
    let id = Merchant_logo_store.save_blob deps.service.db now image in
    Merchant_logo_store.put deps.service.db owner "shop" {source=User; status=Resolved; website=None; logo_id=Some id; confidence=None; resolved_at=now}; id) in
  let target = "/api/logos/" ^ string_of_int id in
  let fetch ?(meth=Http_server.GET) ?(token="alice-token") () =
    Http_server.handle deps (request ~meth ~headers:["authorization", "Bearer " ^ token] target) in
  let unsigned = Http_server.handle deps (request target) in
  Alcotest.(check int) "unsigned" 404 unsigned.status;
  Alcotest.(check (option string)) "unsigned logo cannot be cached" (Some "private, no-store") (header unsigned "cache-control");
  Alcotest.(check int) "other user" 404 (fetch ~token:"bob-token" ()).status;
  let bob_owner = Access.personal bob in
  Merchant_logo_store.put deps.service.db bob_owner "shop"
    {source=User; status=Resolved; website=None; logo_id=Some id; confidence=None; resolved_at=now};
  Alcotest.(check int) "shared hash allows associated user" 200 (fetch ~token:"bob-token" ()).status;
  Merchant_logo_store.delete deps.service.db bob_owner "shop";
  let allowed = fetch () in
  Alcotest.(check int) "allowed" 200 allowed.status;
  Alcotest.(check string) "bytes" png allowed.body;
  Alcotest.(check (option string)) "detected type" (Some "image/png") (header allowed "content-type");
  Alcotest.(check (option string)) "protected image cannot be cached" (Some "private, no-store") (header allowed "cache-control");
  Alcotest.(check bool) "CSP self" true (Str_compat.contains ~sub:"img-src 'self' data:" (Option.get (header allowed "content-security-policy")));
  let head = fetch ~meth:Http_server.HEAD () in
  Alcotest.(check int) "HEAD" 200 head.status;
  Alcotest.(check string) "HEAD empty" "" head.body;
  Alcotest.(check (option string)) "HEAD cannot be cached" (Some "private, no-store") (header head "cache-control");
  Merchant_logo_store.delete deps.service.db owner "shop";
  Alcotest.(check int) "removed association" 404 (fetch ()).status;
  let admin = sign_in deps.service ~admin:true "household-admin" in
  let household = ok (Service.create_household deps.service admin ~name:"Home" ~default_currency:"SEK") in
  let owner = Owner.Household household.id in
  let member = Service.access_for deps.service (Access.user admin) in
  Merchant_logo_store.put deps.service.db owner "shop"
    {source=User; status=Resolved; website=None; logo_id=Some id; confidence=None; resolved_at=now};
  let deps = {deps with verify_bearer=(function "member-token" -> Ok (claims "household-admin") | token -> deps.verify_bearer token)} in
  let member_fetch () = Http_server.handle deps (request ~headers:["authorization", "Bearer member-token"] target) in
  Alcotest.(check int) "household member" 200 (member_fetch ()).status;
  ignore (Service.leave_household deps.service member);
  Alcotest.(check int) "left household" 404 (member_fetch ()).status

(* An SVG opened at its own URL is a document of this origin: it is served only in its cleaned form, sandboxed. *)
let test_svg_logos () =
  let deps = deps () in
  let alice = sign_in deps.service "alice" in
  let owner = Access.personal alice in
  let save key (image : Logo_fetch.image) = Sql.transaction deps.service.db (fun () ->
    let id = Merchant_logo_store.save_blob deps.service.db now image in
    Merchant_logo_store.put deps.service.db owner key {source=User; status=Resolved; website=None; logo_id=Some id; confidence=None; resolved_at=now}; id) in
  let fetch id = Http_server.handle deps (request ~headers:["authorization", "Bearer alice-token"] ("/api/logos/" ^ string_of_int id)) in
  let svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'><circle r='8' onclick='alert(1)'/></svg>" in
  let image = Logo_fetch.fetch Logo_fetch.{with_request=(fun f -> f ()); resolve=(fun ~host:_ -> Ok ["8.8.8.8"]);
    get=(fun ~uri:_ ~address:_ ~max_bytes:_ -> Ok {status=200; headers=[]; body=svg})} "shop.com/logo.svg" |> Result.get_ok in
  let served = fetch (save "shop" image) in
  Alcotest.(check int) "a 16 px SVG is served" 200 served.status;
  Alcotest.(check (option string)) "type" (Some "image/svg+xml") (header served "content-type");
  Alcotest.(check string) "cleaned bytes" (Logo_fetch.bytes image) served.body;
  Alcotest.(check bool) "sandboxed, loads nothing" true
    (List.exists (fun csp -> Str_compat.contains ~sub:"default-src 'none'" csp && Str_compat.contains ~sub:"sandbox" csp)
       (headers_named served "content-security-policy"));
  let id = save "raw" image in
  Sql.exec_script deps.service.db
    (Printf.sprintf "UPDATE logos SET bytes = CAST('%s' AS BLOB) WHERE id = %d" (Str_compat.replace_all ~sub:"'" ~by:"''" svg) id);
  Alcotest.(check int) "an SVG stored without cleaning is not served" 404 (fetch id).status

(* A browser that accepts gzip gets large text gzipped, and what it unpacks is the original; anything else is left alone. *)
let test_gzip () =
  let body = String.concat "," (List.init 500 (fun i -> Printf.sprintf {|{"n":%d}|} i)) in
  let response : Http_server.response = { status = 200; headers = [ ("content-type", "application/json") ]; body } in
  let ask value = Httpun.Request.create ~headers:(Httpun.Headers.of_list (match value with Some v -> [ ("accept-encoding", v) ] | None -> [])) `GET "/" in
  let zipped = Http_server.compressed (ask (Some "br, gzip;q=0.8")) response in
  Alcotest.(check (option string)) "labelled" (Some "gzip") (header zipped "content-encoding");
  Alcotest.(check bool) "smaller" true (String.length zipped.body < String.length body);
  let n = String.length zipped.body in
  Alcotest.(check string) "gzip header" "\x1f\x8b" (String.sub zipped.body 0 2);
  (match Zipc_deflate.inflate ~start:10 ~len:(n - 18) zipped.body with
  | Ok plain -> Alcotest.(check string) "round trip" body plain
  | Error e -> Alcotest.fail e);
  Alcotest.(check (option string)) "not asked" None (header (Http_server.compressed (ask None) response) "content-encoding");
  Alcotest.(check (option string)) "small bodies" None
    (header (Http_server.compressed (ask (Some "gzip")) { response with body = "{}" }) "content-encoding");
  Alcotest.(check (option string)) "images" None
    (header (Http_server.compressed (ask (Some "gzip")) { response with headers = [ ("content-type", "image/png") ] }) "content-encoding")

let () =
  Alcotest.run "http"
    [ ( "routes",
        [ Alcotest.test_case "gzip" `Quick test_gzip; Alcotest.test_case "reporting envelopes" `Quick test_reporting_envelopes;
          Alcotest.test_case "unversioned assets revalidate" `Quick test_unversioned_assets_revalidate;
          Alcotest.test_case "private logos" `Quick test_private_logos;
          Alcotest.test_case "SVG logos" `Quick test_svg_logos;
          Alcotest.test_case "health and metadata" `Quick test_health_and_metadata;
          Alcotest.test_case "mcp requires a token" `Quick test_mcp_requires_a_token; Alcotest.test_case "mcp tools" `Quick test_mcp_tools;
          Alcotest.test_case "mcp scopes" `Quick test_mcp_scopes; Alcotest.test_case "web login and csrf" `Quick test_web_login_and_csrf;
          Alcotest.test_case "login return paths" `Quick test_login_return_paths;
          Alcotest.test_case "static caching and compression" `Quick test_static_caching_and_compression;
          Alcotest.test_case "sign-in keeps starter names" `Quick test_sign_in_keeps_starter_names;
          Alcotest.test_case "static paths stay inside" `Quick test_static_paths_stay_inside ] ) ]
