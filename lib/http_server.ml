(* HTTP routing as a pure function from request to response, so every route is tested without a
   socket; [run] puts it behind httpun (as kcal's lib/http_adapter.ml does). *)

module Access = Cashflow_core.Access
module I18n_locale = Cashflow_core.I18n_locale

let log_src = Logs.Src.create "cashflow.http" ~doc:"HTTP request handling"
module Log = (val Logs.src_log log_src : Logs.LOG)

type meth = GET | POST | HEAD | OTHER
type request = { meth : meth; target : string; headers : (string * string) list; body : string }
type response = { status : int; headers : (string * string) list; body : string }

type deps = {
  service : Service.t;
  verify_bearer : Oidc.t;
  web_auth : Web_auth.t option;
  config : Config.t;
  now : unit -> Ptime.t;
}

let security_headers =
  [ ("x-content-type-options", "nosniff"); ("referrer-policy", "same-origin"); ("x-frame-options", "DENY");
    ("content-security-policy", "default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'; frame-ancestors 'none'; base-uri 'none'") ]

let respond ?(headers = []) status content_type body = { status; headers = (("content-type", content_type) :: headers) @ security_headers; body }
let json ?headers status value = respond ?headers status "application/json" (Yojson.Safe.to_string value)
let plain status body = respond status "text/plain; charset=utf-8" body
let redirect ?(headers = []) location = { status = 302; headers = ("location", location) :: headers; body = "" }
let error_json status message = json status (`Assoc [ ("error", `String message) ])
let header (request : request) name = List.find_map (fun (k, v) -> if String.lowercase_ascii k = name then Some v else None) request.headers

let is_json (request : request) =
  match header request "content-type" with
  | Some value -> String.trim (List.hd (String.split_on_char ';' (String.lowercase_ascii value))) = "application/json"
  | None -> false

let parse_json body = try Ok (Yojson.Safe.from_string body) with Yojson.Json_error message -> Error message

(* Bearer tokens (MCP) *)

let protected_resource deps = (deps.config.oidc_audience, Config.protected_resource_metadata_url ~audience:deps.config.oidc_audience)

let unauthorized deps =
  let _, metadata_url = protected_resource deps in
  { (plain 401 "Unauthorized") with headers = ("www-authenticate", Printf.sprintf {|Bearer resource_metadata="%s"|} metadata_url) :: security_headers }

let bearer (request : request) =
  match header request "authorization" with
  | Some value when String.starts_with ~prefix:"Bearer " value && String.length value > 7 -> Some (String.sub value 7 (String.length value - 7))
  | _ -> None

let bearer_access deps (request : request) =
  match bearer request with
  | None -> None
  | Some token -> (
      match deps.verify_bearer token with
      | Error () -> None
      | Ok claims ->
          let access =
            Service.sign_in deps.service
              { issuer = claims.issuer; subject = claims.subject; display_name = claims.name; email = claims.email;
                is_admin = Option.map (List.mem deps.config.admin_group) claims.groups; locale = None }
          in
          Some (access, claims.scopes))

let mcp deps (request : request) =
  match bearer_access deps request with
  | None -> unauthorized deps
  | Some (access, scopes) when not (is_json request) -> ignore (access, scopes); plain 415 "Unsupported Media Type"
  | Some (access, scopes) -> (
      match parse_json request.body with
      | Error _ -> json 400 (Mcp.rpc_error (-32700) "Parse error")
      | Ok message -> (
          match Mcp.handle deps.service access ~scopes message with
          | Ok `Null -> plain 202 ""
          | Ok response -> json 200 response
          | Error `Forbidden ->
              { (plain 403 "Forbidden") with headers = ("www-authenticate", {|Bearer error="insufficient_scope"|}) :: security_headers }))

let protected_resource_metadata deps =
  let resource, _ = protected_resource deps in
  json 200
    (`Assoc
       [ ("resource", `String resource); ("authorization_servers", `List [ `String deps.config.oidc_issuer ]);
         ("scopes_supported", `List [ `String "ledger:read"; `String "ledger:write"; `String "ledger:admin" ]) ])

(* Web sessions *)

let web_scopes access = [ "ledger:read"; "ledger:write" ] @ if Access.is_admin access then [ "ledger:admin" ] else []

let session deps (request : request) =
  match Web_auth.cookie_value request.headers Web_auth.session_cookie with
  | None -> None
  | Some token -> Web_auth.session deps.service ~now:(deps.now ()) token

let login deps (request : request) query =
  match deps.web_auth with
  | None -> plain 404 "Not Found"
  | Some auth -> (
      ignore request;
      let return_to = match List.assoc_opt "return_to" query with Some [ path ] -> path | _ -> "/" in
      match Web_auth.begin_login auth deps.service.db ~now:(deps.now ()) ~return_to with
      | Ok (url, state) -> redirect url ~headers:[ ("set-cookie", Web_auth.cookie ~max_age:Web_auth.login_lifetime_s Web_auth.login_cookie state) ]
      | Error reason ->
          Log.err (fun m -> m "sign-in could not start: %s" reason);
          plain 502 "Sign-in is unavailable")

let callback deps (request : request) query =
  match deps.web_auth with
  | None -> plain 404 "Not Found"
  | Some auth -> (
      match List.assoc_opt "state" query, List.assoc_opt "code" query with
      | Some [ state ], Some [ code ] -> (
          let cookie_state = Web_auth.cookie_value request.headers Web_auth.login_cookie in
          (* A new user starts in the browser's language: "sv-SE,sv;q=0.9,en" gives sv. *)
          let locale =
            Option.map
              (fun value ->
                I18n_locale.negotiate
                  (List.map (fun part -> String.trim (List.hd (String.split_on_char '-' (List.hd (String.split_on_char ';' part))))) (String.split_on_char ',' value)))
              (header request "accept-language")
          in
          match Web_auth.finish_login auth deps.service ?locale ~now:(deps.now ()) ~state ~code ~cookie_state () with
          | Ok (token, return_to) ->
              { (redirect return_to) with
                headers =
                  [ ("location", return_to); ("set-cookie", Web_auth.cookie Web_auth.session_cookie token);
                    ("set-cookie", Web_auth.clear_cookie Web_auth.login_cookie) ] }
          | Error reason ->
              Log.warn (fun m -> m "sign-in failed: %s" reason);
              plain 400 ("Sign-in failed: " ^ reason))
      | _ -> plain 400 "Sign-in failed: missing state or code")

let logout deps (request : request) =
  Option.iter (Web_auth.sign_out deps.service.db) (Web_auth.cookie_value request.headers Web_auth.session_cookie);
  { (json 200 (`Assoc [ ("status", `String "signed_out") ])) with
    headers = ("set-cookie", Web_auth.clear_cookie Web_auth.session_cookie) :: ("content-type", "application/json") :: security_headers }

let api_status = function
  | `Forbidden _ -> 403
  | `Not_found _ -> 404
  | `Storage_unavailable _ -> 500
  | `Invalid _ | `Unreadable _ -> 422

(* State-changing calls carry the session's CSRF token in a header, which another site can't read
   or set; SameSite=Lax cookies are the second line of defence. *)
let api_command deps (request : request) name =
  match session deps request with
  | None -> error_json 401 "not signed in"
  | Some (access, csrf) -> (
      if header request "x-csrf-token" <> Some csrf then error_json 403 "missing or wrong CSRF token"
      else if not (is_json request) then error_json 415 "send JSON"
      else
        match Api.find name, parse_json request.body with
        | None, _ -> error_json 404 ("unknown command " ^ name)
        | _, Error _ -> error_json 400 "malformed JSON"
        | Some command, Ok args -> (
            match Commands.execute deps.service access ~scopes:(web_scopes access) command args with
            | Ok value -> json 200 value
            | Error (`Storage_unavailable _) -> error_json 500 "internal error"
            | Error error -> error_json (api_status error) (Import.error_to_string error)
            | exception Sql.Sql_error message ->
                Log.err (fun m -> m "command=%s storage error: %s" name message);
                error_json 500 "internal error"))

let api_session deps (request : request) =
  match session deps request with
  | None -> error_json 401 "not signed in"
  | Some (access, csrf) -> (
      match Commands.session_json deps.service access with
      | `Assoc fields -> json 200 (`Assoc (("csrf_token", `String csrf) :: ("web_login", `Bool (Option.is_some deps.web_auth)) :: fields))
      | other -> json 200 other)

let api_commands deps (request : request) =
  match session deps request with
  | None -> error_json 401 "not signed in"
  | Some (access, _) ->
      let scopes = web_scopes access in
      json 200
        (`Assoc
           [ ( "commands",
               `List
                 (List.filter_map
                    (fun (c : Commands.command) ->
                      if Commands.allowed ~scopes access c then Some (`Assoc [ ("name", `String c.name); ("description", `String c.description); ("schema", Commands.schema c) ])
                      else None)
                    (Api.all ())) ) ])

(* Static files: the Bonsai bundle. Paths are confined to the static directory; anything that
   isn't an asset gets index.html, so the UI's own routes work on reload. *)
let content_type path =
  match Filename.extension path with
  | ".js" -> "text/javascript; charset=utf-8"
  | ".css" -> "text/css; charset=utf-8"
  | ".html" -> "text/html; charset=utf-8"
  | ".svg" -> "image/svg+xml"
  | ".png" -> "image/png"
  | ".webmanifest" | ".json" -> "application/json"
  | ".woff2" -> "font/woff2"
  | _ -> "application/octet-stream"

let read_file path = try Some (In_channel.with_open_bin path In_channel.input_all) with Sys_error _ -> None

let static deps path =
  let safe = not (List.mem ".." (String.split_on_char '/' path)) in
  let file =
    if safe && String.starts_with ~prefix:"/assets/" path then Filename.concat deps.config.static_dir (String.sub path 8 (String.length path - 8))
    else Filename.concat deps.config.static_dir "index.html"
  in
  match read_file file with
  | Some body ->
      let cache = if String.starts_with ~prefix:"/assets/" path then "public, max-age=31536000, immutable" else "no-cache" in
      respond ~headers:[ ("cache-control", cache) ] 200 (content_type file) body
  | None -> if String.starts_with ~prefix:"/assets/" path then plain 404 "Not Found" else plain 503 "The web UI is not installed"

let handle deps (request : request) =
  let uri = Uri.of_string request.target in
  let path = Uri.path uri and query = Uri.query uri in
  let metadata_path = Uri.path (Uri.of_string (snd (protected_resource deps))) in
  match request.meth, path with
  | GET, "/health" -> json 200 (`Assoc [ ("status", `String "ok") ])
  | GET, p when p = "/.well-known/oauth-protected-resource" || p = metadata_path -> protected_resource_metadata deps
  | POST, "/mcp" -> mcp deps request
  | GET, "/auth/login" -> login deps request query
  | GET, "/auth/callback" -> callback deps request query
  | POST, "/auth/logout" -> logout deps request
  | GET, "/api/session" -> api_session deps request
  | GET, "/api/commands" -> api_commands deps request
  | POST, p when String.starts_with ~prefix:"/api/commands/" p -> api_command deps request (String.sub p 14 (String.length p - 14))
  | _, p when String.starts_with ~prefix:"/api/" p || p = "/mcp" -> error_json 404 "not found"
  | (GET | HEAD), p -> static deps p
  | _ -> plain 404 "Not Found"

(* The server: from kcal's lib/http_adapter.ml. *)

let max_body_bytes = 8 * 1024 * 1024

let split_address value =
  match String.rindex_opt value ':' with
  | Some index when index > 0 && index < String.length value - 1 ->
      (String.sub value 0 index, String.sub value (index + 1) (String.length value - index - 1))
  | _ -> invalid_arg "invalid listen address"

let status_of_int code = Httpun.Status.of_code code

let respond_with reqd response =
  let headers =
    List.fold_left (fun headers (key, value) -> Httpun.Headers.add_multi headers [ (key, [ value ]) ]) Httpun.Headers.empty response.headers
  in
  let headers = Httpun.Headers.add (Httpun.Headers.remove headers "content-length") "content-length" (string_of_int (String.length response.body)) in
  Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers (status_of_int response.status)) response.body

let serve_request deps reqd =
  let request = Httpun.Reqd.request reqd in
  let meth = match request.meth with `GET -> GET | `POST -> POST | `HEAD -> HEAD | _ -> OTHER in
  let path = match String.index_opt request.target '?' with Some i -> String.sub request.target 0 i | None -> request.target in
  let finish body =
    let response =
      try handle deps { meth; target = request.target; headers = Httpun.Headers.to_list request.headers; body }
      with exn ->
        Log.err (fun m -> m "path=%s raised %s" path (Printexc.to_string exn));
        plain 500 "Internal Server Error"
    in
    let response = if meth = HEAD then { response with body = "" } else response in
    let log = if response.status >= 500 then Log.err else if response.status >= 400 then Log.warn else Log.info in
    log (fun m -> m "method=%s path=%s status=%d" (Httpun.Method.to_string request.meth) path response.status);
    respond_with reqd response
  in
  let buffer = Buffer.create 1024 and responded = ref false in
  let once f = if not !responded then (responded := true; f ()) in
  let rec read () =
    Httpun.Body.Reader.schedule_read (Httpun.Reqd.request_body reqd)
      ~on_eof:(fun () -> once (fun () -> finish (Buffer.contents buffer)))
      ~on_read:(fun bytes ~off ~len ->
        if Buffer.length buffer + len > max_body_bytes then once (fun () -> respond_with reqd (plain 413 "Payload Too Large"))
        else (Buffer.add_string buffer (Bigstringaf.substring bytes ~off ~len); read ()))
  in
  read ()

let run env deps : 'a =
  let host, port = split_address deps.config.listen_address in
  Eio.Switch.run @@ fun sw ->
  let address = List.hd (Eio.Net.getaddrinfo_stream ~service:port env#net host) in
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 env#net address in
  Log.app (fun m -> m "listening on %s" deps.config.listen_address);
  let handler =
    Httpun_eio.Server.create_connection_handler ~sw
      ~request_handler:(fun _ reqd -> serve_request deps reqd.Gluten.Reqd.reqd)
      ~error_handler:(fun _ ?request:_ _ _ -> ())
  in
  Eio.Net.run_server ~on_error:(fun exn -> Log.err (fun m -> m "connection error: %s" (Printexc.to_string exn))) listener (fun socket address -> handler address socket)
