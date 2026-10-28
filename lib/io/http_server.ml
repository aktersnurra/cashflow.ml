(* HTTP routing as a pure function from request to response, so every route is tested without a
   socket; [run] puts it behind httpun (as kcal's lib/http_adapter.ml does). *)

module Access = Cashflow_core.Access
module I18n_locale = Cashflow_core.I18n_locale
module Owner = Cashflow_core.Owner

let log_src = Logs.Src.create "cashflow.http" ~doc:"HTTP request handling"
module Log = (val Logs.src_log log_src : Logs.LOG)

type meth = GET | POST | HEAD | OTHER
type request = { meth : meth; target : string; headers : (string * string) list; body : string }
type response = { status : int; headers : (string * string) list; body : string }

type deps = {
  service : Service.t;
  advisor : Advisor_http.t option;
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
                locale = None }
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

let web_scopes = [ "ledger:read"; "ledger:write"; "ledger:admin" ]

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
  | `Logo (code, _) -> if code = "forbidden" then 403 else 422

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
            match Commands.execute deps.service access ~scopes:(web_scopes) command args with
            | Ok value -> json 200 value
            | Error (`Storage_unavailable _) -> error_json 500 "internal error"
            | Error (`Logo (code, message)) -> json (api_status (`Logo (code, message))) (`Assoc ["error", `String message; "code", `String code])
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
  | Some _ ->
      let scopes = web_scopes in
      json 200
        (`Assoc
           [ ( "commands",
               `List
                 (List.filter_map
                    (fun (c : Commands.command) ->
                      if Commands.allowed ~scopes c then Some (`Assoc [ ("name", `String c.name); ("description", `String c.description); ("schema", Commands.schema c) ])
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

(* Text assets are sent gzipped to clients that accept it: the bundle is 1.9 MB and about a third of that compressed,
   which is most of the wait on a slow link. Compressing takes a fraction of a second, so each distinct file content
   is compressed once and kept; the key is the content's own hash, so a new build can never be served the old bytes. *)
let compressible file = List.mem (Filename.extension file) [ ".js"; ".css"; ".html"; ".svg"; ".webmanifest"; ".json" ]
let gzip_cache : (string, string option) Hashtbl.t = Hashtbl.create 8

let gzip ?(level = `Best) body =
  match Zipc_deflate.crc_32_and_deflate ~level body with
  | Error _ -> None
  | Ok (crc, deflated) ->
      let le32 n = String.init 4 (fun i -> Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical n (8 * i)) 0xffl))) in
      Some ("\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\xff" ^ deflated ^ le32 crc ^ le32 (Int32.of_int (String.length body)))

let gzipped ~digest body =
  match Hashtbl.find_opt gzip_cache digest with
  | Some compressed -> compressed
  | None ->
      if Hashtbl.length gzip_cache >= 16 then Hashtbl.reset gzip_cache;
      let compressed = gzip body in
      Hashtbl.replace gzip_cache digest compressed;
      compressed

(* Whether the request already has this exact copy ([If-None-Match]), so only a 304 need be sent. *)
let has_copy (request : request) etag =
  match header request "if-none-match" with
  | None -> false
  | Some value ->
      List.exists
        (fun tag ->
          let tag = String.trim tag in
          let tag = if String.starts_with ~prefix:"W/" tag then String.sub tag 2 (String.length tag - 2) else tag in
          tag = etag || tag = "*")
        (String.split_on_char ',' value)

(* Files whose URL carries their content hash ([?v=...], see web/build/version_assets.ml) never change under that URL, so
   they are cached for a year and a refresh downloads nothing. Everything else is revalidated with its ETag. *)
let static deps (request : request) path query =
  let safe = not (List.mem ".." (String.split_on_char '/' path)) in
  let asset = safe && String.starts_with ~prefix:"/assets/" path in
  let file =
    if asset then Filename.concat deps.config.static_dir (String.sub path 8 (String.length path - 8))
    else Filename.concat deps.config.static_dir "index.html"
  in
  match read_file file with
  | Some body ->
      let digest = Digest.to_hex (Digest.string body) in
      let cache = if asset && List.mem_assoc "v" query then "public, max-age=31536000, immutable" else "no-cache" in
      let accepts_gzip =
        match header request "accept-encoding" with
        | Some value -> Cashflow_core.Str_compat.contains ~sub:"gzip" (String.lowercase_ascii value)
        | None -> false
      in
      let compressed = if accepts_gzip && compressible file && String.length body >= 1024 then gzipped ~digest body else None in
      let etag, body, encoding =
        match compressed with
        | Some bytes -> (Printf.sprintf "\"%s-gzip\"" digest, bytes, [ ("content-encoding", "gzip") ])
        | None -> (Printf.sprintf "\"%s\"" digest, body, [])
      in
      let headers = ("cache-control", cache) :: ("etag", etag) :: ("vary", "accept-encoding") :: encoding in
      if has_copy request etag then respond ~headers 304 (content_type file) "" else respond ~headers 200 (content_type file) body
  | None -> if String.starts_with ~prefix:"/assets/" path then plain 404 "Not Found" else plain 503 "The web UI is not installed"

let api_logo deps request id =
  let access = match session deps request with
    | Some (access, _) -> Some access
    | None -> (match bearer_access deps request with
      | Some (access, scopes) when List.mem "ledger:read" scopes || List.mem "ledger:write" scopes -> Some access
      | _ -> None) in
  let id = try let n = int_of_string id in
    if n > 0 && string_of_int n = id then Some n else None with Failure _ -> None in
  let blob = match access, id with
    | Some access, Some id ->
        let owners = Access.personal access :: (match Access.household access with
          | None -> [] | Some h -> [Owner.Household h.id]) in
        List.find_map (fun owner -> Merchant_logos.blob deps.service access ~owner ~id) owners
    | _ -> None in
  let response = match blob with
  | None -> plain 404 "Not Found"
  | Some (blob : Merchant_logo_store.blob) ->
      (match Logo_fetch.stored blob.bytes with
       | Some (content_type, width, height) when content_type = blob.content_type
           && (content_type = "image/svg+xml" || (width >= 32 && height >= 32))
           && String.length blob.bytes <= 256 * 1024 ->
           (* Opened at its own URL an SVG is a document of this origin: it may load and run nothing. *)
           let sandbox = if content_type = "image/svg+xml"
             then [ "content-security-policy", "default-src 'none'; style-src 'unsafe-inline'; sandbox" ] else [] in
           respond ~headers:(("content-length", string_of_int (String.length blob.bytes)) :: sandbox) 200 content_type
             (if request.meth = HEAD then "" else blob.bytes)
       | _ -> plain 404 "Not Found") in
  (* Every image request must check current session and ownership, including after logout. *)
  {response with headers=("cache-control", "private, no-store") :: response.headers}

let handle_legacy deps (request : request) =
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
  | (GET | HEAD), p when String.starts_with ~prefix:"/api/logos/" p -> api_logo deps request (String.sub p 11 (String.length p - 11))
  | GET, "/api/session" -> api_session deps request
  | GET, "/api/commands" -> api_commands deps request
  | POST, p when String.starts_with ~prefix:"/api/commands/" p -> api_command deps request (String.sub p 14 (String.length p - 14))
  | _, p when String.starts_with ~prefix:"/api/" p || p = "/mcp" -> error_json 404 "not found"
  | (GET | HEAD), p -> static deps request p query
  | _ -> plain 404 "Not Found"

let advisor_request (request : request) : Advisor_http.request =
  {
    meth = (match request.meth with GET -> GET | POST -> POST | HEAD -> HEAD | OTHER -> OTHER);
    target = request.target;
    headers = request.headers;
    body = request.body;
  }

let advisor_handle deps request =
  match deps.advisor with
  | None -> None
  | Some advisor -> (
      let req = advisor_request request in
      match (request.meth, Uri.path (Uri.of_string request.target)) with
      | POST, "/auth/logout" -> Some (Advisor_http.logout advisor req)
      | POST, "/api/commands/leave_household" -> Some (Advisor_http.leave_household advisor req)
      | _ -> Advisor_http.handle advisor req)

let buffered = function
  | Advisor_http.Buffered (status, headers, body) -> { status; headers; body }
  | Stream _ -> invalid_arg "stream requires native writer"

let handle deps request =
  match advisor_handle deps request with
  | Some (Buffered _ as r) -> buffered r
  | Some (Stream subscription) ->
      Advisor_http.close subscription;
      plain 500 "Streaming requires native transport"
  | None -> handle_legacy deps request

(* The server: from kcal's lib/http_adapter.ml. *)

let max_body_bytes = 8 * 1024 * 1024

let split_address value =
  match String.rindex_opt value ':' with
  | Some index when index > 0 && index < String.length value - 1 ->
      (String.sub value 0 index, String.sub value (index + 1) (String.length value - index - 1))
  | _ -> invalid_arg "invalid listen address"

let status_of_int code = Httpun.Status.of_code code

(* API answers are text too, and a list of transactions is several times smaller gzipped. They differ on every
   request, so they are compressed quickly and not kept. *)
let compressed (request : Httpun.Request.t) response =
  let accepts =
    match Httpun.Headers.get request.headers "accept-encoding" with
    | Some value -> Cashflow_core.Str_compat.contains ~sub:"gzip" (String.lowercase_ascii value)
    | None -> false
  in
  if request.meth = `HEAD || response.status <> 200 || String.length response.body < 1024 || not accepts
     || List.assoc_opt "content-type" response.headers <> Some "application/json"
     || List.mem_assoc "content-encoding" response.headers || List.mem_assoc "content-length" response.headers
  then response
  else
    match gzip ~level:`Fast response.body with
    | Some body -> { response with body; headers = ("content-encoding", "gzip") :: ("vary", "accept-encoding") :: response.headers }
    | None -> response

let respond_with reqd response =
  let response = compressed (Httpun.Reqd.request reqd) response in
  let headers =
    List.fold_left (fun headers (key, value) -> Httpun.Headers.add_multi headers [ (key, [ value ]) ]) Httpun.Headers.empty response.headers
  in
  let headers = if Httpun.Headers.mem headers "content-length" then headers
    else Httpun.Headers.add headers "content-length" (string_of_int (String.length response.body)) in
  Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers (status_of_int response.status)) response.body

let finish_stream ~sw ~sleep_s ~abort = function
  | Advisor_http.Aborted -> abort ()
  | Drained ->
      (* Application frames are acknowledged; bound only the remaining HTTP/1.x drain. *)
      Eio.Fiber.fork_daemon ~sw (fun () ->
          sleep_s 1.;
          abort ();
          `Stop_daemon)

let serve_request ~sw ~on_stream_end deps reqd =
  let request = Httpun.Reqd.request reqd in
  let meth = match request.meth with `GET -> GET | `POST -> POST | `HEAD -> HEAD | _ -> OTHER in
  let finish body =
    let req = { meth; target = request.target; headers = Httpun.Headers.to_list request.headers; body } in
    try
      match advisor_handle deps req with
      | Some (Advisor_http.Stream subscription) ->
          Eio.Fiber.fork ~sw (fun () ->
              match Advisor_http.stream subscription reqd with
              | result -> on_stream_end result
              | exception exn ->
                  on_stream_end Advisor_http.Aborted;
                  raise exn)
      | advisor_response ->
          let response = match advisor_response with Some r -> buffered r | None -> handle_legacy deps req in
          let response = if meth = HEAD then { response with body = "" } else response in
          (if Option.is_none advisor_response then
             let path = Uri.path (Uri.of_string request.target) in
             let log =
               if response.status >= 500 then Log.err else if response.status >= 400 then Log.warn else Log.info
             in
             log (fun m -> m "method=%s path=%s status=%d" (Httpun.Method.to_string request.meth) path response.status));
          respond_with reqd response
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ ->
        Log.err (fun m -> m "HTTP request failed");
        respond_with reqd (plain 500 "Internal Server Error")
  in
  let buffer = Buffer.create 1024 and responded = ref false in
  let once f =
    if not !responded then (
      responded := true;
      f ())
  in
  let rec read () =
    Httpun.Body.Reader.schedule_read (Httpun.Reqd.request_body reqd)
      ~on_eof:(fun () -> once (fun () -> finish (Buffer.contents buffer)))
      ~on_read:(fun bytes ~off ~len ->
        if Buffer.length buffer + len > max_body_bytes then
          once (fun () -> respond_with reqd (plain 413 "Payload Too Large"))
        else (
          Buffer.add_string buffer (Bigstringaf.substring bytes ~off ~len);
          read ()))
  in
  read ()

let run env deps : 'a =
  let host, port = split_address deps.config.listen_address in
  Eio.Switch.run @@ fun sw ->
  let address = List.hd (Eio.Net.getaddrinfo_stream ~service:port env#net host) in
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 env#net address in
  Log.app (fun m -> m "listening on %s" deps.config.listen_address);
  Eio.Net.run_server
    ~on_error:(fun _ -> Log.err (fun m -> m "connection error"))
    listener
    (fun socket address ->
      try
        Eio.Cancel.sub (fun context ->
            Eio.Switch.run (fun connection_sw ->
                let handler =
                  Httpun_eio.Server.create_connection_handler ~sw:connection_sw
                    ~request_handler:(fun _ reqd ->
                      serve_request ~sw:connection_sw
                        ~on_stream_end:
                          (finish_stream ~sw:connection_sw ~sleep_s:(Eio.Time.sleep env#clock) ~abort:(fun () ->
                               Eio.Cancel.cancel context Exit))
                        deps reqd.Gluten.Reqd.reqd)
                    ~error_handler:(fun _ ?request:_ _ _ -> ())
                in
                Fun.protect ~finally:(fun () -> Eio.Cancel.cancel context Exit) (fun () -> handler address socket)))
      with Eio.Cancel.Cancelled Exit -> ())
