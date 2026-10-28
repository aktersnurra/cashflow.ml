(* Signing in to the web UI. The server runs the OpenID Connect authorization code flow with PKCE
   itself (the backend-for-frontend pattern): the browser only ever holds an HttpOnly session
   cookie and never sees a token. The login state is bound to the browser with a short-lived
   cookie, the ID token's nonce is checked, and only hashes of session tokens are stored. *)
module Access = Cashflow_core.Access
module User = Cashflow_core.User
module User_id = Cashflow_core.User_id

open Sql

let log_src = Logs.Src.create "cashflow.web_auth" ~doc:"Web sign-in and sessions"
module Log = (val Logs.src_log log_src : Logs.LOG)

type config = { issuer : string; client_id : string; client_secret : string; redirect_uri : string }

type t = { config : config; client : Oidc.client; verify_id_token : Oidc.t; discovery : Oidc.discovery option ref }

let make ~config ~client ~verify_id_token = { config; client; verify_id_token; discovery = ref None }

let rng = lazy (Mirage_crypto_rng_unix.use_default ())
let random_token bytes = Lazy.force rng; Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet (Mirage_crypto_rng.generate bytes)
let hash value = Digestif.SHA256.(to_hex (digest_string value))
let s256 verifier = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet Digestif.SHA256.(to_raw_string (digest_string verifier))
let session_lifetime_s = 30 * 86_400
let login_lifetime_s = 600
let session_cookie = "cashflow_session"
let login_cookie = "cashflow_login"
let add_seconds time seconds = Option.get (Ptime.add_span time (Ptime.Span.of_int_s seconds))

let discovery auth =
  match !(auth.discovery) with
  | Some d -> Ok d
  | None -> (
      match auth.client.discover ~issuer:auth.config.issuer with
      | Ok d -> auth.discovery := Some d; Ok d
      | Error () -> Error "identity provider discovery failed")

let insert_attempt =
  stmt "INSERT INTO login_attempts (state_hash, code_verifier, nonce, return_to, created_at) VALUES (?, ?, ?, ?, ?)"
    Codec.(text @-> text @-> text @-> text @-> time @-> nil) exec

let take_attempt =
  stmt "SELECT code_verifier, nonce, return_to, created_at FROM login_attempts WHERE state_hash = ?" (text @-> nil)
    (opt (row Codec.(text @-> text @-> text @-> time @-> nil) (fun v n r c -> (v, n, r, c))))

let delete_attempt = stmt "DELETE FROM login_attempts WHERE state_hash = ? OR created_at < ?" Codec.(text @-> time @-> nil) exec

(* Only same-site paths are allowed as a destination after sign-in. *)
let safe_return_to path =
  (* Browsers treat backslashes as slashes and strip some controls while resolving redirects. *)
  let is_unsafe c = c = '\\' || Char.code c < 0x20 || Char.code c = 0x7f in
  if String.starts_with ~prefix:"/" path && not (String.starts_with ~prefix:"//" path) && not (String.exists is_unsafe path) then path else "/"

let begin_login auth db ~now ~return_to =
  match discovery auth with
  | Error _ as e -> e
  | Ok d -> (
      match d.authorization_endpoint with
      | None -> Error "the identity provider has no authorization endpoint"
      | Some endpoint ->
          let state = random_token 24 and verifier = random_token 32 and nonce = random_token 24 in
          run db insert_attempt (hash state) verifier nonce (safe_return_to return_to) now;
          let uri =
            Uri.add_query_params' (Uri.of_string endpoint)
              [ ("response_type", "code"); ("client_id", auth.config.client_id); ("redirect_uri", auth.config.redirect_uri);
                ("scope", "openid profile email"); ("state", state); ("nonce", nonce); ("code_challenge", s256 verifier);
                ("code_challenge_method", "S256") ]
          in
          Ok (Uri.to_string uri, state))

let insert_session =
  stmt "INSERT INTO sessions (token_hash, user_id, csrf_token, created_at, last_seen_at, expires_at) VALUES (?, ?, ?, ?, ?, ?)"
    Codec.(text @-> user_id @-> text @-> time @-> time @-> time @-> nil) exec

let session_by_hash =
  stmt "SELECT user_id, csrf_token, expires_at FROM sessions WHERE token_hash = ?" (text @-> nil)
    (opt (row Codec.(user_id @-> text @-> time @-> nil) (fun u c e -> (u, c, e))))

let touch_session = stmt "UPDATE sessions SET last_seen_at = ?, expires_at = ? WHERE token_hash = ?" Codec.(time @-> time @-> text @-> nil) exec
let delete_session = stmt "DELETE FROM sessions WHERE token_hash = ?" (text @-> nil) exec

let create_session db ~now (user : User.t) =
  let token = random_token 32 and csrf = random_token 24 in
  run db insert_session (hash token) user.id csrf now now (add_seconds now session_lifetime_s);
  token

let finish_login auth (service : Service.t) ?locale ~now ~state ~code ~cookie_state () =
  let db = service.db in
  let state_hash = hash state in
  let attempt = run db take_attempt state_hash in
  run db delete_attempt state_hash (add_seconds now (-login_lifetime_s));
  match attempt, discovery auth with
  | None, _ -> Error "unknown or expired sign-in attempt"
  | _, Error e -> Error e
  | Some (verifier, nonce, return_to, created_at), Ok d -> (
      if cookie_state <> Some state then Error "the sign-in was started in another browser"
      else if Ptime.compare (add_seconds created_at login_lifetime_s) now < 0 then Error "the sign-in attempt expired"
      else
        match d.token_endpoint with
        | None -> Error "the identity provider has no token endpoint"
        | Some token_endpoint -> (
            let form =
              [ ("grant_type", "authorization_code"); ("code", code); ("redirect_uri", auth.config.redirect_uri);
                ("client_id", auth.config.client_id); ("client_secret", auth.config.client_secret); ("code_verifier", verifier) ]
            in
            match auth.client.post_form ~uri:token_endpoint form with
            | Error () -> Error "exchanging the authorization code failed"
            | Ok body -> (
                match (try Some (Yojson.Safe.from_string body) with Yojson.Json_error _ -> None) with
                | Some (`Assoc fields) -> (
                    match List.assoc_opt "id_token" fields with
                    | Some (`String id_token) -> (
                        match auth.verify_id_token id_token with
                        | Error () -> Error "the ID token did not verify"
                        | Ok claims when claims.nonce <> Some nonce -> ignore claims; Error "the ID token's nonce does not match"
                        | Ok claims ->
                            let access =
                              Service.sign_in service
                                { issuer = claims.issuer; subject = claims.subject; display_name = claims.name; email = claims.email; locale }
                            in
                            let user = Access.user access in
                            Log.info (fun m -> m "user=%s signed in" (User_id.to_string user.id));
                            Ok (create_session db ~now user, return_to))
                    | _ -> Error "the token response has no ID token")
                | _ -> Error "the token response is not JSON")))

(* The session's user and CSRF token, extending the session while it is in use. *)
let session (service : Service.t) ~now token =
  let token_hash = hash token in
  match run service.db session_by_hash token_hash with
  | Some (user_id, csrf, expires_at) when Ptime.compare now expires_at < 0 -> (
      match Directory.find_user service.db user_id with
      | Some user ->
          run service.db touch_session now (add_seconds now session_lifetime_s) token_hash;
          Some (Service.access_for service user, csrf)
      | None -> None)
  | _ -> None

module A = Cashflow_core.Advisor

type session_identity = {
  user_id : User_id.t;
  csrf_token : string;
  expires_at : Ptime.t;
  reference : A.Session_ref.t;
}

let session_ref token = Option.get (A.Session_ref.of_hash (hash token))

(* Inspection deliberately does not use [session], which extends the lifetime. *)
let inspect_session (service : Service.t) ~now reference =
  match run service.db session_by_hash (A.Session_ref.to_hash reference) with
  | Some (user_id, csrf_token, expires_at) when Ptime.compare now expires_at < 0 ->
      Some { user_id; csrf_token; expires_at; reference }
  | _ -> None

let sign_out db token = run db delete_session (hash token)

let cookie ?(max_age = session_lifetime_s) name value =
  Printf.sprintf "%s=%s; Path=/; HttpOnly; Secure; SameSite=Lax; Max-Age=%d" name value max_age

let clear_cookie name = Printf.sprintf "%s=; Path=/; HttpOnly; Secure; SameSite=Lax; Max-Age=0" name

let cookie_value headers name =
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii k <> "cookie" then None
      else
        String.split_on_char ';' v
        |> List.find_map (fun part ->
               match String.index_opt part '=' with
               | Some i when String.trim (String.sub part 0 i) = name -> Some (String.trim (String.sub part (i + 1) (String.length part - i - 1)))
               | _ -> None))
    headers
