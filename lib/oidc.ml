(* copied from kcal: lib/oidc.ml; adds the claims and endpoints the web login needs (name, email,
   groups, nonce; authorization and token endpoints; a form POST). *)
let log_src = Logs.Src.create "cashflow.oidc" ~doc:"OIDC discovery and bearer token verification"
module Log = (val Logs.src_log log_src : Logs.LOG)

type claims = {
  issuer : string;
  subject : string;
  audience : string list;
  expires_at : Ptime.t;
  scopes : string list;
  name : string option;
  email : string option;
  groups : string list option;  (** [None] when the token has no groups claim at all *)
  nonce : string option;
}

type clock = unit -> Ptime.t

type discovery = {
  issuer : string;
  jwks_uri : string;
  authorization_endpoint : string option;
  token_endpoint : string option;
  max_age_s : int option;
}

type jwks_response = {
  keys : Jose.Jwks.t;
  max_age_s : int option;
}

type client = {
  discover : issuer:string -> (discovery, unit) result;
  fetch_jwks : uri:string -> (jwks_response, unit) result;
  post_form : uri:string -> (string * string) list -> (string, unit) result;
}

type t = string -> (claims, unit) result

let fallback_cache_ttl_s = 300
let maximum_cache_ttl_s = 3600

let bounded_ttl = function
  | Some seconds when seconds >= 0 -> min seconds maximum_cache_ttl_s
  | _ -> fallback_cache_ttl_s

let unix_clock () = Option.get (Ptime.of_float_s (Unix.gettimeofday ()))

let audience_of_payload = function
  | `Assoc fields ->
      (match List.assoc_opt "aud" fields with
      | Some (`String audience) -> Some [ audience ]
      | Some (`List audiences) ->
          let rec strings values =
            match values with
            | [] -> Some []
            | `String value :: rest -> Option.map (fun tail -> value :: tail) (strings rest)
            | _ -> None
          in
          strings audiences
      | None -> None
      | _ -> None)
  | _ -> None

let scopes_of_payload = function
  | `Assoc fields ->
      (match List.assoc_opt "scope" fields with
      | Some (`String scopes) ->
          String.split_on_char ' ' scopes
          |> List.filter (fun scope -> scope <> "")
      | Some (`List scopes) ->
          List.filter_map
            (function `String scope when scope <> "" -> Some scope | _ -> None)
            scopes
      | _ -> [])
  | _ -> []

let expiration_of_payload = function
  | `Assoc fields ->
      (match List.assoc_opt "exp" fields with
      | Some (`Int seconds) -> Ptime.of_float_s (float_of_int seconds)
      | Some (`Intlit seconds) ->
          (try Ptime.of_float_s (float_of_string seconds) with Failure _ -> None)
      | Some (`Float seconds) -> Ptime.of_float_s seconds
      | _ -> None)
  | _ -> None

let string_claim name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some (`String value) when value <> "" -> Some value | _ -> None)
  | _ -> None

let groups_of_payload = function
  | `Assoc fields -> (
      match List.assoc_opt "groups" fields with
      | Some (`List groups) -> Some (List.filter_map (function `String g -> Some g | _ -> None) groups)
      | Some (`String group) -> Some [ group ]
      | _ -> None)
  | _ -> None

let claims_of_jwt ~issuer ~audience ~now jwt =
  match
    ( Jose.Jwt.get_string_claim jwt "iss",
      Jose.Jwt.get_string_claim jwt "sub",
      audience_of_payload jwt.Jose.Jwt.payload,
      expiration_of_payload jwt.Jose.Jwt.payload )
  with
  | Some token_issuer, Some subject, Some token_audience, Some expires_at
    when token_issuer = issuer
         && subject <> ""
         && List.mem audience token_audience
         && Ptime.compare expires_at now > 0 ->
      Ok
        {
          issuer = token_issuer;
          subject;
          audience = token_audience;
          expires_at;
          scopes = scopes_of_payload jwt.Jose.Jwt.payload;
          name = (match string_claim "name" jwt.Jose.Jwt.payload with Some _ as n -> n | None -> string_claim "preferred_username" jwt.Jose.Jwt.payload);
          email = string_claim "email" jwt.Jose.Jwt.payload;
          groups = groups_of_payload jwt.Jose.Jwt.payload;
          nonce = string_claim "nonce" jwt.Jose.Jwt.payload;
        }
  | Some token_issuer, _, _, _ when token_issuer <> issuer ->
      Log.warn (fun m -> m "rejected bearer token: issuer %S does not match configured issuer %S" token_issuer issuer);
      Error ()
  | _, _, _, Some expires_at when Ptime.compare expires_at now <= 0 ->
      Log.debug (fun m -> m "rejected bearer token: expired at %s" (Ptime.to_rfc3339 ~tz_offset_s:0 expires_at));
      Error ()
  | _, _, None, _ ->
      Log.warn (fun m -> m "rejected bearer token: missing or malformed audience claim");
      Error ()
  | _ ->
      Log.warn (fun m -> m "rejected bearer token: missing required claims (iss/sub/exp)");
      Error ()

let key_for_token keys token =
  match Jose.Jwt.unsafe_of_string token with
  | Error _ ->
      Log.warn (fun m -> m "rejected bearer token: not a well-formed JWT");
      Error ()
  | Ok jwt ->
      (match jwt.Jose.Jwt.header.alg, jwt.Jose.Jwt.header.kid with
      | `None, _ ->
          Log.warn (fun m -> m "rejected bearer token: alg \"none\" is not accepted");
          Error ()
      | _, None ->
          Log.warn (fun m -> m "rejected bearer token: missing key id (kid) header");
          Error ()
      | _, Some kid ->
          (match Jose.Jwks.find_key keys kid with
          | Some key -> Ok key
          | None ->
              Log.warn (fun m -> m "rejected bearer token: kid=%s not found in the cached JWKS" kid);
              Error ()))

let make ~issuer ~audience ?(clock = unix_clock) ~client () =
  let cached_keys = ref None in
  let fresh_keys now =
    match client.discover ~issuer with
    | Error () ->
        Log.err (fun m -> m "OIDC discovery for issuer=%s failed" issuer);
        Error ()
    | Ok discovery when discovery.issuer <> issuer ->
        Log.err (fun m -> m "OIDC discovery document issuer=%s does not match configured issuer=%s" discovery.issuer issuer);
        Error ()
    | Ok discovery ->
        (match client.fetch_jwks ~uri:discovery.jwks_uri with
        | Error () ->
            Log.err (fun m -> m "fetching JWKS from %s failed" discovery.jwks_uri);
            Error ()
        | Ok response ->
            let ttl = bounded_ttl response.max_age_s in
            (match Ptime.add_span now (Ptime.Span.of_int_s ttl) with
            | None -> Error ()
            | Some expires_at ->
                Log.info (fun m -> m "refreshed JWKS from %s, caching for %ds" discovery.jwks_uri ttl);
                cached_keys := Some (response.keys, expires_at);
                Ok response.keys))
  in
  let keys now =
    match !cached_keys with
    | Some (keys, expires_at) when Ptime.compare now expires_at < 0 -> Ok keys
    | _ -> fresh_keys now
  in
  fun token ->
    let now = clock () in
    match keys now with
    | Error () -> Error ()
    | Ok keys ->
        (match key_for_token keys token with
        | Error () -> Error ()
        | Ok key ->
            match Jose.Jwt.of_string ~jwk:key ~now token with
            | Error _ ->
                Log.warn (fun m -> m "rejected bearer token: signature verification failed");
                Error ()
            | Ok jwt -> claims_of_jwt ~issuer ~audience ~now jwt)

let max_age headers =
  match Cohttp.Header.get headers "cache-control" with
  | None -> None
  | Some value ->
      String.split_on_char ',' value
      |> List.find_map (fun directive ->
             match String.split_on_char '=' (String.trim directive) with
             | [ name; seconds ] when String.lowercase_ascii name = "max-age" ->
                 (try Some (int_of_string (String.trim seconds)) with Failure _ -> None)
             | _ -> None)

let https_client env =
  let certificate_bundle () =
    let rec read = function
      | [] -> Error ()
      | path :: rest ->
          (try
             let channel = open_in_bin path in
             let pem =
               Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
                   really_input_string channel (in_channel_length channel))
             in
             match X509.Certificate.decode_pem_multiple pem with
             | Ok certificates when certificates <> [] -> Ok certificates
             | _ -> read rest
           with Sys_error _ -> read rest)
    in
    read [ "/etc/ssl/cert.pem"; "/etc/ssl/certs/ca-certificates.crt" ]
  in
  let tls =
    match certificate_bundle () with
    | Error () ->
        Log.err (fun m -> m "no usable CA certificate bundle found; OIDC discovery and JWKS fetches will fail");
        Error ()
    | Ok certificates ->
        let authenticator =
          X509.Authenticator.chain_of_trust
            ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ())) certificates
        in
        (match Tls.Config.client ~authenticator () with
        | Ok config -> Ok config
        | Error _ ->
            Log.err (fun m -> m "could not build TLS client configuration");
            Error ())
  in
  match tls with
  | Error () -> { discover = (fun ~issuer:_ -> Error ()); fetch_jwks = (fun ~uri:_ -> Error ()); post_form = (fun ~uri:_ _ -> Error ()) }
  | Ok tls_config ->
      Mirage_crypto_rng_unix.use_default ();
      let https uri flow =
        match Uri.host uri with
        | None -> failwith "missing HTTPS host"
        | Some host ->
            (match Domain_name.of_string host with
            | Error _ -> failwith "invalid HTTPS host"
            | Ok host -> Tls_eio.client_of_flow tls_config ~host:(Domain_name.host_exn host) flow)
      in
      let client = Cohttp_eio.Client.make ~https:(Some https) env#net in
      let get uri =
        try
          let uri = Uri.of_string uri in
          if Uri.scheme uri <> Some "https" then (
            Log.err (fun m -> m "refusing to fetch %s: only https is allowed" (Uri.to_string uri));
            Error ())
          else
            Eio.Switch.run @@ fun sw ->
            let response, body = Cohttp_eio.Client.get client ~sw uri in
            if Cohttp.Response.status response <> `OK then (
              Log.warn (fun m -> m "GET %s returned %s" (Uri.to_string uri) (Cohttp.Code.string_of_status (Cohttp.Response.status response)));
              Error ())
            else Ok (Eio.Flow.read_all body, Cohttp.Response.headers response)
        with exn ->
          Log.warn (fun m -> m "GET %s raised %s" uri (Printexc.to_string exn));
          Error ()
      in
      {
        discover =
          (fun ~issuer ->
            if Uri.scheme (Uri.of_string issuer) <> Some "https" then (
              Log.err (fun m -> m "refusing OIDC discovery for issuer=%s: only https is allowed" issuer);
              Error ())
            else
              match get (Uri.to_string (Uri.with_path (Uri.of_string issuer) "/.well-known/openid-configuration")) with
              | Error () -> Error ()
              | Ok (body, headers) ->
                  (try
                     match Yojson.Safe.from_string body with
                     | `Assoc fields ->
                         (match List.assoc_opt "issuer" fields, List.assoc_opt "jwks_uri" fields with
                         | Some (`String discovered_issuer), Some (`String jwks_uri)
                           when Uri.scheme (Uri.of_string jwks_uri) = Some "https" ->
                             let endpoint name = match List.assoc_opt name fields with Some (`String uri) when Uri.scheme (Uri.of_string uri) = Some "https" -> Some uri | _ -> None in
                             Ok { issuer = discovered_issuer; jwks_uri; authorization_endpoint = endpoint "authorization_endpoint";
                                  token_endpoint = endpoint "token_endpoint"; max_age_s = max_age headers }
                         | _ ->
                             Log.err (fun m -> m "OIDC discovery document for issuer=%s is missing issuer/jwks_uri or jwks_uri is not https" issuer);
                             Error ())
                     | _ ->
                         Log.err (fun m -> m "OIDC discovery document for issuer=%s is not a JSON object" issuer);
                         Error ()
                   with Yojson.Json_error message ->
                     Log.err (fun m -> m "OIDC discovery document for issuer=%s is not valid JSON: %s" issuer message);
                     Error ()));
        post_form =
          (fun ~uri form ->
            try
              let uri = Uri.of_string uri in
              if Uri.scheme uri <> Some "https" then Error ()
              else
                Eio.Switch.run @@ fun sw ->
                let body = Cohttp_eio.Body.of_string (Uri.encoded_of_query (List.map (fun (k, v) -> (k, [ v ])) form)) in
                let headers = Cohttp.Header.of_list [ ("content-type", "application/x-www-form-urlencoded") ] in
                let response, body = Cohttp_eio.Client.post client ~sw ~headers ~body uri in
                let text = Eio.Buf_read.(parse_exn take_all) body ~max_size:1_048_576 in
                if Cohttp.Response.status response <> `OK then (
                  Log.warn (fun m -> m "POST %s returned %s" (Uri.to_string uri) (Cohttp.Code.string_of_status (Cohttp.Response.status response)));
                  Error ())
                else Ok text
            with exn ->
              Log.warn (fun m -> m "POST %s raised %s" uri (Printexc.to_string exn));
              Error ());
        fetch_jwks =
          (fun ~uri ->
            match get uri with
            | Error () -> Error ()
            | Ok (body, headers) ->
                (try Ok { keys = Jose.Jwks.of_string body; max_age_s = max_age headers }
                 with exn ->
                   Log.err (fun m -> m "JWKS at %s could not be parsed: %s" uri (Printexc.to_string exn));
                   Error ()));
      }

let of_verified_claims ?(issuer = "https://issuer.example")
    ?(audience = "kcal-client") ?(clock = unix_clock) verify token =
  match verify token with
  | Error () -> Error ()
  | Ok (claims : claims) when claims.issuer = issuer
                              && List.mem audience claims.audience
                              && Ptime.compare claims.expires_at (clock ()) > 0 ->
      Ok claims
  | Ok _ -> Error ()
