(* An HTTPS client on Eio, verifying servers against the system CA bundle. The TLS setup is
   kcal's (lib/oidc.ml), shared here by OIDC and the OpenRouter client. *)
let log_src = Logs.Src.create "cashflow.https" ~doc:"Outbound HTTPS"
module Log = (val Logs.src_log log_src : Logs.LOG)

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
  read [ "/etc/ssl/cert.pem"; "/usr/local/share/certs/ca-root-nss.crt"; "/etc/ssl/certs/ca-certificates.crt" ]

let tls_config () =
  match certificate_bundle () with
  | Error () ->
      Log.err (fun m -> m "no usable CA certificate bundle found; outbound HTTPS will fail");
      Error ()
  | Ok certificates -> (
      let authenticator = X509.Authenticator.chain_of_trust ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ())) certificates in
      match Tls.Config.client ~authenticator () with
      | Ok config -> Ok config
      | Error _ ->
          Log.err (fun m -> m "could not build TLS client configuration");
          Error ())

let connector tls_config uri flow =
  match Uri.host uri with
  | None -> failwith "missing HTTPS host"
  | Some host -> (
      match Domain_name.of_string host with
      | Error _ -> failwith "invalid HTTPS host"
      | Ok host -> Tls_eio.client_of_flow tls_config ~host:(Domain_name.host_exn host) flow)

type post = uri:string -> headers:(string * string) list -> body:string -> (string, string) result

(* POSTs a body and returns the response body of a 2xx response. *)
let post env : post =
  match tls_config () with
  | Error () -> fun ~uri:_ ~headers:_ ~body:_ -> Error "no TLS configuration"
  | Ok tls ->
      Mirage_crypto_rng_unix.use_default ();
      let client = Cohttp_eio.Client.make ~https:(Some (connector tls)) env#net in
      fun ~uri ~headers ~body ->
        try
          let uri = Uri.of_string uri in
          if Uri.scheme uri <> Some "https" then Error "only https is allowed"
          else
            Eio.Switch.run @@ fun sw ->
            let response, body =
              Cohttp_eio.Client.post client ~sw ~headers:(Cohttp.Header.of_list headers) ~body:(Cohttp_eio.Body.of_string body) uri
            in
            let text = Eio.Buf_read.(parse_exn take_all) body ~max_size:4_194_304 in
            match Cohttp.Response.status response with
            | #Cohttp.Code.success_status -> Ok text
            | status -> Error ("HTTP " ^ Cohttp.Code.string_of_status status)
        with exn -> Error (Printexc.to_string exn)

(* The checked address is the only address passed to connect; the URI still supplies Host and SNI. *)
let logo_network env : Logo_fetch.network =
  let with_request : type a. (unit -> (a, Logo_fetch.error) result) -> (a, Logo_fetch.error) result = fun f ->
    try Eio.Time.with_timeout_exn env#clock 5. f
    with Eio.Time.Timeout -> Error Logo_fetch.Unreachable_site in
  let protect f =
    try Ok (f ())
    with Eio.Cancel.Cancelled _ as exn -> raise exn
       | _ -> Error Logo_fetch.Unreachable_site
  in
  let resolve ~host =
    match protect (fun () -> Eio.Net.getaddrinfo_stream ~service:"443" env#net host) with
    | Error e -> Error e
    | Ok addresses ->
        if addresses = [] || List.exists (function `Unix _ -> true | _ -> false) addresses then
          Error Logo_fetch.Unreachable_site
        else Ok (List.filter_map (function
          | `Tcp (ip, _) -> Some (Fmt.str "%a" Eio.Net.Ipaddr.pp ip)
          | `Unix _ -> None) addresses)
  in
  let get ~uri ~address ~max_bytes =
    match protect (fun () ->
      let ip = Ipaddr.of_string_exn address |> Ipaddr.to_octets |> Eio.Net.Ipaddr.of_raw in
      Eio.Switch.run @@ fun sw ->
      let tls = match tls_config () with Ok tls -> tls | Error () -> failwith "TLS unavailable" in
      Mirage_crypto_rng_unix.use_default ();
      let client = Cohttp_eio.Client.make_generic (fun ~sw uri ->
        let socket = Eio.Net.connect ~sw env#net (`Tcp (ip, 443)) in
        (connector tls uri socket :> _ Eio.Flow.two_way)) in
      let response, body = Cohttp_eio.Client.get client ~sw uri in
      let text =
        try Ok (Eio.Buf_read.(parse_exn take_all) body ~max_size:(max_bytes + 1))
        with Eio.Buf_read.Buffer_limit_exceeded -> Error Logo_fetch.Oversized_body in
      Result.map (fun body ->
        { Logo_fetch.status = Cohttp.Code.code_of_status (Cohttp.Response.status response);
          headers = Cohttp.Header.to_list (Cohttp.Response.headers response); body }) text) with
    | Ok response -> response
    | Error e -> Error e
  in { Logo_fetch.with_request; resolve; get }

(* The checked address is the only address passed to connect; the URI still supplies Host and SNI. *)
let logo_network env : Logo_fetch.network =
  let with_request : type a. (unit -> (a, Logo_fetch.error) result) -> (a, Logo_fetch.error) result = fun f ->
    try Eio.Time.with_timeout_exn env#clock 5. f
    with Eio.Time.Timeout -> Error Logo_fetch.Unreachable_site in
  let protect f =
    try Ok (f ())
    with Eio.Cancel.Cancelled _ as exn -> raise exn
       | _ -> Error Logo_fetch.Unreachable_site
  in
  let resolve ~host =
    match protect (fun () -> Eio.Net.getaddrinfo_stream ~service:"443" env#net host) with
    | Error e -> Error e
    | Ok addresses ->
        if addresses = [] || List.exists (function `Unix _ -> true | _ -> false) addresses then
          Error Logo_fetch.Unreachable_site
        else Ok (List.filter_map (function
          | `Tcp (ip, _) -> Some (Fmt.str "%a" Eio.Net.Ipaddr.pp ip)
          | `Unix _ -> None) addresses)
  in
  let get ~uri ~address ~max_bytes =
    match protect (fun () ->
      let ip = Ipaddr.of_string_exn address |> Ipaddr.to_octets |> Eio.Net.Ipaddr.of_raw in
      Eio.Switch.run @@ fun sw ->
      let tls = match tls_config () with Ok tls -> tls | Error () -> failwith "TLS unavailable" in
      Mirage_crypto_rng_unix.use_default ();
      let client = Cohttp_eio.Client.make_generic (fun ~sw uri ->
        let socket = Eio.Net.connect ~sw env#net (`Tcp (ip, 443)) in
        (connector tls uri socket :> _ Eio.Flow.two_way)) in
      let response, body = Cohttp_eio.Client.get client ~sw uri in
      let text =
        try Ok (Eio.Buf_read.(parse_exn take_all) body ~max_size:(max_bytes + 1))
        with Eio.Buf_read.Buffer_limit_exceeded -> Error Logo_fetch.Oversized_body in
      Result.map (fun body ->
        { Logo_fetch.status = Cohttp.Code.code_of_status (Cohttp.Response.status response);
          headers = Cohttp.Header.to_list (Cohttp.Response.headers response); body }) text) with
    | Ok response -> response
    | Error e -> Error e
  in { Logo_fetch.with_request; resolve; get }
