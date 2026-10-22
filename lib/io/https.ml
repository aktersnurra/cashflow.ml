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
