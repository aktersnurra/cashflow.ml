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

(* A separate incremental boundary: buffered callers above are unchanged. *)
type stream_error = Tls_configuration | Invalid_scheme | Http_status of int | Transport of string
type stream_post = uri:string -> headers:(string * string) list -> body:string -> on_chunk:(string -> unit) -> (unit, stream_error) result

(* cohttp-eio 6.3.0's Body.Reader_flow re-copies offset zero on repeated
   partial reads. Use its bounded HTTP parser directly, not that flow adapter.
   Strict EOF also prevents the stock transfer reader accepting truncated bodies. *)
module Stream_io = struct
  include Cohttp_eio.Private.IO
  (* The stock adapter logs plaintext headers and bodies at Debug. Keep the
     same Eio primitives without payload logging, including on failed reads. *)
  let read input len =
    match Eio.Buf_read.ensure input 1 with
    | exception End_of_file -> ""
    | () -> Eio.Buf_read.take (min len (Eio.Buf_read.buffered_bytes input)) input
  let read_line input =
    try Some (Eio.Buf_read.line input) with End_of_file -> None
  let write output text = Eio.Buf_write.string output text
end
module Strict_io = struct
  include Stream_io
  let read input len =
    let chunk = read input len in
    if len > 0 && String.length chunk = 0 then raise End_of_file;
    chunk
  let read_line input =
    match read_line input with None -> raise End_of_file | line -> line
end
module Stream_response = Cohttp.Response.Private.Make (Strict_io)
module Stream_request = Cohttp.Request.Private.Make (Stream_io)
module Lenient_response = Cohttp.Response.Private.Make (Stream_io)

let stream_post ?tls env : stream_post =
  match (match tls with Some config -> Ok config | None -> tls_config ()) with
  | Error () -> fun ~uri:_ ~headers:_ ~body:_ ~on_chunk:_ -> Error Tls_configuration
  | Ok tls ->
      Mirage_crypto_rng_unix.use_default ();
      fun ~uri ~headers ~body ~on_chunk ->
        try
          let uri = Uri.of_string uri in
          if Uri.scheme uri <> Some "https" then Error Invalid_scheme
          else Eio.Switch.run @@ fun sw ->
            let host = Uri.host_with_default ~default:"" uri in
            let service = string_of_int (Option.value ~default:443 (Uri.port uri)) in
            let address = match Eio.Net.getaddrinfo_stream ~service env#net host with
              | address :: _ -> address | [] -> failwith "unresolved HTTPS host" in
            let flow = connector tls uri (Eio.Net.connect ~sw env#net address) in
            let request = Cohttp.Request.make_for_client
              ~headers:(Cohttp.Header.of_list headers) ~chunked:false
              ~body_length:(Int64.of_int (String.length body)) `POST uri in
            Eio.Buf_write.with_flow flow (fun output ->
              Stream_request.write ~flush:true
                (fun writer -> Stream_request.write_body writer body) request output);
            let input = Eio.Buf_read.of_flow ~max_size:65536 flow in
            match Stream_response.read input with
            | `Eof -> Error (Transport "missing response")
            | `Invalid _ -> Error (Transport "invalid response")
            | `Ok response ->
              match Cohttp.Response.status response with
              | #Cohttp.Code.success_status ->
                let next = match Cohttp.Response.encoding response with
                  | Cohttp.Transfer.Unknown ->
                    let reader = Lenient_response.make_body_reader response input in
                    (fun () -> Lenient_response.read_body_chunk reader)
                  | Cohttp.Transfer.Chunked ->
                    (* Cohttp returns Done on an invalid size line. Validate
                       that line with its own parser before the transfer reader
                       can confuse malformed framing with a zero-size chunk.
                       Per reader call, only a line preceding data is a size;
                       later lines are the chunk delimiter or trailers. *)
                    let size_line = ref true in
                    let module Chunk_io = struct
                      include Strict_io
                      let read input len =
                        size_line := false;
                        read input len
                      let read_line input =
                        let line = read_line input in
                        if !size_line then (
                          size_line := false;
                          match line with
                          | Some line ->
                            (match Http.Private.Parser.parse_chunk_length (line ^ "\r\n") with
                             | Ok _ -> ()
                             | Error _ -> failwith "invalid HTTP chunk size")
                          | None -> raise End_of_file);
                        line
                    end in
                    let module Chunk_response = Cohttp.Response.Private.Make (Chunk_io) in
                    let reader = Chunk_response.make_body_reader response input in
                    (fun () -> size_line := true; Chunk_response.read_body_chunk reader)
                  | Cohttp.Transfer.Fixed _ ->
                    let reader = Stream_response.make_body_reader response input in
                    (fun () -> Stream_response.read_body_chunk reader) in
                let rec read () = match next () with
                  | Cohttp.Transfer.Done -> Ok ()
                  | Chunk chunk -> on_chunk chunk; read ()
                  | Final_chunk chunk -> on_chunk chunk; Ok () in
                if Cohttp.Response.has_body response = `No then Ok () else read ()
              | status -> Error (Http_status (Cohttp.Code.code_of_status status))
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> Error (Transport (Printexc.to_string exn))
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
