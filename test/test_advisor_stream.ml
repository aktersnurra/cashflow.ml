module Sse = Cashflow_advisor_protocol.Sse
let ok = function Ok x -> x | Error _ -> Alcotest.fail "unexpected error"
let split_frame () =
  let sample = "id: 1\r\nevent: advisor\r\ndata: {\"text\":\"å\"}\r\n\r\n" in
  for i = 0 to String.length sample do
    let p = Sse.create ~max_frame_bytes:262144 in
    let first = ok (Sse.feed p (String.sub sample 0 i)) in
    let frames = first @ ok (Sse.feed p (String.sub sample i (String.length sample - i))) in
    Alcotest.(check int) "one frame" 1 (List.length frames);
    Alcotest.(check string) "decoded data" "{\"text\":\"å\"}" (List.hd frames).data;
    ok (Sse.finish p)
  done
let bounds () =
  let p = Sse.create ~max_frame_bytes:262144 in
  match Sse.feed p (String.make 262145 'x') with
  | Error _ -> () | Ok _ -> Alcotest.fail "oversized incomplete frame accepted"
let raw_frame_bounds () =
  List.iter (fun newline ->
    let sample = "data:x" ^ newline ^ newline in
    let wire_bytes = String.length sample in
    for split = 0 to wire_bytes do
      let feed_split cap =
        let parser = Sse.create ~max_frame_bytes:cap in
        let first = Sse.feed parser (String.sub sample 0 split) in
        let second = Sse.feed parser (String.sub sample split (wire_bytes - split)) in
        parser, first, second in
      let parser, first, second = feed_split wire_bytes in
      let frames = ok first @ ok second in
      Alcotest.(check int) "exact raw cap emits one frame" 1 (List.length frames);
      ok (Sse.finish parser);
      let parser, first, second = feed_split (wire_bytes - 1) in
      (match first, second with
       | Error Sse.Frame_too_large, _ | _, Error Sse.Frame_too_large -> ()
       | _ -> Alcotest.fail "limit-plus-one raw frame accepted");
      (match Sse.finish parser with
       | Error Sse.Frame_too_large -> ()
       | _ -> Alcotest.fail "oversized raw frame finished successfully")
    done) ["\n"; "\r\n"]

let production_raw_frame_bounds () =
  let cap = 262144 in
  List.iter (fun newline ->
    let delimiter = newline ^ newline in
    List.iter (fun extra ->
      let data = String.make (cap - 5 - String.length delimiter + extra) 'x' in
      let prefix = "data:" ^ data in
      for split = 0 to String.length delimiter do
        let parser = Sse.create ~max_frame_bytes:cap in
        let first = ok (Sse.feed parser prefix) in
        let middle = Sse.feed parser (String.sub delimiter 0 split) in
        let last = Sse.feed parser (String.sub delimiter split (String.length delimiter - split)) in
        if extra = 0 then (
          Alcotest.(check int) "256 KiB exact raw frame" 1
            (List.length (first @ ok middle @ ok last));
          ok (Sse.finish parser);
          let next = ok (Sse.feed parser ("data:next" ^ delimiter)) in
          Alcotest.(check int) "following frame has independent cap" 1 (List.length next);
          ok (Sse.finish parser))
        else (
          (match middle, last with
           | Error Sse.Frame_too_large, _ | _, Error Sse.Frame_too_large -> ()
           | _ -> Alcotest.fail "256 KiB plus one raw frame accepted");
          match Sse.finish parser with
          | Error Sse.Frame_too_large -> ()
          | _ -> Alcotest.fail "oversized frame finish accepted")
      done) [0; 1]) ["\n"; "\r\n"]

let framing () =
  let p = Sse.create ~max_frame_bytes:262144 in
  let frames = ok (Sse.feed p ": heartbeat\r\ndata: first\rdata: second\r\r") in
  Alcotest.(check int) "one" 1 (List.length frames);
  Alcotest.(check string) "multiline" "first\nsecond" (List.hd frames).data;
  let p = Sse.create ~max_frame_bytes:262144 in
  ignore (ok (Sse.feed p "data: partial"));
  match Sse.finish p with Error _ -> () | Ok () -> Alcotest.fail "truncation accepted"
let encode_roundtrip () =
  let frame : Sse.frame = {id=Some "42"; event=Some "advisor"; data="å\nsecond\n"} in
  let parser = Sse.create ~max_frame_bytes:262144 in
  let frames = ok (Sse.feed parser (Sse.encode frame)) in
  Alcotest.(check int) "roundtrip count" 1 (List.length frames);
  let decoded = List.hd frames in
  Alcotest.(check string) "roundtrip data" frame.data decoded.data;
  Alcotest.(check (option string)) "roundtrip id" frame.id decoded.id;
  Alcotest.(check (option string)) "roundtrip event" frame.event decoded.event

let payload = String.init 24000 (fun i -> Char.chr (33 + ((i * 17 + i / 101) mod 90)))
let fake_reader () =
  Eio_main.run @@ fun _ ->
  let remaining = ref true in
  (* Pin the installed adapter defect, independently of network packet boundaries. *)
  let source = Cohttp_eio__Utils.flow_of_reader (fun () ->
    if !remaining then (remaining := false; Cohttp.Transfer.Chunk payload)
    else Cohttp.Transfer.Done) in
  let received = Buffer.create 24000 in
  let buffer = Cstruct.create 4096 in
  let rec read () = match Eio.Flow.single_read source buffer with
    | n -> Buffer.add_string received (Cstruct.to_string ~len:n buffer); read ()
    | exception End_of_file -> () in
  read ();
  Alcotest.(check int) "legacy byte count matches" (String.length payload) (Buffer.length received);
  Alcotest.(check bool) "installed adapter duplicates bytes" false (String.equal payload (Buffer.contents received))

exception Callback_probe of string

let tls_fixture_process () =
  let read path = let ch = open_in_bin path in Fun.protect ~finally:(fun () -> close_in ch) (fun () -> really_input_string ch (in_channel_length ch)) in
  let certificate = ok (X509.Certificate.decode_pem (read Sys.argv.(2))) in
  let private_key = ok (X509.Private_key.decode_pem (read Sys.argv.(3))) in
  let server = ok (Tls.Config.server ~certificates:(`Single ([certificate], private_key)) ()) in
  let large = bool_of_string Sys.argv.(4) and fixed = bool_of_string Sys.argv.(5)
  and truncated = bool_of_string Sys.argv.(6) and malformed = bool_of_string Sys.argv.(7)
  and chunk_extensions = bool_of_string Sys.argv.(8) and status = int_of_string Sys.argv.(9)
  and cancel = bool_of_string Sys.argv.(10)
  and provider_decode = bool_of_string Sys.argv.(11) in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 env#net
    (List.hd (Eio.Net.getaddrinfo_stream ~service:"0" env#net "localhost")) in
  let port = match Eio.Net.listening_addr listener with `Tcp (_, port) -> port | _ -> assert false in
  Eio.Flow.copy_string (string_of_int port ^ "\n") env#stdout;
  let flow, _ = Eio.Net.accept ~sw listener in
  try
    let flow = Tls_eio.server_of_flow server flow in
    let input = Eio.Buf_read.of_flow ~max_size:16384 flow in
    Alcotest.(check string) "POST request target" "POST /stream?probe=1 HTTP/1.1" (Eio.Buf_read.line input);
    let rec headers found =
      let line = Eio.Buf_read.line input in
      if String.length line = 0 then found
      else headers (found || String.equal (String.lowercase_ascii line) (Printf.sprintf "host: localhost:%d" port)) in
    Alcotest.(check bool) "authority with port" true (headers false);
    let text = if provider_decode then "data: PRIVATE-invalid-json\n\n" else if large then payload else "first second" in
    let head = if fixed then Printf.sprintf "Content-Length: %d" (String.length text) else "Transfer-Encoding: chunked" in
    Eio.Flow.copy_string (Printf.sprintf "HTTP/1.1 %d Test\r\n%s\r\nConnection: close\r\nX-Private-Probe: HS6advisor-financial-unique\r\n\r\n" status head) flow;
    if large then (
      Eio.Flow.copy_string (if fixed then text else Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length text) text) flow)
    else (
      Eio.Flow.copy_string (if fixed then "first" else if chunk_extensions then "5;probe=yes\r\nfirst\r\n" else "5\r\nfirst\r\n") flow;
      if status = 200 then (
        if cancel then (
          let b = Cstruct.create 16 in
          (try while true do ignore (Eio.Flow.single_read flow b) done with End_of_file -> ()))
        else (
          ignore (Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:32 env#stdin));
          if not truncated then Eio.Flow.copy_string (if malformed then "bogus\r\n" else if fixed then " second" else if chunk_extensions then "7;probe=yes\r\n second\r\n0;done=yes\r\nX-Trailer: synthetic\r\n\r\n" else "7\r\n second\r\n0\r\n\r\n") flow)))
  with Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _ | End_of_file -> ()

let tls_transport () =
  let dir = Filename.temp_file "advisor-tls-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let cert = Filename.concat dir "cert.pem" and key = Filename.concat dir "key.pem" in
  let cleanup () = List.iter (fun p -> if Sys.file_exists p then Sys.remove p) [cert; key]; Unix.rmdir dir in
  Fun.protect ~finally:cleanup (fun () ->
    let command = Printf.sprintf "openssl req -x509 -newkey rsa:2048 -nodes -keyout %s -out %s -days 1 -subj /CN=localhost -addext subjectAltName=DNS:localhost >/dev/null 2>&1" (Filename.quote key) (Filename.quote cert) in
    Alcotest.(check int) "local certificate" 0 (Sys.command command);
    let read path = let ch = open_in_bin path in Fun.protect ~finally:(fun () -> close_in ch) (fun () -> really_input_string ch (in_channel_length ch)) in
    let certificate = ok (X509.Certificate.decode_pem (read cert)) in
    let authenticator = X509.Authenticator.chain_of_trust ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ())) [certificate] in
    let trusted = ok (Tls.Config.client ~authenticator ()) in
    Eio_main.run @@ fun env ->
    let run ?tls ?(large=false) ?(fixed=false) ?(truncated=false) ?(malformed=false) ?(chunk_extensions=false) ?(callback_failure=false) ?(provider_decode=false) ~status ~cancel () =
      Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let ready, ready_output = Eio.Process.pipe ~sw env#process_mgr in
      let gate_input, release = Eio.Process.pipe ~sw env#process_mgr in
      let fixture = Eio.Process.spawn ~sw env#process_mgr ~stdin:gate_input ~stdout:ready_output
        [Sys.executable_name; "--tls-fixture"; cert; key;
         string_of_bool large; string_of_bool fixed; string_of_bool truncated;
         string_of_bool malformed; string_of_bool chunk_extensions; string_of_int status; string_of_bool cancel; string_of_bool provider_decode] in
      let port = int_of_string (Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:32 ready)) in
      let released = ref false in
      let chunks = Buffer.create 32 in
      let result =
        try
          let post = Cashflow_io.Https.stream_post ?tls env in
          let uri = Printf.sprintf "https://localhost:%d/stream?probe=1" port in
          if provider_decode then (
            let post ~uri:_ ~headers ~body ~on_chunk = post ~uri ~headers ~body ~on_chunk in
            let provider = Cashflow_io.Advisor_provider.create ~post ~api_key:"PRIVATE-dummy-key"
              ~model:"anthropic/claude-sonnet-4.6" ~provider_only:["amazon-bedrock"] () in
            `Provider_result (Cashflow_io.Advisor_provider.stream provider
              { system = "PRIVATE-system"; messages = [] } ~on_event:(fun _ -> Alcotest.fail "unexpected provider event")))
          else
            let outcome = post ~uri ~headers:["authorization", "Bearer AX9advisor-auth-unique"; "x-probe", "QZ7advisor-header-unique"] ~body:"RB8advisor-request-unique"
              ~on_chunk:(fun chunk ->
                if callback_failure then raise (Callback_probe "PRIVATE-callback-exception");
                Alcotest.(check bool) "bounded HTTP fragment" true (String.length chunk <= 32768);
                Buffer.add_string chunks chunk;
                if not large && Buffer.length chunks = 5 then (
                  Alcotest.(check bool) "gate still held on first chunk" false !released;
                  if cancel then raise (Eio.Cancel.Cancelled Exit);
                  released := true; Eio.Flow.copy_string "continue\n" release)) in
            `Result outcome
        with Eio.Cancel.Cancelled Exit -> `Cancelled
           | Callback_probe marker -> `Callback_failed marker in
      Eio.Process.await_exn fixture;
      result, Buffer.contents chunks
    in
    let result, text = run ~tls:trusted ~status:200 ~cancel:false () in
    (match result with `Result (Ok ()) -> () | _ -> Alcotest.fail "trusted TLS failed");
    Alcotest.(check string) "ordered upstream bytes" "first second" text;
    let result, _ = run ~tls:trusted ~large:true ~callback_failure:true ~status:200 ~cancel:false () in
    (match result with `Callback_failed "PRIVATE-callback-exception" -> ()
     | _ -> Alcotest.fail "callback exception must cross native HTTPS unchanged");
    let result, _ = run ~tls:trusted ~large:true ~provider_decode:true ~status:200 ~cancel:false () in
    (match result with `Provider_result (Error { code = Cashflow_core.Advisor.Invalid_stream; _ }) -> ()
     | _ -> Alcotest.fail "provider Decode must not become Transport/Provider_unavailable");
    let result, text = run ~tls:trusted ~status:403 ~cancel:false () in
    (match result with `Result (Error (Cashflow_io.Https.Http_status 403)) -> () | _ -> Alcotest.fail "status not rejected");
    Alcotest.(check string) "no failed response bytes" "" text;
    let result, _ = run ~tls:trusted ~status:200 ~cancel:true () in
    (match result with `Cancelled -> () | _ -> Alcotest.fail "cancellation swallowed");
    let result, text = run ~status:200 ~cancel:false () in
    (match result with `Result (Error _) -> () | _ -> Alcotest.fail "untrusted TLS accepted");
    Alcotest.(check string) "no untrusted bytes" "" text;
    let result, text = run ~tls:trusted ~malformed:true ~status:200 ~cancel:false () in
    (match result with `Result (Error (Cashflow_io.Https.Transport _)) -> ()
     | _ -> Alcotest.fail "malformed chunk size accepted as completion");
    Alcotest.(check string) "valid partial text before malformed size" "first" text;
    let result, text = run ~tls:trusted ~chunk_extensions:true ~status:200 ~cancel:false () in
    (match result with `Result (Ok ()) -> () | _ -> Alcotest.fail "valid chunk extensions/trailers rejected");
    Alcotest.(check string) "valid extended chunks complete" "first second" text;
    List.iter (fun fixed ->
      let result, text = run ~tls:trusted ~large:true ~fixed ~status:200 ~cancel:false () in
      (match result with `Result (Ok ()) -> () | _ -> Alcotest.fail "large body failed");
      Alcotest.(check string) "exact large non-repeating payload" payload text;
      let result, text = run ~tls:trusted ~fixed ~truncated:true ~status:200 ~cancel:false () in
      (match result with `Result (Error _) -> () | _ -> Alcotest.fail "truncation accepted");
      Alcotest.(check string) "partial bytes only" "first" text;
      let result, _ = run ~tls:trusted ~fixed ~status:200 ~cancel:true () in
      match result with `Cancelled -> () | _ -> Alcotest.fail "mid-body cancellation swallowed") [false; true])

let contains text needle =
  let rec search offset =
    offset + String.length needle <= String.length text &&
    (String.equal needle (String.sub text offset (String.length needle)) || search (offset + 1)) in
  search 0

let decode_trace_hex logs =
  (* Ohex's TLS trace format has a six-digit offset, two spaces, 42 hex
     columns, then ASCII. Join decoded rows to detect markers split at 16B. *)
  let decoded = Buffer.create 1024 in
  List.iter (fun line ->
    if String.length line >= 50 && String.equal (String.sub line 6 2) "  " then
      try
        ignore (int_of_string ("0x" ^ String.sub line 0 6));
        Buffer.add_string decoded (Ohex.decode (String.sub line 8 42))
      with Failure _ | Invalid_argument _ -> ()) (String.split_on_char '\n' logs);
  Buffer.contents decoded

let trace_hex_detection () =
  let marker = "split-secret-marker" in
  let trace = Format.asprintf "%a" (Ohex.pp_hexdump ()) (String.make 15 'x' ^ marker) in
  Alcotest.(check bool) "marker split across trace rows" false (contains trace marker);
  Alcotest.(check bool) "decoded split marker detected" true (contains (decode_trace_hex trace) marker)

let tls_transport_no_logs () =
  let previous = Logs.reporter () and global_level = Logs.level () in
  let environment = Sys.getenv_opt "CASHFLOW_LOG_LEVEL" in
  let unrelated = Logs.Src.create "advisor-test-unrelated" in
  let levels = List.map (fun src -> src, Logs.Src.level src) (Logs.Src.list ()) in
  let captured = Buffer.create 1024 in
  let report src _ ~over k msgf =
    msgf (fun ?header:_ ?tags:_ format ->
      Format.kasprintf (fun message ->
        Buffer.add_string captured (Logs.Src.name src ^ ":" ^ message ^ "\n"); over (); k ()) format) in
  Fun.protect ~finally:(fun () ->
    (match environment with None -> Unix.unsetenv "CASHFLOW_LOG_LEVEL" | Some value -> Unix.putenv "CASHFLOW_LOG_LEVEL" value);
    Logs.set_reporter previous;
    Logs.set_level ~all:false global_level;
    List.iter (fun (src, level) -> Logs.Src.set_level src level) levels)
    (fun () ->
      List.iter (fun (name, activate) ->
        Buffer.clear captured;
        activate ();
        Logs.set_reporter {Logs.report};
        let module Other_log = (val Logs.src_log unrelated : Logs.LOG) in
        let module Tls_log = (val Logs.src_log Tls.Core.src : Logs.LOG) in
        Other_log.debug (fun m -> m "unrelated-debug-survives");
        Tls_log.warn (fun m -> m "tls-warning-survives");
        Tls_log.err (fun m -> m "tls-error-survives");
        tls_transport ();
        let logs = Buffer.contents captured in
        let decoded = decode_trace_hex logs in
        List.iter (fun diagnostic ->
          Alcotest.(check bool) (name ^ " safe framing diagnostic") true (contains logs diagnostic))
          ["cashflow.https:HTTPS stream failed: phase=response-body reason=invalid-chunk-size";
           "cashflow.https:HTTPS stream failed: phase=response-body reason=end-of-file";
           "cashflow.https:HTTPS stream failed: phase=callback reason=exception"];
        let decode_logs = String.split_on_char '\n' logs
          |> List.filter (String.starts_with ~prefix:"cashflow.advisor_provider:") in
        Alcotest.(check (list string)) "native callback Decode has only fixed location"
          ["cashflow.advisor_provider:Advisor provider invalid stream: location=json"] decode_logs;
        let boundary_logs = String.split_on_char '\n' logs
          |> List.filter (String.starts_with ~prefix:"cashflow.https:HTTPS stream failed:") in
        Alcotest.(check int) "only actual failures logged, not cancellation/success/status" 6 (List.length boundary_logs);
        List.iter (fun line ->
          Alcotest.(check bool) "closed boundary diagnostic only" true
            (List.mem line
              ["cashflow.https:HTTPS stream failed: phase=tls reason=tls-failure";
               "cashflow.https:HTTPS stream failed: phase=tls reason=tls-alert";
               "cashflow.https:HTTPS stream failed: phase=response-body reason=invalid-chunk-size";
               "cashflow.https:HTTPS stream failed: phase=response-body reason=end-of-file";
               "cashflow.https:HTTPS stream failed: phase=callback reason=exception"])) boundary_logs;
        List.iter (fun marker ->
          Alcotest.(check bool) (name ^ " no plaintext: " ^ marker) false (contains logs marker);
          Alcotest.(check bool) (name ^ " no recoverable hex: " ^ marker) false (contains decoded marker))
          ["AX9"; "QZ7"; "RB8"; "HS6"; "bogus"; "first";
           "advisor-auth-unique"; "advisor-header-unique"; "advisor-request-unique";
           "advisor-financial-unique"; "PRIVATE-callback-exception"; "PRIVATE-invalid-json"; "PRIVATE-dummy-key"; "PRIVATE-system"; String.sub payload 0 100];
        List.iter (fun diagnostic ->
          Alcotest.(check bool) (name ^ " preserves " ^ diagnostic) true (contains logs diagnostic))
          ["unrelated-debug-survives"; "tls-warning-survives"; "tls-error-survives"])
        ["setup env debug", (fun () -> Unix.putenv "CASHFLOW_LOG_LEVEL" "debug"; Cashflow_io.Logging.setup ());
         "configure config debug", (fun () -> Unix.unsetenv "CASHFLOW_LOG_LEVEL"; Cashflow_io.Logging.configure (Some Logs.Debug));
         "configure env override", (fun () -> Unix.putenv "CASHFLOW_LOG_LEVEL" "debug"; Cashflow_io.Logging.configure (Some Logs.Info));
         "reconfigure debug", (fun () -> Cashflow_io.Logging.configure (Some Logs.Debug))])

let () =
  if Array.length Sys.argv > 1 && String.equal Sys.argv.(1) "--tls-fixture" then tls_fixture_process ()
  else Alcotest.run "advisor stream" ["sse", [
  Alcotest.test_case "every split incl UTF-8" `Quick split_frame;
  Alcotest.test_case "256 KiB cap" `Quick bounds;
  Alcotest.test_case "raw LF/CRLF cap at every split" `Quick raw_frame_bounds;
  Alcotest.test_case "256 KiB raw cap at every delimiter split" `Quick production_raw_frame_bounds;
  Alcotest.test_case "line endings, comments, truncation" `Quick framing;
  Alcotest.test_case "encode multiline UTF-8" `Quick encode_roundtrip];
  "https", [Alcotest.test_case "hex trace detection" `Quick trace_hex_detection;
    Alcotest.test_case "fake reader" `Quick fake_reader;
    Alcotest.test_case "gated trusted TLS, status, cancellation, untrusted TLS" `Quick tls_transport_no_logs]]
