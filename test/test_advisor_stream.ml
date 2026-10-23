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
    let private_key = ok (X509.Private_key.decode_pem (read key)) in
    let server = ok (Tls.Config.server ~certificates:(`Single ([certificate], private_key)) ()) in
    let authenticator = X509.Authenticator.chain_of_trust ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ())) [certificate] in
    let trusted = ok (Tls.Config.client ~authenticator ()) in
    Eio_main.run @@ fun env ->
    let run ?tls ?(large=false) ?(fixed=false) ?(truncated=false) ~status ~cancel () =
      Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 env#net (List.hd (Eio.Net.getaddrinfo_stream ~service:"0" env#net "localhost")) in
      let port = match Eio.Net.listening_addr listener with `Tcp (_, port) -> port | _ -> assert false in
      let gate, release = Eio.Promise.create () in
      let server_closed = ref false in
      let server_done = Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Net.accept_fork ~sw listener ~on_error:(fun _ -> ()) (fun flow _ ->
          Fun.protect ~finally:(fun () -> server_closed := true) (fun () ->
            try
              let flow = Tls_eio.server_of_flow server flow in
              let input = Eio.Buf_read.of_flow ~max_size:16384 flow in
              Alcotest.(check string) "POST request target" "POST /stream?probe=1 HTTP/1.1" (Eio.Buf_read.line input);
              let rec headers found =
                let line = Eio.Buf_read.line input in
                if String.length line = 0 then found
                else headers (found || String.equal (String.lowercase_ascii line) (Printf.sprintf "host: localhost:%d" port)) in
              Alcotest.(check bool) "authority with port" true (headers false);
              let text = if large then payload else "first second" in
              let head = if fixed then Printf.sprintf "Content-Length: %d" (String.length text) else "Transfer-Encoding: chunked" in
              Eio.Flow.copy_string (Printf.sprintf "HTTP/1.1 %d Test\r\n%s\r\nConnection: close\r\n\r\n" status head) flow;
              if large then (
                Eio.Flow.copy_string (if fixed then text else Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length text) text) flow)
              else (
              Eio.Flow.copy_string (if fixed then "first" else "5\r\nfirst\r\n") flow;
              if status = 200 then (
                if cancel then (
                  let b = Cstruct.create 16 in
                  (try while true do ignore (Eio.Flow.single_read flow b) done with End_of_file -> ()))
                else (Eio.Promise.await gate;
                  if not truncated then Eio.Flow.copy_string (if fixed then " second" else "7\r\n second\r\n0\r\n\r\n") flow)))
            with Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _ | End_of_file -> ()))) in
      let chunks = Buffer.create 32 in
      let result =
        try
          let outcome = Cashflow_io.Https.stream_post ?tls env ~uri:(Printf.sprintf "https://localhost:%d/stream?probe=1" port) ~headers:[] ~body:""
            ~on_chunk:(fun chunk ->
              Alcotest.(check bool) "bounded HTTP fragment" true (String.length chunk <= 32768);
              Buffer.add_string chunks chunk;
              if not large && Buffer.length chunks = 5 then (
                Alcotest.(check bool) "gate still held on first chunk" false (Eio.Promise.is_resolved gate);
                if cancel then raise (Eio.Cancel.Cancelled Exit);
                Eio.Promise.resolve release ())) in
          `Result outcome
        with Eio.Cancel.Cancelled Exit -> `Cancelled in
      ignore (Eio.Promise.await server_done);
      (* accept_fork is switch-owned; await its cleanup before leaving the switch. *)
      let rec await_close () = if not !server_closed then (Eio.Fiber.yield (); await_close ()) in
      await_close ();
      result, Buffer.contents chunks
    in
    let result, text = run ~tls:trusted ~status:200 ~cancel:false () in
    (match result with `Result (Ok ()) -> () | _ -> Alcotest.fail "trusted TLS failed");
    Alcotest.(check string) "ordered upstream bytes" "first second" text;
    let result, text = run ~tls:trusted ~status:403 ~cancel:false () in
    (match result with `Result (Error (Cashflow_io.Https.Http_status 403)) -> () | _ -> Alcotest.fail "status not rejected");
    Alcotest.(check string) "no failed response bytes" "" text;
    let result, _ = run ~tls:trusted ~status:200 ~cancel:true () in
    (match result with `Cancelled -> () | _ -> Alcotest.fail "cancellation swallowed");
    let result, text = run ~status:200 ~cancel:false () in
    (match result with `Result (Error _) -> () | _ -> Alcotest.fail "untrusted TLS accepted");
    Alcotest.(check string) "no untrusted bytes" "" text;
    List.iter (fun fixed ->
      let result, text = run ~tls:trusted ~large:true ~fixed ~status:200 ~cancel:false () in
      (match result with `Result (Ok ()) -> () | _ -> Alcotest.fail "large body failed");
      Alcotest.(check string) "exact large non-repeating payload" payload text;
      let result, text = run ~tls:trusted ~fixed ~truncated:true ~status:200 ~cancel:false () in
      (match result with `Result (Error _) -> () | _ -> Alcotest.fail "truncation accepted");
      Alcotest.(check string) "partial bytes only" "first" text;
      let result, _ = run ~tls:trusted ~fixed ~status:200 ~cancel:true () in
      match result with `Cancelled -> () | _ -> Alcotest.fail "mid-body cancellation swallowed") [false; true])

let () = Alcotest.run "advisor stream" ["sse", [
  Alcotest.test_case "every split incl UTF-8" `Quick split_frame;
  Alcotest.test_case "256 KiB cap" `Quick bounds;
  Alcotest.test_case "line endings, comments, truncation" `Quick framing;
  Alcotest.test_case "encode multiline UTF-8" `Quick encode_roundtrip];
  "https", [Alcotest.test_case "fake reader" `Quick fake_reader;
    Alcotest.test_case "gated trusted TLS, status, cancellation, untrusted TLS" `Quick tls_transport]]
