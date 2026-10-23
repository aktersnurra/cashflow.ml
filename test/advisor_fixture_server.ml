(* Synthetic loopback-only fixture. No production route/provider bypass. *)
open Cashflow_io
module A = Cashflow_core.Advisor
module R = Advisor_runtime
module X = Advisor_access

let ok = Result.get_ok

let () =
  Eio_main.run @@ fun env ->
  let db = Store_sqlite.open_memory () in
  Fun.protect
    ~finally:(fun () -> Sql.close db)
    (fun () ->
      Eio.Switch.run (fun sw ->
          let browser_text = Array.exists (( = ) "--browser-text") Sys.argv in
          let small_buffers = Array.exists (( = ) "--small-socket-buffers") Sys.argv in
          let pending_writes = Hashtbl.create 4 and next_connection = ref 0 in
          let drain_timeouts = ref 0 in
          let guard = Db_guard.make () in
          Sql.set_guard db guard;
          let released = ref false and producer_closed = ref false and connections = ref 0 in
          let gate, release = Eio.Promise.create () in
          let service = Service.make ~now:Ptime_clock.now db in
          let access =
            Service.sign_in service
              {
                issuer = "https://synthetic.invalid";
                subject = "advisor-fixture";
                display_name = None;
                email = None;
                is_admin = Some false;
                locale = None;
              }
          in
          let user = Cashflow_core.Access.user access in
          let token = Web_auth.create_session db ~now:(service.now ()) user in
          let token_b = Web_auth.create_session db ~now:(service.now ()) user in
          let principal = ok (X.authenticate service ~now:(service.now ()) ~token) in
          ignore (ok (X.set_user_consent service principal ~enabled:true));
          let credentials token =
            let i = Option.get (Web_auth.inspect_session service ~now:(service.now ()) (Web_auth.session_ref token)) in
            `Assoc [ ("token", `String token); ("csrf", `String i.csrf_token) ]
          in
          let post ~uri:_ ~headers:_ ~body:_ ~on_chunk =
            Fun.protect
              ~finally:(fun () -> producer_closed := true)
              (fun () ->
                assert (not (guard.held ()));
                let text value =
                  "data: "
                  ^ Yojson.Safe.to_string
                      (`Assoc
                         [
                           ( "choices",
                             `List
                               [
                                 `Assoc
                                   [
                                     ("index", `Int 0);
                                     ("delta", `Assoc [ ("content", `String value) ]);
                                     ("finish_reason", `Null);
                                   ];
                               ] );
                         ])
                  ^ "\n\n"
                in
                on_chunk
                  (text
                     (if small_buffers then "first" ^ String.make ((60 * 1024) - 5) 'x'
                      else if browser_text then
                        "first héj <img src=x onerror=window.fixtureXss=true> [evidence:not-authorized]\n"
                        ^ String.concat "\n" (List.init 50 (fun n -> "synthetic line " ^ string_of_int n))
                      else "first"));
                Eio.Promise.await gate;
                assert (not (guard.held ()));
                on_chunk
                  (text (if browser_text then "\nsecond <script>window.fixtureXss=true</script>" else " second")
                  ^ "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n");
                Ok ())
          in
          let provider =
            Advisor_provider.create
              ~limits:
                {
                  context_tokens = 262144;
                  completion_tokens = 16384;
                  whole_completion_cap_supported = true;
                  request_tokens_per_byte = 1;
                  request_token_overhead = 0;
                }
              ~post ~api_key:"synthetic" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
          in
          let origin = Eio.Time.Mono.now env#mono_clock in
          let clock : R.clock =
            {
              now = service.now;
              monotonic_s =
                (fun () ->
                  Mtime.span origin (Eio.Time.Mono.now env#mono_clock) |> Mtime.Span.to_float_ns |> fun ns -> ns /. 1e9);
              sleep_s =
                (fun seconds ->
                  assert (not (guard.held ()));
                  Eio.Time.sleep env#clock seconds);
            }
          in
          let runtime = R.create ~sw ~service ~provider ~clock in
          let advisor =
            Some
              (Advisor_http.create ~service ~runtime ~public_origin:"http://127.0.0.1:18197" ~available:true
                 ~sleep_s:clock.sleep_s)
          in
          let config =
            ok
              (Config.parse
                 {|((database :memory:) (listen 127.0.0.1:18197) (public_url http://127.0.0.1:18197) (oidc ((issuer https://synthetic.invalid) (audience synthetic))))|})
          in
          let deps : Http_server.deps =
            { service; advisor; config; verify_bearer = (fun _ -> Error ()); web_auth = None; now = service.now }
          in
          let send reqd value = Http_server.respond_with reqd (Http_server.json 200 value) in
          let handler connection_sw on_stream_end =
            Httpun_eio.Server.create_connection_handler ~sw:connection_sw
              ~error_handler:(fun _ ?request:_ _ _ -> ())
              ~request_handler:(fun _ wrapped ->
                let reqd = wrapped.Gluten.Reqd.reqd in
                match (Httpun.Reqd.request reqd).target with
                | "/fixture/session" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    send reqd (`Assoc [ ("a", credentials token); ("b", credentials token_b) ])
                | "/release" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    if not !released then (
                      released := true;
                      Eio.Promise.resolve release ());
                    send reqd (`String "ok")
                | ("/fixture/expire-a" | "/fixture/expire-b") as path ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    Sql.run db Web_auth.touch_session (service.now ()) Ptime.epoch
                      (Web_auth.hash (if path = "/fixture/expire-a" then token else token_b));
                    send reqd (`String "ok")
                | "/fixture/prune" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    Advisor_store.prune (Advisor_store.create db)
                      ~now:(Option.get (Ptime.add_span (service.now ()) (Ptime.Span.of_int_s (2 * 86400))));
                    send reqd (`String "ok")
                | "/state" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    send reqd
                      (`Assoc
                         [
                           ("drain_timeouts", `Int !drain_timeouts);
                           ("released", `Bool !released);
                           ("producer_closed", `Bool !producer_closed);
                           ("connections", `Int !connections);
                           ( "blocked_writes",
                             `Int
                               (Hashtbl.fold
                                  (fun _ since count ->
                                    if clock.monotonic_s () -. since >= 0.05 then count + 1 else count)
                                  pending_writes 0) );
                         ])
                | ("/" | "/main.bc.js") as path when Array.length Sys.argv > 1 ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    let file = if path = "/" then "index.html" else "main.bc.js" in
                    let body = In_channel.with_open_bin (Filename.concat Sys.argv.(1) file) In_channel.input_all in
                    Http_server.respond_with reqd
                      (Http_server.respond 200 (if path = "/" then "text/html" else "application/javascript") body)
                | _ -> Http_server.serve_request ~sw:connection_sw ~on_stream_end deps reqd)
          in
          let listener =
            Eio.Net.listen ~sw ~reuse_addr:true ~backlog:32 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 18197))
          in
          Eio.Net.run_server
            ~on_error:(fun _ -> ())
            listener
            (fun socket addr ->
              incr connections;
              incr next_connection;
              let id = !next_connection in
              if small_buffers then
                Eio_unix.Fd.use_exn "fixture SO_SNDBUF"
                  (Option.get (Eio_unix.Resource.fd_opt socket))
                  (fun fd -> Unix.setsockopt_int fd Unix.SO_SNDBUF 2048);
              let socket =
                let module Monitor = struct
                  type t = unit
                  type tag = [ `Generic ]

                  let read_methods = []
                  let single_read () buffer = Eio.Flow.single_read socket buffer

                  let single_write () buffers =
                    assert (not (guard.held ()));
                    Hashtbl.replace pending_writes id (clock.monotonic_s ());
                    Fun.protect
                      ~finally:(fun () -> Hashtbl.remove pending_writes id)
                      (fun () -> Eio.Flow.single_write socket buffers)

                  let copy () ~src = Eio.Flow.Pi.simple_copy ~single_write () ~src
                  let shutdown () direction = Eio.Flow.shutdown socket direction
                  let close () = Eio.Flow.close socket
                  let setsockopt () option value = Eio.Net.setsockopt socket option value
                  let getsockopt () option = Eio.Net.getsockopt socket option
                end in
                Eio.Resource.T ((), Eio.Net.Pi.stream_socket (module Monitor))
              in
              Fun.protect
                ~finally:(fun () -> decr connections)
                (fun () ->
                  try
                    Eio.Cancel.sub (fun context ->
                        Eio.Switch.run (fun connection_sw ->
                            Fun.protect
                              ~finally:(fun () -> Eio.Cancel.cancel context Exit)
                              (fun () ->
                                handler connection_sw
                                  (fun result ->
                                    Http_server.finish_stream ~sw:connection_sw ~sleep_s:clock.sleep_s
                                      ~abort:(fun () ->
                                        if result = Advisor_http.Drained then incr drain_timeouts;
                                        Eio.Cancel.cancel context Exit)
                                      result)
                                  addr socket)))
                  with Eio.Cancel.Cancelled Exit -> ()))))
