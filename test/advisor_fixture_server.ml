module Sse = Cashflow_advisor_protocol.Sse
module Http_sse = Cashflow_io.Http_sse
module A = Cashflow_core.Advisor
module R = Cashflow_io.Advisor_runtime
module X = Cashflow_io.Advisor_access
let ok = Result.get_ok
let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let released = ref false and closed = ref false in
  let gate, release = Eio.Promise.create () in
  let db = Cashflow_io.Store_sqlite.open_memory () in
  let guard = Cashflow_io.Db_guard.make () in
  Cashflow_io.Sql.set_guard db guard;
  Eio.Switch.on_release sw (fun () -> Cashflow_io.Sql.close db);
  let service = Cashflow_io.Service.make ~now:Ptime_clock.now db in
  let access = Cashflow_io.Service.sign_in service
    {issuer="https://synthetic.invalid";subject="advisor-fixture";display_name=None;email=None;is_admin=Some false;locale=None} in
  let token = Cashflow_io.Web_auth.create_session db ~now:(service.now ()) (Cashflow_core.Access.user access) in
  let principal = ok (X.authenticate service ~now:(service.now ()) ~token) in
  ignore (ok (X.set_user_consent service principal ~enabled:true));
  let post ~uri:_ ~headers:_ ~body:_ ~on_chunk =
    assert (not (guard.held ()));
    let text value = "data: " ^ Yojson.Safe.to_string (`Assoc ["choices",`List [`Assoc ["index",`Int 0;"delta",`Assoc ["content",`String value];"finish_reason",`Null]]]) ^ "\n\n" in
    on_chunk (text "first");
    Eio.Promise.await gate;
    assert (not (guard.held ()));
    on_chunk (text " second" ^ "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n"); Ok () in
  let provider = Cashflow_io.Advisor_provider.create
    ~limits:{context_tokens=262144;completion_tokens=1024;whole_completion_cap_supported=true;request_tokens_per_byte=1;request_token_overhead=0}
    ~post ~api_key:"synthetic" ~model:"synthetic" ~provider_only:["synthetic"] () in
  let origin = Eio.Time.Mono.now env#mono_clock in
  let clock : R.clock = {now=Ptime_clock.now;monotonic_s=(fun () -> Mtime.span origin (Eio.Time.Mono.now env#mono_clock) |> Mtime.Span.to_float_ns |> fun ns -> ns /. 1e9);sleep_s=Eio.Time.sleep env#clock} in
  let runtime = R.create ~sw ~service ~provider ~clock in
  let request : A.submit = {conversation_id=None;scope=Personal;currency=ok (Cashflow_core.Currency.make ~code:"SEK" ~scale:2);
   locale="en";user_text="Synthetic streaming fixture";idempotency_key=ok (Cashflow_core.Idempotency_key.of_string "fixture");expected_version=0L;source_finding_key=None} in
  let send reqd text = Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers:(Httpun.Headers.of_list ["content-length", string_of_int (String.length text)]) `OK) text in
  let handler sw = Httpun_eio.Server.create_connection_handler ~sw
    ~error_handler:(fun _ ?request:_ _ _ -> ())
    ~request_handler:(fun _ wrapped ->
      let reqd = wrapped.Gluten.Reqd.reqd in
      Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
      match (Httpun.Reqd.request reqd).target with
      | "/events" -> Eio.Fiber.fork ~sw (fun () ->
          let writer = Http_sse.open_response reqd in
          let accepted = ok (R.submit runtime ~principal request) in
          let subscription = ok (R.subscribe runtime ~principal ~run:accepted.ids.run_id ~after:0L) in
          Fun.protect ~finally:(fun () -> R.close subscription; Http_sse.close writer; closed := true) (fun () ->
            let rec send () =
              assert (not (guard.held ()));
              match R.next subscription with
              | Error _ | Ok (R.Resync_required _) -> ()
              | Ok (R.Events events) ->
                let written = List.for_all (fun event -> match event.A.payload with
                 | A.Text_appended delta ->
                   assert (not (guard.held ()));
                   Result.is_ok (Http_sse.write_frame writer {Sse.id=None;event=Some "advisor";data=delta.text})
                 | _ -> true) events in
                if written then send ()
              | Ok R.Heartbeat -> if Result.is_ok (Http_sse.heartbeat writer) then send ()
              | Ok R.End ->
                let rec heartbeats () =
                  Eio.Time.sleep env#clock 0.05;
                  assert (not (guard.held ()));
                  if Result.is_ok (Http_sse.heartbeat writer) then heartbeats () in
                heartbeats () in
            send ()))
      | "/release" -> if not !released then (released := true; Eio.Promise.resolve release ()); send reqd "ok"
      | "/state" ->
          let state = guard.exclusive (fun () -> (if !released then "released" else "waiting") ^ (if !closed then " closed" else " open")) in
          send reqd state
      | "/" | "/main.bc.js" as path ->
          let file = if path = "/" then "index.html" else "main.bc.js" in
          let ch = open_in_bin (Filename.concat Sys.argv.(1) file) in
          let body = Fun.protect ~finally:(fun () -> close_in ch) (fun () -> really_input_string ch (in_channel_length ch)) in
          let headers = Httpun.Headers.of_list ["content-length", string_of_int (String.length body); "content-type", if path = "/" then "text/html" else "application/javascript"] in
          Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers `OK) body
      | _ -> Httpun.Reqd.respond_with_string reqd (Httpun.Response.create `Not_found) "") in
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:32 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback,18197)) in
  Eio.Net.run_server ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn)) listener (fun socket addr ->
    try Eio.Cancel.sub (fun context ->
      Eio.Switch.run (fun connection_sw ->
        Fun.protect ~finally:(fun () -> Eio.Cancel.cancel context Exit)
          (fun () -> handler connection_sw addr socket)))
    with Eio.Cancel.Cancelled Exit -> ())
