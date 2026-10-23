module Sse = Cashflow_advisor_protocol.Sse
module Http_sse = Cashflow_io.Http_sse
let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let released = ref false and closed = ref false in
  let gate, release = Eio.Promise.create () in
  let guard = Cashflow_io.Db_guard.make () in
  let send reqd text = Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers:(Httpun.Headers.of_list ["content-length", string_of_int (String.length text)]) `OK) text in
  let handler sw = Httpun_eio.Server.create_connection_handler ~sw
    ~error_handler:(fun _ ?request:_ _ _ -> ())
    ~request_handler:(fun _ wrapped ->
      let reqd = wrapped.Gluten.Reqd.reqd in
      Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
      match (Httpun.Reqd.request reqd).target with
      | "/events" -> Eio.Fiber.fork ~sw (fun () ->
          let writer = Http_sse.open_response reqd in
          Fun.protect ~finally:(fun () -> Http_sse.close writer; closed := true) (fun () ->
            let frame data = {Sse.id=None; event=Some "advisor"; data} in
            assert (not (guard.held ()));
            match Http_sse.write_frame writer (frame "first") with Error _ -> () | Ok () ->
            Eio.Promise.await gate;
            assert (not (guard.held ()));
            ignore (Http_sse.write_frame writer (frame " second"));
            let rec heartbeats () =
              Eio.Time.sleep env#clock 0.05;
              match Http_sse.heartbeat writer with Ok () -> heartbeats () | Error _ -> () in
            heartbeats ()))
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
