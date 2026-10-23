type writer = Httpun.Body.Writer.t
type write_error = Closed
let open_response reqd =
  let headers = Httpun.Headers.of_list ["transfer-encoding", "chunked"; "content-type", "text/event-stream"; "cache-control", "no-cache"; "x-accel-buffering", "no"] in
  Httpun.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd (Httpun.Response.create ~headers `OK)
let write writer text =
  if Httpun.Body.Writer.is_closed writer then Error Closed
  else (
    Httpun.Body.Writer.write_string writer text;
    let p, r = Eio.Promise.create () in
    Httpun.Body.Writer.flush writer (Eio.Promise.resolve r);
    match Eio.Promise.await p with `Written -> Ok () | `Closed -> Error Closed)
let write_frame w f = write w (Cashflow_advisor_protocol.Sse.encode f)
let heartbeat w = write w ": heartbeat\n\n"
let close = Httpun.Body.Writer.close
