type writer = { body : Httpun.Body.Writer.t; authorize : unit -> bool }
type write_error = Closed

(* HTTP/1.x only: avoid Httpun deferring EOF reads during a persistent response.
   No reuse after SSE; this does not buffer or immediately close the stream. *)
let response_headers =
  [
    ("connection", "close");
    ("transfer-encoding", "chunked");
    ("content-type", "text/event-stream");
    ("cache-control", "private, no-store, no-transform");
    ("x-accel-buffering", "no");
    ("x-content-type-options", "nosniff");
    ("referrer-policy", "same-origin");
    ("x-frame-options", "DENY");
    ("content-security-policy", "default-src 'self'; frame-ancestors 'none'; base-uri 'none'");
  ]

let open_response_checked ~authorize reqd =
  if not (authorize ()) then Error Closed
  else
    let headers = Httpun.Headers.of_list response_headers in
    let body =
      Httpun.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd (Httpun.Response.create ~headers `OK)
    in
    Ok { body; authorize }

let open_response reqd = Result.get_ok (open_response_checked ~authorize:(fun () -> true) reqd)
let is_closed writer = Httpun.Body.Writer.is_closed writer.body
let close writer = Httpun.Body.Writer.close writer.body

let write writer text =
  if Httpun.Body.Writer.is_closed writer.body || not (writer.authorize ()) then (
    close writer;
    Error Closed)
  else (
    Httpun.Body.Writer.write_string writer.body text;
    let p, r = Eio.Promise.create () in
    Httpun.Body.Writer.flush writer.body (Eio.Promise.resolve r);
    match Eio.Promise.await p with
    | `Written when writer.authorize () -> Ok ()
    | `Written | `Closed ->
        close writer;
        Error Closed)

let write_frame w f =
  let text = Cashflow_advisor_protocol.Sse.encode f in
  if String.length text > Cashflow_core.Advisor.limits.frame_bytes then (
    close w;
    Error Closed)
  else write w text

let heartbeat w = write w ": heartbeat\n\n"
