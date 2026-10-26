open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Transport = Cashflow_advisor_stream.Transport
let text = Bonsai.Expert.Var.create ""
let connection = Bonsai.Expert.Var.create "Connecting"
let subscription = ref None
let app _graph =
  let%arr text = Bonsai.Expert.Var.value text
  and connection = Bonsai.Expert.Var.value connection in
  Vdom.Node.div [
    Vdom.Node.div ~attrs:[Vdom.Attr.create "data-testid" "stream-text"] [Vdom.Node.text text];
    Vdom.Node.div ~attrs:[Vdom.Attr.create "data-testid" "connection"] [Vdom.Node.text connection];
    Vdom.Node.button ~attrs:[Vdom.Attr.create "data-testid" "close"; Vdom.Attr.on_click (fun _ -> Effect.of_thunk (fun () -> Option.iter !subscription ~f:Transport.close))] [Vdom.Node.text "Close"]]
let () =
  Async_js.init ();
  let pending = Buffer.create 128 in
  let scheduled = ref false in
  subscription := Some (Transport.connect ~url:"/events" ~after:None
    ~on_frame:(fun frame ->
      Buffer.add_string pending frame.data;
      if not !scheduled then (
        scheduled := true;
        Async_kernel.don't_wait_for (
          let open Async_kernel.Deferred.Let_syntax in
          let%map () = Async_kernel.after (Time_ns.Span.of_ms 40.) in
          scheduled := false;
          Bonsai.Expert.Var.set text (Bonsai.Expert.Var.get text ^ Buffer.contents pending);
          Buffer.clear pending)))
    ~on_connection:(fun state -> Bonsai.Expert.Var.set connection (match state with Connecting -> "Connecting" | Connected -> "Connected" | Reconnecting -> "Reconnecting" | Closed -> "Closed")));
  Bonsai_web.Start.start app
