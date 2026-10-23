open Js_of_ocaml

type connection = Connecting | Connected | Reconnecting | Closed
type subscription = { source : Js.Unsafe.any; mutable closed : bool; notify : connection -> unit }

let close t =
  if not t.closed then (
    t.closed <- true;
    ignore (Js.Unsafe.meth_call t.source "close" [||]);
    t.notify Closed)

let connect ~url ~after ~on_frame ~on_connection =
  let url =
    match after with
    | None -> url
    | Some id -> url ^ (if String.contains url '?' then "&" else "?") ^ "after=" ^ Url.urlencode id
  in
  on_connection Connecting;
  let source = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "EventSource") [| Js.Unsafe.inject (Js.string url) |] in
  let t = { source; closed = false; notify = on_connection } in
  let listen name callback =
    ignore
      (Js.Unsafe.meth_call source "addEventListener"
         [|
           Js.Unsafe.inject (Js.string name);
           Js.Unsafe.inject (Js.wrap_callback (fun e -> if not t.closed then callback e));
         |])
  in
  listen "open" (fun _ -> on_connection Connected);
  listen "error" (fun _ ->
      (* The controller probes the authorized snapshot before reconnecting. *)
      close t;
      on_connection Reconnecting);
  listen "advisor" (fun e ->
      let data = Js.to_string (Js.Unsafe.get e "data") in
      if String.length data > 262144 then (
        close t;
        on_connection Reconnecting)
      else
        let id = Js.to_string (Js.Unsafe.get e "lastEventId") in
        on_frame
          {
            Cashflow_advisor_protocol.Sse.id = (if String.length id = 0 then None else Some id);
            event = Some "advisor";
            data;
          });
  listen "resync_required" (fun e ->
      let data = Js.to_string (Js.Unsafe.get e "data") in
      if String.length data > 262144 then on_connection Reconnecting
      else on_frame { Cashflow_advisor_protocol.Sse.id = None; event = Some "resync_required"; data };
      close t);
  t
