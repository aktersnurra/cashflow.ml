open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire
module T = Cashflow_advisor_stream.Transport

type controller = {
  view : C.view;
  dispatch : C.action -> unit Effect.t;
  open_finding : key:string -> scope:W.scope -> currency:W.currency -> locale:string -> unit Effect.t;
  open_conversation : string -> unit Effect.t;
  open_empty : scope:W.scope -> currency:W.currency -> locale:string -> unit Effect.t;
  open_latest : unit Effect.t;
  leave_surface : unit Effect.t;
  stop : unit Effect.t;
  invalidate : unit Effect.t;
}

type resources = {
  mutable invalidated : bool;
  mutable subscription : (C.token * T.subscription) option;
  mutable timer : (C.token * Dom_html.timeout_id_safe) option;
  mutable visibility : Dom.event_listener_id option;
  mutable visibility_source : C.token option;
  mutable current : C.t;
  mutable ingress_token : C.token option;
  mutable queued_events : int;
  mutable queued_bytes : int;
  mutable resync_queued : bool;
}

let run_effect = Effect.Expert.handle_non_dom_event_exn

let execute r inject command =
  let send a = run_effect (inject a) in
  let valid c = C.command_is_current r.current c in
  let close_obsolete () =
    match r.subscription with
    | Some (tok, s) when not (valid (C.Schedule_flush tok)) ->
        r.subscription <- None;
        T.close s
    | _ -> ()
  in
  let cancel_obsolete () =
    match r.timer with
    | Some (tok, id) when not (valid (C.Schedule_flush tok)) ->
        Dom_html.clearTimeout id;
        r.timer <- None
    | _ -> ()
  in
  if valid command then
    match command with
    | C.Unsubscribe -> close_obsolete ()
    | Cancel_flush -> cancel_obsolete ()
    | Announce_status _ -> () (* The view owns the single polite status region. *)
    | Post_turn (tok, p) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.submit p in
           send (C.Accepted (tok, x)))
    | Retry_turn (tok, p) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.retry p in
           send (C.Accepted (tok, x)))
    | Fetch_snapshot (tok, id) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.snapshot id in
           send (C.Snapshot (tok, x)))
    | Get_status tok ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.status () in
           send (C.Status (tok, x)))
    | List_conversations (tok, limit, offset) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.history ~limit ~offset in
           send (C.History (tok, x)))
    | Set_consent (tok, enabled, household_id) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.consent ~enabled ~household_id in
           send (C.Consent_result (tok, x)))
    | Delete_conversation (tok, id) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.delete id in
           send (C.Deleted (tok, x)))
    | Cancel_run (tok, ids) ->
        don't_wait_for
          (let%map.Deferred x = Advisor_api.cancel ids.run_id in
           send (C.Stopped (tok, x)))
    | Schedule_flush tok ->
        (match r.timer with Some (_, id) -> Dom_html.clearTimeout id | None -> ());
        let id =
          Dom_html.setTimeout
            (fun () ->
              (match r.timer with Some (t, _) when Poly.equal t tok -> r.timer <- None | _ -> ());
              send (C.Flush tok))
            40.
        in
        r.timer <- Some (tok, id)
    | Subscribe (tok, ids, after) ->
        close_obsolete ();
        r.ingress_token <- Some tok;
        r.queued_events <- 0;
        r.queued_bytes <- 0;
        r.resync_queued <- false;
        let resync action =
          if not r.resync_queued then (
            r.resync_queued <- true;
            send action)
        in
        let on_frame (f : Cashflow_advisor_protocol.Sse.frame) =
          if valid (C.Subscribe (tok, ids, after)) && not r.resync_queued then
            if Js.to_bool Dom_html.document##.hidden then resync (C.Background tok)
            else
              match f.event with
              | Some "advisor" -> (
                  match W.decode_event f.data with
                  | Ok e when Option.equal String.equal f.id (Some e.sequence) ->
                      if valid (C.Subscribe (tok, e.ids, e.sequence)) then
                        let bytes = String.length (W.encode_event e) in
                        if r.queued_events >= 128 || r.queued_bytes + bytes > 65536 then resync (C.Resync tok)
                        else (
                          r.queued_events <- r.queued_events + 1;
                          r.queued_bytes <- r.queued_bytes + bytes;
                          send (C.Event (tok, e)))
                  | _ -> resync (C.Stream_invalid tok))
              | Some "resync_required" ->
                  let valid_resync =
                    try
                      match Yojson.Safe.from_string f.data with
                      | `Assoc fields -> (
                          match
                            ( List.Assoc.find fields "version" ~equal:String.equal,
                              List.Assoc.find fields "after" ~equal:String.equal )
                          with
                          | Some (`Int 1), Some (`String after) ->
                              let n = Int64.of_string after in
                              Int64.(n >= 0L) && String.equal after (Int64.to_string n)
                          | _ -> false)
                      | _ -> false
                    with _ -> false
                  in
                  resync (if valid_resync then C.Resync tok else C.Stream_invalid tok)
              | _ -> resync (C.Stream_invalid tok)
        in
        let on_connection = function
          | T.Connecting -> send (C.Connection (tok, C.Connecting))
          | Connected -> send (C.Connection (tok, C.Connected))
          | Reconnecting -> send (C.Connection (tok, C.Reconnecting))
          | Closed -> ()
        in
        let s =
          T.connect
            ~url:("/api/advisor/runs/" ^ Url.urlencode ids.run_id ^ "/events")
            ~after:(Some after) ~on_frame ~on_connection
        in
        r.subscription <- Some (tok, s);
        r.visibility_source <- Some tok;
        if Js.to_bool Dom_html.document##.hidden then send (C.Background tok)

let component ~ctx:_ graph =
  let resources =
    (* Allocated once per shell component instance, never at module scope. *)
    let r =
      {
        invalidated = false;
        subscription = None;
        timer = None;
        visibility = None;
        visibility_source = None;
        current = C.empty;
        ingress_token = None;
        queued_events = 0;
        queued_bytes = 0;
        resync_queued = false;
      }
    in
    r
  in
  let model, dispatch =
    Bonsai.state_machine0 ~default_model:C.empty
      ~apply_action:(fun context state action ->
        (match action with
        | C.Event (tok, e)
          when Poly.equal resources.ingress_token (Some tok)
               && C.command_is_current state (C.Subscribe (tok, e.ids, e.sequence)) ->
            resources.queued_events <- max 0 (resources.queued_events - 1);
            resources.queued_bytes <- max 0 (resources.queued_bytes - String.length (W.encode_event e))
        | _ -> ());
        let state, commands =
          if resources.invalidated && not (Poly.equal action C.Auth_lost || Poly.equal action C.Deactivate)
          then resources.current, [] else C.reduce state action in
        resources.current <- state;
        List.iter commands ~f:(fun command ->
            Bonsai.Apply_action_context.schedule_event context
              (Effect.of_thunk (fun () -> execute resources (Bonsai.Apply_action_context.inject context) command)));
        state)
      graph
  in
  let activate =
    let%arr dispatch in
    Effect.Many
      [
        dispatch C.Activate;
        Effect.of_thunk (fun () ->
            let id =
              Dom_html.addEventListener Dom_html.document
                (Dom_html.Event.make "visibilitychange")
                (Dom_html.handler (fun _ ->
                     (match resources.visibility_source with
                     | Some tok ->
                         run_effect
                           (dispatch
                              (if Js.to_bool Dom_html.document##.hidden then C.Background tok else C.Foreground tok))
                     | _ -> ());
                     Js._true))
                Js._false
            in
            resources.visibility <- Some id);
      ]
  in
  let deactivate =
    let%arr dispatch in
    Effect.Many
      [
        dispatch C.Deactivate;
        Effect.of_thunk (fun () ->
            Option.iter resources.visibility ~f:Dom_html.removeEventListener;
            resources.visibility <- None);
      ]
  in
  Bonsai.Edge.lifecycle ~on_activate:activate ~on_deactivate:deactivate graph;
  let%arr model and dispatch in
  {
    view = C.view model;
    dispatch;
    open_finding = (fun ~key ~scope ~currency ~locale -> dispatch (C.Open_finding (key, scope, currency, locale)));
    open_empty = (fun ~scope ~currency ~locale -> dispatch (C.Open_empty (scope, currency, locale)));
    open_conversation = (fun id -> dispatch (C.Open_conversation id));
    open_latest = dispatch C.Open_latest;
    leave_surface = dispatch C.Leave_surface;
    stop = dispatch C.Stop;
    invalidate =
      Effect.Many [Effect.of_thunk (fun () ->
        resources.invalidated <- true;
        resources.current <- fst (C.reduce resources.current C.Auth_lost);
        Option.iter resources.subscription ~f:(fun (_, s) -> T.close s);
        resources.subscription <- None;
        Option.iter resources.timer ~f:(fun (_, id) -> Dom_html.clearTimeout id);
        resources.timer <- None;
        resources.visibility_source <- None);
        dispatch C.Auth_lost];
  }
