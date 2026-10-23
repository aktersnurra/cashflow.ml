open! Core
open Bonsai_web
open Js_of_ocaml
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

module Follow_hook = Vdom.Attr.Hooks.Make (struct
  module Input = struct
    type t = bool [@@deriving sexp_of]

    let combine _ b = b
  end

  module State = struct
    type t = { mutable following : bool; mutable listener : Dom.event_listener_id option }
  end

  let schedule (state : State.t) element =
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback (fun _ -> if state.following then element##.scrollTop := element##.scrollHeight)))

  let init following element =
    let state : State.t = { following; listener = None } in
    let id =
      Dom_html.addEventListener element Dom_html.Event.scroll
        (Dom_html.handler (fun _ ->
             state.following <- element##.scrollHeight - element##.scrollTop - element##.clientHeight <= 48;
             Js._true))
        Js._false
    in
    state.listener <- Some id;
    state

  let on_mount = `Schedule_animation_frame (fun _ state element -> schedule state element)

  let update ~old_input ~new_input (state : State.t) element =
    if not (Bool.equal old_input new_input) then state.following <- new_input;
    if new_input then schedule state element

  let destroy _ (state : State.t) _ =
    state.following <- false;
    Option.iter state.listener ~f:Dom_html.removeEventListener;
    state.listener <- None
end)

let notice locale = function
  | C.Quiet -> ""
  | Interrupted -> Msg.advisor_interrupted locale
  | Request_failed -> Msg.advisor_request_failed locale
  | Choose_context -> Msg.advisor_choose_context locale
  | Busy -> Msg.advisor_busy locale
  | Invalid_state -> Msg.advisor_invalid_state locale
  | Answer_completed -> Msg.advisor_completed locale
  | Answer_stopped -> Msg.advisor_stopped locale
  | Answer_failed -> Msg.advisor_failed locale
  | Answer_running -> Msg.advisor_running locale
  | Stopping -> Msg.advisor_cancel_requested locale

let run_status locale = function
  | W.Completed -> Msg.advisor_completed locale
  | Cancelled -> Msg.advisor_stopped locale
  | Failed _ -> Msg.advisor_failed locale
  | Cancel_requested -> Msg.advisor_cancel_requested locale
  | Queued | Running _ -> Msg.advisor_running locale

let render_body ~show_source ~locale ~(view : C.view) ~dispatch =
  let text = Vdom.Node.text in
  let button ?(kind = "secondary") label a =
    Vdom.Node.button ~attrs:[ Vdom.Attr.class_ ("button " ^ kind); Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> dispatch a) ] [ text label ]
  in
  let segment = function
    | W.Text_segment d ->
        Vdom.Node.div ~key:d.segment_id ~attrs:[ Vdom.Attr.style (Css_gen.white_space `Pre_wrap) ] [ text d.text ]
    | W.Evidence_segment e ->
        (* References are authorized opaque IDs, never model-controlled URLs. *)
        let facts =
          List.map e.facts ~f:(fun (f : W.fact) ->
              Vdom.Node.div
                [
                  text (Advisor_evidence_text.label locale f.label);
                  text ": ";
                  text (Advisor_evidence_text.value locale f);
                ])
        in
        Vdom.Node.div ~key:e.id
          ~attrs:[ Vdom.Attr.id ("advisor-evidence-" ^ e.id) ]
          ((text (Advisor_evidence_text.title locale e.title) :: text e.sampled_at :: facts)
          @ [
              text
                (match e.import_status with
                | W.Current -> Msg.advisor_current locale
                | Stale -> Msg.advisor_stale locale
                | Incomplete -> Msg.advisor_incomplete locale
                | Unknown -> Msg.advisor_unknown locale);
            ]
          @ (if e.reconciled then [] else [ text (Msg.advisor_unreconciled locale) ])
          @ List.map e.assumptions ~f:(fun a -> text (Advisor_evidence_text.assumption locale a)))
  in
  let attempt (a : W.attempt) =
    let foreground = match view.snapshot with Some s -> Option.equal String.equal s.active_run (Some a.ids.run_id) | None -> false in
    Vdom.Node.div ~key:a.ids.attempt_id ~attrs:[Vdom.Attr.class_ "advisor-answer"]
      (List.map a.segments ~f:segment @ if foreground then [] else [ Vdom.Node.p ~attrs:[Vdom.Attr.class_ "muted"] [ text (run_status locale a.state) ] ])
  in
  let turns =
    match view.snapshot with
    | None -> []
    | Some s ->
        List.map s.turns ~f:(fun (t : W.turn) ->
            Vdom.Node.div ~key:t.turn_id
              (Vdom.Node.p ~key:t.user_message_id ~attrs:[Vdom.Attr.class_ "advisor-user-message"] [ text t.user_text ] :: List.map t.attempts ~f:attempt))
  in
  let retryable =
    match view.snapshot with
    | Some s -> (
        match List.last s.turns with
        | Some t -> (
            match List.last t.attempts with
            | Some a -> ( match a.state with W.Cancelled | W.Failed _ -> true | _ -> false)
            | None -> false)
        | None -> false)
    | None -> false
  in
  let generation_ready = match view.status with Some s -> s.available && s.consent | None -> false in
  let active = match view.snapshot with Some s -> Option.is_some s.active_run | None -> false in
  Vdom.Node.div
    ~attrs:
      [
        Vdom.Attr.class_ "advisor-conversation";
        Vdom.Attr.create_hook "advisor-follow" (Follow_hook.create view.following);
        Vdom.Attr.on_scroll (fun ev ->
            match Js.Opt.to_option ev##.target with
            | None -> Effect.Ignore
            | Some target ->
                let el : Dom_html.element Js.t = Js.Unsafe.coerce target in
                dispatch (C.Follow (el##.scrollHeight - el##.scrollTop - el##.clientHeight <= 48)));
      ]
    ([
       Vdom.Node.div
         ~attrs:
           [
             Vdom.Attr.create "role" "status";
             Vdom.Attr.create "aria-live" "polite";
             Vdom.Attr.create "aria-atomic" "true";
           ]
         [ text (notice locale view.notice) ];
     ]
    @ (match view.status with
      | Some s when not s.consent -> [ Vdom.Node.p [ text (Msg.advisor_consent_required locale) ] ]
      | Some s when not s.available -> [ Vdom.Node.p [ text (Msg.advisor_unavailable locale) ] ]
      | _ -> [])
    @ (match view.snapshot with
      | Some { source_finding = Some f; _ } when show_source -> [ Vdom.Node.h2 [ text f.title ]; Vdom.Node.p [ text f.summary ];
          (if String.equal f.kind "shortfall" then Ui.muted (Msg.advisor_bills_estimate locale) else Vdom.Node.none) ]
      | _ -> [])
    @ turns
    @ (if generation_ready then [
        Vdom.Node.textarea
          ~attrs:
            [
              Vdom.Attr.create "aria-label" (Msg.advisor_draft locale);
              Vdom.Attr.value_prop view.draft;
              Vdom.Attr.on_input (fun _ s -> dispatch (C.Draft_changed s));
            ]
          [];
        button ~kind:"primary" (Msg.advisor_send locale) (C.Submit_draft (Api.fresh_key "advisor"));
      ] else [])
    @ (if
         view.can_reconnect
         && (active || List.mem [ C.Interrupted; C.Request_failed; C.Invalid_state ] view.notice ~equal:Poly.equal)
       then [ button (Msg.advisor_reconnect locale) C.Reconnect ]
       else [])
    @ (if active then [ button (Msg.advisor_stop locale) C.Stop ]
       else if generation_ready && retryable then [ button (Msg.advisor_retry locale) (C.Retry_latest (Api.fresh_key "advisor-retry")) ]
       else [])
    @ (if generation_ready && Option.is_some view.pending then [ button (Msg.advisor_repeat locale) C.Repeat_pending ] else [])
    @ (if view.new_output then [ button (Msg.advisor_follow locale) (C.Follow true) ] else [])
    @ if Option.is_some view.snapshot then
      [Vdom.Node.button ~attrs:[Vdom.Attr.class_ "button secondary"; Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ ->
        if Js.to_bool (Dom_html.window##confirm (Js.string (Msg.advisor_delete_confirm locale))) then dispatch C.Delete else Effect.Ignore)]
        [text (Msg.advisor_delete locale)]] else [])

let render ~locale ~view ~dispatch = render_body ~show_source:true ~locale ~view ~dispatch
let render_inline ~locale ~view ~dispatch = render_body ~show_source:false ~locale ~view ~dispatch

let consent ~locale ~(view : C.view) ~dispatch ~decline =
  match view.status with
  | None -> Ui.muted (Msg.advisor_choose_context locale)
  | Some status when not status.available && not status.consent -> Ui.muted (Msg.advisor_unavailable locale)
  | Some status ->
      Ui.div ~cls:"advisor-consent"
        [(if status.available then Vdom.Node.none else Ui.muted (Msg.advisor_unavailable locale));
         Ui.p (Msg.advisor_disclosure locale);
         Ui.div ~cls:"actions"
           (if status.consent then [Ui.button (Msg.advisor_disable locale) (dispatch (C.Consent (false, None)))]
            else [Ui.button ~kind:"primary" (Msg.advisor_enable locale) (dispatch (C.Consent (true, None)));
                  Ui.button (Msg.advisor_decline locale) decline])]

let household_setting ~locale ~(view : C.view) ~dispatch =
  match view.status with
  | Some { household = Some h; _ } when h.can_manage ->
      Ui.div ~cls:"panel"
        [Ui.p (Msg.advisor_disclosure locale);
         Vdom.Node.button ~attrs:[Vdom.Attr.type_ "button"; Vdom.Attr.class_ "button";
           Vdom.Attr.create "role" "switch"; Vdom.Attr.create "aria-checked" (Bool.to_string h.enabled);
           Vdom.Attr.on_click (fun _ -> dispatch (C.Consent (not h.enabled, Some h.household_id)))]
           [Ui.text (if h.enabled then Msg.advisor_household_disable locale else Msg.advisor_household_enable locale)]]
  | _ -> Vdom.Node.none
