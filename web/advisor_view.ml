open! Core
open Bonsai_web
open Js_of_ocaml
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

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

let sampled_time locale sampled =
  let date : Js.date Js.t =
    Js.Unsafe.new_obj (Js.Unsafe.get Js.Unsafe.global "Date") [| Js.Unsafe.inject (Js.string sampled) |]
  in
  let formatted : Js.js_string Js.t =
    Js.Unsafe.meth_call date "toLocaleString"
      [|
        Js.Unsafe.inject (Js.string locale);
        Js.Unsafe.inject
          (Js.Unsafe.obj
             [|
               ("dateStyle", Js.Unsafe.inject (Js.string "medium")); ("timeStyle", Js.Unsafe.inject (Js.string "short"));
             |]);
      |]
  in
  Js.to_string formatted

let render_turns ~locale ~(snapshot : W.snapshot option) ~dispatch ~(markdown : Vdom.Node.t String.Map.t) =
  let text = Vdom.Node.text in
  let cls name = [ Vdom.Attr.class_ name ] in
  let segment = function
    | W.Text_segment d ->
        Vdom.Node.div ~key:d.segment_id ~attrs:(cls "advisor-prose")
          [ Option.value_or_thunk (Map.find markdown d.segment_id) ~default:(fun () -> Advisor_markdown.render d.text) ]
    | W.Evidence_segment e ->
        let facts =
          List.map e.facts ~f:(fun (f : W.fact) ->
              Vdom.Node.div ~attrs:(cls "advisor-fact")
                [
                  Vdom.Node.create "dt" [ text (Advisor_evidence_text.label locale f.label) ];
                  Vdom.Node.create "dd" [ text (Advisor_evidence_text.value locale f) ];
                ])
        in
        Vdom.Node.details ~key:e.id
          ~attrs:[ Vdom.Attr.class_ "advisor-evidence"; Vdom.Attr.id ("advisor-evidence-" ^ e.id) ]
          [
            Vdom.Node.summary
              ~attrs:[ Vdom.Attr.on_click (fun _ -> dispatch (C.Follow false)) ]
              ([ Vdom.Node.span [ text (Advisor_evidence_text.title locale e.title) ] ]
              @
              if (not (Poly.equal e.import_status W.Current)) || (not e.reconciled) || not (List.is_empty e.assumptions)
              then
                [
                  Vdom.Node.span ~attrs:(cls "advisor-evidence-warning") [ text (Msg.advisor_evidence_caution locale) ];
                ]
              else []);
            Vdom.Node.div ~attrs:(cls "advisor-evidence-body")
              ([
                 Vdom.Node.create "time"
                   ~attrs:[ Vdom.Attr.create "datetime" e.sampled_at ]
                   [ text (sampled_time locale e.sampled_at) ];
                 Vdom.Node.create "dl" ~attrs:(cls "advisor-facts") facts;
                 Vdom.Node.div ~attrs:(cls "advisor-caveats")
                   ([
                      Vdom.Node.span
                        [
                          text
                            (match e.import_status with
                            | W.Current -> Msg.advisor_current locale
                            | Stale -> Msg.advisor_stale locale
                            | Incomplete -> Msg.advisor_incomplete locale
                            | Unknown -> Msg.advisor_unknown locale);
                        ];
                    ]
                   @ if e.reconciled then [] else [ Vdom.Node.span [ text (Msg.advisor_unreconciled locale) ] ]);
               ]
              @
              if List.is_empty e.assumptions then []
              else
                [
                  Vdom.Node.ul
                    (List.map e.assumptions ~f:(fun a ->
                         Vdom.Node.li [ text (Advisor_evidence_text.assumption locale a) ]));
                ]);
          ]
  in
  let attempt (a : W.attempt) =
    let foreground =
      match snapshot with Some s -> Option.equal String.equal s.active_run (Some a.ids.run_id) | None -> false
    in
    Vdom.Node.create "article" ~key:a.ids.attempt_id ~attrs:(cls "advisor-answer")
      ([
         Vdom.Node.div ~attrs:(cls "advisor-answer-heading")
           [
             Vdom.Node.span
               ~attrs:[ Vdom.Attr.class_ "advisor-mark"; Vdom.Attr.create "aria-hidden" "true" ]
               [ text "✦" ];
             Vdom.Node.span [ text (Msg.nav_advisor locale) ];
           ];
       ]
      @ List.map a.segments ~f:segment
      @
      if foreground || Poly.equal a.state W.Completed then []
      else [ Vdom.Node.p ~attrs:(cls "advisor-attempt-status") [ text (run_status locale a.state) ] ])
  in
  match snapshot with
  | None -> []
  | Some s ->
      List.map s.turns ~f:(fun (t : W.turn) ->
          Vdom.Node.div ~key:t.turn_id ~attrs:(cls "advisor-turn")
            (Vdom.Node.p ~key:t.user_message_id
               ~attrs:
                 [ Vdom.Attr.class_ "advisor-user-message"; Vdom.Attr.create "aria-label" (Msg.advisor_you locale) ]
               [ text t.user_text ]
            :: List.map t.attempts ~f:attempt))

let render_body ~turns ~show_source ~locale ~(view : C.view) ~dispatch =
  let text = Vdom.Node.text in
  let cls name = [ Vdom.Attr.class_ name ] in
  let button label action =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ "button secondary"; Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> dispatch action) ]
      [ text label ]
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
  let can_submit = generation_ready && (not active) && Option.is_none view.pending in
  let can_send = can_submit && Advisor_chat_hooks.valid_draft view.draft in
  let submit = function
    | Some draft when can_submit && Advisor_chat_hooks.valid_draft draft ->
        Effect.Many [ dispatch (C.Draft_changed draft); dispatch (C.Submit_draft (Api.fresh_key "advisor")) ]
    | _ -> Effect.Ignore
  in
  let revision =
    match view.snapshot with
    | None -> ""
    | Some s ->
        s.conversation_id ^ ":"
        ^ String.concat ~sep:":"
            (List.concat_map s.turns ~f:(fun t -> List.map t.attempts ~f:(fun a -> a.ids.attempt_id ^ a.last_sequence)))
  in
  let has_messages = not (List.is_empty turns) in
  let follow : Advisor_chat_hooks.Follow_input.t =
    {
      following = view.following;
      revision;
      has_messages;
      on_follow = (fun following -> Effect.Expert.handle_non_dom_event_exn (dispatch (C.Follow following)));
    }
  in
  let composer =
    if not generation_ready then Vdom.Node.none
    else
      Vdom.Node.div ~attrs:(cls "advisor-composer")
        [
          Vdom.Node.textarea
            ~attrs:
              [
                Vdom.Attr.class_ "advisor-input";
                Vdom.Attr.create "aria-label" (Msg.advisor_draft locale);
                Vdom.Attr.placeholder (Msg.advisor_placeholder locale);
                Vdom.Attr.create "rows" "1";
                Vdom.Attr.value_prop view.draft;
                Vdom.Attr.create_hook "advisor-grow"
                  (Advisor_chat_hooks.Grow.create { draft = view.draft; blocked = not can_submit });
                Vdom.Attr.on_input (fun _ s ->
                    if String.length s <= Advisor_chat_hooks.input_bytes_limit then dispatch (C.Draft_changed s)
                    else Effect.Ignore);
                Vdom.Attr.on_keydown (fun event ->
                    let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
                    let composing : bool Js.t Js.optdef = Js.Unsafe.get event "isComposing" in
                    let repeat : bool Js.t Js.optdef = Js.Unsafe.get event "repeat" in
                    if
                      String.equal key "Enter"
                      && (not (Js.to_bool event##.shiftKey))
                      && (not (Js.to_bool event##.ctrlKey))
                      && (not (Js.to_bool event##.metaKey))
                      && (not (Js.to_bool event##.altKey))
                      && not (Js.Optdef.case composing (fun () -> false) Js.to_bool)
                    then
                      let draft = Advisor_chat_hooks.current_draft event ~fallback:view.draft in
                      let send =
                        if Js.Optdef.case repeat (fun () -> false) Js.to_bool then Effect.Ignore else submit draft
                      in
                      Effect.Many [ Effect.Prevent_default; send ]
                    else Effect.Ignore);
              ]
            [];
          Vdom.Node.div ~attrs:(cls "advisor-composer-bottom")
            [
              (let label = if active then Msg.advisor_stop locale else Msg.advisor_send locale in
               Vdom.Node.button
                 ~attrs:
                   [
                     Vdom.Attr.class_ "advisor-send";
                     Vdom.Attr.type_ "button";
                     Vdom.Attr.create "aria-label" label;
                     Vdom.Attr.title label;
                     (if (not active) && not can_send then Vdom.Attr.disabled else Vdom.Attr.empty);
                     (if active then Vdom.Attr.empty else Vdom.Attr.create "data-advisor-send" "");
                     Vdom.Attr.on_click (fun event ->
                         if active then dispatch C.Stop
                         else submit (Advisor_chat_hooks.current_draft event ~fallback:view.draft));
                   ]
                 [
                   Vdom.Node.create_svg "svg"
                     ~attrs:
                       [
                         Vdom.Attr.class_ "advisor-send-icon";
                         Vdom.Attr.create "viewBox" "0 0 24 24";
                         Vdom.Attr.create "width" "20";
                         Vdom.Attr.create "height" "20";
                         Vdom.Attr.create "fill" (if active then "currentColor" else "none");
                         Vdom.Attr.create "stroke" "currentColor";
                         Vdom.Attr.create "stroke-width" "1.5";
                         Vdom.Attr.create "stroke-linecap" "round";
                         Vdom.Attr.create "stroke-linejoin" "miter";
                         Vdom.Attr.create "aria-hidden" "true";
                       ]
                     [
                       (if active then
                          Vdom.Node.create_svg "rect"
                            ~attrs:
                              [ Vdom.Attr.create "x" "6"; Vdom.Attr.create "y" "6"; Vdom.Attr.create "width" "12";
                                Vdom.Attr.create "height" "12"; Vdom.Attr.create "rx" "2" ]
                            []
                        else
                          Vdom.Node.create_svg "path"
                            ~attrs:[ Vdom.Attr.create "d" "M9 6h8v8H4M8 10l-4 4 4 4" ]
                            []);
                     ];
                 ]);
            ];
          Vdom.Node.p
            ~attrs:[ Vdom.Attr.class_ "advisor-input-error"; Vdom.Attr.id "advisor-input-error" ]
            [ text (Msg.advisor_input_limit locale) ];
        ]
  in
  let actions =
    (if active && not generation_ready then [ button (Msg.advisor_stop locale) C.Stop ] else [])
    @ (if
         view.can_reconnect
         && (active || List.mem [ C.Interrupted; C.Request_failed; C.Invalid_state ] view.notice ~equal:Poly.equal)
       then [ button (Msg.advisor_reconnect locale) C.Reconnect ]
       else [])
    @ (if generation_ready && (not active) && retryable then
         [ button (Msg.advisor_retry locale) (C.Retry_latest (Api.fresh_key "advisor-retry")) ]
       else [])
    @ (if generation_ready && Option.is_some view.pending then [ button (Msg.advisor_repeat locale) C.Repeat_pending ]
       else [])
    @ (if view.new_output then [ button (Msg.advisor_follow locale) (C.Follow true) ] else [])

  in
  Vdom.Node.div
    ~attrs:
      [
        Vdom.Attr.class_ "advisor-conversation";
        Vdom.Attr.create_hook "advisor-follow" (Advisor_chat_hooks.Follow.create follow);
      ]
    ((match view.status with
       | Some s when not s.consent -> [ Vdom.Node.p [ text (Msg.advisor_consent_required locale) ] ]
       | Some s when not s.available -> [ Vdom.Node.p [ text (Msg.advisor_unavailable locale) ] ]
       | _ -> [])
    @ (match view.snapshot with
      | Some { source_finding = Some f; _ } when show_source ->
          [
            Vdom.Node.div ~attrs:(cls "advisor-source")
              [
                Vdom.Node.h2 [ text f.title ];
                Vdom.Node.p [ text f.summary ];
                (if String.equal f.kind "shortfall" then Ui.muted (Msg.advisor_bills_estimate locale)
                 else Vdom.Node.none);
              ];
          ]
      | _ -> [])
    @ [
        Vdom.Node.div ~attrs:(cls "advisor-messages") turns;
        Vdom.Node.div ~attrs:(cls "advisor-composer-area")
          [
            composer;
            Vdom.Node.div
              ~attrs:
                [
                  Vdom.Attr.class_ "advisor-status";
                  Vdom.Attr.create "role" "status";
                  Vdom.Attr.create "aria-live" "polite";
                  Vdom.Attr.create "aria-atomic" "true";
                ]
              [ text (notice locale view.notice) ];
          ];
        Vdom.Node.div ~attrs:(cls "advisor-conversation-actions") actions;
      ])

let render ~locale ~(view : C.view) ~dispatch =
  render_body
    ~turns:(render_turns ~locale ~snapshot:view.snapshot ~dispatch ~markdown:String.Map.empty)
    ~show_source:true ~locale ~view ~dispatch

let render_inline ~locale ~(view : C.view) ~dispatch =
  render_body
    ~turns:(render_turns ~locale ~snapshot:view.snapshot ~dispatch ~markdown:String.Map.empty)
    ~show_source:false ~locale ~view ~dispatch

let consent ~locale ~(view : C.view) ~dispatch ~decline =
  match view.status with
  | None -> Ui.muted (Msg.advisor_choose_context locale)
  | Some status when (not status.available) && not status.consent -> Ui.muted (Msg.advisor_unavailable locale)
  | Some status ->
      Ui.div ~cls:"advisor-consent"
        [
          (if status.available then Vdom.Node.none else Ui.muted (Msg.advisor_unavailable locale));
          Ui.p (Msg.advisor_disclosure locale);
          Ui.div ~cls:"actions"
            (if status.consent then [ Ui.button (Msg.advisor_disable locale) (dispatch (C.Consent (false, None))) ]
             else
               [
                 Ui.button ~kind:"primary" (Msg.advisor_enable locale) (dispatch (C.Consent (true, None)));
                 Ui.button (Msg.advisor_decline locale) decline;
               ]);
        ]

let household_setting ~locale ~(view : C.view) ~dispatch =
  match view.status with
  | Some { household = Some h; _ } when h.can_manage ->
      Ui.div ~cls:"panel"
        [
          Ui.p (Msg.advisor_disclosure locale);
          Vdom.Node.button
            ~attrs:
              [
                Vdom.Attr.type_ "button";
                Vdom.Attr.class_ "button";
                Vdom.Attr.create "role" "switch";
                Vdom.Attr.create "aria-checked" (Bool.to_string h.enabled);
                Vdom.Attr.on_click (fun _ -> dispatch (C.Consent (not h.enabled, Some h.household_id)));
              ]
            [
              Ui.text (if h.enabled then Msg.advisor_household_disable locale else Msg.advisor_household_enable locale);
            ];
        ]
  | _ -> Vdom.Node.none
