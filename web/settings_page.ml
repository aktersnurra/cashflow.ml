open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type draft = { locale : string; timezone : string; currency : string; start : string; adjustment : string }

let draft_of_session session =
  let user = J.field "user" session in
  let cycle = String.split (J.str (J.field "pay_cycle" user)) ~on:';' in
  { locale = J.str ~default:"en" (J.field "locale" user); timezone = J.str (J.field "timezone" user); currency = J.str (J.field "default_currency" user);
    start = (match cycle with s :: _ -> s | [] -> "25");
    adjustment = (match cycle with _ :: "next" :: _ -> "next_banking_day" | _ :: "none" :: _ -> "none" | _ -> "previous_banking_day") }

let sign_out = Effect.of_deferred_thunk Api.sign_out

let page ~(ctx : Ctx.t) ~(advisor : Advisor.controller Bonsai.t) graph =
  let edits, set_edits = Bonsai.state None graph in
  let%arr advisor and edits and set_edits and session = ctx.session and locale = ctx.locale and changed = ctx.changed and notify = ctx.notify and theme = ctx.theme
  and set_theme = ctx.set_theme in
  let draft = Option.value edits ~default:(draft_of_session session) in
  let set f = set_edits (Some (f draft)) in
  let save =
    let start = if String.equal draft.start "last" then `String "last_banking_day" else `Int (Int.of_string draft.start) in
    let args =
      `Assoc
        [ ("locale", `String draft.locale); ("timezone", `String draft.timezone); ("default_currency", `String draft.currency);
          ("pay_cycle", `Assoc [ ("start", start); ("adjustment", `String draft.adjustment) ]) ]
    in
    match%bind.Effect Api.run "update_settings" args with
    | Ok _ -> Effect.Many [ set_edits None; changed; notify (Msg.settings_saved draft.locale) ]
    | Error message -> notify message
  in
  let timezones = List.map (J.list (J.field "timezones" session)) ~f:(fun t -> (J.str t, J.str t)) in
  let days = List.init 28 ~f:(fun i -> (Int.to_string (i + 1), Int.to_string (i + 1))) @ [ ("last", Msg.label_cycle_last_banking_day locale) ] in
  (* One setting per row: what it is (and what it affects) on the left, its control on the right. *)
  let setting ?hint label control =
    Ui.div ~cls:"row setting-row"
      [ Ui.div [ Ui.div ~cls:"row-title" [ Ui.text label ]; (match hint with Some h -> Ui.div ~cls:"row-sub" [ Ui.text h ] | None -> Vdom.Node.none) ];
        Ui.div ~cls:"setting-control" [ control ] ]
  in
  let section title rows = Ui.div [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text title ] ]; Ui.div ~cls:"table" rows ] in
  let name = J.str (J.path [ "user"; "display_name" ] session) in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
    [ Ui.div ~cls:"page-head" [ Ui.div [ Vdom.Node.h1 [ Ui.text (Msg.settings_title locale) ]; Ui.div ~cls:"subtitle" [ Ui.text (Msg.settings_subtitle locale) ] ] ];
      section (Msg.settings_account locale)
        [ Ui.div ~cls:"row"
            [ Ui.div ~cls:"category-cell"
                [ Ui.span ~cls:"tile" [ Ui.text (Ui.initial name) ];
                  Ui.div [ Ui.div ~cls:"row-title" [ Ui.text name ]; Ui.div ~cls:"row-sub" [ Ui.text (J.str (J.path [ "user"; "email" ] session)) ] ] ];
              Ui.button (Msg.action_sign_out locale) (let%bind.Effect () = advisor.invalidate in
                let%bind.Effect signed_out = sign_out in
                if signed_out then Bonsai_web.Effect.reload_page else
                  Effect.of_thunk (fun () -> Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string (Msg.signout_failed locale)))) ] ];
      section (Msg.nav_advisor locale)
        [Advisor_view.consent ~locale ~view:advisor.view ~dispatch:advisor.dispatch ~decline:Effect.Ignore];
      section (Msg.settings_general locale)
        [ setting (Msg.label_language locale) (Ui.select ~value:draft.locale ~options:[ ("en", "English"); ("sv", "Svenska") ] ~on_change:(fun v -> set (fun d -> { d with locale = v })));
          setting (Msg.label_timezone locale) (Ui.select ~value:draft.timezone ~options:timezones ~on_change:(fun v -> set (fun d -> { d with timezone = v })));
          setting ~hint:(Msg.settings_currency_hint locale) (Msg.label_currency locale)
            (Ui.input ~value:draft.currency ~on_input:(fun v -> set (fun d -> { d with currency = String.uppercase v })) ());
          setting ~hint:(Msg.settings_cycle_hint locale) (Msg.label_cycle_start locale)
            (Ui.select ~value:draft.start ~options:days ~on_change:(fun v -> set (fun d -> { d with start = v })));
          setting (Msg.label_cycle_adjust locale)
            (Ui.select ~value:draft.adjustment
               ~options:[ ("previous_banking_day", Msg.label_cycle_previous locale); ("next_banking_day", Msg.label_cycle_next locale); ("none", Msg.label_cycle_none locale) ]
               ~on_change:(fun v -> set (fun d -> { d with adjustment = v })));
          Ui.div ~cls:"row table-foot" [ Ui.button ~kind:"primary" ~disabled:(Option.is_none edits) (Msg.action_save locale) save ] ];
      (* Applies at once and stays on this device, so it needs no Save. *)
      section (Msg.settings_appearance locale)
        [ setting ~hint:(Msg.settings_theme_hint locale) (Msg.settings_theme locale)
            (Ui.div ~cls:"segmented"
               (List.map Theme.[ (System, Msg.theme_system locale); (Light, Msg.theme_light locale); (Dark, Msg.theme_dark locale) ] ~f:(fun (t, label) ->
                    Ui.button ~kind:(if Theme.equal t theme then "active" else "") label (set_theme t)))) ] ]
