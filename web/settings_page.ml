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

let page ~(ctx : Ctx.t) graph =
  let edits, set_edits = Bonsai.state None graph in
  let%arr edits and set_edits and session = ctx.session and locale = ctx.locale and changed = ctx.changed and notify = ctx.notify in
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
  Ui.page ~title:(Msg.settings_title locale)
    [ Ui.card ~title:(J.str (J.path [ "user"; "display_name" ] session)) ~eyebrow:(J.str (J.path [ "user"; "email" ] session))
        ~body:
          [ Ui.div ~cls:"form-row"
              [ Ui.field (Msg.label_language locale) (Ui.select ~value:draft.locale ~options:[ ("en", "English"); ("sv", "Svenska") ] ~on_change:(fun v -> set (fun d -> { d with locale = v })));
                Ui.field (Msg.label_timezone locale) (Ui.select ~value:draft.timezone ~options:timezones ~on_change:(fun v -> set (fun d -> { d with timezone = v })));
                Ui.field (Msg.label_currency locale) (Ui.input ~value:draft.currency ~on_input:(fun v -> set (fun d -> { d with currency = String.uppercase v })) ()) ];
            Ui.div ~cls:"form-row"
              [ Ui.field (Msg.label_cycle_start locale) (Ui.select ~value:draft.start ~options:days ~on_change:(fun v -> set (fun d -> { d with start = v })));
                Ui.field (Msg.label_cycle_adjust locale)
                  (Ui.select ~value:draft.adjustment
                     ~options:[ ("previous_banking_day", Msg.label_cycle_previous locale); ("next_banking_day", Msg.label_cycle_next locale); ("none", Msg.label_cycle_none locale) ]
                     ~on_change:(fun v -> set (fun d -> { d with adjustment = v }))) ] ]
        ~actions:[ Ui.button ~kind:"primary" ~disabled:(Option.is_none edits) (Msg.action_save locale) save ] ();
      Ui.div ~cls:"actions"
        [ Ui.button ~kind:"danger" (Msg.action_sign_out locale) (let%bind.Effect () = sign_out in Bonsai_web.Effect.reload_page) ] ]
