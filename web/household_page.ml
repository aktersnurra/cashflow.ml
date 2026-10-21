open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

(* The household's figures are only fetched in the branch where there is a household. *)
let rec page ~(ctx : Ctx.t) graph =
  let has_household = let%arr session = ctx.session in Ctx.has_household session in
  match%sub has_household with
  | true ->
      let plan = Remote.load ~name:"contribution_plan" ~ctx graph in
      let owed = Remote.load ~name:"settle_up_status" ~ctx graph in
      view ~ctx ~plan ~owed graph
  | false -> view ~ctx ~plan:(Bonsai.return Remote.Loading) ~owed:(Bonsai.return Remote.Loading) graph

and view ~(ctx : Ctx.t) ~plan ~owed graph =
  let email, set_email = Bonsai.state "" graph in
  let name, set_name = Bonsai.state "" graph in
  let%arr plan and owed and email and set_email and name and set_name and locale = ctx.locale and session = ctx.session and changed = ctx.changed
  and notify = ctx.notify in
  let admin = Ctx.is_admin session in
  let run ?(after = Effect.Ignore) command args =
    match%bind.Effect Api.run command (`Assoc args) with Ok _ -> Effect.Many [ after; changed ] | Error message -> notify message
  in
  if not (Ctx.has_household session) then
    Ui.page ~title:(Msg.household_title locale)
      [ Ui.empty (Msg.household_none locale);
        (if admin then
           Ui.card ~title:(Msg.action_create_household locale)
             ~body:[ Ui.field (Msg.label_name locale) (Ui.input ~value:name ~on_input:set_name ()) ]
             ~actions:[ Ui.button ~kind:"primary" ~disabled:(String.is_empty (String.strip name)) (Msg.action_create_household locale) (run "create_household" [ ("name", `String name) ]) ] ()
         else Vdom.Node.none) ]
  else
    let household = J.field "household" session in
    let code json = J.str ~default:(J.str ~default:"SEK" (J.field "default_currency" household)) (J.field "currency" json) in
    Ui.page ~title:(J.str (J.field "name" household))
      [ Ui.div ~cls:"section-title" [ Ui.text (Msg.household_plan locale) ];
        Remote.view ~locale plan ~f:(fun json ->
            let c = code json in
            Ui.div ~cls:"list"
              (List.map (J.list (J.field "members" json)) ~f:(fun m ->
                   let contribution = Fmt.to_float (J.field "contribution" m) and transferred = Fmt.to_float (J.field "transferred" m) in
                   Ui.div ~cls:"row"
                     [ Ui.div
                         [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "name" m)) ];
                           Ui.meter (if Float.(contribution > 0.) then transferred /. contribution else 0.);
                           Ui.div ~cls:"row-sub"
                             [ Ui.text (Msg.label_transferred locale ^ " " ^ Fmt.format_money locale ~code:c (J.field "transferred" m) ^ " / " ^ Fmt.format_money locale ~code:c (J.field "contribution" m)) ] ];
                       Ui.div ~cls:"amount" [ Ui.text (Fmt.format_money locale ~code:c (J.field "remaining" m)) ] ])));
        (match owed with
        | Loaded json when not (List.is_empty (J.list (J.field "owed" json))) ->
            Ui.div
              (List.map (J.list (J.field "owed" json)) ~f:(fun o ->
                   Ui.card ~title:(Msg.card_settle_title ~name:(J.str (J.field "name" o)) ~amount:(Fmt.money_json ~code:(code json) (J.field "amount" o)) locale) ()))
        | _ -> Vdom.Node.none);
        Ui.div ~cls:"section-title" [ Ui.text (Msg.label_members locale) ];
        Ui.div ~cls:"list"
          (List.map (J.list (J.field "members" session)) ~f:(fun m ->
               Ui.div ~cls:"row"
                 [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "display_name" m)) ];
                   Ui.span ~cls:"muted" [ Ui.text (match J.str (J.field "status" m) with "invited" -> Msg.label_invitation_pending locale | _ -> "") ] ]));
        (if admin then
           Ui.card ~title:(Msg.action_invite locale)
             ~body:[ Ui.field (Msg.label_email locale) (Ui.input ~type_:"email" ~value:email ~on_input:set_email ()); Ui.muted (Msg.household_invite_hint locale) ]
             ~actions:[ Ui.button ~kind:"primary" ~disabled:(String.is_empty (String.strip email)) (Msg.action_invite locale) (run ~after:(set_email "") "invite_member" [ ("email", `String email) ]) ] ()
         else Vdom.Node.none) ]
