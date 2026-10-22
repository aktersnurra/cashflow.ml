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
  and notify = ctx.notify and navigate = ctx.navigate in
  let admin = Ctx.is_admin session in
  let run ?(after = Effect.Ignore) command args =
    match%bind.Effect Api.run command (`Assoc args) with Ok _ -> Effect.Many [ after; changed ] | Error message -> notify message
  in
  let section title body = Ui.div [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text title ] ]; body ] in
  if not (Ctx.has_household session) then
    Ui.page ~title:(Msg.household_title locale)
      [ Ui.div ~cls:"panel empty-panel"
          ([ Ui.p (Msg.household_none locale); Ui.muted (Msg.household_none_body locale) ]
          @
          if admin then
            [ Ui.div ~cls:"inline-form"
                [ Ui.input ~placeholder:(Msg.label_name locale) ~value:name ~on_input:set_name ();
                  Ui.button ~kind:"primary" ~disabled:(String.is_empty (String.strip name)) (Msg.action_create_household locale) (run "create_household" [ ("name", `String name) ]) ] ]
          else []) ]
  else
    let household = J.field "household" session in
    let code json = J.str ~default:(J.str ~default:"SEK" (J.field "default_currency" household)) (J.field "currency" json) in
    Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
      [ Ui.div ~cls:"page-head"
          [ Ui.div [ Vdom.Node.h1 [ Ui.text (J.str (J.field "name" household)) ]; Ui.div ~cls:"subtitle" [ Ui.text (Msg.household_subtitle locale) ] ] ];
        (* What the household owes a member, from shared costs they paid personally. *)
        (match owed with
        | Loaded json when not (List.is_empty (J.list (J.field "owed" json))) ->
            Ui.div
              (List.map (J.list (J.field "owed" json)) ~f:(fun o ->
                   Ui.card ~title:(Msg.card_settle_title ~name:(J.str (J.field "name" o)) ~amount:(Fmt.money_json ~code:(code json) (J.field "amount" o)) locale) ()))
        | _ -> Vdom.Node.none);
        (* Each member's planned transfer this cycle, how much of it has arrived, and what is left. *)
        section (Msg.household_plan locale)
          (Remote.view ~locale plan ~f:(fun json ->
               let c = code json in
               let members = J.list (J.field "members" json) in
               if List.for_all members ~f:(fun m -> Float.(Fmt.to_float (J.field "contribution" m) = 0.)) then
                 Ui.div ~cls:"panel empty-panel"
                   [ Ui.p (Msg.household_no_plan locale); Ui.div ~cls:"actions" [ Ui.button ~kind:"primary" (Msg.nav_budgets locale) (navigate Route.Budgets) ] ]
               else
                 Ui.div ~cls:"table"
                   (Ui.div ~cls:"table-head budget-row"
                      [ Ui.span [ Ui.text (Msg.household_col_member locale) ]; Ui.span [ Ui.text (Msg.label_transferred locale) ];
                        Ui.span ~cls:"num" [ Ui.text (Msg.household_col_planned locale) ]; Ui.span ~cls:"num" [ Ui.text (Msg.budgets_col_remaining locale) ] ]
                   :: List.map members ~f:(fun m ->
                          let name = J.str (J.field "name" m) in
                          let contribution = Fmt.to_float (J.field "contribution" m) and transferred = Fmt.to_float (J.field "transferred" m) in
                          let share = if Float.(contribution > 0.) then Float.clamp_exn (transferred /. contribution *. 100.) ~min:0. ~max:100. else 0. in
                          Ui.div ~cls:"row budget-row"
                            [ Ui.div ~cls:"category-cell" [ Ui.span ~cls:"tile" [ Ui.text (Ui.initial name) ]; Ui.span ~cls:"row-title" [ Ui.text name ] ];
                              Ui.div ~cls:"spent-cell"
                                [ Ui.div ~cls:"amount" [ Ui.text (Fmt.whole locale ~code:c (J.field "transferred" m)) ];
                                  Ui.div ~cls:"bar"
                                    [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "fill"; Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_percentage share))) ] [] ] ];
                              Ui.span ~cls:"amount" [ Ui.text (Fmt.whole locale ~code:c (J.field "contribution" m)) ];
                              Ui.span ~cls:"amount muted" [ Ui.text (Fmt.whole locale ~code:c (J.field "remaining" m)) ] ]))));
        section (Msg.label_members locale)
          (Ui.div
             [ Ui.div ~cls:"table"
                 (List.map (J.list (J.field "members" session)) ~f:(fun m ->
                      let name = J.str (J.field "display_name" m) in
                      Ui.div ~cls:"row"
                        [ Ui.div ~cls:"category-cell" [ Ui.span ~cls:"tile" [ Ui.text (Ui.initial name) ]; Ui.span ~cls:"row-title" [ Ui.text name ] ];
                          Ui.span ~cls:"muted" [ Ui.text (match J.str (J.field "status" m) with "invited" -> Msg.label_invitation_pending locale | _ -> "") ] ])
                 @
                 if admin then
                   [ Ui.div ~cls:"row inline-form"
                       [ Ui.input ~type_:"email" ~placeholder:(Msg.label_email locale) ~value:email ~on_input:set_email ();
                         Ui.button ~kind:"primary" ~disabled:(String.is_empty (String.strip email)) (Msg.action_invite locale)
                           (run ~after:(set_email "") "invite_member" [ ("email", `String email) ]) ] ]
                 else []);
               (if admin then Ui.div ~cls:"provisional table-note" [ Ui.text (Msg.household_invite_hint locale) ] else Vdom.Node.none) ]) ]
