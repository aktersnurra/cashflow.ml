open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type goal_draft = { name : string; target : string; date : string; monthly : string }
type editing = Not_editing | Budget of { category : string; amount : string; rollover : bool } | Goal of goal_draft

let page ~(ctx : Ctx.t) graph =
  let scope, set_scope = Bonsai.state "me" graph in
  let args = let%arr scope in `Assoc [ ("scope", `String scope) ] in
  let status = Remote.load ~name:"budget_status" ~args ~ctx graph in
  let goals = Remote.load ~name:"savings_goals" ~args ~ctx graph in
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let editing, set_editing = Bonsai.state Not_editing graph in
  let%arr status and goals and accounts and scope and set_scope and editing and set_editing and locale = ctx.locale and changed = ctx.changed
  and notify = ctx.notify and session = ctx.session in
  let code json = J.str ~default:"SEK" (J.field "currency" json) in
  let save name args =
    match%bind.Effect Api.run name (`Assoc (("scope", `String scope) :: args)) with
    | Ok _ -> Effect.Many [ set_editing Not_editing; changed ]
    | Error message -> notify message
  in
  let budgeted = match status with Loaded json -> List.map (J.list (J.field "lines" json)) ~f:(fun l -> J.str (J.path [ "category"; "id" ] l)) | _ -> [] in
  let owner = if String.equal scope "household" then "household" else "personal" in
  let unbudgeted =
    match accounts with
    | Loaded json ->
        List.filter (Accounts_data.of_list json) ~f:(fun a ->
            String.equal a.kind "expense" && Option.is_none a.role && String.equal a.owner owner && not (List.mem budgeted a.id ~equal:String.equal))
    | _ -> []
  in
  let budget_form ~category ~name ~amount ~rollover =
    Ui.card ~title:(Msg.budgets_set ~category:name locale)
      ~body:
        [ Ui.field (Msg.label_monthly_amount locale) (Ui.input ~value:amount ~placeholder:"0.00" ~on_input:(fun amount -> set_editing (Budget { category; amount; rollover })) ());
          Vdom.Node.label
            ~attrs:[ Vdom.Attr.class_ "checkbox" ]
            [ Vdom.Node.input ~attrs:([ Vdom.Attr.type_ "checkbox"; Vdom.Attr.on_change (fun _ _ -> set_editing (Budget { category; amount; rollover = not rollover })) ] @ if rollover then [ Vdom.Attr.checked ] else []) ();
              Ui.text (Msg.label_rollover locale) ] ]
      ~actions:
        [ Ui.button ~kind:"primary" (Msg.action_save locale) (save "set_budget" [ ("category_id", `String category); ("monthly_amount", `String amount); ("rollover", `Bool rollover) ]);
          Ui.button (Msg.action_cancel locale) (set_editing Not_editing) ] ()
  in
  let line json l =
    let c = code json in
    let spent = Fmt.to_float (J.field "spent" l) and budget = Fmt.to_float (J.field "budget" l) +. Fmt.to_float (J.field "carried" l) in
    let expected = Fmt.to_float (J.field "expected_by_now" l) in
    let left = budget -. spent in
    let money_of f = Mf2_runtime.{ minor = Int64.of_float (Float.round (Float.abs f *. 100.)); scale = 2; code = c } in
    let category = J.str (J.path [ "category"; "id" ] l) and name = J.str (J.path [ "category"; "name" ] l) in
    match editing with
    | Budget b when String.equal b.category category -> budget_form ~category ~name ~amount:b.amount ~rollover:b.rollover
    | _ ->
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "row clickable"; Vdom.Attr.on_click (fun _ -> set_editing (Budget { category; amount = J.str (J.field "budget" l); rollover = J.bool (J.field "rollover" l) })) ]
          [ Ui.div
              [ Ui.div ~cls:"row-title" [ Ui.text name ];
                Ui.meter ~marker:(if Float.(budget > 0.) then Some (expected /. budget) else None) (if Float.(budget > 0.) then spent /. budget else 0.);
                Ui.div ~cls:"row-sub" [ Ui.text (Fmt.format_money locale ~code:c (J.field "spent" l) ^ " / " ^ Mf2_runtime.format_money locale (money_of budget)) ] ];
            Ui.div ~cls:("amount " ^ if Float.(left < 0.) then "negative" else "")
              [ Ui.text (if Float.(left < 0.) then Msg.label_over ~over:(money_of left) locale else Msg.label_left ~left:(money_of left) locale) ] ]
  in
  let goal json g =
    let c = code json in
    let saved = Fmt.to_float (J.field "saved" g) and target = Fmt.to_float (J.field "target" g) in
    Ui.div ~cls:"row"
      [ Ui.div
          [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "name" g)) ];
            Ui.meter (if Float.(target > 0.) then saved /. target else 0.);
            Ui.div ~cls:"row-sub"
              [ Ui.text
                  (Fmt.format_money locale ~code:c (J.field "saved" g) ^ " / " ^ Fmt.format_money locale ~code:c (J.field "target" g)
                  ^ if J.is_null (J.field "target_date" g) then "" else " · " ^ Fmt.format_date locale (J.field "target_date" g)) ] ];
        Ui.div ~cls:(if J.bool (J.field "on_track" g) then "positive" else "warning") [ Ui.text (if J.bool (J.field "on_track" g) then "✓" else "!") ] ]
  in
  let scopes = [ ("me", Msg.label_personal locale) ] @ if Ctx.has_household session then [ ("household", Msg.label_household locale) ] else [] in
  Ui.page ~title:(Msg.budgets_title locale)
    [ (if List.length scopes < 2 then Vdom.Node.none else Ui.div ~cls:"segmented"
        (List.map scopes ~f:(fun (value, label) -> Ui.button ~kind:(if String.equal value scope then "active" else "") label (set_scope value))));
      Remote.view ~locale status ~f:(fun json ->
          let lines = J.list (J.field "lines" json) in
          Ui.div
            [ Ui.div ~cls:"eyebrow" [ Ui.text (Fmt.month_label locale (J.str (J.path [ "cycle"; "cycle" ] json))) ];
              (if List.is_empty lines then Ui.empty (Msg.budgets_empty locale) else Ui.div ~cls:"list" (List.map lines ~f:(line json))) ]);
      (match editing, unbudgeted with
      | Budget b, _ when not (List.mem budgeted b.category ~equal:String.equal) ->
          let name = Option.value_map (List.find unbudgeted ~f:(fun a -> String.equal a.id b.category)) ~default:"" ~f:(fun a -> a.name) in
          budget_form ~category:b.category ~name ~amount:b.amount ~rollover:b.rollover
      | _, [] -> Vdom.Node.none
      | _, unbudgeted ->
          Ui.div
            [ Ui.div ~cls:"section-title" [ Ui.text (Msg.budgets_no_budget locale) ];
              Ui.div ~cls:"chips"
                (List.map unbudgeted ~f:(fun a ->
                     Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "chip"; Vdom.Attr.on_click (fun _ -> set_editing (Budget { category = a.id; amount = ""; rollover = false })) ] [ Ui.text a.name ])) ]);
      Ui.div ~cls:"section-title" [ Ui.text (Msg.label_goals locale) ];
      Remote.view ~locale goals ~f:(fun json ->
          match J.list (J.field "goals" json) with [] -> Ui.muted (Msg.goals_empty locale) | gs -> Ui.div ~cls:"list" (List.map gs ~f:(goal json)));
      (match editing with
      | Goal g ->
          let set f = set_editing (Goal (f g)) in
          Ui.card ~title:(Msg.action_new_goal locale)
            ~body:
              [ Ui.field (Msg.label_name locale) (Ui.input ~value:g.name ~on_input:(fun v -> set (fun g -> { g with name = v })) ());
                Ui.div ~cls:"form-row"
                  [ Ui.field (Msg.label_target locale) (Ui.input ~value:g.target ~placeholder:"0.00" ~on_input:(fun v -> set (fun g -> { g with target = v })) ());
                    Ui.field (Msg.label_target_date locale) (Ui.input ~type_:"date" ~value:g.date ~on_input:(fun v -> set (fun g -> { g with date = v })) ());
                    Ui.field (Msg.label_monthly_amount locale) (Ui.input ~value:g.monthly ~placeholder:"0.00" ~on_input:(fun v -> set (fun g -> { g with monthly = v })) ()) ] ]
            ~actions:
              [ Ui.button ~kind:"primary" (Msg.action_save locale)
                  (save "save_savings_goal"
                     ([ ("name", `String g.name); ("target_amount", `String g.target) ]
                     @ (if String.is_empty g.date then [] else [ ("target_date", `String g.date) ])
                     @ if String.is_empty g.monthly then [] else [ ("monthly_amount", `String g.monthly) ]));
                Ui.button (Msg.action_cancel locale) (set_editing Not_editing) ] ()
      | _ -> Ui.div ~cls:"actions" [ Ui.button (Msg.action_new_goal locale) (set_editing (Goal { name = ""; target = ""; date = ""; monthly = "" } : editing)) ]) ]
