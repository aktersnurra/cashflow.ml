(* Triage from the keyboard: j / k move, enter accepts the suggestion, c chooses a category,
   s shares the row with the household, p pairs it with the other side of a move between
   accounts, x skips it. On a phone the same actions are buttons under the selected row. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

type picker = No_picker | Category of string | Share of string | Pair of J.t option

let page ~(ctx : Ctx.t) graph =
  let inbox = Remote.load ~name:"list_inbox" ~args:(Bonsai.return (`Assoc [ ("limit", `Int 200) ])) ~ctx graph in
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let selected, set_selected = Bonsai.state 0 graph in
  let picker, set_picker = Bonsai.state No_picker graph in
  (* The picker's input takes focus whenever the picker opens, so typing filters right away. *)
  let picker_open = let%arr picker in match picker with Category _ | Share _ -> true | No_picker | Pair _ -> false in
  let focus = match%sub picker_open with true -> Bonsai_web.Effect.Focus.on_activate () graph | false -> Bonsai.return Vdom.Attr.empty in
  let%arr focus and inbox and accounts and selected and set_selected and picker and set_picker and locale = ctx.locale and changed = ctx.changed
  and notify = ctx.notify and session = ctx.session in
  let currency = J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
  let accounts = match accounts with Loaded json -> Accounts_data.of_list json | _ -> [] in
  let items = match inbox with Loaded json -> J.list (J.field "items" json) | _ -> [] in
  let count = List.length items in
  let selected = if count = 0 then 0 else Int.min selected (count - 1) in
  let current = List.nth items selected in
  let transaction item = J.field "transaction" item in
  let id item = J.str (J.field "id" (transaction item)) in
  let side name item = Accounts_data.find accounts (J.str (J.field "id" (J.field name (transaction item)))) in
  (* The bank side is the one that isn't an uncategorized account. *)
  let outflow item = match side "to" item with Some a -> String.equal a.kind "expense" | None -> true in
  let bank item = if outflow item then side "from" item else side "to" item in
  let finish result = match result with Ok _ -> Effect.Many [ set_picker No_picker; changed ] | Error message -> notify message in
  let categorize item category_id =
    let%bind.Effect result = Api.run "categorize" (`Assoc [ ("id", `String (id item)); ("category_id", `String category_id) ]) in
    finish result
  in
  let share item category_id =
    let%bind.Effect result = Api.run "share_with_household" (`Assoc [ ("id", `String (id item)); ("category_id", `String category_id) ]) in
    finish result
  in
  let dismiss item =
    let%bind.Effect result = Api.run "dismiss_inbox_item" (`Assoc [ ("id", `String (id item)) ]) in
    finish result
  in
  let pair item other =
    let out_id, in_id = if outflow item then (id item, id other) else (id other, id item) in
    let%bind.Effect result = Api.run "pair_transfers" (`Assoc [ ("out_id", `String out_id); ("in_id", `String in_id) ]) in
    finish result
  in
  let open_pairs item =
    let%bind.Effect () = set_picker (Pair None) in
    match%bind.Effect Api.run "inbox_pair_candidates" (`Assoc [ ("id", `String (id item)) ]) with
    | Ok json -> set_picker (Pair (Some json))
    | Error message -> Effect.Many [ set_picker No_picker; notify message ]
  in
  let accept item =
    match J.str_opt (J.path [ "suggested_category"; "id" ] item) with Some category -> categorize item category | None -> set_picker (Category "")
  in
  let categories item =
    let bank_owner = Option.value_map (bank item) ~default:"personal" ~f:(fun a -> a.owner) in
    List.filter accounts ~f:(fun a ->
        String.equal a.owner bank_owner && not a.closed
        && (if outflow item then String.equal a.kind "expense" && Option.is_none a.role else String.equal a.kind "income" && Option.is_none a.role
           || (Accounts_data.is_bank a && not (Option.equal String.equal (Some a.id) (Option.map (bank item) ~f:(fun b -> b.id))))))
  in
  let household_categories = List.filter accounts ~f:(fun a -> String.equal a.owner "household" && String.equal a.kind "expense" && Option.is_none a.role) in
  let on_key (event : Dom_html.keyboardEvent Js.t) =
    let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
    let in_field = match Js.Opt.to_option event##.target with Some el -> String.equal (String.lowercase (Js.to_string el##.tagName)) "input" | None -> false in
    match picker, current with
    | No_picker, Some item when not in_field -> (
        match key with
        | "j" | "ArrowDown" -> set_selected (Int.min (selected + 1) (count - 1))
        | "k" | "ArrowUp" -> set_selected (Int.max (selected - 1) 0)
        | "Enter" -> accept item
        | "c" -> Effect.Many [ Effect.Prevent_default; set_picker (Category "") ]
        | "s" when Ctx.has_household session && outflow item -> Effect.Many [ Effect.Prevent_default; set_picker (Share "") ]
        | "p" -> open_pairs item
        | "x" -> dismiss item
        | _ -> Effect.Ignore)
    | (Category _ | Share _ | Pair _), _ when String.equal key "Escape" -> set_picker No_picker
    | _ -> Effect.Ignore
  in
  let row index item =
    let t = transaction item in
    let amount = Fmt.money_json ~code:(J.str ~default:currency (J.field "currency" t)) (J.field "amount" t) in
    let signed = if outflow item then { amount with minor = Int64.neg amount.minor } else amount in
    let suggestion = J.str_opt (J.path [ "suggested_category"; "name" ] item) in
    let is_selected = index = selected in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ ("row clickable" ^ if is_selected then " selected" else ""); Vdom.Attr.on_click (fun _ -> set_selected index) ]
      [ Ui.div
          [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "description" t)) ];
            Ui.div ~cls:"row-sub"
              [ Ui.text
                  (Fmt.format_date locale (J.field "date" t) ^ " · " ^ Option.value_map (bank item) ~default:"" ~f:(fun a -> a.name)
                  ^ match suggestion with Some c -> " · " ^ Msg.label_suggested ~category:c locale | None -> "") ];
            (if is_selected then
               Ui.div ~cls:"actions"
                 ([ (match suggestion with Some c -> Ui.button ~kind:"primary" ~key_hint:"↵" c (accept item) | None -> Vdom.Node.none);
                    Ui.button ~key_hint:"c" (Msg.action_categorize locale) (set_picker (Category "")) ]
                 @ (if Ctx.has_household session && outflow item then [ Ui.button ~key_hint:"s" (Msg.action_share locale) (set_picker (Share "")) ] else [])
                 @ [ Ui.button ~key_hint:"p" (Msg.action_pair locale) (open_pairs item); Ui.button ~key_hint:"x" (Msg.action_skip locale) (dismiss item) ])
             else Vdom.Node.none) ];
        Ui.div ~cls:("amount " ^ if outflow item then "" else "positive") [ Ui.text (Mf2_runtime.format_money locale signed) ] ]
  in
  let category_picker ~title ~query ~set_query ~options ~choose =
    let all_options = options in
    let options = List.filter options ~f:(Accounts_data.matches query) in
    Ui.div ~cls:"overlay"
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "palette" ]
          [ Vdom.Node.input
              ~attrs:
                [ focus; Vdom.Attr.placeholder title; Vdom.Attr.value_prop query;
                  Vdom.Attr.on_input (fun _ v -> set_query v);
                  Vdom.Attr.on_keydown (fun e ->
                      match Js.Optdef.case e##.key (fun () -> "") Js.to_string with
                      | "Enter" -> (
                          match List.filter all_options ~f:(Accounts_data.matches (Ui.event_value e)) with
                          | first :: _ -> choose first.Accounts_data.id
                          | [] -> Effect.Ignore)
                      | "Escape" -> set_picker No_picker
                      | _ -> Effect.Ignore) ]
              ();
            Ui.div ~cls:"list"
              (List.map (List.take options 12) ~f:(fun (a : Accounts_data.account) ->
                   Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "row clickable"; Vdom.Attr.on_click (fun _ -> choose a.id) ] [ Ui.div ~cls:"row-title" [ Ui.text a.name ]; Ui.span ~cls:"row-sub" [ Ui.text a.kind ] ])) ] ]
  in
  let overlay =
    match picker, current with
    | Category query, Some item ->
        category_picker ~title:(Msg.inbox_pick_category locale) ~query ~set_query:(fun q -> set_picker (Category q)) ~options:(categories item) ~choose:(categorize item)
    | Share query, Some item ->
        category_picker ~title:(Msg.inbox_share_pick locale) ~query ~set_query:(fun q -> set_picker (Share q)) ~options:household_categories ~choose:(share item)
    | Pair candidates, Some item ->
        let loaded = Option.is_some candidates in
        let candidates = Option.value_map candidates ~default:[] ~f:(fun json -> J.list (J.field "candidates" json)) in
        Ui.div ~cls:"overlay"
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "sheet" ]
              [ Vdom.Node.h2 [ Ui.text (Msg.inbox_pick_pair locale) ];
                (if loaded && List.is_empty candidates then Ui.muted (Msg.inbox_no_pairs locale)
                 else
                   Ui.div ~cls:"list"
                     (List.map candidates ~f:(fun other ->
                          let t = transaction other in
                          Vdom.Node.div
                            ~attrs:[ Vdom.Attr.class_ "row clickable"; Vdom.Attr.on_click (fun _ -> pair item other) ]
                            [ Ui.div [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "description" t)) ]; Ui.div ~cls:"row-sub" [ Ui.text (Fmt.format_date locale (J.field "date" t)) ] ] ])));
                Ui.div ~cls:"actions" [ Ui.button (Msg.action_cancel locale) (set_picker No_picker) ] ] ]
    | _ -> Vdom.Node.none
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.Global_listeners.keydown on_key ]
    [ Ui.page ~title:(Msg.inbox_title locale)
        [ Remote.view ~locale inbox ~f:(fun _ ->
              if count = 0 then Ui.empty (Msg.inbox_empty locale)
              else Ui.div [ Ui.div ~cls:"hint kbd-only" [ Ui.text (Msg.inbox_keys locale) ]; Ui.div ~cls:"list" (List.mapi items ~f:row) ]) ];
      overlay ]
