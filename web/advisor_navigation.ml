open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire
module H = Advisor_navigation_hooks
module Title = Cashflow_advisor_protocol.Conversation_title

type menu = Closed | History | Actions
type title_source = New | Existing | Unavailable | First_message of string
type date_group = Today | Yesterday | Week | Older

let can_read_scope status scope =
  match status with
  | None -> true
  | Some (status : C.status) -> status.consent && (match scope, status.household with
      | W.Personal, _ -> true
      | W.Household _, _ when not status.household_known -> true
      | W.Household id, Some h -> h.enabled && String.equal id h.household_id
      | W.Household _, None -> false)

let title_source (view : C.view) =
  if not view.authenticated || view.access_denied then Unavailable else
  match view.snapshot with
  | Some snapshot when not (can_read_scope view.status snapshot.scope) -> Unavailable
  | Some {turns=first::_; _} -> First_message first.user_text
  | _ -> match view.selected_conversation with
      | None -> New
      | Some id -> match List.find view.history ~f:(fun entry -> String.equal entry.conversation_id id) with
          | Some {preview=Some preview; _} when can_read_scope view.status preview.scope -> First_message preview.title
          | _ -> Existing

let date value : Js.date Js.t = Js.Unsafe.new_obj (Js.Unsafe.get Js.Unsafe.global "Date") [|value|]
let now_ms () : float = Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global "Date") "now" [||]
let day_start value days =
  let date = date (Js.Unsafe.inject value) in
  ignore (Js.Unsafe.meth_call date "setHours" [|Js.Unsafe.inject 0;Js.Unsafe.inject 0;Js.Unsafe.inject 0;Js.Unsafe.inject 0|] : float);
  ignore (date##setDate (date##getDate - days)); date##getTime

let date_caption locale now value =
  let date = date (Js.Unsafe.inject (Js.string value)) in
  let timestamp = date##getTime in
  match Stdlib.classify_float timestamp with
  | FP_nan | FP_infinite -> Older, ""
  | _ ->
      let today = day_start now 0 in
      let group = if Float.(timestamp >= today) then Today
        else if Float.(timestamp >= day_start now 1) then Yesterday
        else if Float.(timestamp >= day_start now 7) then Week else Older in
      let format method_ fields =
        let options = Js.Unsafe.obj (Array.of_list (List.map fields ~f:(fun (key,value) -> key,Js.Unsafe.inject (Js.string value)))) in
        let result : Js.js_string Js.t = Js.Unsafe.meth_call date method_ [|Js.Unsafe.inject (Js.string locale);Js.Unsafe.inject options|] in
        Js.to_string result in
      let label = match group with
        | Today -> format "toLocaleTimeString" ["hour","2-digit";"minute","2-digit"]
        | Yesterday -> Msg.advisor_history_days locale ~days:"1"
        | Week -> let days = Int.of_float (Stdlib.floor ((today -. day_start timestamp 0) /. 86400000. +. 0.5)) in
            Msg.advisor_history_days locale ~days:(Int.to_string (Int.max 1 days))
        | Older -> format "toLocaleDateString" ["month","short";"day","numeric"] in
      group,label

let component ~locale ~advisor ~route ~on_select ~on_new graph =
  let menu, set_menu = Bonsai.state ~equal:Poly.equal Closed graph in
  let mobile, set_mobile = Bonsai.state ~equal:Bool.equal (H.is_mobile ()) graph in
  let source = Bonsai.cutoff (let%arr advisor in title_source advisor.Advisor.view) ~equal:Poly.equal in
  let title = let%arr source and locale in match source with
    | New -> Msg.advisor_new_conversation locale
    | Existing -> Msg.advisor_conversation locale
    | Unavailable -> Msg.advisor_history_unavailable_title locale
    | First_message message -> let title = Title.of_first_message message in
        if String.is_empty title then Msg.advisor_conversation locale else title in
  let history = Bonsai.cutoff (let%arr advisor in advisor.Advisor.view.history) ~equal:phys_equal in
  let formatted = let%arr history and locale and menu in
    if not (Poly.equal menu History) then [] else
    let now = now_ms () in List.map history ~f:(fun entry -> let group,age=date_caption locale now entry.C.updated_at in entry,group,age) in
  let boundary = let%arr advisor in
    let v = advisor.Advisor.view in
    v.authenticated,v.access_denied,v.selected_conversation,v.context_scope,v.denied_conversation,
      (match v.status with Some status -> not status.consent | None -> false),
      (match v.context_scope with Some scope -> not (can_read_scope v.status scope) | None -> false) in
  Bonsai.Edge.on_change ~equal:Poly.equal boundary
    ~callback:(let%arr set_menu in fun _ -> set_menu Closed) graph;
  Bonsai.Edge.on_change ~equal:Route.equal route
    ~callback:(let%arr set_menu in fun _ -> set_menu Closed) graph;
  let%arr advisor and locale and title and formatted and menu and set_menu and mobile and set_mobile and on_select and on_new in
  let view = advisor.Advisor.view in
  let text = Vdom.Node.text in
  let close restore = Effect.Many [set_menu Closed;
    (if restore then Effect.of_thunk (fun () -> H.restore_trigger (Poly.equal menu Actions)) else Effect.Ignore)] in
  let open_history = Effect.Many [set_menu History;advisor.dispatch (C.Follow false);advisor.dispatch C.Refresh_status;advisor.dispatch (C.Load_history 0)] in
  let scope = Option.value view.context_scope ~default:W.Personal in
  let can_new scope = match view.status with
    | Some status when status.available && status.consent -> (match scope,status.household with
        | W.Personal,_ -> true | W.Household id,Some h -> h.enabled && String.equal id h.household_id | _ -> false)
    | _ -> false in
  let item ?key ?(disabled=false) ?(attrs=[]) label action =
    Vdom.Node.button ?key ~attrs:([Vdom.Attr.class_ "advisor-menu-item";Vdom.Attr.type_ "button";
      Vdom.Attr.create "role" "menuitem";Vdom.Attr.create "tabindex" "-1";
      (if disabled then Vdom.Attr.disabled else Vdom.Attr.empty);Vdom.Attr.on_click (fun _ -> action)] @ attrs) label in
  let new_item scope label = item ~disabled:(not (can_new scope))
    [Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-menu-icon";Vdom.Attr.create "aria-hidden" "true"] [text "+"];text label]
    (Effect.Many [close false;on_new scope]) in
  let groups = List.filter_map [Today;Yesterday;Week;Older] ~f:(fun group ->
    let entries = List.filter formatted ~f:(fun (_,g,_) -> Poly.equal group g) in
    if List.is_empty entries then None else
    let label = match group with Today->Msg.advisor_history_today locale | Yesterday->Msg.advisor_history_yesterday locale
      | Week->Msg.advisor_history_week locale | Older->Msg.advisor_history_older locale in
    Some (Vdom.Node.div ~key:(match group with Today->"today"|Yesterday->"yesterday"|Week->"week"|Older->"older")
      ~attrs:[Vdom.Attr.class_ "advisor-history-group";Vdom.Attr.create "role" "group";Vdom.Attr.create "aria-label" label]
      (Vdom.Node.div ~attrs:[Vdom.Attr.class_ "advisor-history-caption";Vdom.Attr.create "aria-hidden" "true"] [text label] ::
        List.map entries ~f:(fun ((entry:C.metadata),_,age) ->
          let current = Option.equal String.equal view.selected_conversation (Some entry.conversation_id) in
          let name = match entry.preview with Some p when not (String.is_empty p.title)->p.title
            | _ when current -> title | _ -> Msg.advisor_conversation locale in
          item ~key:entry.conversation_id ~attrs:[Vdom.Attr.create "data-conversation-row" "";
            (if current then Vdom.Attr.create "aria-current" "page" else Vdom.Attr.empty)]
            [Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-menu-icon";Vdom.Attr.create "aria-hidden" "true"] [text (if current then "✓" else "")];
             Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-history-name";Vdom.Attr.create "dir" "auto"] [text name];
             (match entry.preview with Some {scope=W.Household _;_} -> Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-history-scope"] [text (Msg.label_household locale)] | _ -> Vdom.Node.none);
             Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-history-age"] [text age]]
            (Effect.Many [close true;(if current then Effect.Ignore else on_select entry.conversation_id)]))))) in
  let delete_target = if not view.authenticated then None else match view.snapshot with
    | Some snapshot -> Some snapshot.conversation_id | None -> view.denied_conversation in
  let delete = match delete_target with
    | None -> Effect.Ignore
    | Some id -> Effect.Many [close true; Effect.of_thunk (fun () ->
        if Js.to_bool (Dom_html.window##confirm (Js.string (Msg.advisor_delete_confirm locale))) then
          Effect.Expert.handle_non_dom_event_exn (advisor.dispatch (C.Delete_owned id)))] in
  let panel_label = if Poly.equal menu Actions then Msg.advisor_conversation_actions locale else Msg.advisor_history locale in
  let panel = if Poly.equal menu Closed then Vdom.Node.none else
    Vdom.Node.div ~key:(if Poly.equal menu Actions then "actions" else "history")
      ~attrs:[Vdom.Attr.class_ (if Poly.equal menu Actions then "advisor-menu-panel advisor-action-panel" else "advisor-menu-panel advisor-history-panel");
        (if mobile then Vdom.Attr.create "role" "dialog" else Vdom.Attr.empty);
        (if mobile then Vdom.Attr.create "aria-modal" "true" else Vdom.Attr.empty);
        Vdom.Attr.create "aria-label" panel_label;
        Vdom.Attr.create_hook "advisor-navigation-menu" (H.Menu.create {actions=Poly.equal menu Actions;
          close=(fun restore -> Effect.Expert.handle_non_dom_event_exn (close restore))})]
      [Vdom.Node.div ~attrs:[Vdom.Attr.class_ "advisor-sheet-heading"]
        [Vdom.Node.h2 [text panel_label];Vdom.Node.button ~attrs:[Vdom.Attr.type_ "button";Vdom.Attr.class_ "advisor-sheet-close";
          Vdom.Attr.create "aria-label" (Msg.advisor_close_history locale);Vdom.Attr.on_click (fun _ -> close true)] [text "×"]];
       Vdom.Node.div ~attrs:[Vdom.Attr.create "role" "menu";Vdom.Attr.create "aria-label" panel_label;
         Vdom.Attr.id (if Poly.equal menu Actions then "advisor-action-menu" else "advisor-history-menu")]
         (if Poly.equal menu Actions then [item ~attrs:[Vdom.Attr.class_ "advisor-menu-delete"] [text (Msg.advisor_delete locale)] delete]
          else
            [new_item scope (Msg.advisor_new_conversation locale)] @
            (match scope,view.status with
             | W.Household _,_ when can_new W.Personal -> [new_item W.Personal (Msg.advisor_new_personal locale)]
             | W.Personal,Some {household=Some h;_} when can_new (W.Household h.household_id) ->
                 [new_item (W.Household h.household_id) (Msg.advisor_new_household locale)]
             | _ -> []) @
            [Vdom.Node.div ~attrs:[Vdom.Attr.class_ "advisor-history-list";
              Vdom.Attr.create "aria-busy" (Bool.to_string view.history_loading)]
              groups] @
            (if view.history_failed then [Vdom.Node.p ~key:"history-error" ~attrs:[Vdom.Attr.class_ "advisor-history-caption"]
              [text (Msg.advisor_history_failed locale)]] else []) @
            (* Keep the focused footer node when the final page arrives. Removing
               it sends focus to VDOM's outside sentinel and dismisses the menu. *)
            [let label,action,enabled =
               if view.history_loading then Msg.advisor_history_loading locale,Effect.Ignore,false
               else if view.history_failed then Msg.advisor_history_retry locale,advisor.dispatch (C.Load_history 0),true
               else match view.history_next_offset with
                 | Some offset -> Msg.advisor_history_load_more locale,advisor.dispatch (C.Load_history offset),true
                 | None -> (if List.is_empty groups then Msg.advisor_history_empty locale else Msg.advisor_history_complete locale),Effect.Ignore,false in
             item ~key:"history-footer" ~attrs:[Vdom.Attr.create "aria-disabled" (Bool.to_string (not enabled))]
               [text label] action])] in
  Vdom.Node.div ~attrs:[Vdom.Attr.class_ "advisor-navigation";
    (if Poly.equal menu Closed then Vdom.Attr.empty else Vdom.Attr.create "data-menu-open" "");
    Vdom.Attr.create_hook "advisor-navigation-viewport" (H.Viewport.create {on_change=(fun value -> Effect.Expert.handle_non_dom_event_exn (set_mobile value))})]
    [Vdom.Node.button ~attrs:[Vdom.Attr.class_ "advisor-title-button";Vdom.Attr.type_ "button";
       Vdom.Attr.create "aria-label" (Msg.advisor_switch_conversation locale ~title);Vdom.Attr.create "aria-haspopup" "menu";
       Vdom.Attr.create "aria-controls" "advisor-history-menu";Vdom.Attr.create "aria-expanded" (Bool.to_string (Poly.equal menu History));
       Vdom.Attr.on_click (fun _ -> if Poly.equal menu History then close true else open_history);
       Vdom.Attr.on_keydown (fun event ->
         let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
         if String.equal key "ArrowDown" || String.equal key "ArrowUp" then
           Effect.Many [Effect.Prevent_default; (if Poly.equal menu History then Effect.of_thunk (fun () ->
             Js.Opt.iter (Dom_html.document##querySelector (Js.string ".advisor-history-panel")) H.focus_first) else open_history)]
         else Effect.Ignore)]
       [(* On a phone the conversation list opens from a round menu button, as in a native chat app. *)
        Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-title-menu";Vdom.Attr.create "aria-hidden" "true"]
          [Vdom.Node.create_svg "svg" ~attrs:[Vdom.Attr.create "viewBox" "0 0 24 24";Vdom.Attr.create "width" "20";Vdom.Attr.create "height" "20";
             Vdom.Attr.create "fill" "none";Vdom.Attr.create "stroke" "currentColor";Vdom.Attr.create "stroke-width" "2";Vdom.Attr.create "stroke-linecap" "round"]
             [Vdom.Node.create_svg "path" ~attrs:[Vdom.Attr.create "d" "M5 8h14M5 12h10M5 16h14"] []]];
        Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-title-text";Vdom.Attr.create "dir" "auto"] [text title];
        Vdom.Node.create_svg "svg" ~attrs:[Vdom.Attr.class_ "advisor-title-chevron";Vdom.Attr.create "viewBox" "0 0 24 24";
          Vdom.Attr.create "width" "16";Vdom.Attr.create "height" "16";Vdom.Attr.create "fill" "none";Vdom.Attr.create "stroke" "currentColor";
          Vdom.Attr.create "stroke-width" "2";Vdom.Attr.create "stroke-linecap" "round";Vdom.Attr.create "stroke-linejoin" "round";
          Vdom.Attr.create "aria-hidden" "true"]
          [Vdom.Node.create_svg "path" ~attrs:[Vdom.Attr.create "d" "M6 9l6 6 6-6"] []]];
     Vdom.Node.span ~attrs:[Vdom.Attr.class_ "advisor-navigation-meta"] [text
       ((match view.context_scope with None->"" | Some W.Personal->Msg.label_personal locale ^ " · " | Some (W.Household _)->Msg.label_household locale ^ " · ") ^ Msg.advisor_private locale)];
     (if Option.is_none delete_target then Vdom.Node.none else Vdom.Node.button ~attrs:[Vdom.Attr.class_ "advisor-actions-button";
       Vdom.Attr.type_ "button";Vdom.Attr.create "aria-label" (Msg.advisor_conversation_actions locale);Vdom.Attr.create "aria-haspopup" "menu";
       Vdom.Attr.create "aria-controls" "advisor-action-menu";Vdom.Attr.create "aria-expanded" (Bool.to_string (Poly.equal menu Actions));
       Vdom.Attr.on_click (fun _ -> if Poly.equal menu Actions then close true else Effect.Many [set_menu Actions;advisor.dispatch (C.Follow false)])]
       [Vdom.Node.span ~attrs:[Vdom.Attr.create "aria-hidden" "true"] [text "⋯"]]);
     (if Poly.equal menu Closed then Vdom.Node.none else Vdom.Node.div ~attrs:[Vdom.Attr.class_ "advisor-menu-backdrop";
       Vdom.Attr.create "aria-hidden" "true";Vdom.Attr.on_click (fun _ -> close true)] []);
     panel]
