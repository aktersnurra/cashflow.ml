(* The app shell: loads the session, then shows the sign-in page or the app. The app has a
   sidebar on wide screens and a tab bar on phones, a command palette (⌘K or /), single-key
   navigation (g then h, c, t, i, a, b, f, o, n, s), and a light/dark switch. *)
open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

(* New contexts use the selected scope's registry-backed default. Snapshot currency is
   pinned independently and never rewritten by a settings/session refresh. *)
let advisor_currency session scope : W.currency option =
  let code = match scope with
    | W.Personal -> J.str (J.path ["user"; "default_currency"] session)
    | Household id when String.equal id (J.str (J.path ["household"; "id"] session)) ->
        J.str (J.path ["household"; "default_currency"] session)
    | Household _ -> "" in
  if String.length code <> 3 || not (String.for_all code ~f:(fun c -> Char.(c >= 'A' && c <= 'Z'))) then None
  else match List.filter (J.list (J.field "currency_metadata" session)) ~f:(fun m -> String.equal (J.str (J.field "code" m)) code) with
    | [`Assoc fields as metadata] when List.length fields = 2 ->
        (match J.field "scale" metadata with `Int scale when scale >= 0 && scale <= 4 -> Some {W.code; scale} | _ -> None)
    | _ -> None

let focus_advisor = Effect.of_thunk (fun () ->
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ ->
    match Js.Opt.to_option (Dom_html.document##querySelector (Js.string ".advisor-conversation textarea, .advisor-consent button")) with
    | Some el -> ignore (Js.Unsafe.meth_call el "focus" [|Js.Unsafe.inject (Js.Unsafe.obj [|"preventScroll", Js.Unsafe.inject Js._true|])|])
    | None -> ()))))

let session_effect = Effect.of_deferred_thunk (fun () -> Api.session ~publish_csrf:false ())

(* The history entry is pushed when the effect runs, not when it is built: views build navigation
   effects for every link and button they render. *)
let navigate_to set_route route =
  Effect.Many
    [ Effect.of_thunk (fun () -> Dom_html.window##.history##pushState Js.null (Js.string "") (Js.some (Js.string (Route.to_path route))));
      set_route route ]

(* The sidebar: what you look at, then where the money lives; Settings sits at the foot. *)
let nav_groups = Route.[ [ (Home, None); (Import, None); (Advisor None, None) ]; [ (section_entry Money, Some Money); (section_entry Plan, Some Plan) ]; [ (Household, None) ] ]

(* On a phone, importing a statement is the way money gets in, so it has the middle tab; adding a
   transaction by hand is a link from there. *)
let tab_items = Route.[ (Home, None); (section_entry Money, Some Money); (Import, None); (section_entry Plan, Some Plan); (Settings, None) ]

let icon route =
  let name : Feather_icon.t =
    match route with
    | Route.Home -> Home
    | Advisor _ -> Message_circle
    | Cashflow -> Shuffle
    | Inbox -> Inbox
    | Transactions _ -> List
    | Accounts | Account _ -> Credit_card
    | Budgets -> Pie_chart
    | Forecast -> Activity
    | Household -> Users
    | Insights -> Trending_up
    | Settings -> Settings
    | Add -> Plus_circle
    | Import -> Upload
  in
  Feather_icon.svg ~size:(`Px 18) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) name

let shortcut = function
  | "h" -> Some Route.Home
  | "i" -> Some Route.Inbox
  | "t" -> Some Route.(Transactions { offset = 0; category = None })
  | "a" -> Some Route.Accounts
  | "b" -> Some Route.Budgets
  | "f" -> Some Route.Forecast
  | "o" -> Some Route.Household
  | "c" -> Some Route.Cashflow
  | "n" -> Some Route.Insights
  | "s" -> Some Route.Settings
  | _ -> None

let signed_in ~(session : J.t Bonsai.t) ~(reload_session : unit Effect.t Bonsai.t) ~register_invalidation ~auth_lost graph =
  let route, set_route = Bonsai.state ~equal:Route.equal (Route.of_path (Js.to_string Dom_html.window##.location##.pathname)) graph in
  let version, set_version = Bonsai.state 0 graph in
  let toast, set_toast = Bonsai.state None graph in
  let pending_g, set_pending_g = Bonsai.state false graph in
  let palette, set_palette = Bonsai.state false graph in
  let locale = let%arr session in J.str ~default:"en" (J.path [ "user"; "locale" ] session) in
  let currency = let%arr session in J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
  let changed = let%arr set_version and version and reload_session in Effect.Many [ set_version (version + 1); reload_session ] in
  let scrolls = ref String.Map.empty in
  let surface = function Route.Home | Advisor _ -> "continuous" | route -> Route.to_path route in
  let current_surface = ref (surface (Route.of_path (Js.to_string Dom_html.window##.location##.pathname))) in
  let scroll_transition ~from ~dest effect =
    if String.equal (surface from) (surface dest) then effect else
    Effect.Many [Effect.of_thunk (fun () ->
      scrolls := Map.set !scrolls ~key:(surface from) ~data:(Js.Unsafe.get Dom_html.window "scrollY" : int);
      current_surface := surface dest);
      effect;
      Effect.of_thunk (fun () -> ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ ->
        ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ ->
          if String.equal !current_surface (surface dest) then Dom_html.window##scroll 0 (Option.value (Map.find !scrolls (surface dest)) ~default:0))))))))] in
  let navigate = let%arr set_route and set_palette and route in fun dest ->
    scroll_transition ~from:route ~dest (Effect.Many [ set_palette false; navigate_to set_route dest ]) in
  let notify =
    let%arr set_toast in
    fun message ->
      let%bind.Effect () = set_toast (Some message) in
      let%bind.Effect () = Effect.of_deferred_thunk (fun () -> Async_kernel.after (Time_ns.Span.of_sec 4.)) in
      set_toast None
  in
  let theme, set_theme_state = Bonsai.state ~equal:Theme.equal (Theme.load ()) graph in
  let set_theme = let%arr set_theme_state in fun t -> Effect.Many [ Effect.of_thunk (fun () -> Theme.save t); set_theme_state t ] in
  let ctx : Ctx.t = { session; locale; currency; version; changed; navigate; notify; theme; set_theme } in
  (* The controller and Home host are above every route alternative. CSS hiding does not
     deactivate either the stream owner or the selected keyed finding. *)
  let advisor = Advisor.component ~ctx graph in
  (* These projections live with the shell and the selected authorized snapshot.
     Draft/follow changes do not reparse history; only changed text segments do.
     Empty/revoked snapshots remove the derived nodes rather than keeping a global cache. *)
  let advisor_snapshot = Bonsai.cutoff (let%arr advisor in advisor.view.snapshot) ~equal:phys_equal in
  let advisor_dispatch = Bonsai.cutoff (let%arr advisor in advisor.dispatch) ~equal:phys_equal in
  let advisor_locale = Bonsai.cutoff locale ~equal:String.equal in
  let markdown_sources = let%arr advisor_snapshot in
    List.fold (Option.value_map advisor_snapshot ~default:[] ~f:(fun s -> s.turns))
      ~init:String.Map.empty ~f:(fun sources (turn : W.turn) ->
        List.fold turn.attempts ~init:sources ~f:(fun sources (attempt : W.attempt) ->
          List.fold attempt.segments ~init:sources ~f:(fun sources -> function
            | W.Text_segment d -> Map.set sources ~key:d.segment_id ~data:d.text
            | W.Evidence_segment _ -> sources))) in
  let markdown_nodes = Bonsai.assoc (module String) markdown_sources ~f:(fun _ source _graph ->
    let source = Bonsai.cutoff source ~equal:String.equal in
    let%arr source in Advisor_markdown.render source) graph in
  (* The state the server recorded for a proposal the person has just acted on; a reloaded snapshot carries the same. *)
  let proposal_states, record_proposal_state =
    Bonsai.state_machine0 ~default_model:String.Map.empty
      ~apply_action:(fun _ model (id, state) -> Map.set model ~key:id ~data:state) graph in
  let decide = let%arr record_proposal_state and notify and locale in
    fun id action ->
      match%bind.Effect Effect.of_deferred_thunk (fun () -> Advisor_api.decide_proposal ~id action) with
      | Ok state -> record_proposal_state (id, state)
      (* Only a refused undo means "it changed since"; anything else is a failure to try again. *)
      | Error (C.Not_accepted W.Version_conflict) when String.equal action "undo" -> notify (Msg.advisor_proposal_undo_failed locale)
      | Error _ -> notify (Msg.advisor_proposal_failed locale) in
  let advisor_turns = let%arr advisor_snapshot and advisor_dispatch and advisor_locale and markdown_nodes and proposal_states and decide in
    Advisor_view.render_turns ~locale:advisor_locale ~snapshot:advisor_snapshot
      ~dispatch:advisor_dispatch ~decided:(Map.find proposal_states) ~decide ~markdown:markdown_nodes in
  let selected_finding, set_selected_finding = Bonsai.state None graph in
  let selected_scope, set_selected_scope = Bonsai.state W.Personal graph in
  let declined, set_declined = Bonsai.state false graph in
  let confirmed_membership, set_confirmed_membership = Bonsai.state None graph in
  let membership_reload, set_membership_reload = Bonsai.state None graph in
  let authenticated = let%arr advisor in advisor.view.authenticated in
  let route_key = Bonsai.both route ctx.version in
  let route_changed = let%arr advisor and locale and session and set_selected_finding and set_declined in
    fun (route, _) ->
      Effect.Many [advisor.dispatch C.Refresh_status; (match route with
      | Route.Advisor (Some id) ->
          Effect.Many [set_declined false;
            (if Option.equal String.equal advisor.view.selected_conversation (Some id) then Effect.Ignore
             else Effect.Many [set_selected_finding None; advisor.open_conversation id])]
      | Advisor None ->
          Effect.Many [set_declined false;
            (if Option.is_some advisor.view.selected_conversation || Option.is_some advisor.view.context_scope then Effect.Ignore
             else Effect.Many [(match advisor_currency session W.Personal with Some currency -> advisor.open_empty ~scope:W.Personal ~currency ~locale | None -> Effect.Ignore); advisor.open_latest])]
      | Home when advisor.view.access_denied -> Effect.Many [set_selected_finding None;
          (match advisor_currency session W.Personal with Some currency -> advisor.open_empty ~scope:W.Personal ~currency ~locale | None -> advisor.leave_surface)]
      | _ -> advisor.leave_surface)] in
  Bonsai.Edge.on_change ~equal:Poly.equal route_key ~callback:route_changed graph;
  let consent_key = let%arr advisor in Option.map advisor.view.status ~f:(fun s -> s.consent) in
  let consent_changed = let%arr advisor and selected_finding and selected_scope and locale and session in
    fun old consent -> match old, consent with
      | Some (Some false), Some true when Option.is_none advisor.view.selected_conversation ->
          (match advisor_currency session selected_scope with
           | None -> Effect.Ignore
           | Some currency -> Effect.Many [(match selected_finding with
               | Some key -> Effect.Many [advisor.open_finding ~key ~scope:selected_scope ~currency ~locale; advisor.dispatch (C.Follow false)]
               | None ->
                   (* Enabling the advisor must not lose a question typed into the palette beforehand. *)
                   Effect.Many [advisor.open_empty ~scope:selected_scope ~currency ~locale; advisor.dispatch (C.Draft_changed advisor.view.draft)]); focus_advisor])
      | _ -> Effect.Ignore in
  Bonsai.Edge.on_change' ~equal:Poly.equal consent_key ~callback:consent_changed graph;
  let accepted_key = let%arr advisor and route in advisor.view.first_acceptance, advisor.view.selected_conversation, route in
  let accepted_changed = let%arr advisor and set_route in
    fun (accepted, selected, route) -> match accepted, selected, route with
    | Some id, Some selected, (Route.Home | Advisor None) when String.equal id selected ->
        let next = Route.Advisor (Some id) in
        Effect.Many [Effect.of_thunk (fun () -> Dom_html.window##.history##replaceState Js.null (Js.string "") (Js.some (Js.string (Route.to_path next))));
          set_route next; advisor.dispatch (C.Consume_first_acceptance id)]
    | Some id, Some selected, Advisor (Some destination)
      when String.equal id selected && String.equal id destination -> advisor.dispatch (C.Consume_first_acceptance id)
    | _ -> Effect.Ignore in
  Bonsai.Edge.on_change ~equal:Poly.equal accepted_key ~callback:accepted_changed graph;
  let on_ask = let%arr advisor and locale and session and notify and set_selected_scope and set_selected_finding and set_declined in
    fun ~finding_key ~scope -> match advisor_currency session scope with
    | None -> notify (Msg.advisor_currency_unavailable locale)
    | Some currency -> Effect.Many [set_selected_scope scope; set_selected_finding (Some finding_key); set_declined false;
      advisor.open_finding ~key:finding_key ~scope ~currency ~locale;
      (* Opening a finding keeps its viewport anchor. Page following resumes
         when the reader reaches the tail or explicitly asks to follow. *)
      advisor.dispatch (C.Follow false); focus_advisor] in
  let new_conversation = let%arr advisor and session and locale and notify and set_selected_scope and set_selected_finding and set_declined and navigate in
    fun scope -> match advisor_currency session scope with
    | None -> notify (Msg.advisor_currency_unavailable locale)
    | Some currency -> Effect.Many [set_selected_scope scope; set_selected_finding None; set_declined false;
        advisor.open_empty ~scope ~currency ~locale; navigate (Route.Advisor None)] in
  let select_conversation = let%arr set_selected_finding and navigate in
    fun id -> Effect.Many [set_selected_finding None; navigate (Route.Advisor (Some id))] in
  let navigation = Advisor_navigation.component ~locale ~advisor ~route
    ~on_select:select_conversation ~on_new:new_conversation graph in
  let outlet = let%arr advisor and advisor_turns and navigation and selected_finding and locale and session and declined and set_declined and selected_scope and navigate in
    let view = advisor.view in
    let scope = Option.value view.context_scope ~default:selected_scope in
    Vdom.Node.div ~attrs:[Vdom.Attr.class_ "advisor-outlet"; Vdom.Attr.create "data-advisor-controller" ""]
      ([navigation] @
       if view.access_denied then [Ui.p (Msg.advisor_access_lost locale)]
       else if Option.is_none view.snapshot && Option.is_none (advisor_currency session scope) then
         [Ui.muted (Msg.advisor_currency_unavailable locale)]
       else if declined then [Ui.button (Msg.advisor_ask locale) (set_declined false)]
       else match view.status with
       | Some s when s.consent && (Option.is_some view.snapshot || s.available) && (match scope, s.household with W.Personal, _ -> true | Household id, Some h -> h.enabled && String.equal id h.household_id | _ -> false) ->
           [(if Option.is_some selected_finding then Ui.muted (Msg.advisor_historical locale) else Vdom.Node.none);
            (if Option.is_none view.snapshot then Ui.p (Msg.advisor_empty locale) else Vdom.Node.none);
            Ui.muted (Msg.advisor_readonly locale);
            Advisor_view.render_body ~show_source:(Option.is_none selected_finding) ~turns:advisor_turns
              ~locale ~view ~dispatch:advisor.dispatch]
       | Some s when s.consent && s.available -> [Ui.muted (Msg.advisor_household_off locale); Ui.button (Msg.nav_household locale) (navigate Route.Household)]
       | _ -> [Advisor_view.consent ~locale ~view ~dispatch:advisor.dispatch ~decline:(set_declined true)]) in
  let access_denied = let%arr advisor in advisor.view.access_denied in
  let home_visible = let%arr authenticated and membership_reload in authenticated && Option.is_none membership_reload in
  let home_access = Bonsai.both home_visible access_denied in
  let home, reset_home = Bonsai.with_model_resetter ~f:(fun graph ->
    match%sub home_access with
    | false, _ -> Bonsai.return Vdom.Node.none
    | true, true -> let%arr outlet and locale in Ui.page ~title:(Msg.nav_advisor locale) [outlet]
    | true, false -> Home.page ~ctx ~on_ask ~selected_finding ~outlet
        ~advisor_route:(let%arr route in match route with Route.Advisor _ -> true | _ -> false)
        ~visible:(let%arr route in match route with Route.Home | Route.Advisor _ -> true | _ -> false) graph) graph in
  let clear_home = let%arr reset_home and set_selected_finding and set_selected_scope in fun denied ->
    if denied then Effect.Many [set_selected_finding None; set_selected_scope W.Personal; reset_home] else Effect.Ignore in
  Bonsai.Edge.on_change ~equal:Bool.equal access_denied ~callback:clear_home graph;
  (* A current explicit membership loss/change is an authorization boundary, unlike
     household Advisor opt-out. Hide/deactivate Home before resetting and reloading it. *)
  let membership = let%arr advisor in match advisor.view.status with
    | Some status when status.household_known -> Some (Option.map status.household ~f:(fun h -> h.household_id))
    | _ -> None in
  let membership_changed = let%arr confirmed_membership and membership_reload and set_confirmed_membership and set_membership_reload in
    fun membership -> match membership with
    | None -> Effect.Ignore
    | Some current -> Effect.Many [
        (match membership_reload, confirmed_membership with
         | Some pending, _ when not (Option.equal String.equal pending current) -> set_membership_reload (Some current)
         | _, Some (Some previous) when not (Option.equal String.equal (Some previous) current) -> set_membership_reload (Some current)
         | _ -> Effect.Ignore);
        set_confirmed_membership (Some current)] in
  Bonsai.Edge.on_change ~equal:Poly.equal membership ~callback:membership_changed graph;
  let reload_home_membership = let%arr reset_home and set_selected_finding and reload_session in
    fun target -> match target with
    | None -> Effect.Ignore
    | Some _ -> Effect.Many [set_selected_finding None; reset_home; reload_session] in
  Bonsai.Edge.on_change ~equal:Poly.equal membership_reload ~callback:reload_home_membership graph;
  let session_membership = let%arr session in
    if Ctx.has_household session then Some (J.str (J.path ["household"; "id"] session)) else None in
  let membership_loaded = let%arr advisor and set_membership_reload in fun (target, current) ->
    match target with
    | Some expected when Option.equal String.equal expected current -> set_membership_reload None
    | Some _ -> advisor.dispatch C.Refresh_status
    | None -> Effect.Ignore in
  Bonsai.Edge.on_change ~equal:Poly.equal (Bonsai.both membership_reload session_membership) ~callback:membership_loaded graph;
  let invalidate_shell = let%arr advisor and reset_home and set_selected_finding and set_selected_scope and set_toast and set_confirmed_membership and set_membership_reload in
    Effect.Many [advisor.invalidate; set_selected_finding None; set_selected_scope W.Personal;
      set_toast None; set_confirmed_membership None; set_membership_reload None;
      reset_home; Effect.of_thunk (fun () -> scrolls := String.Map.empty)] in
  let authentication_changed = let%arr invalidate_shell and auth_lost in fun old current ->
    if Option.value old ~default:false && not current then Effect.Many [invalidate_shell; auth_lost] else Effect.Ignore in
  Bonsai.Edge.on_change' ~equal:Bool.equal authenticated ~callback:authentication_changed graph;
  (* Home's authenticated session establishes its authorization baseline even when
     initial Advisor readiness omits household metadata. Never overwrite a known baseline. *)
  let initialize = let%arr invalidate_shell and confirmed_membership and set_confirmed_membership and session_membership in
    Effect.Many [
      (if Option.is_none confirmed_membership then set_confirmed_membership (Some session_membership) else Effect.Ignore);
      Effect.of_thunk (fun () -> register_invalidation invalidate_shell)] in
  Bonsai.Edge.lifecycle ~on_activate:initialize graph;
  let page =
    match%sub route with
    | Home | Advisor _ -> Bonsai.return Vdom.Node.none
    | Cashflow -> Cashflow_page.page ~ctx graph
    | Inbox -> Transactions_page.page ~ctx ~review:true ~offset:(Bonsai.return 0) ~category:(Bonsai.return None) graph
    | Transactions { offset; category } -> Transactions_page.page ~ctx ~review:false ~offset ~category graph
    | Accounts -> Accounts_page.list ~ctx graph
    | Account id -> Accounts_page.detail ~ctx ~account_id:id graph
    | Budgets -> Budgets_page.page ~ctx graph
    | Forecast -> Forecast_page.page ~ctx graph
    | Household -> Household_page.page ~ctx ~advisor graph
    | Insights -> Insights_page.page ~ctx graph
    | Settings -> Settings_page.page ~ctx ~advisor graph
    | Add -> Add_page.page ~ctx graph
    | Import -> Import_page.page ~ctx graph
  in
  (* The question becomes the draft of a new personal conversation only if that conversation opens. *)
  let ask_advisor = let%arr new_conversation and advisor and session in
    fun question -> match advisor_currency session W.Personal with
      | None -> new_conversation W.Personal
      | Some _ -> Effect.Many [ new_conversation W.Personal; advisor.dispatch (C.Draft_changed question); focus_advisor ] in
  let palette_view = Palette.component ~ctx ~ask:ask_advisor ~open_:palette ~close:(let%arr set_palette in set_palette false) graph in
  let inbox_count = let%arr session in J.int (J.field "inbox_count" session) in
  let%arr page and home and route and locale and navigate and toast and pending_g and set_pending_g and set_palette and palette_view and inbox_count
  and set_route and theme and set_theme in
  (* Back and forward buttons. *)
  Dom_html.window##.onpopstate :=
    Dom_html.handler (fun _ ->
        let dest = Route.of_path (Js.to_string Dom_html.window##.location##.pathname) in
        Effect.Expert.handle_non_dom_event_exn (scroll_transition ~from:route ~dest (set_route dest));
        Js._true);
  let link ~cls (r, section) =
    let active =
      match section with
      | Some s -> Option.equal Route.equal_section (Route.section route) (Some s)
      | None -> Route.equal r route || (match r, route with Route.Advisor _, Advisor _ -> true | _ -> false)
    in
    Vdom.Node.a
      ~attrs:
        ([ Vdom.Attr.class_ (cls ^ if active then " active" else ""); Vdom.Attr.href (Route.to_path r);
          Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate r ]) ]
         @ if active then [Vdom.Attr.create "aria-current" (if Option.is_some section then "true" else "page")] else [])
      [ Ui.span ~cls:"icon" [ icon r ]; Ui.span ~cls:"label" [ Ui.text (match section with Some s -> Route.section_title locale s | None -> Route.title locale r) ];
        (match r with Route.Transactions _ when inbox_count > 0 -> Ui.span ~cls:"badge" [ Ui.text (Int.to_string inbox_count) ] | _ -> Vdom.Node.none) ]
  in
  let section_strip =
    match Route.section route with
    | None -> Vdom.Node.none
    | Some s ->
        Vdom.Node.create "nav" ~attrs:[ Vdom.Attr.class_ "section-nav"; Vdom.Attr.create "aria-label" (Route.section_title locale s) ]
          (List.map (Route.section_pages s) ~f:(fun r ->
               let active = Route.equal r (Route.section_page route) in
               Vdom.Node.a
                 ~attrs:
                   ([ Vdom.Attr.class_ (if active then "active" else ""); Vdom.Attr.href (Route.to_path r);
                      Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate r ]) ]
                    @ if active then [ Vdom.Attr.create "aria-current" "page" ] else [])
                 [ Ui.text (Route.title locale r) ]))
  in
  (* Keyboard: ⌘K or / opens the palette; g then a letter goes to a page. Typing in a field is left alone. *)
  let on_key (event : Dom_html.keyboardEvent Js.t) =
    (* A disabled Save can move focus off the sheet while its request is pending. The
       dialog's presence, not just the event target, must suppress shell shortcuts. *)
    let logo_dialog_open = Js.Opt.test (Dom_html.document##getElementById (Js.string "logo-dialog")) in
    let target_is_field =
      match Js.Opt.to_option event##.target with
      | Some el -> (match String.lowercase (Js.to_string el##.tagName) with "input" | "textarea" | "select" -> true | _ -> false)
      | None -> false
    in
    let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
    let meta = Js.to_bool event##.metaKey || Js.to_bool event##.ctrlKey in
    if logo_dialog_open then Effect.Ignore
    else if meta && String.equal key "k" then Effect.Many [ Effect.Prevent_default; set_palette true ]
    else if target_is_field then Effect.Ignore
    else if String.equal key "/" then Effect.Many [ Effect.Prevent_default; set_palette true ]
    else if pending_g then Effect.Many ([ set_pending_g false ] @ match shortcut key with Some r -> [ navigate r ] | None -> [])
    else if String.equal key "g" then set_pending_g true
    else Effect.Ignore
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "shell"; Vdom.Attr.Global_listeners.keydown on_key ]
    [ Vdom.Node.create "nav"
        ~attrs:[ Vdom.Attr.class_ "sidebar" ]
        ([ Ui.div ~cls:"brand"
             [ Ui.span [ Ui.text (Msg.app_name locale) ];
               Vdom.Node.button
                 ~attrs:
                   [ Vdom.Attr.class_ "button icon-button"; Vdom.Attr.type_ "button"; Vdom.Attr.title (Msg.command_placeholder locale ^ " (⌘K)");
                     Vdom.Attr.create "aria-label" (Msg.command_placeholder locale); Vdom.Attr.on_click (fun _ -> set_palette true) ]
                 [ Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Search ] ] ]
        @ List.concat_map nav_groups ~f:(fun routes -> Ui.div ~cls:"nav-gap" [] :: List.map routes ~f:(link ~cls:"nav-link"))
        @ [ Ui.div ~cls:"sidebar-foot"
              [ link ~cls:"nav-link" (Route.Settings, None);
                Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.class_ "button icon-button"; Vdom.Attr.type_ "button"; Vdom.Attr.title (Msg.palette_flip_theme locale);
                      Vdom.Attr.create "aria-label" (Msg.palette_flip_theme locale); Vdom.Attr.on_click (fun _ -> set_theme (Theme.flipped theme)) ]
                  [ Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2)
                      (if (match theme with Theme.Light -> false | Dark -> true | System -> Theme.system_is_dark ()) then Sun else Moon) ] ] ]);
      Ui.div ~cls:"content" [Vdom.Node.div
        ~attrs:(if (match route with Route.Home | Advisor _ -> true | _ -> false) then [] else [Vdom.Attr.create "hidden" ""]) [home]; section_strip; page];
      Vdom.Node.create "nav" ~attrs:[ Vdom.Attr.class_ "tabbar" ] (List.map tab_items ~f:(link ~cls:"tab"));
      palette_view;
      (match toast with Some message -> Ui.div ~cls:"toast" [ Ui.text message ] | None -> Vdom.Node.none) ]

let sign_in_page locale =
  Vdom.Node.main
    ~attrs:[ Vdom.Attr.class_ "signin" ]
    [ Ui.div ~cls:"brand" [ Ui.text (Msg.app_name locale) ]; Vdom.Node.h1 [ Ui.text (Msg.signin_title locale) ]; Ui.muted (Msg.signin_body locale);
      Vdom.Node.a ~attrs:[ Vdom.Attr.class_ "button primary"; Vdom.Attr.href ("/auth/login?return_to=" ^ Js.to_string Dom_html.window##.location##.pathname) ] [ Ui.text (Msg.action_sign_in locale) ] ]

let browser_locale () =
  let language = Js.Optdef.case Dom_html.window##.navigator##.language (fun () -> "en") Js.to_string in
  if String.is_prefix language ~prefix:"sv" then "sv" else "en"

let app graph =
  let session, set_session = Bonsai.state None graph in
  let generation = ref 0 and identity = ref None and invalidation = ref Effect.Ignore in
  let register_invalidation effect = invalidation := effect in
  let auth_lost = let%arr set_session in Effect.Many [Effect.of_thunk (fun () -> identity := None; incr generation); set_session (Some Api.Signed_out)] in
  let reload = let%arr set_session in
    let%bind.Effect request = Effect.of_thunk (fun () -> incr generation; !generation) in
    let%bind.Effect s = session_effect in
    if request <> !generation then Effect.Ignore else
    let next = match s with Api.Signed_in json -> Some (J.str (J.path ["user"; "id"] json), J.str (J.field "csrf_token" json)) | _ -> None in
    match !identity, next, s with
    | Some old, Some next, _ when not (Poly.equal old next) ->
        Effect.Many [!invalidation; set_session (Some Api.Signed_out); Effect.reload_page]
    | Some _, None, Api.Signed_out ->
        Effect.Many [!invalidation; Effect.of_thunk (fun () -> identity := None); set_session (Some s)]
    | _ -> Effect.Many [Effect.of_thunk (fun () ->
        if Option.is_some next then identity := next;
        match s with Api.Signed_in json -> Api.accept_session json | _ -> ()); set_session (Some s)] in
  Bonsai.Edge.lifecycle ~on_activate:reload graph;
  let signed_in_session = let%arr session in match session with Some (Api.Signed_in json) -> json | _ -> `Null in
  let has_session = let%arr session in match session with Some (Api.Signed_in _) -> true | _ -> false in
  let shell =
    match%sub has_session with
    | true -> signed_in ~session:signed_in_session ~reload_session:reload ~register_invalidation ~auth_lost graph
    | false -> Bonsai.return Vdom.Node.none
  in
  let%arr session and shell in
  let locale = browser_locale () in
  match session with
  | None -> Ui.div ~cls:"boot" [ Ui.div ~cls:"shimmer" [] ]
  | Some Signed_out -> sign_in_page locale
  | Some (Unreachable message) -> Ui.div ~cls:"boot" [ Ui.muted (Msg.error_generic ~message locale) ]
  | Some (Signed_in _) -> shell

let () =
  Theme.apply (Theme.load ());
  Keyboard.install ();
  Async_js.init ();
  Bonsai_web.Start.start app
