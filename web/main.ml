(* The app shell: loads the session, then shows the sign-in page or the app. The app has a
   sidebar on wide screens and a tab bar on phones, a command palette (⌘K or /), single-key
   navigation (g then h, c, t, i, a, b, f, o, n, s), and a light/dark switch. *)
open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

let session_effect = Effect.of_deferred_thunk Api.session

(* The history entry is pushed when the effect runs, not when it is built: views build navigation
   effects for every link and button they render. *)
let navigate_to set_route route =
  Effect.Many
    [ Effect.of_thunk (fun () -> Dom_html.window##.history##pushState Js.null (Js.string "") (Js.some (Js.string (Route.to_path route))));
      set_route route ]

(* The sidebar: what you look at, then where the money lives; Settings sits at the foot. *)
let nav_groups = Route.[ [ Home; Cashflow; Transactions { offset = 0; category = None }; Budgets; Forecast; Insights ]; [ Accounts; Household ] ]
let tab_items = Route.[ Home; Transactions { offset = 0; category = None }; Add; Budgets; Settings ]

let icon route =
  let name : Feather_icon.t =
    match route with
    | Route.Home -> Home
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

let signed_in ~(session : J.t Bonsai.t) ~(reload_session : unit Effect.t Bonsai.t) graph =
  let route, set_route = Bonsai.state ~equal:Route.equal (Route.of_path (Js.to_string Dom_html.window##.location##.pathname)) graph in
  let version, set_version = Bonsai.state 0 graph in
  let toast, set_toast = Bonsai.state None graph in
  let pending_g, set_pending_g = Bonsai.state false graph in
  let palette, set_palette = Bonsai.state false graph in
  let locale = let%arr session in J.str ~default:"en" (J.path [ "user"; "locale" ] session) in
  let currency = let%arr session in J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
  let changed = let%arr set_version and version and reload_session in Effect.Many [ set_version (version + 1); reload_session ] in
  let navigate = let%arr set_route and set_palette in fun route -> Effect.Many [ set_palette false; navigate_to set_route route ] in
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
  let page =
    match%sub route with
    | Home -> Home.page ~ctx graph
    | Cashflow -> Cashflow_page.page ~ctx graph
    | Inbox -> Transactions_page.page ~ctx ~review:true ~offset:(Bonsai.return 0) ~category:(Bonsai.return None) graph
    | Transactions { offset; category } -> Transactions_page.page ~ctx ~review:false ~offset ~category graph
    | Accounts -> Accounts_page.list ~ctx graph
    | Account id -> Accounts_page.detail ~ctx ~account_id:id graph
    | Budgets -> Budgets_page.page ~ctx graph
    | Forecast -> Forecast_page.page ~ctx graph
    | Household -> Household_page.page ~ctx graph
    | Insights -> Insights_page.page ~ctx graph
    | Settings -> Settings_page.page ~ctx graph
    | Add -> Add_page.page ~ctx graph
  in
  let palette_view = Palette.component ~ctx ~open_:palette ~close:(let%arr set_palette in set_palette false) graph in
  let inbox_count = let%arr session in J.int (J.field "inbox_count" session) in
  let%arr page and route and locale and navigate and toast and pending_g and set_pending_g and set_palette and palette_view and inbox_count
  and set_route and theme and set_theme in
  (* Back and forward buttons. *)
  Dom_html.window##.onpopstate :=
    Dom_html.handler (fun _ ->
        Effect.Expert.handle_non_dom_event_exn (set_route (Route.of_path (Js.to_string Dom_html.window##.location##.pathname)));
        Js._true);
  let link ~cls r =
    Vdom.Node.a
      ~attrs:
        [ Vdom.Attr.class_ (cls ^ if Route.equal r route then " active" else ""); Vdom.Attr.href (Route.to_path r);
          Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate r ]) ]
      [ Ui.span ~cls:"icon" [ icon r ]; Ui.span ~cls:"label" [ Ui.text (Route.title locale r) ];
        (match r with Route.Transactions _ when inbox_count > 0 -> Ui.span ~cls:"badge" [ Ui.text (Int.to_string inbox_count) ] | _ -> Vdom.Node.none) ]
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
              [ link ~cls:"nav-link" Route.Settings;
                Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.class_ "button icon-button"; Vdom.Attr.type_ "button"; Vdom.Attr.title (Msg.palette_flip_theme locale);
                      Vdom.Attr.create "aria-label" (Msg.palette_flip_theme locale); Vdom.Attr.on_click (fun _ -> set_theme (Theme.flipped theme)) ]
                  [ Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2)
                      (if (match theme with Theme.Light -> false | Dark -> true | System -> Theme.system_is_dark ()) then Sun else Moon) ] ] ]);
      Ui.div ~cls:"content" [ page ];
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
  let reload = let%arr set_session in let%bind.Effect s = session_effect in set_session (Some s) in
  Bonsai.Edge.lifecycle ~on_activate:reload graph;
  let signed_in_session = let%arr session in match session with Some (Api.Signed_in json) -> json | _ -> `Null in
  let has_session = let%arr session in match session with Some (Api.Signed_in _) -> true | _ -> false in
  let shell =
    match%sub has_session with
    | true -> signed_in ~session:signed_in_session ~reload_session:reload graph
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
  Async_js.init ();
  Bonsai_web.Start.start app
