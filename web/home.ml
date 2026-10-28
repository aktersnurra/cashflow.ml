(* The landing page: the attention feed, most important card first. Each card is its own
   component (Bonsai.assoc), so an import in progress on one card keeps its state while the feed
   reloads around it. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let act ~key ~action = Api.run "act_on_card" (`Assoc [ ("key", `String key); ("action", `String action) ])

let card_view ~(ctx : Ctx.t) (key : string Bonsai.t) (card : J.t Bonsai.t) graph =
  let account_id = let%arr card in Some (J.str (J.path [ "data"; "account"; "id" ] card)) in
  let import = Import_widget.component ~ctx ~account_id graph in
  let%arr card and key and import and locale = ctx.locale and changed = ctx.changed and navigate = ctx.navigate and notify = ctx.notify and session = ctx.session in
  ignore key;
  let currency = J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
  let data = J.field "data" card in
  let card_key = J.str (J.field "key" card) in
  let money ?(code = currency) json = Fmt.money_json ~code json in
  let account_currency = J.str ~default:currency (J.path [ "account"; "currency" ] data) in
  let after effect =
    let%bind.Effect result = effect in
    match result with Ok _ -> changed | Error message -> notify message
  in
  let later = Ui.button (Msg.action_snooze locale) (after (act ~key:card_key ~action:"snooze")) in
  let done_ = Ui.button (Msg.action_done locale) (after (act ~key:card_key ~action:"resolve")) in
  let month json = Fmt.month_label locale (J.str (J.field "cycle" json)) in
  match J.str (J.field "kind" card) with
  | "invitation" ->
      let id = J.str (J.field "id" data) in
      let respond accept = after (Api.run "respond_to_invitation" (`Assoc [ ("household_id", `String id); ("accept", `Bool accept) ])) in
      Ui.card ~tone:"accent" ~title:(Msg.card_invitation_title ~household:(J.str (J.field "name" data)) locale)
        ~body:[ Ui.muted (Msg.card_invitation_body locale) ]
        ~actions:[ Ui.button ~kind:"primary" (Msg.action_accept locale) (respond true); Ui.button (Msg.action_decline locale) (respond false) ] ()
  | "reconciliation_mismatch" ->
      let account = J.field "account" data in
      let m json = money ~code:account_currency json in
      let adjust =
        after
          (Api.run "reconcile"
             (`Assoc [ ("account_id", J.field "id" account); ("date", J.field "date" data); ("statement_balance", J.field "statement_balance" data); ("adjust", `Bool true) ]))
      in
      Ui.card ~tone:"warning"
        ~title:(Msg.card_mismatch_title ~account:(Accounts_data.name ~locale account) ~difference:(m (J.field "difference" data)) locale)
        ~body:[ Ui.muted (Msg.card_mismatch_body ~statement:(m (J.field "statement_balance" data)) ~date:(Fmt.date_json (J.field "date" data)) ~ledger:(m (J.field "ledger_balance" data)) locale) ]
        ~actions:[ Ui.button ~kind:"primary" (Msg.action_adjust locale) adjust; later ] ()
  | "import_due" ->
      let days = J.field "days" data in
      Ui.card ~title:(Msg.card_import_title ~account:(Accounts_data.name ~locale (J.field "account" data)) locale)
        ~body:[ Ui.muted (if J.is_null days then Msg.card_import_never locale else Msg.card_import_days ~days:(J.int days) locale); import ]
        ~actions:[ later ] ()
  | "inbox" ->
      Ui.card ~tone:"accent" ~title:(Msg.card_inbox_title ~count:(J.int (J.field "count" data)) locale)
        ~body:[ Ui.muted (Msg.card_inbox_body locale) ]
        ~actions:[ Ui.button ~kind:"primary" (Msg.action_review locale) (navigate Route.Inbox) ] ()
  | "unmatched_entry" ->
      let t = J.field "transaction" data in
      let void = after (Api.run "void_pending" (`Assoc [ ("idempotency_key", `String ("void:" ^ J.str (J.field "id" t))); ("id", J.field "id" t) ])) in
      Ui.card ~tone:"warning" ~title:(Msg.card_unmatched_title ~description:(J.str (J.field "description" t)) locale)
        ~body:[ Ui.muted (Msg.card_unmatched_body ~amount:(money ~code:(J.str ~default:currency (J.field "currency" t)) (J.field "amount" t)) ~date:(Fmt.date_json (J.field "date" t)) locale) ]
        ~actions:[ Ui.button (Msg.action_void locale) void; later ] ()
  | "month_review" ->
      let summary = J.field "summary" data in
      Ui.card ~eyebrow:(month (J.field "cycle" data)) ~title:(Msg.card_review_title ~month:(month (J.field "cycle" data)) locale)
        ~body:
          ((match J.str_opt (J.field "text" data) with Some text -> [ Ui.p text ] | None -> [])
          @
          if J.is_null summary then []
          else
            [ Ui.div ~cls:"kpis"
                [ Ui.kpi (Msg.label_income locale) (Fmt.format_money locale ~code:currency (J.field "income" summary));
                  Ui.kpi (Msg.label_spending locale) (Fmt.format_money locale ~code:currency (J.field "spending" summary));
                  Ui.kpi ~tone:(Fmt.sign_tone (J.field "savings" summary)) (Msg.label_savings locale) (Fmt.format_money locale ~code:currency (J.field "savings" summary));
                  Ui.kpi (Msg.label_savings_rate locale) (J.str ~default:"–" (J.field "savings_rate" summary)) ] ])
        ~actions:[ done_ ] ()
  | "budget_alert" ->
      let category = Accounts_data.name ~locale (J.field "category" data) in
      let over = String.equal (J.str (J.field "status" data)) "over_budget" in
      let spent = Fmt.to_float (J.field "spent" data) and budget = Fmt.to_float (J.field "budget" data) and expected = Fmt.to_float (J.field "expected_by_now" data) in
      Ui.card ~tone:(if over then "danger" else "warning")
        ~title:(if over then Msg.card_budget_over ~category locale else Msg.card_budget_ahead ~category locale)
        ~body:
          [ Ui.meter ~marker:(if Float.(budget > 0.) then Some (expected /. budget) else None) (if Float.(budget > 0.) then spent /. budget else 0.);
            Ui.muted (Msg.card_budget_body ~spent:(money (J.field "spent" data)) ~budget:(money (J.field "budget" data)) ~expected:(money (J.field "expected_by_now" data)) locale) ]
        ~actions:[ done_; later ] ()
  | "shortfall" ->
      let m json = money ~code:account_currency json in
      Ui.card ~tone:"warning" ~title:(Msg.card_shortfall_title ~account:(Accounts_data.name ~locale (J.field "account" data)) locale)
        ~body:
          (Ui.muted (Msg.card_shortfall_body ~due:(m (J.field "due" data)) ~available:(m (J.field "available" data)) ~short:(m (J.field "short_by" data)) locale)
          :: Ui.muted (Msg.advisor_bills_estimate locale)
          :: List.map (J.list (J.field "upcoming" data)) ~f:(fun u ->
                 Ui.muted (Msg.card_shortfall_item ~merchant:(J.str (J.field "merchant" u)) ~amount:(m (J.field "amount" u)) ~date:(Fmt.date_json (J.field "expected" u)) locale)))
        ~actions:
          [ Ui.button ~kind:"primary" (Msg.action_move_money locale) (navigate Route.Add); Ui.button (Msg.action_see_forecast locale) (navigate Route.Forecast);
            done_; later ] ()
  | "unusual_spend" ->
      let category = Accounts_data.name ~locale (J.field "category" data) in
      let spent = Fmt.to_float (J.field "spent" data) and usual = Fmt.to_float (J.field "usual" data) in
      Ui.card
        ~eyebrow:(if String.equal (J.str (J.field "scope" data)) "household" then Msg.label_household locale else Msg.label_personal locale)
        ~title:(Msg.card_unusual_title ~category locale)
        ~body:
          ((match J.str_opt (J.field "text" data) with Some text -> [ Ui.p text ] | None -> [])
          @ [ (* Past the usual whole cycle, so the bar runs over; the marker is where the usual ends. *)
              Ui.meter ~marker:(if Float.(spent > 0.) then Some (usual /. spent) else None) (if Float.(usual > 0.) then spent /. usual else 2.);
              Ui.muted (Msg.card_unusual_body ~spent:(money (J.field "spent" data)) ~usual:(money (J.field "usual" data)) locale) ])
        ~actions:[ Ui.button (Msg.action_see_spending locale) (navigate Route.Insights); done_; later ] ()
  | "contributions_due" ->
      Ui.card ~title:(Msg.card_contributions_title locale)
        ~body:[ Ui.muted (Msg.card_contributions_body ~remaining:(money (J.field "remaining" data)) ~contribution:(money (J.field "contribution" data)) locale) ]
        ~actions:[ Ui.button (Msg.nav_household locale) (navigate Route.Household); done_ ] ()
  | "settle_up" ->
      Ui.card ~title:(Msg.label_household locale)
        ~body:(List.map (J.list (J.field "owed" data)) ~f:(fun o -> Ui.p (Msg.card_settle_title ~name:(J.str (J.field "name" o)) ~amount:(money (J.field "amount" o)) locale)))
        ~actions:[ later ] ()
  | "count_cash" ->
      Ui.card ~title:(Msg.card_cash_title ~account:(Accounts_data.name ~locale (J.field "account" data)) locale)
        ~body:[ Ui.muted (Msg.card_cash_body ~since:(Fmt.date_json (J.field "since" data)) locale) ]
        ~actions:[ Ui.button (Msg.action_count locale) (navigate (Route.Account (J.str (J.path [ "account"; "id" ] data)))); later ] ()
  | "goal_off_track" ->
      Ui.card ~title:(Msg.card_goal_title ~goal:(J.str (J.field "goal" data)) locale)
        ~body:
          [ Ui.meter (Fmt.to_float (J.field "saved" data) /. Float.max 1. (Fmt.to_float (J.field "target" data)));
            Ui.muted (Msg.card_goal_body ~saved:(money (J.field "saved" data)) ~target:(money (J.field "target" data)) ~required:(money (J.field "required_monthly" data)) locale) ]
        ~actions:[ done_; later ] ()
  | "recurring_change" ->
      let merchant = J.str (J.field "merchant" data) in
      Ui.card
        ~title:
          (if J.bool (J.field "price_changed" data) then
             Msg.card_recurring_changed ~merchant ~previous:(money (J.field "previous_amount" data)) ~last:(money (J.field "last_amount" data)) locale
           else Msg.card_recurring_new ~cadence:(J.str (J.field "cadence" data)) ~merchant locale)
        ~actions:[ Ui.button (Msg.action_keep locale) (after (act ~key:card_key ~action:"resolve")); later ] ()
  | "overview" ->
      let summary = J.field "summary" data in
      let code = J.str ~default:currency (J.field "currency" data) in
      Ui.card ~tone:"overview" ~eyebrow:(Msg.label_personal locale) ~title:(Msg.card_overview_title ~month:(month (J.field "cycle" data)) locale)
        ~body:
          (if J.is_null summary then []
           else
             [ Ui.div ~cls:"kpis"
                 [ Ui.kpi (Msg.label_income locale) (Fmt.format_money locale ~code (J.field "income" summary));
                   Ui.kpi (Msg.label_spending locale) (Fmt.format_money locale ~code (J.field "spending" summary));
                   Ui.kpi ~tone:(Fmt.sign_tone (J.field "savings" summary)) (Msg.label_savings locale) (Fmt.format_money locale ~code (J.field "savings" summary)) ];
               (if J.bool (J.field "reconciled" summary) then Vdom.Node.none else Ui.muted (Msg.label_provisional locale)) ])
        ~actions:[ Ui.button (Msg.nav_insights locale) (navigate Route.Insights) ] ()
  | _ -> Vdom.Node.none

(* Overview, in the order the questions come: which period and whose books, how much was saved,
   where the money went, and then the few things that need you. Notices that only restate the
   figures above (the overview and month-review cards) are left out; at most three remain. *)
let figure_kinds = [ "overview"; "month_review" ]

(* Cards that ask for something before the figures can be trusted or before it is too late. When one is there, the
   cards come first. *)
let urgent_kinds = [ "invitation"; "reconciliation_mismatch"; "import_due"; "inbox"; "shortfall" ]

(* Where the money went says little while most of it went nowhere yet: until the inbox is mostly cleared, the chart
   would be one Uncategorized band. The UI can't rename a starter category, so the starter slug finds it. *)
let mostly_uncategorized categories_json =
  let rows = J.list (J.field "categories" categories_json) in
  let total f = List.sum (module Float) rows ~f:(fun r -> if f r then Float.abs (Fmt.to_float (J.field "total" r)) else 0.) in
  let uncategorized r = String.is_prefix (J.str ~default:"" (J.path [ "category"; "starter"; "slug" ] r)) ~prefix:"uncategorized" in
  Float.(total uncategorized > total (fun _ -> true) /. 2.)

let page ~(ctx : Ctx.t) ~(on_ask : (finding_key:string -> scope:Cashflow_advisor_protocol.Wire.scope -> unit Effect.t) Bonsai.t)
    ~selected_finding ~outlet ~advisor_route ~(visible : bool Bonsai.t) graph =
  let captured, set_captured = Bonsai.state None graph in
  let offset, set_offset = Bonsai.state 0 graph in
  let scope, set_scope = Bonsai.state "me" graph in
  let summary = Remote.load ~active:visible ~name:"cashflow_summary" ~args:(let%arr scope in `Assoc [ ("scope", `String scope); ("cycles", `Int 13) ]) ~ctx graph in
  let categories =
    Remote.load ~active:visible ~name:"spending_by_category" ~args:(let%arr scope and offset in `Assoc [ ("scope", `String scope); ("cycle_offset", `Int offset) ]) ~ctx graph
  in
  (* The figures show at once; when there is no text yet, a second load asks for it to be written. That request is armed for
     one scope and one version of the data, a few seconds after the figures arrived: a change of either lets it lapse (a run
     of changes, like clearing last week's inbox, writes once when it settles) and a scope switch starts over. The server
     writes once per set of figures and remembers a failure. Never on the Advisor page, where this page is not showing. *)
  let armed, set_armed = Bonsai.state None graph in
  let brief =
    Remote.load ~active:visible ~name:"get_weekly_brief"
      ~args:(let%arr armed and scope and version = ctx.version in
             `Assoc [ ("scope", `String scope); ("write", `Bool (Option.equal [%equal: string * int] armed (Some (scope, version)))) ])
      ~ctx graph
  in
  Bonsai.Edge.on_change ~equal:phys_equal brief
    ~callback:(let%arr set_armed and advisor_route and scope and version = ctx.version in fun (result : Remote.t) ->
      match result with
      | Loaded json when J.bool (J.path [ "brief"; "can_write" ] json) && not advisor_route ->
          let%bind.Effect () = Effect.of_deferred_thunk (fun () -> Async_kernel.after (Time_ns.Span.of_sec 6.)) in
          set_armed (Some (scope, version))
      | _ -> Effect.Ignore)
    graph;
  let feed = Remote.load ~active:visible ~name:"get_attention_feed" ~args:(Bonsai.return (`Assoc [("narrative", `Bool false)])) ~ctx graph in
  let cards =
    let%arr feed in
    match feed with
    | Loaded json ->
        J.list (J.field "cards" json)
        |> List.filter ~f:(fun card -> not (List.mem figure_kinds (J.str (J.field "kind" card)) ~equal:String.equal))
        |> Fn.flip List.take 3
        |> List.mapi ~f:(fun i card -> (Printf.sprintf "%03d:%s" i (J.str (J.field "key" card)), card))
        |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
    | Loading | Failed _ -> String.Map.empty
  in
  let visible_cards = let%arr cards and captured and selected_finding in
    match captured, selected_finding with Some (key, cards), Some selected when String.equal key selected -> cards | _ -> cards in
  let views = Bonsai.assoc (module String) visible_cards ~f:(fun key card graph ->
    let body = card_view ~ctx key card graph in
    let%arr body and card and on_ask and selected_finding and outlet and cards and set_captured and session = ctx.session and locale = ctx.locale in
    let finding_key = J.str (J.field "key" card) in
    let selected = Option.equal String.equal selected_finding (Some finding_key) in
    let eligible = List.mem ["unusual_spend"; "shortfall"] (J.str (J.field "kind" card)) ~equal:String.equal in
    let scope = if String.equal (J.str (J.path ["data"; "scope"] card)) "household"
      || String.equal (J.str (J.path ["data"; "account"; "owner"] card)) "household" then
      Cashflow_advisor_protocol.Wire.Household (J.str (J.path ["household"; "id"] session)) else Personal in
    Vdom.Node.div ~attrs:([Vdom.Attr.create "data-finding-key" finding_key] @ if selected then [Vdom.Attr.create "data-testid" "selected-finding"] else [])
      ([body] @ (if eligible then [Ui.div ~cls:"finding-reply" [Ui.button (Msg.advisor_ask locale)
        (Effect.Many [set_captured (Some (finding_key, cards)); on_ask ~finding_key ~scope])]] else [])
       @ if selected then [outlet] else [])) graph in
  let%arr summary and categories and brief and views and offset and set_offset and scope and set_scope and locale = ctx.locale and session = ctx.session
  and navigate = ctx.navigate and outlet and selected_finding and visible_cards and advisor_route in
  let cycles = match summary with Loaded json -> J.list (J.field "cycles" json) | Loading | Failed _ -> [] in
  let code = match summary with Loaded json -> J.str ~default:"SEK" (J.field "currency" json) | Loading | Failed _ -> "SEK" in
  let index = List.length cycles - 1 + offset in
  let selected = List.nth cycles index in
  let whole json = Fmt.whole locale ~code json in
  let scopes =
    if Ctx.has_household session then [ ("me", Msg.label_personal locale); ("household", Msg.label_household locale); ("family", Msg.label_family locale) ] else []
  in
  let head =
    Ui.div ~cls:"page-head"
      [ Vdom.Node.h1 [ Ui.text (Msg.nav_home locale) ];
        Vdom.Node.a ~attrs:([Vdom.Attr.class_ "advisor-mobile-link button"; Vdom.Attr.href "/advisor";
          Vdom.Attr.on_click (fun _ -> Effect.Many [Effect.Prevent_default; navigate (Route.Advisor None)])]
          @ if advisor_route then [Vdom.Attr.create "aria-current" "page"] else []) [Ui.text (Msg.nav_advisor locale)];
        Ui.div ~cls:"page-controls"
          [ (if List.is_empty scopes then Vdom.Node.none
             else Ui.div ~cls:"segmented" (List.map scopes ~f:(fun (value, label) -> Ui.button ~kind:(if String.equal value scope then "active" else "") label (set_scope value))));
            Ui.period
              ~label:(match selected with Some c -> Fmt.month_label locale (J.str (J.field "cycle" c)) | None -> "")
              ~previous:(Msg.overview_previous locale, index <= 0 || offset <= -6, set_offset (offset - 1))
              ~next:(Msg.overview_next locale, offset >= 0, set_offset (offset + 1)) ] ]
  in
  let figures (c : J.t) =
    let savings = Fmt.to_float (J.field "savings" c) in
    (* Compared only with earlier cycles that had income, and only when there are at least three:
       a comparison with empty months says nothing. *)
    let before = List.filteri cycles ~f:(fun i c -> i < index && i >= index - 6 && Float.(Fmt.to_float (J.field "income" c) > 0.)) in
    let average = if List.is_empty before then 0. else List.sum (module Float) before ~f:(fun c -> Fmt.to_float (J.field "savings" c)) /. Float.of_int (List.length before) in
    let trend =
      if List.length before < 3 || Float.(abs average < 1.) then Vdom.Node.none
      else
        let change = (savings -. average) /. Float.abs average *. 100. in
        Ui.div ~cls:("trend " ^ if Float.(change >= 0.) then "positive" else "negative")
          [ Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) (if Float.(change >= 0.) then Trending_up else Trending_down);
            Ui.span [ Ui.text (Msg.overview_vs_average ~change:(Fmt.percent ~signed:true locale change) locale) ] ]
    in
    let line label value = Ui.div ~cls:"figure-line" [ Ui.span ~cls:"figure-label" [ Ui.text label ]; Ui.span ~cls:"figure-value" [ Ui.text value ] ] in
    Ui.div ~cls:"panel figures"
      [ Ui.div ~cls:"figures-main"
          [ Ui.div ~cls:"figure-label" [ Ui.text (Msg.overview_net_savings locale) ];
            Ui.div ~cls:("hero " ^ Fmt.sign_tone (J.field "savings" c)) [ Ui.text (Fmt.signed_whole locale ~code (J.field "savings" c)) ];
            trend;
            (if J.bool (J.field "reconciled" c) then Vdom.Node.none else Ui.div ~cls:"provisional" [ Ui.text (Msg.label_provisional locale) ]) ];
        Ui.div ~cls:"figures-side"
          [ line (Msg.label_income locale) (whole (J.field "income" c));
            line (Msg.overview_expenses locale) (whole (J.field "spending" c));
            line (Msg.label_savings_rate locale)
              (match J.str_opt (J.field "savings_rate" c) with
              | Some rate -> Fmt.percent locale (Option.value (Float.of_string_opt (String.chop_suffix_if_exists rate ~suffix:"%")) ~default:0.)
              | None -> "–") ] ]
  in
  let flow (c : J.t) categories_json =
    let left, right = Flow.nodes ~locale ~code c categories_json in
    let on_select id = navigate (Route.Transactions { offset; category = Some id }) in
    Ui.div ~cls:"panel flow" (Vdom.Node.h2 [ Ui.text (Msg.overview_where locale) ] :: Flow.view ~on_select ~left ~right ())
  in
  let body =
    match summary, selected with
    | Failed _, _ | Loading, _ -> Remote.view ~locale summary ~f:(fun _ -> Vdom.Node.none)
    | Loaded _, None -> Ui.empty (Msg.overview_empty locale)
    | Loaded _, Some c when Float.(Fmt.to_float (J.field "income" c) = 0. && Fmt.to_float (J.field "spending" c) = 0.) ->
        Ui.div ~cls:"panel empty-panel"
          [ Ui.p (Msg.overview_empty locale); Ui.div ~cls:"actions" [ Ui.button ~kind:"primary" (Msg.import_title locale) (navigate Route.Import) ] ]
    | Loaded _, Some c ->
        Ui.div [ figures c; Remote.view ~locale categories ~f:(fun json -> if mostly_uncategorized json then Vdom.Node.none else flow c json) ]
  in
  let weekly =
    match brief with
    | Loaded json when not (J.is_null (J.field "brief" json)) ->
        let b = J.field "brief" json in
        let code = J.str ~default:code (J.field "currency" b) in
        let money field = Fmt.format_money locale ~code (J.field field b) in
        let top = J.list (J.field "top" b) in
        Ui.div ~cls:"panel weekly-brief"
          [ Vdom.Node.h2 [ Ui.text (Msg.brief_title ~first:(Fmt.date_json (J.field "first" b)) ~last:(Fmt.date_json (J.field "last" b)) locale) ];
            (match J.str_opt (J.field "text" b) with
             | Some text -> Ui.p text
             | None -> if J.bool (J.field "can_write" b) then Ui.muted (Msg.brief_writing locale) else Vdom.Node.none);
            Ui.div ~cls:"kpis"
              [ Ui.kpi (Msg.label_income locale) (money "income"); Ui.kpi (Msg.label_spending locale) (money "spending") ];
            Ui.muted (Msg.brief_before ~amount:(Fmt.money_json ~code (J.field "previous_spending" b)) locale);
            (if J.int (J.field "upcoming_count" b) = 0 then Vdom.Node.none
             else Ui.muted (Msg.brief_upcoming ~amount:(Fmt.money_json ~code (J.field "upcoming_total" b)) locale));
            (if List.is_empty top then Vdom.Node.none
             else
               Ui.div
                 [ Ui.muted (Msg.brief_top locale);
                   Vdom.Node.ul
                     (List.map top ~f:(fun c ->
                          Vdom.Node.li [ Ui.text (J.str (J.field "name" c) ^ " " ^ Fmt.format_money locale ~code (J.field "total" c)) ])) ]) ]
    | Loaded _ | Loading | Failed _ -> Vdom.Node.none
  in
  let attention =
    match Map.data views with
    | [] -> Vdom.Node.none
    | items ->
        Ui.div ~cls:"attention"
          [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.overview_attention locale) ]; Ui.span ~cls:"count" [ Ui.text (Int.to_string (List.length items)) ] ];
            Ui.div ~cls:"attention-list" items ]
  in
  let hosted = Map.exists visible_cards ~f:(fun card -> Option.equal String.equal selected_finding (Some (J.str (J.field "key" card)))) in
  let standalone = advisor_route && not hosted in
  Vdom.Node.main ~attrs:[Vdom.Attr.class_ (if standalone then "page advisor-page" else "page")]
    [(if standalone then Vdom.Node.h1 [Ui.text (Msg.nav_advisor locale)] else Vdom.Node.none);
     (* Keep the financial surface mounted for Home continuity; an unanchored
        Advisor conversation is not a second dashboard above the chat. *)
     Vdom.Node.div ~attrs:(if standalone then [Vdom.Attr.create "hidden" ""] else [])
       (let urgent = Map.exists visible_cards ~f:(fun card -> List.mem urgent_kinds (J.str (J.field "kind" card)) ~equal:String.equal) in
        if urgent then [head; attention; body] else [head; body; attention]);
     (if standalone then outlet else Vdom.Node.none)]
