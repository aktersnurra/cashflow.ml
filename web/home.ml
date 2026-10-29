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
  (* Whole kronor for what is read at a glance. *)
  let kr ?(code = currency) json = Fmt.whole locale ~code json in
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
          [ Ui.button ~kind:"primary" (Msg.action_move_money locale) (navigate Route.Add); Ui.button (Msg.action_see_forecast locale) (navigate Route.Home);
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
        ~actions:[ Ui.button (Msg.action_see_spending locale) (navigate (Route.Transactions { offset = 0; category = None })); done_; later ] ()
  | "contributions_due" ->
      Ui.card ~title:(Msg.card_contributions_title locale)
        ~body:[ Ui.muted (Msg.card_contributions_body ~remaining:(money (J.field "remaining" data)) ~contribution:(money (J.field "contribution" data)) locale) ]
        ~actions:[ Ui.button (Msg.nav_household locale) (navigate Route.Settings); done_ ] ()
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
             Msg.card_recurring_changed ~merchant ~previous:(kr (J.field "previous_amount" data)) ~last:(kr (J.field "last_amount" data)) locale
           else Msg.card_recurring_new ~cadence:(J.str (J.field "cadence" data)) ~merchant locale)
        ~actions:[ Ui.button (Msg.action_keep locale) (after (act ~key:card_key ~action:"resolve")); later ] ()
  (* Things in the statements that need you: what, how much, when, and what to do about it. *)
  | "missing_payment" ->
      let name = J.str (J.field "name" data) and amount = kr (J.field "amount" data) and date = Fmt.date_json (J.field "expected" data) in
      if J.bool (J.field "income" data) then
        Ui.card ~tone:"warning" ~title:(Msg.card_missing_income_title ~name locale) ~body:[ Ui.muted (Msg.card_missing_income_body ~amount ~date locale) ]
          ~actions:[ Ui.button (Msg.action_paid_elsewhere locale) (after (act ~key:card_key ~action:"resolve")); later ] ()
      else
        let cadence =
          String.lowercase
            (match J.str (J.field "cadence" data) with
             | "weekly" -> Msg.forecast_cadence_weekly locale
             | "quarterly" -> Msg.forecast_cadence_quarterly locale
             | "yearly" -> Msg.forecast_cadence_yearly locale
             | _ -> Msg.forecast_cadence_monthly locale)
        in
        Ui.card ~tone:"warning" ~title:(Msg.card_missing_bill_title ~name locale) ~body:[ Ui.muted (Msg.card_missing_bill_body ~amount ~cadence ~date locale) ]
          ~actions:[ Ui.button (Msg.action_paid_elsewhere locale) (after (act ~key:card_key ~action:"resolve")); later ] ()
  | "double_charge" ->
      Ui.card ~tone:"warning"
        ~title:(Msg.card_double_charge_title ~merchant:(J.str (J.field "merchant" data)) locale)
        ~body:
          [ Ui.muted
              (Msg.card_double_charge_body ~amount:(kr (J.field "amount" data)) ~first:(Fmt.date_json (J.field "first_date" data))
                 ~second:(Fmt.date_json (J.field "second_date" data)) locale) ]
        ~actions:
          [ Ui.button (Msg.action_see_transactions locale) (navigate (Route.Transactions { offset = 0; category = None }));
            Ui.button (Msg.action_both_right locale) (after (act ~key:card_key ~action:"resolve")) ] ()
  | "yearly_bill" ->
      Ui.card ~title:(Msg.card_yearly_bill_title ~name:(J.str (J.field "name" data)) ~date:(Fmt.date_json (J.field "due" data)) locale)
        ~body:[ Ui.muted (Msg.card_yearly_bill_body ~amount:(kr (J.field "amount" data)) ~last:(Fmt.date_json (J.field "last_paid" data)) locale) ]
        ~actions:[ done_; later ] ()
  | "large_purchase" ->
      Ui.card ~title:(Msg.card_large_purchase_title locale)
        ~body:
          [ Ui.muted
              (Msg.card_large_purchase_body ~amount:(kr (J.field "amount" data)) ~description:(J.str (J.field "description" data))
                 ~date:(Fmt.date_json (J.field "date" data)) locale) ]
        ~actions:[ done_ ] ()
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
        ~actions:[] ()
  | _ -> Vdom.Node.none

(* Overview, in the order the questions come: am I OK (safe to spend), what is worth knowing (the
   briefing), then what needs doing. Cards that are insights rather than to-dos are left to the briefing,
   which answers them better: the overview card restates the figures, the month review is the briefing's
   "why it cost more", a large purchase is its one-off, and a price change its regular bills. At most three
   remain. *)
let restates_figures card =
  List.mem [ "overview"; "month_review"; "large_purchase"; "recurring_change" ] (J.str (J.field "kind" card)) ~equal:String.equal

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
    ~(desk : Desk.t Bonsai.t) ~selected_finding ~outlet ~advisor_route ~(visible : bool Bonsai.t) graph =
  let captured, set_captured = Bonsai.state None graph in
  let offset, set_offset = Bonsai.state 0 graph in
  let scope, set_scope = Bonsai.state "me" graph in
  let summary = Remote.load ~active:visible ~name:"cashflow_summary" ~args:(let%arr scope in `Assoc [ ("scope", `String scope); ("cycles", `Int 13) ]) ~ctx graph in
  (* The cycle in progress: how far the statements reach and where it is heading. *)
  let outlook = Remote.load ~active:visible ~name:"period_outlook" ~args:(let%arr scope in `Assoc [ ("scope", `String scope) ]) ~ctx graph in
  let categories =
    Remote.load ~active:visible ~name:"spending_by_category" ~args:(let%arr scope and offset in `Assoc [ ("scope", `String scope); ("cycle_offset", `Int offset) ]) ~ctx graph
  in
  (* The accountant's briefing: the questions worth answering before they're asked. *)
  (* What is ahead: the forecast, then goals and budgets. *)
  let forecast = Forecast_page.section ~ctx ~visible graph in
  let goals = Budgets_page.section ~ctx ~visible graph in
  let briefing = Briefing_view.component ~ctx ~scope ~visible ~on_ask ~selected_finding ~outlet graph in
  let feed = Remote.load ~active:visible ~name:"get_attention_feed" ~args:(Bonsai.return (`Assoc [("narrative", `Bool false)])) ~ctx graph in
  let cards =
    let%arr feed in
    match feed with
    | Loaded json ->
        J.list (J.field "cards" json)
        |> List.filter ~f:(fun card -> not (restates_figures card))
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
  let%arr desk and forecast and goals and summary and outlook and categories and briefing and views and offset and set_offset and scope and set_scope and locale = ctx.locale and session = ctx.session
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
    (* A cycle the statements don't reach the end of has figures only so far: right after payday the
       salary is in and the bills aren't. It is never compared with whole cycles. *)
    let complete = match J.field "complete" c with `Bool b -> b | _ -> true in
    (* Compared only with earlier cycles that had income, and only when there are at least three:
       a comparison with empty months says nothing. *)
    let before = List.filteri cycles ~f:(fun i c -> i < index && i >= index - 6 && Float.(Fmt.to_float (J.field "income" c) > 0.)) in
    let average = if List.is_empty before then 0. else List.sum (module Float) before ~f:(fun c -> Fmt.to_float (J.field "savings" c)) /. Float.of_int (List.length before) in
    let trend =
      if (not complete) || List.length before < 3 || Float.(abs average < 1.) then Vdom.Node.none
      else
        let change = (savings -. average) /. Float.abs average *. 100. in
        Ui.div ~cls:("trend " ^ if Float.(change >= 0.) then "positive" else "negative")
          [ Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) (if Float.(change >= 0.) then Trending_up else Trending_down);
            Ui.span [ Ui.text (Msg.overview_vs_average ~change:(Fmt.percent ~signed:true locale change) locale) ] ]
    in
    let line label value = Ui.div ~cls:"figure-line" [ Ui.span ~cls:"figure-label" [ Ui.text label ]; Ui.span ~cls:"figure-value" [ Ui.text value ] ] in
    let note text = Ui.div ~cls:"figure-note" [ Ui.text text ] in
    let outlook =
      match outlook with
      | Loaded o when (not complete) && String.equal (J.str (J.path [ "cycle"; "cycle" ] o)) (J.str (J.field "cycle" c)) -> Some o
      | _ -> None
    in
    let main =
      match outlook with
      | Some o ->
          (* Glanceable: the number, one line under it, and three figures. The sum behind the number folds
             out (Breakdown); the forecast's range and track record sit behind the ± beside where it is heading. *)
          let amount path = Fmt.to_float (J.path path o) in
          let items = J.list (J.field "still_due" o) in
          let sum f = List.sum (module Float) items ~f:(fun i -> let a = Fmt.to_float (J.field "amount" i) in if f a then a else 0.) in
          let signed x = Fmt.signed_whole locale ~code (`String (Printf.sprintf "%.2f" x)) in
          let kr x = whole (`String (Printf.sprintf "%.2f" (Float.abs x))) in
          let bills = sum Float.is_negative and income = sum Float.is_positive in
          let safe = J.field "safe_to_spend" o in
          let payday = Fmt.date_json (J.path [ "cycle"; "last" ] o) in
          let figure ?(hint = "") ?(aside = "") label value =
            Ui.div ~cls:"figure-line outlook-row"
              [ Ui.span ~cls:"figure-label" [ Ui.text label ];
                Vdom.Node.span
                  ~attrs:(Vdom.Attr.class_ "figure-value" :: (if String.is_empty hint then [] else [ Vdom.Attr.title hint ]))
                  [ Ui.text value; (if String.is_empty aside then Vdom.Node.none else Ui.span ~cls:"figure-aside" [ Ui.text aside ]) ] ]
          in
          let workings =
            let row ?(cls = "") label x = Ui.div ~cls:("figure-line workings-row " ^ cls) [ Ui.span [ Ui.text label ]; Ui.span ~cls:"num" [ Ui.text (signed x) ] ] in
            Vdom.Node.create "details" ~attrs:[ Vdom.Attr.class_ "workings" ]
              (Vdom.Node.create "summary" [ Ui.text (Msg.overview_workings locale) ]
               :: List.filter_opt
                    [ Some (row (Msg.overview_workings_saved locale) (amount [ "so_far"; "savings" ]));
                      (if Float.(income > 0.) then Some (row (Msg.overview_workings_income locale) income) else None);
                      (if Float.(bills < 0.) then Some (row (Msg.overview_workings_bills locale) bills) else None);
                      (if Float.(amount [ "goals_set_aside" ] > 0.) then Some (row (Msg.overview_workings_goals locale) (-. amount [ "goals_set_aside" ])) else None);
                      (match J.field "covered_through" o with
                       | `String _ as d when Float.(amount [ "unseen_spending" ] > 0.) ->
                           Some (row (Msg.overview_workings_unseen ~date:(Fmt.date_json d) locale) (-. amount [ "unseen_spending" ]))
                       | _ -> None);
                      Some (row ~cls:"workings-total" (Msg.overview_workings_safe locale) (Fmt.to_float safe)) ])
          in
          let range_hint =
            String.concat ~sep:". "
              (List.filter_opt
                 [ (match J.field "expected_range" o with
                    | `Null -> None
                    | r -> Some (Msg.overview_likely ~low:(Fmt.signed_whole locale ~code (J.field "low" r)) ~high:(Fmt.signed_whole locale ~code (J.field "high" r)) locale));
                   (match J.field "track_record" o with
                    | `Null -> None
                    | t ->
                        Some
                          (Msg.overview_track ~amount:(whole (J.field "miss" t)) ~count:(Int.to_string (J.int (J.field "forecasts" t)))
                             ~date:(Fmt.date_json (J.field "since" t)) locale)) ])
          in
          [ Ui.div ~cls:"figure-label" [ Ui.text (Msg.overview_safe_title ~date:payday locale) ];
            Ui.div ~cls:("hero " ^ Fmt.sign_tone safe) [ Ui.text (whole safe) ];
            (if Float.(Fmt.to_float safe < 0.) then note (Msg.overview_safe_over ~amount:(kr (Fmt.to_float safe)) locale)
             else note (Msg.overview_safe_sub ~amount:(whole (J.field "safe_per_day" o)) ~days:(Int.to_string (J.int (J.field "days_to_payday" o))) locale));
            workings;
            Ui.div ~cls:"figure-divider" [];
            figure ~hint:range_hint
              ~aside:(match J.field "track_record" o with `Null -> "" | t -> "±" ^ whole (J.field "miss" t))
              (Msg.overview_row_heading locale) (Fmt.signed_whole locale ~code (J.field "expected" o));
            (if Float.(bills < 0.) then figure (Msg.overview_row_bills locale) (signed bills) else Vdom.Node.none);
            (if Float.(income > 0.) then figure (Msg.overview_row_income locale) (signed income) else Vdom.Node.none);
            (match J.field "covered_through" o with
             | `Null -> Ui.div ~cls:"figure-foot" [ Ui.text (Msg.overview_nothing_yet locale) ]
             | d -> Ui.div ~cls:"figure-foot" [ Ui.text (Msg.overview_through ~date:(Fmt.date_json d) locale) ]) ]
      | None ->
          [ Ui.div ~cls:"figure-label" [ Ui.text (Msg.overview_net_savings locale) ];
            Ui.div ~cls:("hero " ^ Fmt.sign_tone (J.field "savings" c)) [ Ui.text (Fmt.signed_whole locale ~code (J.field "savings" c)) ];
            trend;
            (match complete, J.field "covered_through" c with
             | false, (`String _ as d) -> note (Msg.overview_covered_through ~date:(Fmt.date_json d) locale)
             | _ -> Vdom.Node.none) ]
    in
    Ui.div ~cls:"panel figures"
      [ Ui.div ~cls:"figures-main"
          (main @ [ (if J.bool (J.field "reconciled" c) then Vdom.Node.none else Ui.div ~cls:"provisional" [ Ui.text (Msg.label_provisional locale) ]) ]);
        Ui.div ~cls:"figures-side"
          [ line (if complete then Msg.label_income locale else Msg.label_income_so_far locale) (whole (J.field "income" c));
            line (if complete then Msg.overview_expenses locale else Msg.label_spending_so_far locale) (whole (J.field "spending" c));
            (* Spending so far set beside the same days of earlier cycles; a savings rate means nothing yet. *)
            (match outlook with
             | Some o -> (
                 match J.field "usual_by_now" o with
                 | `Null -> Vdom.Node.none
                 | u -> Ui.div ~cls:"figure-aside-line" [ Ui.text (Msg.overview_usual ~amount:(whole (J.field "spending" u)) locale) ])
             | None ->
                 line (Msg.label_savings_rate locale)
                   (match J.str_opt (J.field "savings_rate" c) with
                   | Some rate when complete -> Fmt.percent locale (Option.value (Float.of_string_opt (String.chop_suffix_if_exists rate ~suffix:"%")) ~default:0.)
                   | _ -> "–")) ] ]
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
          [ Ui.p (Msg.overview_empty locale) ]
    | Loaded _, Some c ->
        Ui.div
          [ figures c; (if offset = 0 then briefing else Vdom.Node.none);
            Remote.view ~locale categories ~f:(fun json -> if mostly_uncategorized json then Vdom.Node.none else flow c json) ]
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
  (* Statements can be dropped while talking to the advisor too; what became of them shows above the conversation. *)
  Vdom.Node.main ~attrs:[Vdom.Attr.class_ ((if standalone then "page advisor-page" else "page has-desk") ^ if desk.dragging then " dropping" else ""); desk.drop]
    [(if standalone then Vdom.Node.h1 [Ui.text (Msg.nav_advisor locale)] else Vdom.Node.none);
     (* Keep the financial surface mounted for Home continuity; an unanchored
        Advisor conversation is not a second dashboard above the chat. *)
     Vdom.Node.div ~attrs:(if standalone then [Vdom.Attr.create "hidden" ""] else [])
       (let urgent = Map.exists visible_cards ~f:(fun card -> List.mem urgent_kinds (J.str (J.field "kind" card)) ~equal:String.equal) in
        (* What was handed over shows where the page starts; the box waits at its foot. *)
        (* What is ahead follows what needs doing, for the current period only. *)
        let ahead = if offset = 0 then Ui.div ~cls:"ahead" [forecast; goals] else Vdom.Node.none in
        if urgent then [head; desk.files; attention; body; ahead; desk.view] else [head; desk.files; body; attention; ahead; desk.view]);
     (if standalone then desk.files else Vdom.Node.none);
     (if standalone then outlet else Vdom.Node.none)]
