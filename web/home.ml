(* The landing page: the attention feed, most important card first. Each card is its own
   component (Bonsai.assoc), so an import in progress on one card keeps its state while the feed
   reloads around it. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let act ~key ~action = Api.run "act_on_card" (`Assoc [ ("key", `String key); ("action", `String action) ])

let card_view ~(ctx : Ctx.t) (key : string Bonsai.t) (card : J.t Bonsai.t) graph =
  let account_id = let%arr card in J.str (J.path [ "data"; "account"; "id" ] card) in
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
        ~title:(Msg.card_mismatch_title ~account:(J.str (J.field "name" account)) ~difference:(m (J.field "difference" data)) locale)
        ~body:[ Ui.muted (Msg.card_mismatch_body ~statement:(m (J.field "statement_balance" data)) ~date:(Fmt.date_json (J.field "date" data)) ~ledger:(m (J.field "ledger_balance" data)) locale) ]
        ~actions:[ Ui.button ~kind:"primary" (Msg.action_adjust locale) adjust; later ] ()
  | "import_due" ->
      let days = J.field "days" data in
      Ui.card ~title:(Msg.card_import_title ~account:(J.str (J.path [ "account"; "name" ] data)) locale)
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
      let category = J.str (J.path [ "category"; "name" ] data) in
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
      Ui.card ~tone:"warning" ~title:(Msg.card_shortfall_title ~account:(J.str (J.path [ "account"; "name" ] data)) locale)
        ~body:
          (Ui.muted (Msg.card_shortfall_body ~due:(m (J.field "due" data)) ~available:(m (J.field "available" data)) ~short:(m (J.field "short_by" data)) locale)
          :: List.map (J.list (J.field "upcoming" data)) ~f:(fun u ->
                 Ui.muted (Msg.card_shortfall_item ~merchant:(J.str (J.field "merchant" u)) ~amount:(m (J.field "amount" u)) ~date:(Fmt.date_json (J.field "expected" u)) locale)))
        ~actions:[ Ui.button ~kind:"primary" (Msg.action_move_money locale) (navigate Route.Add); done_; later ] ()
  | "unusual_spend" ->
      let category = J.str (J.path [ "category"; "name" ] data) in
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
      Ui.card ~title:(Msg.card_cash_title ~account:(J.str (J.path [ "account"; "name" ] data)) locale)
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

let page ~(ctx : Ctx.t) graph =
  let feed = Remote.load ~name:"get_attention_feed" ~ctx graph in
  let cards =
    let%arr feed in
    match feed with
    | Loaded json ->
        String.Map.of_alist_reduce ~f:(fun a _ -> a)
          (List.mapi (J.list (J.field "cards" json)) ~f:(fun i card -> (Printf.sprintf "%03d:%s" i (J.str (J.field "key" card)), card)))
    | Loading | Failed _ -> String.Map.empty
  in
  let views = Bonsai.assoc (module String) cards ~f:(fun key card graph -> card_view ~ctx key card graph) graph in
  let%arr feed and views and locale = ctx.locale in
  Ui.page ~title:(Msg.nav_home locale) [ Remote.view ~locale feed ~f:(fun _ -> Ui.div ~cls:"feed" (Map.data views)) ]
