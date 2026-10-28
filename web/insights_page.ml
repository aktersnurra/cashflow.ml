(* Insights: what changed, each finding a card with the evidence drawn small beside it. Every
   finding is computed here from the ledger's figures (no prose generation), and appears only when
   the data supports it: comparisons need at least two earlier periods with activity, and a
   finding that compares an unreconciled period says so. Tabs filter by subject. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type subject = Spending | Income | Savings | Subscriptions [@@deriving equal]

type finding = {
  subject : subject;
  tone : string;  (** the marker colour: "blue", "green", "red" *)
  title : string;
  body : string;
  chart : Vdom.Node.t;
  link : (string * Route.t) option;
  provisional : bool;
}

let average = function [] -> 0. | xs -> List.sum (module Float) xs ~f:Fn.id /. Float.of_int (List.length xs)

(* Recurring amounts to a monthly figure, so different cadences add up. *)
let per_month cadence amount =
  match cadence with "weekly" -> amount *. 52. /. 12. | "quarterly" -> amount /. 3. | "yearly" -> amount /. 12. | _ -> amount

let page ~(ctx : Ctx.t) graph =
  let scope, set_scope = Bonsai.state "me" graph in
  let range, set_range = Bonsai.state 6 graph in
  let tab, set_tab = Bonsai.state None graph in
  let args extra = let%arr scope and range in `Assoc ([ ("scope", `String scope) ] @ extra range) in
  let summary = Remote.load ~name:"cashflow_summary" ~args:(args (fun r -> [ ("cycles", `Int r) ])) ~ctx graph in
  let categories = Remote.load ~name:"spending_by_category" ~args:(args (fun _ -> [])) ~ctx graph in
  let worth = Remote.load ~name:"net_worth_series" ~args:(args (fun r -> [ ("cycles", `Int r) ])) ~ctx graph in
  let recurring = Remote.load ~name:"recurring_payments" ~args:(args (fun _ -> [])) ~ctx graph in
  let%arr summary and categories and worth and recurring and scope and set_scope and range and set_range and tab and set_tab
  and locale = ctx.locale and session = ctx.session and navigate = ctx.navigate in
  let code = match summary with Remote.Loaded json -> J.str ~default:"SEK" (J.field "currency" json) | _ -> "SEK" in
  let whole v = Fmt.whole locale ~code (`String (Float.to_string v)) in
  let exact v = Mf2_runtime.format_money locale (Fmt.in_sentence (Fmt.money ~code (sprintf "%.2f" v))) in
  let cycles = match summary with Remote.Loaded json -> J.list (J.field "cycles" json) | _ -> [] in
  let field name c = Fmt.to_float (J.field name c) in
  let current = List.last cycles in
  let earlier = match List.rev cycles with _ :: rest -> List.rev rest | [] -> [] in
  let reconciled = match current with Some c -> J.bool (J.field "reconciled" c) | None -> true in
  let rate c = Option.bind (J.str_opt (J.field "savings_rate" c)) ~f:(fun r -> Float.of_string_opt (String.chop_suffix_if_exists r ~suffix:"%")) in
  (* Current against the earlier periods that had activity; None without enough history. *)
  let compare name =
    match current with
    | None -> None
    | Some c ->
        let prior = List.filter_map earlier ~f:(fun e -> if Float.(field name e > 0.) then Some (field name e) else None) in
        if List.length prior < 2 then None else Some (field name c, average prior, List.length prior)
  in
  let spending =
    Option.bind (compare "spending") ~f:(fun (now, avg, count) ->
        let change = if Float.(avg > 0.) then (now -. avg) /. avg *. 100. else 0. in
        (* A change of a few percent is noise, not a finding. *)
        if Float.(avg <= 0. || abs change < 3.) then None
        else
          let up = Float.(change >= 0.) in
          Some
            { subject = Spending; tone = "blue";
              title = (if up then Msg.insights_spending_up else Msg.insights_spending_down) ~change:(Fmt.percent locale (Float.abs change)) locale;
              body =
                (if up then Msg.insights_spending_more else Msg.insights_spending_less)
                  ~amount:(whole (Float.abs (now -. avg))) ~count:(Int.to_string count) locale;
              chart = Chart.mini_bars ~tone:"blue" (List.map cycles ~f:(field "spending"));
              link = Some (Msg.insights_view_budget locale, Route.Budgets); provisional = not reconciled })
  in
  let income =
    Option.bind (compare "income") ~f:(fun (now, avg, _) ->
        if Float.(avg <= 0. || abs ((now -. avg) /. avg) < 0.05) then None
        else
          Some
            { subject = Income; tone = "blue";
              title = (if Float.(now > avg) then Msg.insights_income_up else Msg.insights_income_down) locale;
              body = Msg.insights_income_body ~amount:(whole now) ~average:(whole avg) locale;
              chart = Chart.mini_bars ~tone:"blue" (List.map cycles ~f:(field "income"));
              link = Some (Msg.insights_view_accounts locale, Route.Accounts); provisional = not reconciled })
  in
  let savings_rate =
    match current with
    | None -> None
    | Some c -> (
        let prior = List.filter_map earlier ~f:(fun e -> if Float.(field "income" e > 0.) then rate e else None) in
        match rate c with
        | Some now when List.length prior >= 2 ->
            let avg = average prior in
            Some
              { subject = Savings; tone = (if Float.(now >= avg) then "green" else "red");
                title = (if Float.(now >= avg) then Msg.insights_rate_up else Msg.insights_rate_down) locale;
                body = Msg.insights_rate_body ~rate:(Fmt.percent locale now) ~average:(Fmt.percent locale avg) locale;
                chart = Chart.mini_line ~tone:(if Float.(now >= avg) then "green" else "red") (List.filter_map cycles ~f:rate);
                link = None; provisional = not reconciled }
        | _ -> None)
  in
  let net_worth =
    match worth with
    | Remote.Loaded json -> (
        let values = List.map (J.list (J.field "points" json)) ~f:(fun p -> Fmt.to_float (J.field "net_worth" p)) in
        match values, List.last values with
        | first :: _ :: _, Some last ->
            let grew = Float.(last >= first) in
            Some
              { subject = Savings; tone = (if grew then "green" else "red");
                title = (if grew then Msg.insights_worth_up else Msg.insights_worth_down) locale;
                body = Msg.insights_worth_body ~amount:(whole last) ~change:((if grew then "+" else "") ^ whole (last -. first)) locale;
                chart = Chart.mini_line ~tone:(if grew then "green" else "red") values;
                link = Some (Msg.insights_view_accounts locale, Route.Accounts); provisional = false }
        | _ -> None)
    | _ -> None
  in
  let payments = match recurring with Remote.Loaded json -> J.list (J.field "payments" json) | _ -> [] in
  let price_changes =
    List.filter_map payments ~f:(fun p ->
        if not (J.bool (J.field "price_changed" p)) then None
        else
          let before = Float.abs (field "typical_amount" p) and now = Float.abs (field "last_amount" p) in
          let merchant = J.str (J.field "merchant" p) in
          Some
            { subject = Subscriptions; tone = (if Float.(now > before) then "red" else "green");
              title = Msg.insights_price_title ~merchant locale;
              body = Msg.insights_price_body ~merchant ~previous:(exact before) ~last:(exact now) locale;
              chart = Chart.mini_line ~step:true ~tone:(if Float.(now > before) then "red" else "green") [ before; before; before; now; now ];
              link = None; provisional = false })
  in
  let recurring_total =
    match payments with
    | [] -> None
    | ps ->
        let monthly = List.sum (module Float) ps ~f:(fun p -> per_month (J.str (J.field "cadence" p)) (Float.abs (field "last_amount" p))) in
        let initials name = Ui.initial (String.strip name) in
        Some
          { subject = Subscriptions; tone = "blue"; title = Msg.insights_recurring_title locale;
            body = Msg.insights_recurring_body ~count:(Int.to_string (List.length ps)) ~amount:(whole monthly) locale;
            chart =
              Ui.div ~cls:"merchants"
                (List.map (List.take ps 6) ~f:(fun p ->
                     let name = J.str (J.field "merchant" p) in
                     Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "merchant"; Vdom.Attr.title name ] [ Ui.text (initials name) ]));
            link = None; provisional = false }
  in
  let top_category =
    match categories with
    | Remote.Loaded json -> (
        let rows = List.filter_map (J.list (J.field "categories" json)) ~f:(fun r -> let v = Fmt.to_float (J.field "total" r) in if Float.(v > 0.) then Some (Accounts_data.name ~locale (J.field "category" r), v) else None) in
        let total = List.sum (module Float) rows ~f:snd in
        match rows with
        | (name, v) :: _ when Float.(total > 0.) ->
            let top = List.take rows 4 and other = List.sum (module Float) (List.drop rows 4) ~f:snd in
            let slices = List.mapi top ~f:(fun i (n, v) -> (n, v, sprintf "c%d" (i + 1))) @ if Float.(other > 0.) then [ (Msg.overview_other locale, other, "c-other") ] else [] in
            let share = Fmt.percent locale (v /. total *. 100.) in
            Some
              { subject = Spending; tone = "blue"; title = Msg.insights_top_title locale; body = Msg.insights_top_body ~category:name ~share locale;
                chart =
                  Ui.div ~cls:"donut-row"
                    [ Chart.donut ~centre:share (List.map slices ~f:(fun (_, v, cls) -> (v, cls)));
                      Ui.div ~cls:"donut-legend"
                        (List.map slices ~f:(fun (n, v, cls) ->
                             Ui.div ~cls:"legend-row" [ Ui.span ~cls:("swatch " ^ cls) []; Ui.span ~cls:"legend-name" [ Ui.text n ]; Ui.span ~cls:"legend-share" [ Ui.text (Fmt.percent locale (v /. total *. 100.)) ] ])) ];
                link = Some (Msg.insights_view_budget locale, Route.Budgets); provisional = not reconciled }
        | _ -> None)
    | _ -> None
  in
  let findings = List.filter_opt [ spending; savings_rate; top_category; income; net_worth; recurring_total ] @ price_changes in
  let tabs = [ (None, Msg.insights_tab_all locale); (Some Spending, Msg.insights_tab_spending locale); (Some Income, Msg.insights_tab_income locale);
               (Some Savings, Msg.insights_tab_savings locale); (Some Subscriptions, Msg.insights_tab_subscriptions locale) ] in
  let shown = List.filter findings ~f:(fun f -> match tab with None -> true | Some s -> equal_subject s f.subject) in
  let card f =
    Ui.div ~cls:"insight"
      [ Ui.div ~cls:"insight-head" [ Ui.span ~cls:("insight-mark " ^ f.tone) []; Vdom.Node.h3 [ Ui.text f.title ] ];
        Ui.p f.body;
        Ui.div ~cls:"insight-chart" [ f.chart ];
        (if f.provisional then Ui.div ~cls:"provisional" [ Ui.text (Msg.label_provisional locale) ] else Vdom.Node.none);
        (match f.link with
        | Some (label, route) ->
            Vdom.Node.a
              ~attrs:[ Vdom.Attr.class_ "insight-link"; Vdom.Attr.href (Route.to_path route); Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate route ]) ]
              [ Ui.text label; Feather_icon.svg ~size:(`Px 14) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Arrow_right ]
        | None -> Vdom.Node.none) ]
  in
  let scopes =
    if Ctx.has_household session then [ ("me", Msg.label_personal locale); ("household", Msg.label_household locale); ("family", Msg.label_family locale) ] else []
  in
  let loading = match summary with Remote.Loading -> true | _ -> false in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
    [ Ui.div ~cls:"page-head"
        [ Ui.div [ Vdom.Node.h1 [ Ui.text (Msg.insights_title locale) ]; Ui.div ~cls:"subtitle" [ Ui.text (Msg.insights_subtitle locale) ] ];
          Ui.div ~cls:"page-controls"
            [ (if List.is_empty scopes then Vdom.Node.none
               else Ui.div ~cls:"segmented" (List.map scopes ~f:(fun (value, label) -> Ui.button ~kind:(if String.equal value scope then "active" else "") label (set_scope value))));
              Ui.div ~cls:"range"
                [ Ui.select ~value:(Int.to_string range)
                    ~options:(List.map [ 3; 6; 12 ] ~f:(fun n -> (Int.to_string n, Msg.insights_range ~count:(Int.to_string n) locale)))
                    ~on_change:(fun v -> set_range (Int.of_string v)) ] ] ];
      Ui.div ~cls:"tabs"
        (List.map tabs ~f:(fun (subject, label) ->
             Vdom.Node.button
               ~attrs:
                 [ Vdom.Attr.class_ ("tab-button" ^ if Option.equal equal_subject subject tab then " active" else ""); Vdom.Attr.type_ "button";
                   Vdom.Attr.on_click (fun _ -> set_tab subject) ]
               [ Ui.text label ]));
      (match summary with
      | Remote.Failed _ -> Remote.view ~locale summary ~f:(fun _ -> Vdom.Node.none)
      | _ when loading -> Remote.view ~locale summary ~f:(fun _ -> Vdom.Node.none)
      | _ -> if List.is_empty shown then Ui.empty (Msg.insights_empty locale) else Ui.div ~cls:"insight-grid" (List.map shown ~f:card)) ]
