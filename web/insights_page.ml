open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let page ~(ctx : Ctx.t) graph =
  let scope, set_scope = Bonsai.state "me" graph in
  let args extra = let%arr scope in `Assoc (("scope", `String scope) :: extra) in
  let summary = Remote.load ~name:"cashflow_summary" ~args:(args [ ("cycles", `Int 6) ]) ~ctx graph in
  let categories = Remote.load ~name:"spending_by_category" ~args:(args [ ("compare_previous", `Bool true) ]) ~ctx graph in
  let worth = Remote.load ~name:"net_worth_series" ~args:(args [ ("cycles", `Int 12) ]) ~ctx graph in
  let recurring = Remote.load ~name:"recurring_payments" ~args:(args []) ~ctx graph in
  let largest = Remote.load ~name:"largest_transactions" ~args:(args [ ("limit", `Int 8) ]) ~ctx graph in
  let runway = Remote.load ~name:"runway" ~args:(args []) ~ctx graph in
  let%arr summary and categories and worth and recurring and largest and runway and scope and set_scope and locale = ctx.locale and session = ctx.session in
  let code json = J.str ~default:"SEK" (J.field "currency" json) in
  let scopes =
    [ ("me", Msg.label_personal locale) ] @ if Ctx.has_household session then [ ("household", Msg.label_household locale); ("family", Msg.label_family locale) ] else []
  in
  let short_month cycle = match String.split cycle ~on:'-' with [ _; m ] -> Mf2_runtime.month_name locale (Int.of_string m) | _ -> cycle in
  Ui.page ~title:(Msg.insights_title locale)
    [ (if List.length scopes < 2 then Vdom.Node.none else Ui.div ~cls:"segmented" (List.map scopes ~f:(fun (value, label) -> Ui.button ~kind:(if String.equal value scope then "active" else "") label (set_scope value))));
      Remote.view ~locale summary ~f:(fun json ->
          let cycles = J.list (J.field "cycles" json) in
          let c = code json in
          let current = List.last cycles in
          Ui.div
            [ (match current with
              | Some cur ->
                  Ui.div ~cls:"kpis"
                    [ Ui.kpi (Msg.label_income locale) (Fmt.format_money locale ~code:c (J.field "income" cur));
                      Ui.kpi (Msg.label_spending locale) (Fmt.format_money locale ~code:c (J.field "spending" cur));
                      Ui.kpi ~tone:"positive" (Msg.label_savings locale) (Fmt.format_money locale ~code:c (J.field "savings" cur));
                      Ui.kpi (Msg.label_savings_rate locale) (J.str ~default:"–" (J.field "savings_rate" cur)) ]
              | None -> Vdom.Node.none);
              Ui.card ~title:(Msg.label_income locale ^ " / " ^ Msg.label_spending locale)
                ~body:
                  [ Ui.div ~cls:"legend"
                      [ Ui.span [ Ui.span ~cls:"dot bar-income-dot" []; Ui.text (Msg.label_income locale) ]; Ui.span [ Ui.span ~cls:"dot bar-spending-dot" []; Ui.text (Msg.label_spending locale) ] ];
                    Chart.paired_bars
                      (List.map cycles ~f:(fun cy -> (short_month (J.str (J.field "cycle" cy)), Fmt.to_float (J.field "income" cy), Fmt.to_float (J.field "spending" cy)))) ]
                () ]);
      Remote.view ~locale categories ~f:(fun json ->
          let c = code json in
          let rows = J.list (J.field "categories" json) in
          let top = List.fold rows ~init:1. ~f:(fun m r -> Float.max m (Fmt.to_float (J.field "total" r))) in
          Ui.card ~title:(Msg.label_categories locale)
            ~body:
              [ Ui.div ~cls:"list"
                  (List.map rows ~f:(fun r ->
                       Ui.div ~cls:"row"
                         [ Ui.div
                             [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.path [ "category"; "name" ] r)) ]; Ui.meter (Fmt.to_float (J.field "total" r) /. top);
                               (if J.is_null (J.field "previous" r) then Vdom.Node.none
                                else Ui.div ~cls:"row-sub" [ Ui.text (Msg.label_compared ~previous:(Fmt.money_json ~code:c (J.field "previous" r)) locale) ]) ];
                           Ui.div ~cls:"amount" [ Ui.text (Fmt.format_money locale ~code:c (J.field "total" r)) ] ])) ]
            ());
      Remote.view ~locale worth ~f:(fun json ->
          let points = J.list (J.field "points" json) in
          Ui.card ~title:(Msg.label_net_worth locale)
            ~body:
              [ (match List.last points with Some p -> Ui.div ~cls:"kpi-value" [ Ui.text (Fmt.format_money locale ~code:(code json) (J.field "net_worth" p)) ] | None -> Vdom.Node.none);
                Chart.line (List.map points ~f:(fun p -> Fmt.to_float (J.field "net_worth" p))) ]
            ());
      Remote.view ~locale runway ~f:(fun json ->
          match J.field "months" json with
          | `Float months -> Ui.div ~cls:"kpis" [ Ui.kpi (Msg.label_runway locale) (Msg.label_runway_months ~months locale) ]
          | _ -> Vdom.Node.none);
      Remote.view ~locale recurring ~f:(fun json ->
          let c = code json in
          match J.list (J.field "payments" json) with
          | [] -> Vdom.Node.none
          | payments ->
              Ui.card ~title:(Msg.label_recurring locale)
                ~body:
                  [ Ui.div ~cls:"list"
                      (List.map payments ~f:(fun p ->
                           Ui.div ~cls:"row"
                             [ Ui.div
                                 [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "merchant" p)) ];
                                   Ui.div ~cls:"row-sub" [ Ui.text (J.str (J.field "cadence" p) ^ " · " ^ Fmt.format_date locale (J.field "next_expected" p)) ] ];
                               Ui.div ~cls:("amount" ^ if J.bool (J.field "price_changed" p) then " warning" else "") [ Ui.text (Fmt.format_money locale ~code:c (J.field "last_amount" p)) ] ])) ]
                ());
      Remote.view ~locale largest ~f:(fun json ->
          match J.list (J.field "transactions" json) with
          | [] -> Vdom.Node.none
          | ts ->
              Ui.card ~title:(Msg.label_largest locale)
                ~body:
                  [ Ui.div ~cls:"list"
                      (List.map ts ~f:(fun t ->
                           Ui.div ~cls:"row"
                             [ Ui.div [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "description" t)) ]; Ui.div ~cls:"row-sub" [ Ui.text (Fmt.format_date locale (J.field "date" t)) ] ];
                               Ui.div ~cls:"amount" [ Ui.text (Fmt.format_money locale ~code:(J.str ~default:"SEK" (J.field "currency" t)) (J.field "amount" t)) ] ])) ]
                ()) ]
