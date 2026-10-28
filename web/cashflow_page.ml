(* Cash flow: where a pay cycle's money came from and went (Flow), and how income, spending and
   what was left over moved over the last year (Trend). Flow pairs the Sankey with a breakdown
   table; Trend pairs grouped bars with the figures per month. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

(* Axis labels: "20k", "-5k", "0". *)
let short v =
  if Float.(abs v >= 1000.) then
    let k = v /. 1000. in
    (if Float.(k = Float.round k) then Int.to_string (Float.to_int k) else sprintf "%.1f" k) ^ "k"
  else Int.to_string (Float.iround_nearest_exn v)

let page ~(ctx : Ctx.t) graph =
  let scope, set_scope = Bonsai.state "me" graph in
  let offset, set_offset = Bonsai.state 0 graph in
  let trend, set_trend = Bonsai.state false graph in
  let summary = Remote.load ~name:"cashflow_summary" ~args:(let%arr scope in `Assoc [ ("scope", `String scope); ("cycles", `Int 12) ]) ~ctx graph in
  let categories =
    Remote.load ~name:"spending_by_category"
      ~args:(let%arr scope and offset in `Assoc [ ("scope", `String scope); ("cycle_offset", `Int offset); ("compare_previous", `Bool true) ])
      ~ctx graph
  in
  let%arr summary and categories and scope and set_scope and offset and set_offset and trend and set_trend and locale = ctx.locale
  and session = ctx.session and navigate = ctx.navigate in
  let code = match summary with Remote.Loaded json -> J.str ~default:"SEK" (J.field "currency" json) | _ -> "SEK" in
  let whole v = Fmt.whole locale ~code (`String (Float.to_string v)) in
  let cycles = match summary with Remote.Loaded json -> J.list (J.field "cycles" json) | _ -> [] in
  let index = List.length cycles - 1 + offset in
  let selected = List.nth cycles index and previous = List.nth cycles (index - 1) in
  let field name c = Fmt.to_float (J.field name c) in
  let month c = Fmt.month_label locale (J.str (J.field "cycle" c)) in
  let scopes =
    if Ctx.has_household session then [ ("me", Msg.label_personal locale); ("household", Msg.label_household locale); ("family", Msg.label_family locale) ] else []
  in
  let tab active label effect =
    Vdom.Node.button ~attrs:[ Vdom.Attr.class_ ("tab-button" ^ if active then " active" else ""); Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> effect) ] [ Ui.text label ]
  in
  let head =
    Ui.div ~cls:"page-head"
      [ Vdom.Node.h1 [ Ui.text (Msg.nav_cashflow locale) ];
        Ui.div ~cls:"page-controls"
          [ (if List.is_empty scopes then Vdom.Node.none
             else Ui.div ~cls:"segmented" (List.map scopes ~f:(fun (value, label) -> Ui.button ~kind:(if String.equal value scope then "active" else "") label (set_scope value))));
            (if trend then Vdom.Node.none
             else
               Ui.period
                 ~label:(match selected with Some c -> month c | None -> "")
                 ~previous:(Msg.overview_previous locale, index <= 0, set_offset (offset - 1))
                 ~next:(Msg.overview_next locale, offset >= 0, set_offset (offset + 1))) ] ]
  in
  (* Change against the previous period, coloured by whether it is good news: less spending and
     more saving are. Without a previous figure there is nothing to compare. *)
  let change ~more_is_good now before =
    match before with
    | Some b when Float.(abs b > 0.) ->
        let pct = (now -. b) /. Float.abs b *. 100. in
        let good = if more_is_good then Float.(pct >= 0.) else Float.(pct <= 0.) in
        Ui.span ~cls:("num " ^ if Float.(abs pct < 0.5) then "" else if good then "positive" else "negative") [ Ui.text (Fmt.percent ~signed:true locale pct) ]
    | _ -> Ui.span ~cls:"num muted" [ Ui.text "–" ]
  in
  let open_category id = navigate (Route.Transactions { offset; category = Some id }) in
  let flow c json =
    let income = Float.max 1. (field "income" c) in
    let left, right = Flow.nodes ~locale ~code c json in
    let rows = J.list (J.field "categories" json) |> List.filter ~f:(fun r -> Float.(Fmt.to_float (J.field "total" r) > 0.)) in
    let row ?id ~tile ~name ~amount ~share ~delta () =
      Vdom.Node.div
        ~attrs:
          ([ Vdom.Attr.class_ ("row breakdown-row" ^ if Option.is_some id then " clickable" else "") ]
          @ Option.value_map id ~default:[] ~f:(fun id -> [ Vdom.Attr.on_click (fun _ -> open_category id) ]))
        [ Ui.div ~cls:"category-cell" [ tile; Ui.span ~cls:"row-title" [ Ui.text name ] ]; Ui.span ~cls:"amount" [ Ui.text (whole amount) ];
          Ui.span ~cls:"num muted" [ Ui.text (Fmt.percent locale (share *. 100.)) ]; delta ]
    in
    let savings = field "savings" c in
    Ui.div
      [ Ui.div ~cls:"panel flow flow-large" (Flow.view ~on_select:open_category ~left ~right ());
        Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.cashflow_breakdown locale) ] ];
        Ui.div ~cls:"table"
          ((Ui.div ~cls:"table-head breakdown-row"
              [ Ui.span [ Ui.text (Msg.budgets_col_category locale) ]; Ui.span ~cls:"num" [ Ui.text (Msg.cashflow_col_amount locale) ];
                Ui.span ~cls:"num" [ Ui.text (Msg.cashflow_col_share locale) ]; Ui.span ~cls:"num" [ Ui.text (Msg.cashflow_col_change locale) ] ]
           :: List.mapi rows ~f:(fun i r ->
                  let total = Fmt.to_float (J.field "total" r) and name = Accounts_data.name ~locale (J.field "category" r) in
                  row ?id:(J.str_opt (J.path [ "category"; "id" ] r))
                    ~tile:(Ui.span ~cls:(if i < 5 then sprintf "tile c%d" (i + 1) else "tile") [ Ui.text (Ui.initial name) ])
                    ~name ~amount:total ~share:(total /. income)
                    ~delta:(change ~more_is_good:false total (if J.is_null (J.field "previous" r) then None else Some (Fmt.to_float (J.field "previous" r)))) ()))
          @
          if Float.(savings > 0.) then
            [ row ~tile:(Ui.span ~cls:"tile c-savings" [ Ui.text (Ui.initial (Msg.overview_savings locale)) ]) ~name:(Msg.overview_savings locale) ~amount:savings
                ~share:(savings /. income) ~delta:(change ~more_is_good:true savings (Option.map previous ~f:(field "savings"))) () ]
          else []);
        (if J.bool (J.field "reconciled" c) then Vdom.Node.none else Ui.div ~cls:"provisional table-note" [ Ui.text (Msg.label_provisional locale) ]) ]
  in
  let trend_view () =
    let shown = List.filter cycles ~f:(fun c -> Float.(field "income" c > 0. || field "spending" c > 0.)) in
    let short_month c = match String.split (J.str (J.field "cycle" c)) ~on:'-' with [ _; m ] -> String.prefix (Mf2_runtime.month_name locale (Int.of_string m)) 3 | _ -> "" in
    let legend cls label = Ui.span [ Ui.span ~cls:("dot " ^ cls) []; Ui.text label ] in
    let rate c = match J.str_opt (J.field "savings_rate" c) with Some r -> Fmt.percent locale (Option.value (Float.of_string_opt (String.chop_suffix_if_exists r ~suffix:"%")) ~default:0.) | None -> "–" in
    Ui.div
      [ Ui.div ~cls:"panel"
          [ Ui.div ~cls:"legend"
              [ legend "bar-income-dot" (Msg.label_income locale); legend "bar-spending-dot" (Msg.overview_expenses locale); legend "bar-net-dot" (Msg.cashflow_net locale) ];
            Chart.grouped_bars ~series:[ "income"; "spending"; "net" ] ~tick:short
              (List.map shown ~f:(fun c -> (short_month c, [ field "income" c; field "spending" c; field "savings" c ]))) ];
        Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.cashflow_details locale) ] ];
        Ui.div ~cls:"table"
          (Ui.div ~cls:"table-head month-row"
             [ Ui.span [ Ui.text (Msg.cashflow_col_month locale) ]; Ui.span ~cls:"num" [ Ui.text (Msg.label_income locale) ];
               Ui.span ~cls:"num" [ Ui.text (Msg.overview_expenses locale) ]; Ui.span ~cls:"num" [ Ui.text (Msg.cashflow_col_net locale) ];
               Ui.span ~cls:"num" [ Ui.text (Msg.label_savings_rate locale) ] ]
          :: List.rev_map shown ~f:(fun c ->
                 Ui.div ~cls:"row month-row"
                   [ Ui.span ~cls:"row-title" [ Ui.text (month c) ]; Ui.span ~cls:"amount" [ Ui.text (whole (field "income" c)) ];
                     Ui.span ~cls:"amount" [ Ui.text (whole (field "spending" c)) ];
                     Ui.span ~cls:("amount " ^ Fmt.sign_tone (J.field "savings" c)) [ Ui.text (whole (field "savings" c)) ]; Ui.span ~cls:"num" [ Ui.text (rate c) ] ])) ]
  in
  let body =
    match summary, selected with
    | (Remote.Loading | Remote.Failed _), _ -> Remote.view ~locale summary ~f:(fun _ -> Vdom.Node.none)
    | Remote.Loaded _, None -> Ui.empty (Msg.overview_empty locale)
    | Remote.Loaded _, Some c ->
        if trend then trend_view ()
        else if Float.(field "income" c = 0. && field "spending" c = 0.) then Ui.empty (Msg.overview_empty locale)
        else Remote.view ~locale categories ~f:(flow c)
  in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
    [ head; Ui.div ~cls:"tabs" [ tab (not trend) (Msg.cashflow_tab_flow locale) (set_trend false); tab trend (Msg.cashflow_tab_trend locale) (set_trend true) ]; body ]
