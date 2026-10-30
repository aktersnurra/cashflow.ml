(* Forecast: cash over the coming months as one large chart, the four numbers that matter, any
   account that would go below zero, the payments behind the line, a few what-ifs beside the
   baseline, and what it all assumes. The projection is the server's; this page shows it and
   sends the what-if. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module D = Forecast_data

type one_off = { date : string; amount : string; description : string } [@@deriving equal]
type what_if = { leave_out : string list; one_offs : one_off list; follow_budgets : bool } [@@deriving equal]

let no_what_if = { leave_out = []; one_offs = []; follow_budgets = false }

let args ~scope ~months what_if =
  `Assoc
    ([ ("scope", `String scope); ("cycles", `Int months) ]
    @
    if equal_what_if what_if no_what_if then []
    else
      [ ( "scenario",
          `Assoc
            [ ("leave_out", `List (List.map what_if.leave_out ~f:(fun id -> `String id)));
              ( "one_off",
                `List
                  (List.map what_if.one_offs ~f:(fun o -> `Assoc [ ("date", `String o.date); ("amount", `String o.amount); ("description", `String o.description) ])) );
              ("follow_budgets", `Bool what_if.follow_budgets) ] ) ])

(* A typed amount as the server's decimal: "12 000,50" is "12000.50"; signed by direction. *)
let amount_of_text ~out value =
  let text = String.filter (String.strip value) ~f:(fun c -> not (Char.is_whitespace c)) |> String.tr ~target:',' ~replacement:'.' in
  match Float.of_string_opt text with
  | Some v when Float.(v > 0.) -> Some ((if out then "-" else "") ^ text)
  | _ -> None

(* A section of the Overview, under the briefing: what is ahead. *)
let section ~(ctx : Ctx.t) ~(visible : bool Bonsai.t) graph =
  (* Open or closed is the page's to keep, so a re-render never closes what the reader opened. *)
  let opened, set_opened = Bonsai.state false graph in
  let scope, set_scope = Bonsai.state "me" graph in
  let months, set_months = Bonsai.state 3 graph in
  let what_if, set_what_if = Bonsai.state ~equal:equal_what_if no_what_if graph in
  (* The one-off being typed: one state per field, so a quick edit to one never writes back a
     stale copy of another. *)
  let on, set_on = Bonsai.state "" graph in
  let text, set_text = Bonsai.state "" graph in
  let value, set_value = Bonsai.state "" graph in
  let out, set_out = Bonsai.state true graph in
  let hover, set_hover = Bonsai.state None graph in
  let expanded, set_expanded = Bonsai.state None graph in
  let forecast = Remote.load ~active:visible ~name:"forecast" ~args:(let%arr scope and months and what_if in args ~scope ~months what_if) ~ctx graph in
  let%arr opened and set_opened and forecast and scope and set_scope and months and set_months and what_if and set_what_if and on and set_on and text and set_text and value and set_value and out and set_out and hover and set_hover
  and expanded and set_expanded and locale = ctx.locale and session = ctx.session and navigate = ctx.navigate in
  let scopes = if Ctx.has_household session then [ ("me", Msg.label_personal locale); ("household", Msg.label_household locale) ] else [] in
  let segmented options ~active ~set = Ui.div ~cls:"segmented" (List.map options ~f:(fun (value, label) -> Ui.button ~kind:(if active value then "active" else "") label (set value))) in
  let controls =
    Ui.div ~cls:"plan-controls"
      [ Ui.div ~cls:"page-controls"
          [ (if List.is_empty scopes then Vdom.Node.none else segmented scopes ~active:(String.equal scope) ~set:set_scope);
            segmented (List.map [ 1; 3; 6 ] ~f:(fun n -> (n, Msg.forecast_months ~count:n locale))) ~active:(Int.equal months) ~set:set_months ] ]
  in
  let view (f : D.t) =
    let code = f.currency in
    let money json = Fmt.format_money locale ~code json and whole json = Fmt.whole locale ~code json in
    let whole_float v = whole (`String (Float.to_string v)) in
    let date d = Fmt.date d and nice_date d = Mf2_runtime.format_date locale (Fmt.date d) in
    let timeline = f.history @ f.baseline in
    let n = List.length timeline and today_index = List.length f.history in
    let index_of d = List.findi timeline ~f:(fun _ (p : D.point) -> String.equal p.date d) |> Option.map ~f:fst in
    let cash_at i = D.value (List.nth_exn timeline i).cash in
    let last_point = List.last_exn f.baseline in
    let salary = List.find f.items ~f:(fun i -> String.equal i.kind "income") in
    (* Key numbers, under the chart they sum up: the lowest point, what is left the day before the
       salary, what is due before it (or the chance of staying above zero), and where the line
       ends. *)
    let kpi ?(tone = "") label value sub =
      Ui.div ~cls:("kpi " ^ tone) [ Ui.div ~cls:"kpi-label" [ Ui.text label ]; Ui.div ~cls:"kpi-value" [ Ui.text value ]; Ui.div ~cls:"kpi-sub" [ Ui.text sub ] ]
    in
    (* Left out when the lowest point is that same day: one number, not the same one twice. *)
    let before_salary =
      match salary with
      | Some s ->
          let before = List.filter f.baseline ~f:(fun p -> String.( < ) p.date s.date) in
          let p = Option.value (List.last before) ~default:(List.hd_exn f.baseline) in
          if String.equal p.date f.lowest_date then Vdom.Node.none
          else kpi (Msg.forecast_before_salary locale) (whole p.cash) (Msg.forecast_on ~date:(date p.date) locale)
      | None -> kpi (Msg.forecast_before_salary locale) "–" (Msg.forecast_no_salary locale)
    in
    let due =
      let until = match salary with Some s -> s.date | None -> Option.value_map (List.nth f.baseline 30) ~default:last_point.date ~f:(fun p -> p.date) in
      List.filter f.items ~f:(fun i -> String.( < ) i.date until && Float.(D.value i.amount < 0.) && not (String.equal i.kind "one_off"))
    in
    let due_total = List.sum (module Float) due ~f:(fun i -> D.value i.amount) in
    let kpis =
      Ui.div ~cls:"kpis forecast-kpis"
        [ kpi ~tone:(if Float.(D.value f.lowest < 0.) then "negative" else "") (Msg.forecast_lowest locale) (whole f.lowest) (Msg.forecast_on ~date:(date f.lowest_date) locale);
          before_salary;
          (match f.chance_positive with
          | Some p ->
              let forecasts = List.find_map f.assumptions ~f:(fun a -> if String.equal (J.str (J.field "kind" a)) "range_from" then Some (J.int (J.field "forecasts" a)) else None) in
              kpi
                ~tone:(if p < 80 then "negative" else "")
                (Msg.forecast_chance locale) (Fmt.percent locale (Float.of_int p))
                (Option.value_map forecasts ~default:"" ~f:(fun count -> Msg.forecast_based_on ~count locale))
          | None ->
              kpi
                (if Option.is_some salary then Msg.forecast_due_before_salary locale else Msg.forecast_due_next_month locale)
                (whole_float (Float.abs due_total)) (Msg.forecast_payments ~count:(List.length due) locale));
          kpi (Msg.forecast_end ~date:(date f.horizon_end) locale) (whole last_point.cash) "" ]
    in
    (* The chart, then one line saying what the hovered day holds or what the line is, the legend,
       and the key numbers. *)
    let solid =
      match f.imported_through with
      | None -> today_index
      | Some d -> if String.( >= ) d f.today then today_index else Option.value (index_of d) ~default:0
    in
    let short_month d = match String.split d ~on:'-' with [ _; m; _ ] -> String.prefix (Mf2_runtime.month_name locale (Int.of_string m)) 3 | _ -> "" in
    let months_axis = List.filter_mapi timeline ~f:(fun i (p : D.point) -> if String.is_suffix p.date ~suffix:"-01" then Some (i, short_month p.date) else None) in
    let paydays = List.filter_map f.items ~f:(fun i -> if String.equal i.kind "income" then index_of i.date else None) |> List.dedup_and_sort ~compare:Int.compare in
    let overdraws = List.filter_map f.accounts ~f:(fun a -> Option.bind a.overdraw ~f:index_of) in
    let scenario = Option.map f.scenario ~f:(fun s -> List.map f.history ~f:(fun p -> D.value p.cash) @ List.map s ~f:(fun p -> D.value p.cash)) in
    let item_name (i : D.item) = if String.equal i.id "contribution" then Msg.forecast_household_transfer locale else i.name in
    let readout =
      match hover with
      | Some i when i < n ->
          let p = List.nth_exn timeline i in
          let on_day = List.filter f.items ~f:(fun it -> String.equal it.date p.date) in
          let spread =
            match List.find f.range ~f:(fun (d, _, _) -> String.equal d p.date) with
            | Some (_, low, high) -> Ui.span ~cls:"muted num" [ Ui.text (sprintf "(%s – %s)" (whole low) (whole high)) ]
            | None -> Vdom.Node.none
          in
          Ui.div ~cls:"projection-readout"
            [ Ui.span ~cls:"readout-date" [ Ui.text (nice_date p.date) ]; Ui.span ~cls:"amount" [ Ui.text (whole p.cash) ]; spread;
              Ui.span ~cls:"muted" [ Ui.text (String.concat ~sep:", " (List.map on_day ~f:(fun it -> item_name it ^ " " ^ whole it.amount))) ] ]
      | _ -> Ui.div ~cls:"projection-readout muted" [ Ui.text (if List.is_empty f.range then Msg.forecast_note locale else Msg.forecast_range_note locale) ]
    in
    let chart =
      Ui.div ~cls:"panel"
        (List.map [ `Wide; `Narrow ] ~f:(fun shape ->
             Chart.projection ~shape ~cash:(List.init n ~f:cash_at)
               ~earmarked:(List.map timeline ~f:(fun p -> D.value p.earmarked)) ~scenario
               ~range:(List.map f.range ~f:(fun (_, low, high) -> (D.value low, D.value high))) ~solid ~today:today_index ~overdraws ~months:months_axis ~paydays
               ~tick:Chart.short ~today_label:(Msg.forecast_today locale) ~what_if_label:(Msg.forecast_what_if locale) ~hover ~on_hover:set_hover)
        @ [
          readout;
          Ui.div ~cls:"legend"
            ([ Ui.span [ Ui.span ~cls:"dot projected-dot" []; Ui.text (Msg.forecast_legend_projected locale) ] ]
            @ (if List.is_empty f.range then [] else [ Ui.span [ Ui.span ~cls:"dot range-dot" []; Ui.text (Msg.forecast_legend_range locale) ] ])
            @
            if List.exists f.baseline ~f:(fun p -> Float.(D.value p.earmarked > 0.)) then
              [ Ui.span [ Ui.span ~cls:"dot band-dot" []; Ui.text (Msg.forecast_set_aside locale) ] ]
            else []);
          kpis ])
    in
    (* Accounts that go below zero, with the payments that take them there. *)
    let overdraw =
      List.filter_map f.accounts ~f:(fun a ->
          Option.map a.overdraw ~f:(fun d ->
              let causes =
                List.filter f.items ~f:(fun i -> String.equal i.account_id a.account_id && String.( <= ) i.date d && Float.(D.value i.amount < 0.))
                |> List.rev |> Fn.flip List.take 3 |> List.rev_map ~f:item_name
              in
              Ui.card ~tone:"warning"
                ~title:(Msg.forecast_overdraw ~account:a.account_name ~date:(date d) locale)
                ~body:
                  [ Ui.muted
                      (if List.is_empty causes then Msg.forecast_overdraw_spending ~amount:(Fmt.money_json ~code a.lowest) ~date:(date a.lowest_date) locale
                       else Msg.forecast_overdraw_body ~amount:(Fmt.money_json ~code a.lowest) ~date:(date a.lowest_date) ~items:(String.concat ~sep:", " causes) locale) ]
                ~actions:[ Ui.button (Msg.action_review locale) (navigate (Route.Account a.account_id)) ]
                ()))
    in
    (* Upcoming payments by day; a row opens to say where it comes from. *)
    let left_out id = List.mem what_if.leave_out id ~equal:String.equal in
    let toggle id =
      set_what_if
        { what_if with leave_out = (if left_out id then List.filter what_if.leave_out ~f:(Fn.non (String.equal id)) else id :: what_if.leave_out) }
    in
    let cadence = function
      | "weekly" -> Msg.forecast_cadence_weekly locale
      | "quarterly" -> Msg.forecast_cadence_quarterly locale
      | "yearly" -> Msg.forecast_cadence_yearly locale
      | _ -> Msg.forecast_cadence_monthly locale
    in
    let row (i : D.item) =
      let key = i.id ^ "@" ^ i.date in
      let open_ = Option.equal String.equal expanded (Some key) in
      let past = match i.category_id with Some c when String.equal i.kind "bill" -> Route.Transactions { offset = 0; category = Some c } | _ -> Route.Account i.account_id in
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.class_ ("row forecast-row clickable" ^ (if left_out i.id then " left-out" else "") ^ if open_ then " selected" else "");
            Vdom.Attr.on_click (fun _ -> set_expanded (if open_ then None else Some key)) ]
        [ Ui.span ~cls:"muted txn-date" [ Ui.text (nice_date i.date) ];
          Ui.div ~cls:"category-cell"
            [ Ui.span ~cls:"tile" [ Ui.text (Ui.initial (item_name i)) ];
              Ui.div
                ([ Ui.div ~cls:"row-title" [ Ui.text (item_name i) ] ]
                @
                match i.basis, open_ with
                | Some b, true ->
                    [ Ui.div ~cls:"row-sub"
                        [ Ui.text
                            (Msg.forecast_basis ~cadence:(cadence b.cadence) ~since:(date b.since) ~typical:(Fmt.money_json ~code b.typical)
                               ~last:(Fmt.money_json ~code b.last) locale);
                          Ui.text " ";
                          Vdom.Node.a
                            ~attrs:
                              [ Vdom.Attr.class_ "insight-link"; Vdom.Attr.href (Route.to_path past);
                                Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; Effect.Stop_propagation; navigate past ]) ]
                            [ Ui.text (Msg.forecast_past_payments locale) ] ] ]
                | _ -> []) ];
          Ui.span ~cls:"muted txn-account" [ Ui.text i.account_name ];
          Ui.span ~cls:("amount" ^ if Float.(D.value i.amount > 0.) then " positive" else "") [ Ui.text (money i.amount) ];
          (match i.basis with
          | Some _ ->
              Vdom.Node.button
                ~attrs:[ Vdom.Attr.class_ "button ghost"; Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Stop_propagation; toggle i.id ]) ]
                [ Ui.text (if left_out i.id then Msg.forecast_include locale else Msg.forecast_leave_out locale) ]
          | None -> Ui.span []) ]
    in
    let upcoming =
      let series_items = List.filter f.items ~f:(fun i -> not (String.equal i.kind "one_off")) in
      Ui.div
        [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.forecast_upcoming locale) ] ];
          (if List.for_all series_items ~f:(fun i -> Option.is_none i.basis) then Ui.div ~cls:"panel empty-panel" [ Ui.p (Msg.forecast_no_series locale) ]
           else
             Ui.div ~cls:"table" (List.map series_items ~f:row)) ]
    in
    (* What if: a one-off, keeping to the budgets, and what is left out, as removable chips. *)
    let draft_amount = amount_of_text ~out value in
    let draft_ok = Option.is_some draft_amount && String.( >= ) on f.today && String.( <= ) on f.horizon_end in
    let add =
      match draft_amount with
      | Some amount when draft_ok ->
          Effect.Many
            [ set_what_if { what_if with one_offs = what_if.one_offs @ [ { date = on; amount; description = String.strip text } ] }; set_on ""; set_text "";
              set_value "" ]
      | _ -> Effect.Ignore
    in
    let chip label effect =
      Vdom.Node.button ~attrs:[ Vdom.Attr.class_ "chip active"; Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> effect) ] [ Ui.text (label ^ " ×") ]
    in
    let name_of id = Option.value_map (List.find f.items ~f:(fun i -> String.equal i.id id)) ~default:id ~f:item_name in
    let chips =
      List.map what_if.one_offs ~f:(fun o ->
          chip
            (String.concat ~sep:" " [ (if String.is_empty o.description then nice_date o.date else o.description); money (`String o.amount); nice_date o.date ])
            (set_what_if { what_if with one_offs = List.filter what_if.one_offs ~f:(Fn.non (equal_one_off o)) }))
      @ List.map what_if.leave_out ~f:(fun id -> chip (Msg.forecast_leave_out locale ^ ": " ^ name_of id) (toggle id))
    in
    let difference =
      match f.scenario with
      | Some s when not (List.is_empty s) ->
          let low points = List.fold points ~init:Float.infinity ~f:(fun m (p : D.point) -> Float.min m (D.value p.cash)) in
          let signed v = Fmt.signed_whole locale ~code (`String (Float.to_string v)) in
          Ui.muted
            (Msg.forecast_difference ~lowest:(signed (low s -. low f.baseline))
               ~end_:(signed (D.value (List.last_exn s).cash -. D.value last_point.cash)) locale)
      | _ -> Vdom.Node.none
    in
    let what_if_panel =
      Ui.div
        [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.forecast_what_if locale) ] ];
          Ui.div ~cls:"panel what-if"
            [ Ui.div ~cls:"what-if-form"
                [ Ui.field (Msg.label_date locale)
                    (Vdom.Node.input
                       ~attrs:
                         [ Vdom.Attr.type_ "date"; Vdom.Attr.value_prop on; Vdom.Attr.create "min" f.today; Vdom.Attr.create "max" f.horizon_end;
                           Vdom.Attr.on_input (fun _ v -> set_on v) ]
                       ());
                  Ui.field (Msg.label_description locale) (Ui.input ~value:text ~on_input:set_text ());
                  Ui.field (Msg.label_amount locale) (Ui.input ~type_:"text" ~placeholder:"12 000" ~value ~on_input:set_value ());
                  Ui.div ~cls:"what-if-actions"
                    [ segmented
                        [ (true, Msg.forecast_money_out locale); (false, Msg.forecast_money_in locale) ]
                        ~active:(Bool.equal out) ~set:set_out;
                      Ui.button ~kind:"primary" ~disabled:(not draft_ok) (Msg.forecast_add_one_off locale) add ] ];
              Ui.div ~cls:"chips"
                (Vdom.Node.button
                   ~attrs:
                     [ Vdom.Attr.class_ ("chip" ^ if what_if.follow_budgets then " active" else ""); Vdom.Attr.type_ "button";
                       Vdom.Attr.on_click (fun _ -> set_what_if { what_if with follow_budgets = not what_if.follow_budgets }) ]
                   [ Ui.text (Msg.forecast_follow_budgets locale) ]
                 :: chips
                @
                if equal_what_if what_if no_what_if then [] else [ Ui.button ~kind:"ghost" (Msg.forecast_reset locale) (set_what_if no_what_if) ]);
              difference ] ]
    in
    (* What the line assumes, in words. *)
    let assumption a =
      let d name = date (J.str (J.field name a)) in
      match J.str (J.field "kind" a) with
      | "usual_spending" -> Some (Msg.forecast_a_usual ~amount:(Fmt.money_json ~code (J.field "amount" a)) ~from:(d "from") ~to_:(d "to") locale)
      | "stale_import" -> Some (Msg.forecast_a_stale ~date:(d "date") locale)
      | "stopped" -> Some (Msg.forecast_a_stopped ~name:(J.str (J.field "name" a)) ~date:(d "last_date") locale)
      | "other_currency" ->
          Some (Msg.forecast_a_other_currency ~accounts:(String.concat ~sep:", " (List.map (J.list (J.field "accounts" a)) ~f:(Accounts_data.name ~locale))) locale)
      | "short_history" -> (
          match J.int (J.field "days" a) with 0 -> Some (Msg.forecast_a_no_history locale) | days -> Some (Msg.forecast_a_short ~days locale))
      | "no_income_detected" -> Some (Msg.forecast_a_no_income locale)
      | "range_from" -> Some (Msg.forecast_a_range ~count:(Int.to_string (J.int (J.field "forecasts" a))) ~date:(d "since") locale)
      | "no_range_yet" -> Some (Msg.forecast_a_no_range locale)
      | "seasonal" ->
          let month = J.int (J.field "month" a) and year = J.int (J.field "year" a) and percent = J.int (J.field "percent" a) in
          Some
            (Msg.forecast_a_seasonal ~month:(Mf2_runtime.month_name locale month)
               ~percent:(Fmt.percent ~signed:true locale (Float.of_int percent)) ~last:(Fmt.month_label locale (sprintf "%04d-%02d" year month)) locale)
      | _ -> None
    in
    let assumptions =
      Ui.div
        [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.forecast_assumptions locale) ] ];
          Vdom.Node.ul ~attrs:[ Vdom.Attr.class_ "panel assumptions" ]
            (List.map (List.filter_map f.assumptions ~f:assumption @ [ Msg.forecast_a_cards locale ]) ~f:(fun s -> Vdom.Node.li [ Ui.text s ])) ]
    in
    if List.is_empty f.accounts then
      Ui.div ~cls:"panel empty-panel"
        [ Ui.p (Msg.forecast_no_accounts locale); Ui.div ~cls:"actions" [ Ui.button ~kind:"primary" (Msg.nav_accounts locale) (navigate Route.Accounts) ] ]
    else
      (* The chart and its numbers, and any account that would go below zero, at a glance; the payments behind
         the line, the what-ifs and the assumptions when asked for. *)
      Ui.div
        ([ chart ] @ overdraw
        @ [ Vdom.Node.create "details" ~attrs:[ Vdom.Attr.class_ "forecast-details" ]
              [ Vdom.Node.create "summary" [ Ui.text (Msg.forecast_details locale) ]; upcoming; what_if_panel; assumptions ] ])
  in
  (* Closed, the forecast is a sparkline and two facts: the cash so far (solid), the line ahead (dashed) inside
     the range past forecasts landed in, today marked; the lowest point and where it ends. Open, it is all of it. *)
  let spark (f : D.t) =
    let code = f.currency in
    let timeline = f.history @ f.baseline in
    let n = List.length timeline in
    let cash = List.map timeline ~f:(fun (p : D.point) -> D.value p.cash) in
    let lows = List.map f.range ~f:(fun (_, l, _) -> D.value l) and highs = List.map f.range ~f:(fun (_, _, h) -> D.value h) in
    let all = cash @ lows @ highs in
    let lo = Option.value (List.min_elt all ~compare:Float.compare) ~default:0. and hi = Option.value (List.max_elt all ~compare:Float.compare) ~default:1. in
    let w = 340. and h = 88. in
    let x i = if n <= 1 then 0. else Float.of_int i *. w /. Float.of_int (n - 1) in
    let y v = if Float.(hi = lo) then h /. 2. else 6. +. (hi -. v) /. (hi -. lo) *. (h -. 12.) in
    let today = List.length f.history in
    let path pts = String.concat ~sep:" " (List.mapi pts ~f:(fun i (px, py) -> Printf.sprintf "%s%.1f %.1f" (if i = 0 then "M" else "L") px py)) in
    let past = List.filteri cash ~f:(fun i _ -> i <= today) |> List.mapi ~f:(fun i v -> (x i, y v)) in
    let ahead = List.filteri cash ~f:(fun i _ -> i >= today) |> List.mapi ~f:(fun i v -> (x (today + i), y v)) in
    let index_of d = List.findi timeline ~f:(fun _ (p : D.point) -> String.equal p.date d) |> Option.map ~f:fst in
    let band =
      let edge pick = List.filter_map f.range ~f:(fun (d, l, hgh) -> Option.map (index_of d) ~f:(fun i -> (x i, y (D.value (pick l hgh))))) in
      match edge (fun _ h -> h), List.rev (edge (fun l _ -> l)) with
      | (_ :: _ as top), bottom -> Some (path (top @ bottom) ^ " Z")
      | [], _ -> None
    in
    let svg_attr k v = Vdom.Attr.create k v in
    let node tag attrs = Vdom.Node.create_svg tag ~attrs [] in
    let last = List.last timeline in
    let nice d = Mf2_runtime.format_date locale (Fmt.date d) in
    Ui.div ~cls:"spark-block"
      [ Vdom.Node.create_svg "svg"
          ~attrs:[ Vdom.Attr.class_ "spark"; svg_attr "viewBox" "0 0 340 88"; svg_attr "preserveAspectRatio" "none"; svg_attr "aria-hidden" "true" ]
          (List.filter_opt
             [ Option.map band ~f:(fun d -> node "path" [ svg_attr "d" d; Vdom.Attr.class_ "spark-band" ]);
               (if n > 0 then Some (node "line" [ svg_attr "x1" (Printf.sprintf "%.1f" (x today)); svg_attr "x2" (Printf.sprintf "%.1f" (x today)); svg_attr "y1" "2"; svg_attr "y2" "86"; Vdom.Attr.class_ "spark-today" ]) else None);
               (if List.length past > 1 then Some (node "path" [ svg_attr "d" (path past); Vdom.Attr.class_ "spark-line" ]) else None);
               (if List.length ahead > 1 then Some (node "path" [ svg_attr "d" (path ahead); Vdom.Attr.class_ "spark-line ahead" ]) else None);
               Option.map (List.nth cash today) ~f:(fun v ->
                   node "circle" [ svg_attr "cx" (Printf.sprintf "%.1f" (x today)); svg_attr "cy" (Printf.sprintf "%.1f" (y v)); svg_attr "r" "3.5"; Vdom.Attr.class_ "spark-dot" ]) ]);
        Ui.div ~cls:"spark-caption num"
          [ Ui.span [ Ui.text (Msg.ahead_lowest ~amount:(Fmt.whole locale ~code f.lowest) ~date:(nice f.lowest_date) locale) ];
            (match last with Some p when not (String.equal p.date f.lowest_date) -> Ui.span [ Ui.text (Msg.ahead_end ~date:(nice p.date) ~amount:(Fmt.whole locale ~code p.cash) locale) ] | _ -> Vdom.Node.none) ] ]
  in
  let body =
    match forecast with
    | Remote.Loading -> Ui.div ~cls:"loading" [ Ui.div ~cls:"shimmer" []; Ui.muted (Msg.forecast_loading locale) ]
    | Remote.Failed _ -> Remote.view ~locale forecast ~f:(fun _ -> Vdom.Node.none)
    | Remote.Loaded json ->
        let f = D.of_json ~locale json in
        Vdom.Node.create "details" ~attrs:([ Vdom.Attr.class_ "ahead-details" ] @ if opened then [ Vdom.Attr.create "open" "" ] else [])
          [ Vdom.Node.create "summary" ~attrs:[ Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; set_opened (not opened) ]) ]
              [ Ui.div ~cls:"section-title" [ Vdom.Node.h2 [ Ui.text (Msg.ahead_title locale) ]; Ui.span ~cls:"section-link" [ Ui.text (Msg.nav_forecast locale) ] ];
                (if List.is_empty f.accounts then Ui.muted (Msg.forecast_no_accounts locale) else spark f) ];
            controls; view f ]
  in
  Ui.div ~cls:"plan-section" [ body ]
