(* The accountant's briefing (design/superpowers/specs/2026-10-29-accountant-briefing-design.md): the few
   questions about the person's money worth answering before they ask, best first. Each card is the question
   as its title, the answer as one number and one short line, and what to say back: got it (this answer is
   known; a materially different one comes back) or not useful (stop asking this kind). The server ranks
   them and computes every figure; this only words them, except a question the model proposed, whose title is
   the model's (it carries no figures) and whose figures are the engine's check of it.

   The figures arrive at once. When the model hasn't been asked about these books yet, a second load a moment
   later asks it (it takes seconds), armed for one scope and version of the data. Ask opens the advisor on the
   question, under its card, with the question and its figures as the server states them. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let component ~(ctx : Ctx.t) ~(scope : string Bonsai.t) ~(visible : bool Bonsai.t)
    ~(on_ask : (finding_key:string -> scope:Cashflow_advisor_protocol.Wire.scope -> unit Effect.t) Bonsai.t) ~(selected_finding : string option Bonsai.t)
    ~(outlet : Vdom.Node.t Bonsai.t) graph =
  let armed, set_armed = Bonsai.state None graph in
  let briefing =
    Remote.load ~active:visible ~name:"get_briefing"
      ~args:(let%arr scope and armed and version = ctx.version in
             `Assoc [ ("scope", `String scope); ("propose", `Bool (Option.equal [%equal: string * int] armed (Some (scope, version)))) ])
      ~ctx graph
  in
  Bonsai.Edge.on_change ~equal:phys_equal briefing
    ~callback:(let%arr set_armed and scope and version = ctx.version and visible in fun (result : Remote.t) ->
      match result with
      | Loaded json when J.bool (J.field "can_propose" json) && visible ->
          let%bind.Effect () = Effect.of_deferred_thunk (fun () -> Async_kernel.after (Time_ns.Span.of_sec 2.)) in
          set_armed (Some (scope, version))
      | _ -> Effect.Ignore)
    graph;
  let%arr briefing and locale = ctx.locale and changed = ctx.changed and notify = ctx.notify and on_ask and selected_finding and outlet and scope and session = ctx.session in
  match briefing with
  | Remote.Loaded json when not (List.is_empty (J.list (J.field "questions" json))) ->
      let code = J.str ~default:"SEK" (J.field "currency" json) in
      let kr v = Fmt.whole locale ~code v in
      let say key action = match%bind.Effect Api.run "act_on_question" (`Assoc [ ("key", `String key); ("action", `String action) ]) with Ok _ -> changed | Error m -> notify m in
      let row ?(tag = "") label value =
        Ui.div ~cls:"figure-line question-row"
          [ Ui.span ~cls:"figure-label" [ Ui.text label; (if String.is_empty tag then Vdom.Node.none else Ui.span ~cls:"review-tag" [ Ui.text tag ]) ];
            Ui.span ~cls:"figure-value" [ Ui.text value ] ]
      in
      let card q =
        let a = J.field "answer" q in
        let f name = J.field name a in
        let n name = Int.to_string (J.int (f name)) in
        let category v = Accounts_data.name ~locale v in
        let signed x = Fmt.signed_whole locale ~code (`String (Printf.sprintf "%.2f" x)) in
        let title, value, sub, rows =
          match J.str (J.field "family" q) with
          | "proposed" -> (
              let g name = J.path [ "figures"; name ] a in
              let title = J.str (f "title") in
              match J.str (g "check") with
              | "merchant_spend" -> (title, kr (g "total"), Msg.q_leak_sub ~count:(Int.to_string (J.int (g "count"))) ~months:(Int.to_string (J.int (g "months"))) locale, [])
              | "category_change" -> (title, kr (g "recent"), Msg.q_change_sub ~earlier:(kr (g "earlier")) locale, [])
              | "new_merchant" ->
                  (title, kr (g "total"), Msg.q_new_sub ~count:(Int.to_string (J.int (g "count"))) ~date:(Fmt.date_json (g "first")) locale, [])
              | _ -> ("", "", "", []))
          | "last_month" ->
              (* What made the difference, each said as what it is. *)
              let driver d =
                let amount = Fmt.to_float (J.field "amount" d) and name x = J.str (J.field x d) in
                match J.str (J.field "kind" d) with
                | "yearly" -> row ~tag:(Msg.review_tag_yearly locale) (name "description") (kr (J.field "amount" d))
                | "one_off" -> row ~tag:(Msg.review_tag_one_off locale) (name "description") (kr (J.field "amount" d))
                | "habit" when J.int (J.field "cycles_up" d) >= 2 ->
                    row ~tag:(Msg.review_tag_habit_running ~cycles:(Int.to_string (J.int (J.field "cycles_up" d))) locale) (category (J.field "category" d)) (signed amount)
                | "habit" -> row ~tag:(Msg.review_tag_habit locale) (category (J.field "category" d)) (signed amount)
                | "less" -> row ~tag:(Msg.review_tag_less locale) (category (J.field "category" d)) (signed (-.amount))
                | _ -> row (Msg.review_tag_income locale) (signed amount)
              in
              let month = Fmt.month_label locale (J.str (J.path [ "cycle"; "cycle" ] a)) in
              let more = Float.(Fmt.to_float (f "spending") >= Fmt.to_float (f "usual_spending")) in
              ( (if more then Msg.q_month_more ~month locale else Msg.q_month_less ~month locale), kr (f "spending"),
                Msg.q_month_sub ~usual:(kr (f "usual_spending")) locale, List.map (J.list (f "drivers")) ~f:driver )
          | "where_it_goes" ->
              ( Msg.q_where_title locale, kr (f "flexible"), Msg.q_where_sub ~fixed:(kr (f "fixed")) ~one_off:(kr (f "one_off")) locale,
                List.map (J.list (f "top")) ~f:(fun t -> row (category (J.field "category" t)) (kr (J.field "total" t))) )
          | "regular_bills" ->
              let fresh = J.list (f "fresh") and raised = J.list (f "raised") in
              let named b = J.str (J.field "name" b) in
              ( Msg.q_bills_title locale, kr (f "yearly"), Msg.q_bills_sub ~count:(n "count") locale,
                List.map fresh ~f:(fun b -> row ~tag:(Msg.q_new locale) (named b) (kr (J.field "yearly" b)))
                @ List.map raised ~f:(fun b -> row ~tag:(Msg.q_raised ~amount:(kr (J.field "rise" b)) locale) (named b) (kr (J.field "yearly" b)))
                @ (if List.is_empty fresh && List.is_empty raised then List.map (J.list (f "biggest")) ~f:(fun b -> row (named b) (kr (J.field "yearly" b))) else []) )
          | "drift" ->
              ( Msg.q_drift_title ~category:(category (f "category")) locale, kr (f "recent"),
                Msg.q_drift_sub ~usual:(kr (f "usual")) ~cycles:(n "cycles") locale, [] )
          | "leak" -> (Msg.q_leak_title ~name:(J.str (f "name")) locale, kr (f "total"), Msg.q_leak_sub ~count:(n "count") ~months:(n "months") locale, [])
          | "fees" -> (Msg.q_fees_title locale, kr (J.field "stake" q), Msg.q_fees_sub ~amount:(kr (f "total")) ~months:(n "months") locale, [])
          | "normal_month" ->
              let months = match f "months_covered" with `Float x -> Float.to_string_hum ~decimals:1 ~strip_zero:true x | `Int x -> Int.to_string x | _ -> "0" in
              (Msg.q_normal_title locale, kr (f "baseline"), Msg.q_normal_sub ~months locale, [])
          | "real_savings" -> (Msg.q_savings_title locale, kr (f "usual"), Msg.q_savings_sub ~one_offs:(kr (f "one_offs")) locale, [])
          | "season_ahead" ->
              ( Msg.q_season_title ~month:(Fmt.month_label locale (J.str (J.path [ "cycle"; "cycle" ] a))) locale, kr (f "last_year"),
                Msg.q_season_sub ~usual:(kr (f "usual")) locale, [] )
          | "payday_week" -> (Msg.q_payday_title locale, kr (f "first_week"), Msg.q_payday_sub ~rest:(kr (f "rest")) locale, [])
          | _ -> ("", "", "", [])
        in
        let key = J.str (J.field "key" q) in
        let finding_scope =
          if String.equal scope "household" then Cashflow_advisor_protocol.Wire.Household (J.str (J.path [ "household"; "id" ] session)) else Personal
        in
        let selected = Option.equal String.equal selected_finding (Some key) in
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.create "data-finding-key" key ]
          (* A quiet row: the question and its number; opened, what is behind it and what to say back. *)
          ([ Vdom.Node.create "details"
               ~attrs:([ Vdom.Attr.class_ "wk-row" ] @ if selected then [ Vdom.Attr.create "open" "" ] else [])
               [ Vdom.Node.create "summary" ~attrs:[ Vdom.Attr.class_ "wk-head" ]
                   [ Ui.div ~cls:"wk-main" [ Vdom.Node.h3 ~attrs:[ Vdom.Attr.class_ "wk-title" ] [ Ui.text title ]; Ui.div ~cls:"wk-sub" [ Ui.text sub ] ];
                     Ui.div ~cls:"wk-value num" [ Ui.text value ];
                     Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Chevron_right ];
                 Ui.div ~cls:"wk-detail"
                   ((if List.is_empty rows then [] else [ Ui.div ~cls:"wk-lines" rows ])
                   @ [ Ui.div ~cls:"wk-actions"
                         [ Ui.button ~kind:"primary" (Msg.advisor_ask locale) (on_ask ~finding_key:key ~scope:finding_scope);
                           Ui.button (Msg.q_got_it locale) (say key "got_it"); Ui.button ~kind:"quiet" (Msg.q_not_useful locale) (say key "not_useful") ] ]) ] ]
          @ if selected then [ outlet ] else [])
      in
      Ui.div ~cls:"briefing"
        [ Ui.div ~cls:"section-title" [ Vdom.Node.h2 [ Ui.text (Msg.briefing_title locale) ] ];
          Ui.div ~cls:"briefing-list" (List.map (J.list (J.field "questions" json)) ~f:card) ]
  | _ -> Vdom.Node.none
