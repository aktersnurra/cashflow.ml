(* What the accountant says of a pay cycle that is over (design/superpowers/specs/2026-11-04-accountant-review-design.md):
   the cycle's dates and how it ended, how the budgets held, and what was agreed during it. Worked out from the books as
   they stand now, so a statement imported late corrects it. Every figure is the engine's. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module W = Cashflow_advisor_protocol.Wire

let in_scope scope (entry : W.agreed_entry) =
  match scope, entry.scope with
  | "me", W.Personal | "household", W.Household _ | "family", _ -> true
  | _ -> false

let component ~(ctx : Ctx.t) ~(scope : string Bonsai.t) ~(offset : int Bonsai.t) ~(cycle : J.t option Bonsai.t) ~(code : string Bonsai.t)
    ~(visible : bool Bonsai.t) ~(select : (string -> unit Effect.t) Bonsai.t) graph =
  let active = let%arr visible and offset in visible && offset < 0 in
  (* Budgets belong to a person or a household; the whole family has none of its own. *)
  let budgets_active = let%arr active and scope in active && not (String.equal scope "family") in
  let budgets =
    Remote.load ~active:budgets_active ~name:"budget_status"
      ~args:(let%arr scope and offset in `Assoc [ ("scope", `String scope); ("cycle_offset", `Int offset) ])
      ~ctx graph
  in
  let agreed, set_agreed = Bonsai.state ~equal:phys_equal ([] : W.agreed_entry list) graph in
  let dates =
    let%arr cycle in
    Option.map cycle ~f:(fun c -> (J.str (J.field "first" c), J.str (J.field "last" c)))
  in
  let key = let%arr active and dates and version = ctx.version in (active, dates, version) in
  (* The latest request wins; and each new question clears the last answer, as these titles are private. *)
  let latest = ref 0 in
  let refresh =
    let%arr set_agreed in
    fun (active, dates, _) ->
      let%bind.Effect request = Effect.of_thunk (fun () -> incr latest; !latest) in
      let%bind.Effect () = set_agreed [] in
      match active, dates with
      | true, Some within -> (
          let%bind.Effect result = Effect.of_deferred_thunk (fun () -> Advisor_api.agreed ~within ~limit:50 ~offset:0 ()) in
          if request <> !latest then Effect.Ignore
          else match result with Ok page -> set_agreed page.entries | Error _ -> Effect.Ignore)
      | _ -> Effect.Ignore
  in
  Bonsai.Edge.on_change ~equal:[%equal: bool * (string * string) option * int] key ~callback:refresh graph;
  let%arr active and cycle and budgets and agreed and scope and code and select and locale = ctx.locale in
  match active, cycle with
  | false, _ | _, None -> Vdom.Node.none
  | true, Some c ->
      let whole json = Fmt.whole locale ~code json in
      let month = Fmt.month_label locale (J.str (J.field "cycle" c)) in
      let period =
        Msg.review_past_period ~first:(Fmt.date_json (J.field "first" c)) ~last:(Fmt.date_json (J.field "last" c)) locale
      in
      let complete = match J.field "complete" c with `Bool b -> b | _ -> true in
      let empty = Float.(Fmt.to_float (J.field "income" c) = 0. && Fmt.to_float (J.field "spending" c) = 0.) in
      let verdict =
        (* A cycle the statements don't reach the end of says so under its figures: no verdict on part of a month. *)
        if (not complete) || empty then Vdom.Node.none
        else
          let savings = Fmt.to_float (J.field "savings" c) in
          let amount = whole (`String (Printf.sprintf "%.2f" (Float.abs savings))) in
          let income = whole (J.field "income" c) and spending = whole (J.field "spending" c) in
          Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "past-verdict" ]
            [ Ui.text
                (if Float.is_negative savings then Msg.review_past_short ~month ~amount ~income ~spending locale
                 else Msg.review_past_saved ~month ~amount ~income ~spending locale) ]
      in
      let budget_rows =
        match budgets with
        | Loaded json when not (List.is_empty (J.list (J.field "lines" json))) ->
            let lines = J.list (J.field "lines" json) in
            let over = List.filter lines ~f:(fun l -> String.equal (J.str (J.field "status" l)) "over_budget") in
            let held = List.length lines - List.length over in
            [ Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "past-part"; Vdom.Attr.create "role" "group"; Vdom.Attr.create "aria-label" (Msg.review_past_budgets locale) ]
                (Vdom.Node.h2 [ Ui.text (Msg.review_past_budgets_held ~held:(Int.to_string held) ~total:(Int.to_string (List.length lines)) locale) ]
                 :: List.map over ~f:(fun l ->
                        Ui.div ~cls:"figure-line"
                          [ Ui.span ~cls:"figure-label" [ Ui.text (Accounts_data.name ~locale (J.field "category" l)) ];
                            Ui.span ~cls:"figure-value"
                              [ Ui.text (Msg.review_past_budget_over ~spent:(whole (J.field "spent" l)) ~budget:(whole (J.field "budget" l)) locale) ] ])) ]
        | _ -> []
      in
      let agreed_rows =
        match List.filter agreed ~f:(in_scope scope) with
        | [] -> []
        | entries ->
            [ Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "past-part"; Vdom.Attr.create "role" "group"; Vdom.Attr.create "aria-label" (Msg.review_past_agreed locale) ]
                (Vdom.Node.h2 [ Ui.text (Msg.review_past_agreed locale) ]
                 :: List.map entries ~f:(fun (m : W.agreed_entry) ->
                        let target = Route.Advisor (Some m.conversation_id) in
                        let status =
                          match m.matter with
                          | W.Decided -> Msg.matter_decide locale
                          | W.Open -> Msg.agreed_open locale
                          | W.Parked -> Msg.agreed_parked locale
                        in
                        Vdom.Node.a
                          ~attrs:
                            [ Vdom.Attr.class_ "past-agreed"; Vdom.Attr.href (Route.to_path target);
                              Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; select m.conversation_id ]) ]
                          [ Vdom.Node.span
                              ~attrs:[ Vdom.Attr.class_ "past-agreed-title"; Vdom.Attr.create "dir" "auto" ]
                              [ Ui.text (Option.value m.title ~default:(Msg.agreed_untitled locale)) ];
                            Ui.span ~cls:"past-agreed-status" [ Ui.text status ] ])) ]
      in
      Vdom.Node.section ~attrs:[ Vdom.Attr.class_ "past-review" ]
        ([ Ui.div ~cls:"past-period" [ Ui.text period ]; verdict ] @ budget_rows @ agreed_rows)
