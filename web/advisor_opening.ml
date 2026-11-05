(* How a new conversation opens: the accountant speaks first (design/superpowers/specs/2026-11-04-accountant-review-design.md).
   The facts in a sentence, what needs a decision, and what can be looked at together, each from a real finding with
   its figure. Every figure is the engine's; nothing here asks what the person wants to know. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
module W = Cashflow_advisor_protocol.Wire

(* The conversation a finding on the Overview belongs to. *)
let card_scope session card =
  if String.equal (J.str (J.path [ "data"; "scope" ] card)) "household"
     || String.equal (J.str (J.path [ "data"; "account"; "owner" ] card)) "household"
  then W.Household (J.str (J.path [ "household"; "id" ] session))
  else W.Personal

let max_topics = 4

(* The finding just opened may be further down the review, so bring its reply box into view once it has been drawn. *)
let reveal_opened =
  let next_frame f = ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> f ()))) in
  Effect.of_thunk (fun () ->
      next_frame (fun () ->
          next_frame (fun () ->
              Js.Opt.iter (Dom_html.document##querySelector (Js.string "[data-finding-key] .advisor-conversation textarea")) (fun el ->
                  ignore
                    (Js.Unsafe.meth_call el "scrollIntoView"
                       [| Js.Unsafe.inject (Js.Unsafe.obj [| ("block", Js.Unsafe.inject (Js.string "center")) |]) |])))))

let component ~(ctx : Ctx.t) ~(scope : string option Bonsai.t) ~(cards : J.t list Bonsai.t)
    ~(open_card : (J.t -> unit Effect.t) Bonsai.t)
    ~(on_ask : (finding_key:string -> scope:W.scope -> unit Effect.t) Bonsai.t)
    ~(matters : (string * string * string) list Bonsai.t) ~(select_matter : (string -> unit Effect.t) Bonsai.t) graph =
  let active = let%arr scope in Option.is_some scope in
  let scope_arg = let%arr scope in `String (Option.value scope ~default:"me") in
  let outlook = Remote.load ~active ~name:"period_outlook" ~args:(let%arr scope_arg in `Assoc [ ("scope", scope_arg) ]) ~ctx graph in
  let briefing =
    Remote.load ~active ~name:"get_briefing" ~args:(let%arr scope_arg in `Assoc [ ("scope", scope_arg); ("propose", `Bool false) ]) ~ctx graph
  in
  let%arr scope and outlook and briefing and cards and open_card and on_ask and matters and select_matter and locale = ctx.locale and session = ctx.session in
  match scope with
  | None -> Vdom.Node.none
  | Some scope ->
      let household = String.equal scope "household" in
      let currency = J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
      let row ~title ~value ~on_click =
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.class_ "opening-row"; Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> on_click) ]
          [ Ui.span ~cls:"opening-title" [ Ui.text title ]; Ui.span ~cls:"opening-value num" [ Ui.text value ];
            Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Chevron_right ]
      in
      let group heading rows =
        if List.is_empty rows then Vdom.Node.none
        else Vdom.Node.section [ Vdom.Node.h2 [ Ui.text heading ]; Ui.div ~cls:"opening-list" rows ]
      in
      let facts =
        match outlook with
        | Loading -> Vdom.Node.none
        | Failed _ -> Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "advisor-facts" ] [ Ui.text (Msg.opening_failed locale) ]
        | Loaded o ->
            let sentence =
              match J.field "covered_through" o with
              | `String _ ->
                  let code = J.str ~default:currency (J.field "currency" o) in
                  let safe = Fmt.to_float (J.field "safe_to_spend" o) in
                  let amount = Fmt.whole locale ~code (`String (Printf.sprintf "%.2f" (Float.abs safe))) in
                  let date = Fmt.date_json (J.path [ "cycle"; "last" ] o) in
                  if Float.is_negative safe then Msg.opening_short ~amount ~date locale else Msg.opening_safe ~amount ~date locale
              | _ -> Msg.opening_nothing locale
            in
            Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "advisor-facts" ] [ Ui.text sentence ]
      in
      let mine card = match card_scope session card with W.Household _ -> household | W.Personal -> not household in
      let of_kind kind = List.filter cards ~f:(fun card -> mine card && String.equal (J.str (J.field "kind" card)) kind) in
      let decisions =
        List.map (of_kind "shortfall") ~f:(fun card ->
            let data = J.field "data" card in
            let code = J.str ~default:currency (J.path [ "account"; "currency" ] data) in
            row
              ~title:(Msg.card_shortfall_title ~account:(Accounts_data.name ~locale (J.field "account" data)) locale)
              ~value:(Fmt.whole locale ~code (J.field "short_by" data)) ~on_click:(open_card card))
      in
      let unusual =
        List.map (of_kind "unusual_spend") ~f:(fun card ->
            let data = J.field "data" card in
            row
              ~title:(Msg.card_unusual_title ~category:(Accounts_data.name ~locale (J.field "category" data)) locale)
              ~value:(Fmt.whole locale ~code:currency (J.field "spent" data)) ~on_click:(open_card card))
      in
      let questions =
        match briefing with
        | Loaded json ->
            let code = J.str ~default:currency (J.field "currency" json) in
            let finding_scope = if household then W.Household (J.str (J.path [ "household"; "id" ] session)) else W.Personal in
            List.filter_map (J.list (J.field "questions" json)) ~f:(fun q ->
                let title, value, unit, _, _ = Briefing_view.words ~locale ~code q in
                let value = if String.is_empty unit then value else value ^ " " ^ unit in
                if String.is_empty title then None
                else Some (row ~title ~value ~on_click:(on_ask ~finding_key:(J.str (J.field "key" q)) ~scope:finding_scope)))
        | Loading | Failed _ -> []
      in
      let still_open =
        let now = Advisor_navigation.now_ms () in
        List.map matters ~f:(fun (id, title, updated_at) ->
            row ~title ~value:(Advisor_navigation.age_caption locale now updated_at) ~on_click:(select_matter id))
      in
      Ui.div ~cls:"advisor-opening"
        [ facts; group (Msg.opening_decide locale) decisions; group (Msg.opening_explore locale) (List.take (unusual @ questions) max_topics);
          group (Msg.opening_still_open locale) still_open ]
