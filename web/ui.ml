(* Small view helpers. Styling lives in style.css; these only pick classes. *)
open! Core
open! Bonsai_web.Cont

let text = Vdom.Node.text
let div ?(cls = "") children = Vdom.Node.div ~attrs:[ Vdom.Attr.class_ cls ] children
let span ?(cls = "") children = Vdom.Node.span ~attrs:[ Vdom.Attr.class_ cls ] children

let button ?(kind = "secondary") ?(disabled = false) ?(key_hint = "") label effect =
  Vdom.Node.button
    ~attrs:
      ([ Vdom.Attr.class_ ("button " ^ kind); Vdom.Attr.type_ "button"; Vdom.Attr.on_click (fun _ -> effect) ]
      @ if disabled then [ Vdom.Attr.disabled ] else [])
    ([ text label ] @ if String.is_empty key_hint then [] else [ Vdom.Node.kbd [ text key_hint ] ])

let card ?(tone = "") ?(eyebrow = "") ~title ?(body = []) ?(actions = []) () =
  Vdom.Node.section
    ~attrs:[ Vdom.Attr.class_ ("card " ^ tone) ]
    ([ (if String.is_empty eyebrow then Vdom.Node.none else div ~cls:"eyebrow" [ text eyebrow ]); Vdom.Node.h2 [ text title ] ]
    @ body
    @ if List.is_empty actions then [] else [ div ~cls:"actions" actions ])

let kpi ?(tone = "") label value = div ~cls:("kpi " ^ tone) [ div ~cls:"kpi-label" [ text label ]; div ~cls:"kpi-value" [ text value ] ]
let muted s = Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "muted" ] [ text s ]
let p s = Vdom.Node.p [ text s ]
let empty s = div ~cls:"empty" [ text s ]
let page ~title children = Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ] (Vdom.Node.h1 [ text title ] :: children)

(* A horizontal bar: [fraction] of the track filled, red beyond 1. *)
let meter ?(marker = None) fraction =
  let width = Float.clamp_exn (fraction *. 100.) ~min:0. ~max:100. in
  div ~cls:("meter" ^ if Float.(fraction > 1.) then " over" else "")
    ([ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "fill"; Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_percentage width))) ] [] ]
    @
    match marker with
    | Some m -> [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "marker"; Vdom.Attr.style (Css_gen.left (`Percent (Percent.of_percentage (Float.clamp_exn (m *. 100.) ~min:0. ~max:100.)))) ] [] ]
    | None -> [])

let input ?(type_ = "text") ?(placeholder = "") ~value ~on_input () =
  Vdom.Node.input ~attrs:[ Vdom.Attr.type_ type_; Vdom.Attr.value_prop value; Vdom.Attr.placeholder placeholder; Vdom.Attr.on_input (fun _ v -> on_input v) ] ()

let select ~value ~options ~on_change =
  Vdom.Node.select
    ~attrs:[ Vdom.Attr.on_change (fun _ v -> on_change v) ]
    (List.map options ~f:(fun (v, label) ->
         Vdom.Node.option ~attrs:([ Vdom.Attr.value v ] @ if String.equal v value then [ Vdom.Attr.create "selected" "" ] else []) [ text label ]))

let field label control = Vdom.Node.label ~attrs:[ Vdom.Attr.class_ "field" ] [ span ~cls:"field-label" [ text label ]; control ]

(* The current text of the input an event came from. Enter handlers read it rather than the last
   rendered value, which can lag behind fast typing. *)
let event_value (event : #Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t) =
  let open Js_of_ocaml in
  Js.Opt.case event##.target (fun () -> "") (fun target ->
      Js.Opt.case (Dom_html.CoerceTo.input target) (fun () -> "") (fun input -> Js.to_string input##.value))

let icon_button ~label ?(disabled = false) (icon : Feather_icon.t) effect =
  Vdom.Node.button
    ~attrs:
      ([ Vdom.Attr.class_ "button icon-button"; Vdom.Attr.type_ "button"; Vdom.Attr.title label; Vdom.Attr.create "aria-label" label;
         Vdom.Attr.on_click (fun _ -> effect) ]
      @ if disabled then [ Vdom.Attr.disabled ] else [])
    [ Feather_icon.svg ~size:(`Px 14) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) icon ]

(* ‹ October 2026 ›: step through pay cycles. Each side is (label, disabled, effect). *)
(* [dates] says which days a pay cycle covers when its month name alone would mislead. *)
let period_dates = function Some dates -> span ~cls:"period-dates" [ text dates ] | None -> Vdom.Node.none

let period ?dates ~label ~previous:(previous_label, previous_disabled, previous) ~next:(next_label, next_disabled, next) () =
  div ~cls:"period-group"
    [ period_dates dates;
      div ~cls:"period"
        [ icon_button ~label:previous_label ~disabled:previous_disabled Chevron_left previous; span ~cls:"period-label" [ text label ];
          icon_button ~label:next_label ~disabled:next_disabled Chevron_right next ] ]

(* The first letter of a name, capitalised, for a tile: "Övrigt" is "Ö", not half of its first
   byte pair. *)
let initial name =
  let open Js_of_ocaml in
  Js.to_string ((Js.string name)##charAt 0)##toUpperCase
