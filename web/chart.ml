(* Charts drawn as plain SVG: no chart library, so they follow the stylesheet's colours and stay
   small. *)
open! Core
open! Bonsai_web.Cont
module S = Virtual_dom_svg

let svg ~width ~height children =
  S.Node.svg ~attrs:[ S.Attr.viewbox ~min_x:0. ~min_y:0. ~width ~height; Vdom.Attr.create "class" "chart"; Vdom.Attr.create "preserveAspectRatio" "none" ] children

(* Paired bars per period, income and spending, with the period names in HTML below so they
   don't stretch with the drawing. *)
let paired_bars (points : (string * float * float) list) =
  let height = 100. and gap = 8. in
  let n = Float.of_int (max 1 (List.length points)) in
  let width = 320. in
  let slot = width /. n in
  let top = List.fold points ~init:1. ~f:(fun m (_, a, b) -> Float.max m (Float.max a b)) in
  let bar ~x ~value ~cls =
    let h = Float.max 1. (value /. top *. (height -. 4.)) in
    S.Node.rect ~attrs:[ S.Attr.x x; S.Attr.y (height -. h); S.Attr.width ((slot -. gap) /. 2.); S.Attr.height h; S.Attr.rx 1.5; Vdom.Attr.create "class" cls ] []
  in
  Vdom.Node.div
    [ svg ~width ~height
        (List.concat_mapi points ~f:(fun i (_, income, spending) ->
             let x = (Float.of_int i *. slot) +. (gap /. 2.) in
             [ bar ~x ~value:income ~cls:"bar-income"; bar ~x:(x +. ((slot -. gap) /. 2.)) ~value:spending ~cls:"bar-spending" ]));
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "chart-labels" ] (List.map points ~f:(fun (label, _, _) -> Vdom.Node.span [ Vdom.Node.text label ])) ]

(* A line with a soft area under it, for net worth. *)
let line (values : float list) =
  let width = 320. and height = 100. in
  match values with
  | [] | [ _ ] -> Vdom.Node.none
  | values ->
      let lo = List.fold values ~init:Float.infinity ~f:Float.min and hi = List.fold values ~init:Float.neg_infinity ~f:Float.max in
      let span = if Float.(hi -. lo < 1.) then 1. else hi -. lo in
      let step = width /. Float.of_int (List.length values - 1) in
      let points = List.mapi values ~f:(fun i v -> (Float.of_int i *. step, height -. 8. -. ((v -. lo) /. span *. (height -. 16.)))) in
      svg ~width ~height
        [ S.Node.polygon ~attrs:[ S.Attr.points ((0., height) :: points @ [ (width, height) ]); Vdom.Attr.create "class" "area" ] [];
          S.Node.polyline ~attrs:[ S.Attr.points points; Vdom.Attr.create "class" "line" ] [] ]

let sparkline values =
  match values with
  | [] | [ _ ] -> Vdom.Node.none
  | values ->
      let width = 80. and height = 24. in
      let lo = List.fold values ~init:Float.infinity ~f:Float.min and hi = List.fold values ~init:Float.neg_infinity ~f:Float.max in
      let span = if Float.(hi -. lo < 1.) then 1. else hi -. lo in
      let step = width /. Float.of_int (List.length values - 1) in
      svg ~width ~height [ S.Node.polyline ~attrs:[ S.Attr.points (List.mapi values ~f:(fun i v -> (Float.of_int i *. step, height -. 2. -. ((v -. lo) /. span *. (height -. 4.))))); Vdom.Attr.create "class" "spark" ] [] ]
