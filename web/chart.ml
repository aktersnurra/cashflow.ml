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

(* A two-column Sankey: money in on the left (income, and whatever else funded the period),
   where it went on the right (categories, transfers, savings). Every bar and ribbon uses one
   scale, so widths are proportional to money. The left side is one bar whose segments are its
   nodes; ribbons leave it back to back. On the right each node gets a slot at least tall enough
   for its two-line label, with the bar centred in it, so small categories stay readable without
   being drawn bigger than they are. Hovering a ribbon or node shows the exact figure; a node
   with an [id] (a category) is a link: clicking its ribbon or bar calls [on_select]. *)
type sankey_node = { label : string; amount : string; share : string; value : float; cls : string; id : string option }

let sankey ?on_select ~(left : sankey_node list) ~(right : sankey_node list) () =
  let select n =
    match n.id, on_select with
    | Some id, Some f -> [ Vdom.Attr.on_click (fun _ -> f id); Vdom.Attr.create "data-link" "true" ]
    | _ -> []
  in
  let width = 640. and gap = 12. and bar = 8. and slot_min = 36. and pad = 4. in
  let left_x = 150. and right_x = 470. in
  let total nodes = List.fold nodes ~init:0. ~f:(fun s n -> s +. n.value) in
  let flow_height = 220. in
  let scale = flow_height /. Float.max 1. (Float.max (total left) (total right)) in
  let rights =
    let y = ref pad in
    List.map right ~f:(fun n ->
        let h = Float.max 2. (n.value *. scale) in
        let slot = Float.max slot_min h in
        let top = !y +. ((slot -. h) /. 2.) in
        let centre = !y +. (slot /. 2.) in
        y := !y +. slot +. gap;
        (n, top, h, centre))
  in
  let height = Float.max (flow_height +. (2. *. pad)) (match List.last rights with Some (_, top, h, centre) -> Float.max (top +. h) (centre +. 18.) +. pad | None -> 0.) in
  let left_span = total left *. scale in
  let lefts =
    let y = ref ((height -. left_span) /. 2.) in
    List.map left ~f:(fun n ->
        let h = n.value *. scale in
        let top = !y in
        y := !y +. h;
        (n, top, h, top +. (h /. 2.)))
  in
  (* Left labels can be closer together than their segments are tall; push them apart. *)
  let lefts =
    let last = ref Float.neg_infinity in
    List.map lefts ~f:(fun (n, top, h, centre) ->
        let centre = Float.max centre (!last +. slot_min) in
        last := centre;
        (n, top, h, centre))
  in
  let title n = S.Node.title [ Vdom.Node.text (n.label ^ ": " ^ n.amount ^ " (" ^ n.share ^ ")") ] in
  let cursor = ref ((height -. left_span) /. 2.) in
  let ribbons =
    List.map rights ~f:(fun (n, top, h, _) ->
        let h0 = n.value *. scale in
        let y0 = !cursor in
        cursor := !cursor +. h0;
        let x0 = left_x +. bar and x1 = right_x in
        let mid = (x0 +. x1) /. 2. in
        let d =
          sprintf "M%.1f %.1f C%.1f %.1f %.1f %.1f %.1f %.1f L%.1f %.1f C%.1f %.1f %.1f %.1f %.1f %.1f Z" x0 y0 mid y0 mid top x1 top x1 (top +. h) mid
            (top +. h) mid (y0 +. h0) x0 (y0 +. h0)
        in
        S.Node.path ~attrs:([ Vdom.Attr.create "d" d; Vdom.Attr.create "class" ("ribbon " ^ n.cls) ] @ select n) [ title n ])
  in
  let node ~x (n, top, h, _) =
    S.Node.rect ~attrs:([ S.Attr.x x; S.Attr.y top; S.Attr.width bar; S.Attr.height (Float.max 2. h); S.Attr.rx 1.; Vdom.Attr.create "class" ("node " ^ n.cls) ] @ select n) [ title n ]
  in
  let text ~x ~y ~anchor ~cls s =
    S.Node.text ~attrs:[ S.Attr.x x; S.Attr.y y; Vdom.Attr.create "text-anchor" anchor; Vdom.Attr.create "class" cls ] [ Vdom.Node.text s ]
  in
  let labels ~x ~anchor (n, _, _, centre) =
    [ text ~x ~y:(centre -. 3.) ~anchor ~cls:"sankey-label" n.label; text ~x ~y:(centre +. 13.) ~anchor ~cls:"sankey-amount" n.amount ]
    @ if String.equal anchor "start" then [ text ~x:(x +. 132.) ~y:(centre +. 13.) ~anchor:"end" ~cls:"sankey-share" n.share ] else []
  in
  S.Node.svg
    ~attrs:[ S.Attr.viewbox ~min_x:0. ~min_y:0. ~width ~height; Vdom.Attr.create "class" "sankey"; Vdom.Attr.create "role" "img" ]
    (ribbons
    @ List.map lefts ~f:(node ~x:left_x)
    @ List.map rights ~f:(node ~x:right_x)
    @ List.concat_map lefts ~f:(labels ~x:(left_x -. 14.) ~anchor:"end")
    @ List.concat_map rights ~f:(labels ~x:(right_x +. bar +. 14.) ~anchor:"start"))

(* Small charts for insight cards. They scale uniformly (no stretching), so the end dot stays
   round; colour comes from a tone class: "blue", "green", "red". *)
let mini_svg ~cls children =
  S.Node.svg ~attrs:[ S.Attr.viewbox ~min_x:0. ~min_y:0. ~width:300. ~height:84.; Vdom.Attr.create "class" ("mini " ^ cls); Vdom.Attr.create "aria-hidden" "true" ] children

(* One bar per period; the last (the current one) is drawn solid. *)
let mini_bars ~tone values =
  match values with
  | [] -> Vdom.Node.none
  | values ->
      let n = List.length values in
      let top = List.fold values ~init:1. ~f:(fun m v -> Float.max m (Float.abs v)) in
      let slot = 300. /. Float.of_int n in
      let w = Float.min 28. (slot *. 0.62) in
      mini_svg ~cls:tone
        (List.mapi values ~f:(fun i v ->
             let h = Float.max 2. (Float.abs v /. top *. 76.) in
             S.Node.rect
               ~attrs:
                 [ S.Attr.x ((Float.of_int i *. slot) +. ((slot -. w) /. 2.)); S.Attr.y (82. -. h); S.Attr.width w; S.Attr.height h; S.Attr.rx 2.;
                   Vdom.Attr.create "class" (if i = n - 1 then "col current" else "col") ]
               []))

(* A line with a soft area under it and a dot on the latest value; [step] holds each value until
   the next, for prices. *)
let mini_line ?(step = false) ~tone values =
  match values with
  | [] | [ _ ] -> Vdom.Node.none
  | values ->
      let lo = List.fold values ~init:Float.infinity ~f:Float.min and hi = List.fold values ~init:Float.neg_infinity ~f:Float.max in
      let span = if Float.(hi -. lo < 1e-9) then 1. else hi -. lo in
      let x i = 6. +. (Float.of_int i *. 288. /. Float.of_int (List.length values - 1)) in
      let y v = 70. -. ((v -. lo) /. span *. 56.) in
      let points = List.mapi values ~f:(fun i v -> (x i, y v)) in
      let points = if step then List.concat_mapi points ~f:(fun i (px, py) -> if i = 0 then [ (px, py) ] else [ (px, snd (List.nth_exn points (i - 1))); (px, py) ]) else points in
      let last_x, last_y = List.last_exn points in
      mini_svg ~cls:tone
        [ S.Node.polygon ~attrs:[ S.Attr.points ((6., 84.) :: points @ [ (last_x, 84.) ]); Vdom.Attr.create "class" "fill" ] [];
          S.Node.polyline ~attrs:[ S.Attr.points points; Vdom.Attr.create "class" "stroke" ] [];
          S.Node.circle ~attrs:[ S.Attr.cx last_x; S.Attr.cy last_y; S.Attr.r 3.5; Vdom.Attr.create "class" "end" ] [] ]

(* A donut of shares, largest first, with the first share in the middle. *)
let donut ~centre (shares : (float * string) list) =
  let r = 40. and c = 2. *. Float.pi *. 40. in
  let total = Float.max 1e-9 (List.sum (module Float) shares ~f:fst) in
  let offset = ref 0. in
  S.Node.svg
    ~attrs:[ S.Attr.viewbox ~min_x:0. ~min_y:0. ~width:120. ~height:120.; Vdom.Attr.create "class" "donut"; Vdom.Attr.create "aria-hidden" "true" ]
    (List.map shares ~f:(fun (v, cls) ->
         let len = v /. total *. c in
         let seg =
           S.Node.circle
             ~attrs:
               [ S.Attr.cx 60.; S.Attr.cy 60.; S.Attr.r r; Vdom.Attr.create "class" ("seg " ^ cls);
                 Vdom.Attr.create "stroke-dasharray" (sprintf "%.2f %.2f" (Float.max 0. (len -. 1.5)) (c -. Float.max 0. (len -. 1.5)));
                 Vdom.Attr.create "stroke-dashoffset" (sprintf "%.2f" (Float.neg !offset)); Vdom.Attr.create "transform" "rotate(-90 60 60)" ]
             []
         in
         offset := !offset +. len;
         seg)
    @ [ S.Node.text ~attrs:[ S.Attr.x 60.; S.Attr.y 66.; Vdom.Attr.create "text-anchor" "middle"; Vdom.Attr.create "class" "donut-centre" ] [ Vdom.Node.text centre ] ])

(* Grouped bars per period with a labelled value axis: gridlines at round steps, a firmer zero
   line when values go negative, period names underneath. [series] gives each bar's class, in
   the same order as each period's values. *)
let grouped_bars ~(series : string list) ~(tick : float -> string) (periods : (string * float list) list) =
  match periods with
  | [] -> Vdom.Node.none
  | periods ->
      let width = 640. and height = 240. and left = 48. and bottom = 24. and top = 8. in
      let values = List.concat_map periods ~f:snd in
      let lo = Float.min 0. (List.fold values ~init:0. ~f:Float.min) and hi = Float.max 1. (List.fold values ~init:0. ~f:Float.max) in
      let step =
        let raw = (hi -. lo) /. 4. in
        let magnitude = 10. **. Float.round_down (Float.log10 raw) in
        List.find_exn [ 1.; 2.; 2.5; 5.; 10. ] ~f:(fun m -> Float.(m *. magnitude >= raw)) *. magnitude
      in
      let lo = Float.round_down (lo /. step) *. step and hi = Float.round_up (hi /. step) *. step in
      let plot_h = height -. top -. bottom and plot_w = width -. left in
      let y v = top +. ((hi -. v) /. (hi -. lo) *. plot_h) in
      let ticks = List.init (Float.iround_nearest_exn ((hi -. lo) /. step) + 1) ~f:(fun i -> lo +. (Float.of_int i *. step)) in
      let n = Float.of_int (List.length periods) and k = Float.of_int (List.length series) in
      let slot = plot_w /. n in
      let bar_w = Float.min 14. (slot *. 0.7 /. k) in
      let text ~x ~y ~anchor ~cls s =
        S.Node.text ~attrs:[ S.Attr.x x; S.Attr.y y; Vdom.Attr.create "text-anchor" anchor; Vdom.Attr.create "class" cls ] [ Vdom.Node.text s ]
      in
      S.Node.svg
        ~attrs:[ S.Attr.viewbox ~min_x:0. ~min_y:0. ~width ~height; Vdom.Attr.create "class" "bars"; Vdom.Attr.create "role" "img" ]
        (List.concat_map ticks ~f:(fun t ->
             [ S.Node.line ~attrs:[ S.Attr.x1 left; S.Attr.x2 width; S.Attr.y1 (y t); S.Attr.y2 (y t); Vdom.Attr.create "class" (if Float.(t = 0.) then "zero" else "grid") ] [];
               text ~x:(left -. 8.) ~y:(y t +. 4.) ~anchor:"end" ~cls:"axis-label" (tick t) ])
        @ List.concat_mapi periods ~f:(fun i (label, vs) ->
              let centre = left +. (slot *. (Float.of_int i +. 0.5)) in
              let start = centre -. (bar_w *. k /. 2.) -. ((k -. 1.) /. 2.) in
              text ~x:centre ~y:(height -. 6.) ~anchor:"middle" ~cls:"axis-label" label
              :: List.mapi (List.zip_exn series vs) ~f:(fun j (cls, v) ->
                     let y0 = y (Float.max v 0.) and y1 = y (Float.min v 0.) in
                     S.Node.rect
                       ~attrs:
                         [ S.Attr.x (start +. (Float.of_int j *. (bar_w +. 1.))); S.Attr.y y0; S.Attr.width bar_w; S.Attr.height (Float.max 1. (y1 -. y0)); S.Attr.rx 2.;
                           Vdom.Attr.create "class" ("col " ^ cls) ]
                       [ S.Node.title [ Vdom.Node.text (label ^ ": " ^ tick v) ] ])))
