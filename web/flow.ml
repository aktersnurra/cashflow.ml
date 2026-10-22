(* The money flow of one pay cycle, as Sankey nodes: what came in on the left (income, transfers
   in, and savings drawn on when spending exceeded income), where it went on the right (the
   largest categories, the rest as Other, transfers out, and what was saved). Shares are of
   everything that came in. Overview and Cash flow draw the same flow from this. *)
open! Core
open! Bonsai_web.Cont

let nodes ~locale ~code ?(categories = 5) (cycle : J.t) (spending : J.t) =
  let money v = Fmt.whole locale ~code (`String (Float.to_string v)) in
  let field name = Fmt.to_float (J.field name cycle) in
  let income = field "income" and savings = field "savings" and transfers_in = field "transfers_in" and transfers_out = field "transfers_out" in
  let rows =
    List.filter_map (J.list (J.field "categories" spending)) ~f:(fun r ->
        let v = Fmt.to_float (J.field "total" r) in
        if Float.(v > 0.) then Some (J.str (J.path [ "category"; "name" ] r), v, J.str_opt (J.path [ "category"; "id" ] r)) else None)
  in
  let top = List.take rows categories and other = List.sum (module Float) (List.drop rows categories) ~f:(fun (_, v, _) -> v) in
  let left_values =
    [ (Msg.label_income locale, income, "in", None) ]
    @ (if Float.(transfers_in > 0.) then [ (Msg.overview_transfers_in locale, transfers_in, "in-transfer", None) ] else [])
    @ if Float.(savings < 0.) then [ (Msg.overview_from_savings locale, Float.neg savings, "in-savings", None) ] else []
  in
  let basis = Float.max 1. (List.sum (module Float) left_values ~f:(fun (_, v, _, _) -> v)) in
  let node (label, value, cls, id) : Chart.sankey_node = { label; amount = money value; share = Fmt.percent locale (value /. basis *. 100.); value; cls; id } in
  let right_values =
    List.mapi top ~f:(fun i (name, v, id) -> (name, v, sprintf "c%d" ((i % 5) + 1), id))
    @ (if Float.(other > 0.) then [ (Msg.overview_other locale, other, "c-other", None) ] else [])
    @ (if Float.(transfers_out > 0.) then [ (Msg.overview_transfers_out locale, transfers_out, "c-transfer", None) ] else [])
    @ if Float.(savings > 0.) then [ (Msg.overview_savings locale, savings, "c-savings", None) ] else []
  in
  (List.map left_values ~f:node, List.map right_values ~f:node)

(* The Sankey, and the same figures as a list for screens too small for its labels. *)
let view ?on_select ~left ~right () =
  [ Chart.sankey ?on_select ~left ~right ();
    Ui.div ~cls:"sankey-legend"
      (List.map (left @ right) ~f:(fun (n : Chart.sankey_node) ->
           Ui.div ~cls:"legend-row"
             [ Ui.span ~cls:("swatch " ^ n.cls) []; Ui.span ~cls:"legend-name" [ Ui.text n.label ]; Ui.span ~cls:"amount" [ Ui.text n.amount ];
               Ui.span ~cls:"legend-share" [ Ui.text n.share ] ])) ]
