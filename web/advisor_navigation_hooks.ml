open! Core
open Bonsai_web
open Js_of_ocaml

let is_mobile () = Dom_html.document##.documentElement##.clientWidth <= 600
let px n = Js.string (Int.to_string n ^ "px")
let closest element selector : Dom_html.element Js.t Js.opt =
  Js.Unsafe.meth_call element "closest" [|Js.Unsafe.inject (Js.string selector)|]

let focus (element : Dom_html.element Js.t) =
  if element##.offsetWidth > 0 && element##.offsetHeight > 0 then
    ignore (Js.Unsafe.meth_call element "focus"
      [|Js.Unsafe.inject (Js.Unsafe.obj [|"preventScroll", Js.Unsafe.inject Js._true|])|] : unit)

let buttons element selector =
  let nodes = element##querySelectorAll (Js.string selector) in
  List.init nodes##.length ~f:(fun i -> Js.Opt.to_option (nodes##item i)) |> List.filter_opt

let menu_items element = buttons element "button[role=menuitem]:not(:disabled):not([aria-disabled=true])"
let focus_first element =
  match Js.Opt.to_option (element##querySelector (Js.string "button[aria-current=page]:not(:disabled)")) with
  | Some current -> focus current
  | None -> Option.iter (List.hd (menu_items element)) ~f:focus

let restore_trigger actions =
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ ->
    Js.Opt.iter (Dom_html.document##querySelector (Js.string
      (if actions then ".advisor-actions-button" else ".advisor-title-button"))) focus)))

module Viewport_input = struct
  type t = { on_change : bool -> unit }
  let sexp_of_t _ = Sexp.Atom "navigation-viewport"
  let combine _ b = b
end
module Viewport = Vdom.Attr.Hooks.Make (struct
  module Input = Viewport_input
  module State = struct
    type t = {mutable input:Input.t; mutable alive:bool; mutable listener:Dom.event_listener_id option}
  end
  let init input _ =
    let state = {State.input; alive=true; listener=None} in
    let listener = Dom_html.addEventListener Dom_html.window Dom_html.Event.resize
      (Dom_html.handler (fun _ -> if state.alive then state.input.Input.on_change (is_mobile ()); Js._true)) Js._false in
    state.listener <- Some listener; state
  let on_mount = `Schedule_animation_frame (fun _ state _ -> if state.State.alive then state.input.Input.on_change (is_mobile ()))
  let update ~old_input:_ ~new_input state _ = state.State.input <- new_input
  let destroy _ state _ = state.State.alive <- false; Option.iter state.listener ~f:Dom_html.removeEventListener; state.listener <- None
end)

module Menu_input = struct
  type t = { actions : bool; close : bool -> unit }
  let sexp_of_t _ = Sexp.Atom "conversation-menu"
  let combine _ b = b
end
module Menu = Vdom.Attr.Hooks.Make (struct
  module Input = Menu_input
  module State = struct
    type t = { mutable input:Input.t; mutable alive:bool; mutable listeners:Dom.event_listener_id list }
  end

  let place state (element : Dom_html.element Js.t) =
    if state.State.alive && element##.offsetWidth > 0 then (
      if is_mobile () then (
        element##.style##.top := Js.string ""; element##.style##.bottom := Js.string "";
        element##.style##.left := Js.string ""; element##.style##.right := Js.string "";
        element##.style##.maxHeight := Js.string "")
      else Js.Opt.iter (closest element ".advisor-navigation") (fun root ->
        let selector = if state.input.actions then ".advisor-actions-button" else ".advisor-title-button" in
        Js.Opt.iter (root##querySelector (Js.string selector)) (fun trigger ->
          let rect = trigger##getBoundingClientRect in
          let width = element##.offsetWidth and viewport = Dom_html.document##.documentElement in
          let below = viewport##.clientHeight - Int.of_float rect##.bottom - 16 in
          let above = Int.of_float rect##.top - 16 in
          let up = below < 180 && above > below in
          let left = Int.of_float (if state.input.actions then rect##.right -. Float.of_int width else rect##.left) in
          element##.style##.left := px (Int.max 12 (Int.min left (viewport##.clientWidth - width - 12)));
          element##.style##.right := Js.string "auto";
          element##.style##.maxHeight := px (Int.min 420 (Int.max 80 (if up then above else below)));
          if up then (
            element##.style##.top := Js.string "auto";
            element##.style##.bottom := px (viewport##.clientHeight - Int.of_float rect##.top + 8))
          else (
            element##.style##.top := px (Int.of_float rect##.bottom + 8);
            element##.style##.bottom := Js.string "auto")));
      element##setAttribute (Js.string "data-menu-placed") (Js.string ""))

  let init input element =
    let state = {State.input; alive=true; listeners=[]} in
    let outside event =
      if state.alive then Js.Opt.iter event##.target (fun target ->
        Js.Opt.iter (closest element ".advisor-navigation") (fun root ->
          let inside : bool Js.t = Js.Unsafe.meth_call root "contains" [|Js.Unsafe.inject target|] in
          if not (Js.to_bool inside) then state.input.close false));
      Js._true in
    let pointer = Dom_html.addEventListener Dom_html.document (Dom_html.Event.make "pointerdown") (Dom_html.handler outside) Js._false in
    let focus_listener = Dom_html.addEventListener Dom_html.document (Dom_html.Event.make "focusin") (Dom_html.handler outside) Js._false in
    let scroll = Dom_html.addEventListener Dom_html.window Dom_html.Event.scroll
      (Dom_html.handler (fun _ -> if state.alive then state.input.close false; Js._true)) Js._false in
    let resize = Dom_html.addEventListener Dom_html.window Dom_html.Event.resize
      (Dom_html.handler (fun _ -> place state element; Js._true)) Js._false in
    let keyboard = Dom_html.addEventListener element Dom_html.Event.keydown (Dom_html.handler (fun event ->
      if state.alive then (
        let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
        let move items delta edge =
          match items with [] -> () | _ ->
            let index = List.findi items ~f:(fun _ item -> Js.Opt.case Dom_html.document##.activeElement (fun () -> false) (phys_equal item))
              |> Option.map ~f:fst |> Option.value ~default:(if delta < 0 then 0 else -1) in
            let next = match edge with Some index -> index | None -> (index + delta + List.length items) mod List.length items in
            focus (List.nth_exn items next) in
        let handled = match key with
          | "Escape" -> state.input.close true; true
          | "ArrowDown" -> move (menu_items element) 1 None; true
          | "ArrowUp" -> move (menu_items element) (-1) None; true
          | "Home" -> move (menu_items element) 0 (Some 0); true
          | "End" -> let items = menu_items element in move items 0 (Some (List.length items - 1)); true
          | "Tab" when is_mobile () ->
              move (buttons element "button:not(:disabled):not([aria-disabled=true])") (if Js.to_bool event##.shiftKey then -1 else 1) None; true
          | "Tab" -> state.input.close false; false
          | "k" when Js.to_bool event##.ctrlKey || Js.to_bool event##.metaKey -> state.input.close false; false
          | _ -> false in
        if handled then (Dom.preventDefault event; Dom_html.stopPropagation event));
      Js._true)) Js._false in
    state.listeners <- [pointer;focus_listener;scroll;resize;keyboard]; state

  let on_mount = `Schedule_animation_frame (fun _ state element ->
    if state.State.alive then (place state element; focus_first element))
  let update ~old_input:_ ~new_input state _ = state.State.input <- new_input
  let destroy _ state _ =
    state.State.alive <- false; List.iter state.listeners ~f:Dom_html.removeEventListener; state.listeners <- []
end)
