open! Core
open Bonsai_web
open Js_of_ocaml

let input_bytes_limit = 8192
let valid_draft draft = String.length draft <= input_bytes_limit && not (String.is_empty (String.strip draft))

let composer_of (element : Dom_html.element Js.t) : Dom_html.element Js.t Js.opt =
  Js.Unsafe.meth_call element "closest" [| Js.Unsafe.inject (Js.string ".advisor-composer") |]

let current_draft (event : #Dom_html.event Js.t) ~fallback =
  Js.Opt.case event##.target
    (fun () -> Some fallback)
    (fun target ->
      Js.Opt.case (composer_of target)
        (fun () -> None)
        (fun composer ->
          if Js.to_bool (composer##hasAttribute (Js.string "data-over-limit")) then None
          else
            Js.Opt.case
              (composer##querySelector (Js.string "textarea"))
              (fun () -> None)
              (fun element ->
                Js.Opt.case (Dom_html.CoerceTo.textarea element)
                  (fun () -> None)
                  (fun input -> Some (Js.to_string input##.value)))))

(* A page-flow transcript follows its tail only while the reader wants it to.
   A genuinely scrollable embedding still works, without imposing one on the app. *)
module Follow_input = struct
  type t = { following : bool; revision : string; has_messages : bool; on_follow : bool -> unit }

  let sexp_of_t _ = Sexp.Atom "chat-follow"
  let combine _ b = b
end

module Follow = Vdom.Attr.Hooks.Make (struct
  module Input = Follow_input

  module State = struct
    type t = {
      mutable input : Input.t;
      mutable following : bool;
      mutable alive : bool;
      mutable listeners : Dom.event_listener_id list;
    }
  end

  let visible (element : Dom_html.element Js.t) = element##.offsetWidth > 0 && element##.offsetHeight > 0

  let scroll_box (element : Dom_html.element Js.t) =
    let style : Dom_html.cssStyleDeclaration Js.t =
      Js.Unsafe.meth_call Dom_html.window "getComputedStyle" [| Js.Unsafe.inject element |]
    in
    List.mem [ "auto"; "scroll" ] (Js.to_string style##.overflowY) ~equal:String.equal

  let distance (element : Dom_html.element Js.t) =
    if scroll_box element then element##.scrollHeight - element##.scrollTop - element##.clientHeight
    else
      let bottom = element##getBoundingClientRect##.bottom in
      Int.of_float bottom - Dom_html.document##.documentElement##.clientHeight + 20

  let follow state element =
    if state.State.alive && state.following && state.input.has_messages && visible element then
      if scroll_box element then element##.scrollTop := element##.scrollHeight
      else
        let delta = distance element in
        if delta > 0 then
          ignore
            (Js.Unsafe.meth_call Dom_html.window "scrollBy" [| Js.Unsafe.inject 0; Js.Unsafe.inject delta |] : unit)

  let schedule state element =
    ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> follow state element)))

  let init input element =
    let state = { State.input; following = input.Input.following; alive = true; listeners = [] } in
    let changed () =
      (if state.alive && visible element then
         let following = distance element <= 48 in
         if not (Bool.equal following state.following) then (
           state.following <- following;
           state.input.on_follow following));
      Js._true
    in
    let window_id =
      Dom_html.addEventListener Dom_html.window Dom_html.Event.scroll
        (Dom_html.handler (fun _ -> if not (scroll_box element) then changed () else Js._true))
        Js._false
    in
    let element_id =
      Dom_html.addEventListener element Dom_html.Event.scroll
        (Dom_html.handler (fun _ -> if scroll_box element then changed () else Js._true))
        Js._false
    in
    state.listeners <- [ window_id; element_id ];
    state

  let on_mount = `Schedule_animation_frame (fun _ state element -> follow state element)

  let update ~old_input ~new_input state element =
    state.State.input <- new_input;
    if not (Bool.equal old_input.Input.following new_input.Input.following) then state.following <- new_input.following;
    if new_input.following && ((not old_input.following) || not (String.equal old_input.revision new_input.revision))
    then schedule state element

  let destroy _ state _ =
    state.State.alive <- false;
    state.following <- false;
    List.iter state.listeners ~f:Dom_html.removeEventListener;
    state.listeners <- []
end)

module Grow_input = struct
  type t = { draft : string; blocked : bool }

  let sexp_of_t _ = Sexp.Atom "chat-draft"
  let combine _ b = b
end

module Grow = Vdom.Attr.Hooks.Make (struct
  module Input = Grow_input

  module State = struct
    type t = {
      mutable blocked : bool;
      mutable oversized : bool;
      mutable last_valid_input : string;
      mutable listener : Dom.event_listener_id option;
    }
  end

  let resize ~blocked ~oversized (element : Dom_html.element Js.t) =
    element##.style##.height := Js.string "auto";
    let height = Int.min 240 (Int.max 50 (element##.scrollHeight + 2)) in
    element##.style##.height := Js.string (Int.to_string height ^ "px");
    Js.Opt.iter (Dom_html.CoerceTo.textarea element) (fun input ->
        let draft = Js.to_string input##.value in
        if oversized then (
          element##setAttribute (Js.string "aria-invalid") (Js.string "true");
          element##setAttribute (Js.string "aria-describedby") (Js.string "advisor-input-error"))
        else (
          element##removeAttribute (Js.string "aria-invalid");
          element##removeAttribute (Js.string "aria-describedby"));
        Js.Opt.iter (composer_of element) (fun composer ->
            if oversized then composer##setAttribute (Js.string "data-over-limit") (Js.string "")
            else composer##removeAttribute (Js.string "data-over-limit");
            Js.Opt.iter
              (composer##querySelector (Js.string "[data-advisor-send]"))
              (fun send -> Js.Unsafe.set send "disabled" (Js.bool (blocked || oversized || not (valid_draft draft))))))

  let init input element =
    let state =
      { State.blocked = input.Input.blocked; oversized = false; last_valid_input = input.draft; listener = None }
    in
    let listener =
      Dom_html.addEventListener element Dom_html.Event.input
        (Dom_html.handler (fun _ ->
             Js.Opt.iter (Dom_html.CoerceTo.textarea element) (fun input ->
                 let draft = Js.to_string input##.value in
                 state.oversized <- String.length draft > input_bytes_limit;
                 if not state.oversized then state.last_valid_input <- draft);
             resize ~blocked:state.blocked ~oversized:state.oversized element;
             Js._true))
        Js._false
    in
    state.listener <- Some listener;
    state

  let on_mount =
    `Schedule_animation_frame
      (fun _ state element -> resize ~blocked:state.State.blocked ~oversized:state.oversized element)

  let update ~old_input:_ ~new_input state element =
    state.State.blocked <- new_input.Input.blocked;
    (* An acknowledgement of the previous valid input must not clear rejection
       of a later oversized paste, even if the controlled value rewinds. *)
    if not (String.equal new_input.draft state.last_valid_input) then (
      state.oversized <- false;
      state.last_valid_input <- new_input.draft);
    resize ~blocked:state.blocked ~oversized:state.oversized element

  let destroy _ state _ =
    Option.iter state.State.listener ~f:Dom_html.removeEventListener;
    state.listener <- None;
    state.last_valid_input <- ""
end)
