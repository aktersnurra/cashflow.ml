(* Phones: the page stays put under the on-screen keyboard, as in a native app.

   While a text field has focus on a touch screen, <html> carries the class "typing" (the tab bar steps
   aside and the advisor's composer sits on the keyboard), and --keyboard holds how much of the layout
   viewport the keyboard covers: iOS Safari overlays the keyboard instead of resizing the page, so a
   sticky composer would sit behind it. Chrome on Android resizes the page itself
   (interactive-widget=resizes-content), and there --keyboard stays 0.

   Pinch zoom is refused too: the viewport tag says so, iOS Safari ignores that for pinching, and its
   gesture events are the only way to stop it. *)
open Js_of_ocaml

let touch () =
  Js.to_bool (Js.Unsafe.meth_call Dom_html.window "matchMedia" [| Js.Unsafe.inject (Js.string "(hover: none) and (pointer: coarse)") |])##.matches

let text_field (element : Dom_html.element Js.t) =
  match String.lowercase_ascii (Js.to_string element##.tagName) with
  | "textarea" | "select" -> true
  | "input" ->
      let kind = String.lowercase_ascii (Js.to_string (Js.Unsafe.get element "type")) in
      not (List.mem kind [ "checkbox"; "radio"; "button"; "submit"; "file"; "range"; "color" ])
  | _ -> Js.to_bool (Js.Unsafe.get element "isContentEditable")

let install () =
  let root = Dom_html.document##.documentElement in
  let set_covered px = ignore (Js.Unsafe.meth_call root##.style "setProperty" [| Js.Unsafe.inject (Js.string "--keyboard"); Js.Unsafe.inject (Js.string (string_of_int px ^ "px")) |]) in
  let viewport = Js.Optdef.to_option (Js.Unsafe.get Dom_html.window "visualViewport") in
  let measure () =
    match viewport with
    | None -> ()
    | Some vv ->
        let inner : float = Js.Unsafe.get Dom_html.window "innerHeight" in
        let height : float = Js.Unsafe.get vv "height" and offset : float = Js.Unsafe.get vv "offsetTop" in
        set_covered (max 0 (int_of_float (inner -. height -. offset)))
  in
  let typing on =
    if on then root##.classList##add (Js.string "typing") else root##.classList##remove (Js.string "typing");
    measure ()
  in
  if touch () then (
    let focused (event : Dom_html.event Js.t) =
      Js.Opt.case event##.target (fun () -> false) (fun target -> text_field target)
    in
    ignore (Dom_html.addEventListener Dom_html.document (Dom_html.Event.make "focusin") (Dom_html.handler (fun e -> if focused e then typing true; Js._true)) Js._false);
    ignore
      (Dom_html.addEventListener Dom_html.document (Dom_html.Event.make "focusout")
         (Dom_html.handler (fun _ ->
              (* Focus moving to another field fires focusout then focusin: look once it has settled. *)
              ignore
                (Dom_html.window##setTimeout
                   (Js.wrap_callback (fun () ->
                        let still =
                          Js.Opt.case Dom_html.document##.activeElement (fun () -> false) (fun active -> text_field active)
                        in
                        typing still))
                   50.);
              Js._true))
         Js._false);
    Option.iter
      (fun vv ->
        let listen name = ignore (Js.Unsafe.meth_call vv "addEventListener" [| Js.Unsafe.inject (Js.string name); Js.Unsafe.inject (Js.wrap_callback (fun _ -> measure ())) |]) in
        listen "resize";
        listen "scroll")
      viewport;
    (* iOS Safari pinch zoom, which it allows whatever the viewport tag says. *)
    List.iter
      (fun name ->
        ignore
          (Dom_html.addEventListener Dom_html.document (Dom_html.Event.make name)
             (Dom_html.handler (fun (e : Dom_html.event Js.t) -> Dom.preventDefault e; Js._false))
             Js._false))
      [ "gesturestart"; "gesturechange" ])
