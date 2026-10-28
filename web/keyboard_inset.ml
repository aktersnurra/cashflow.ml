(* How much of the page an on-screen keyboard covers, as --keyboard-inset on <html>, so the advisor's composer can sit on
   top of it. Android shrinks the page for the keyboard (interactive-widget in index.html), leaving this 0; iOS
   doesn't, and only its visual viewport says where the keyboard begins. *)
open! Core
open Js_of_ocaml

let install () =
  Js.Optdef.iter (Js.Unsafe.get Dom_html.window "visualViewport") (fun viewport ->
      let update () =
        let height : float = Js.Unsafe.get viewport "height" and top : float = Js.Unsafe.get viewport "offsetTop" in
        let zoom : float = Js.Unsafe.get viewport "scale" in
        (* Pinch zoom shrinks the visual viewport too; that is not a keyboard. *)
        let inset = if Float.(zoom > 1.01) then 0. else Float.max 0. (Float.of_int Dom_html.window##.innerHeight -. height -. top) in
        ignore
          (Dom_html.document##.documentElement##.style##setProperty (Js.string "--keyboard-inset") (Js.string (sprintf "%.0fpx" inset))
             Js.undefined)
      in
      let listen name =
        ignore (Js.Unsafe.meth_call viewport "addEventListener" [| Js.Unsafe.inject (Js.string name); Js.Unsafe.inject (Js.wrap_callback (fun _ -> update ())) |] : unit)
      in
      listen "resize";
      listen "scroll";
      update ())
