(* The accountant's desk: the one place things are handed over. Type a question and it goes to the advisor;
   drop statements on the desk (or anywhere on the page, or pick them with the paperclip) and each is read,
   matched to its account and filed, the way an accountant files what lands on their desk: without asking
   when nothing needs the person, with the questions it must ask when something does (Import_widget, one
   per file). There is no separate import page to find.

   It stays out of the way until wanted: at the foot of the page only its handle shows (a grab bar on a phone,
   a chevron on a computer). Swiping it up or tapping it opens the box with the cursor in it; swiping down,
   the chevron or Escape puts it away. Dragging files over the page opens it. What was handed over is shown
   in the page, not in the box, so it stays in sight when the box is put away. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

type t = {
  view : Vdom.Node.t;  (** the box, at the foot of the page *)
  drop : Vdom.Attr.t;  (** for the page around the desk: dropping files anywhere hands them over *)
  dragging : bool;
  files : Vdom.Node.t;  (** what was handed over and what became of it *)
  attach : Vdom.Node.t;  (** the "+" that picks files to hand over, for the advisor's composer too *)
}

let input_id = "desk-input"

(* Focused in the same gesture that opens the box, so a phone shows its keyboard. *)
let focus_input = Effect.of_thunk (fun () -> Option.iter (Dom_html.getElementById_opt input_id) ~f:(fun el -> el##focus))

let files_of (list : File.fileList Js.t) = List.filter_map (List.init list##.length ~f:Fn.id) ~f:(fun i -> Js.Opt.to_option (list##item i))

let component ~(ctx : Ctx.t) ~(on_question : (string -> unit Effect.t) Bonsai.t) graph =
  let files, set_files = Bonsai.state Int.Map.empty graph in
  let next, set_next = Bonsai.state 0 graph in
  let draft, set_draft = Bonsai.state "" graph in
  let dragging, set_dragging = Bonsai.state false graph in
  let opened, set_opened = Bonsai.state false graph in
  (* Where a swipe on the handle began. *)
  let pressed, set_pressed = Bonsai.state None graph in
  let hand_over =
    let%arr files and set_files and next and set_next in
    fun (incoming : File.file Js.t list) ->
      let files, next = List.fold incoming ~init:(files, next) ~f:(fun (files, n) f -> (Map.set files ~key:n ~data:f, n + 1)) in
      Effect.Many [ set_files files; set_next next ]
  in
  let remove = let%arr files and set_files in fun id -> set_files (Map.remove files id) in
  let imports =
    Bonsai.assoc (module Int) files
      ~f:(fun id file graph ->
        Import_widget.component ~ctx ~account_id:(Bonsai.return None) ~incoming:(let%arr file in Some file)
          ~on_close:(let%arr remove and id in remove id) graph)
      graph
  in
  let%arr imports and draft and set_draft and dragging and set_dragging and hand_over and on_question and opened and set_opened and pressed and set_pressed
  and locale = ctx.locale in
  let open_box = Effect.Many [ set_opened true; focus_input ] in
  let close_box = Effect.Many [ set_opened false; Effect.of_thunk (fun () -> Option.iter (Dom_html.getElementById_opt input_id) ~f:(fun el -> el##blur)) ] in
  (* The text as it is in the field now: Enter can come before the last keystroke has been rendered. *)
  let current () =
    match Dom_html.getElementById_opt input_id with
    | Some el -> (match Js.Opt.to_option (Dom_html.CoerceTo.textarea el) with Some t -> Js.to_string t##.value | None -> draft)
    | None -> draft
  in
  let send () =
    let text = String.strip (current ()) in
    if String.is_empty text then Effect.Ignore else Effect.Many [ on_question text; set_draft ""; set_opened false ]
  in
  let dropped event =
    let files = Option.value_map (Js.Optdef.to_option (Js.Unsafe.coerce event)##.dataTransfer) ~default:[] ~f:(fun dt -> files_of dt##.files) in
    Effect.Many ([ Effect.Prevent_default; set_dragging false ] @ if List.is_empty files then [] else [ hand_over files ])
  in
  let drop =
    Vdom.Attr.many
      [ Vdom.Attr.on_dragover (fun _ -> Effect.Many [ Effect.Prevent_default; (if dragging then Effect.Ignore else Effect.Many [ set_dragging true; set_opened true ]) ]);
        Vdom.Attr.on_dragleave (fun event ->
            (* Leaving for a child is not leaving the page. *)
            if Js.Opt.test (Js.Unsafe.coerce event)##.relatedTarget then Effect.Ignore else set_dragging false);
        Vdom.Attr.on_drop dropped ]
  in
  let attach =
    Vdom.Node.label
      ~attrs:[ Vdom.Attr.class_ "desk-attach"; Vdom.Attr.title (Msg.desk_attach locale); Vdom.Attr.create "aria-label" (Msg.desk_attach locale) ]
      [ Feather_icon.svg ~size:(`Px 22) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Plus;
        Vdom.Node.input
          ~attrs:
            [ Vdom.Attr.type_ "file"; Vdom.Attr.create "multiple" "";
              Vdom.Attr.create "accept" ".csv,.xlsx,.zip,text/csv,text/plain,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,application/zip";
              Vdom.Attr.on_file_input (fun _ list -> hand_over (files_of list)) ]
          () ]
  in
  let send_button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ "desk-send"; Vdom.Attr.type_ "button"; Vdom.Attr.create "aria-label" (Msg.desk_send locale); Vdom.Attr.title (Msg.desk_send locale);
          (if String.is_empty (String.strip draft) then Vdom.Attr.disabled else Vdom.Attr.empty); Vdom.Attr.on_click (fun _ -> send ()) ]
      (* The return key, as on the advisor's composer. *)
      [ Vdom.Node.create_svg "svg"
          ~attrs:
            [ Vdom.Attr.create "viewBox" "0 0 24 24"; Vdom.Attr.create "width" "20"; Vdom.Attr.create "height" "20"; Vdom.Attr.create "fill" "none";
              Vdom.Attr.create "stroke" "currentColor"; Vdom.Attr.create "stroke-width" "2"; Vdom.Attr.create "stroke-linecap" "round";
              Vdom.Attr.create "stroke-linejoin" "round"; Vdom.Attr.create "aria-hidden" "true" ]
          [ Vdom.Node.create_svg "path" ~attrs:[ Vdom.Attr.create "d" "M20 5v6a4 4 0 0 1-4 4H5M9 11l-4 4 4 4" ] [] ] ]
  in
  let files = if Map.is_empty imports then Vdom.Node.none else Ui.div ~cls:"desk-files" (Map.data imports) in
  (* The handle: a swipe up opens, a swipe down closes, a tap (or a click) toggles. *)
  let handle =
    let y (e : Dom_html.pointerEvent Js.t) = Float.to_int (Js.Unsafe.get e "clientY") in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ "desk-handle"; Vdom.Attr.type_ "button";
          Vdom.Attr.create "aria-label" (if opened then Msg.desk_hide locale else Msg.desk_placeholder locale);
          Vdom.Attr.create "aria-expanded" (Bool.to_string opened); Vdom.Attr.title (if opened then Msg.desk_hide locale else Msg.desk_placeholder locale);
          Vdom.Attr.on_pointerdown (fun e ->
              (* Keep the pointer until it is released, wherever it goes: the end of a swipe is off the handle. *)
              Effect.Many
                [ Effect.of_thunk (fun () ->
                      Js.Opt.iter e##.target (fun t -> ignore (Js.Unsafe.meth_call t "setPointerCapture" [| Js.Unsafe.inject (Js.Unsafe.get e "pointerId") |])));
                  set_pressed (Some (y e)) ]);
          Vdom.Attr.on_pointerup (fun e ->
              let moved = match pressed with Some start -> y e - start | None -> 0 in
              Effect.Many
                [ set_pressed None;
                  (if moved < -16 then open_box else if moved > 16 then close_box else if opened then close_box else open_box) ]);
          Vdom.Attr.on_keydown (fun e ->
              let key = Js.Optdef.case e##.key (fun () -> "") Js.to_string in
              if String.equal key "Enter" || String.equal key " " then Effect.Many [ Effect.Prevent_default; (if opened then close_box else open_box) ] else Effect.Ignore) ]
      [ Ui.span ~cls:"desk-grabber" [];
        Ui.span ~cls:"desk-chevron" [ Feather_icon.svg ~size:(`Px 18) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) (if opened then Chevron_down else Chevron_up) ];
        Ui.span ~cls:"desk-handle-label" [ Ui.text (Msg.desk_placeholder locale) ] ]
  in
  let view =
    Ui.div ~cls:("desk" ^ (if dragging then " dragging" else "") ^ if opened then " open" else "")
      [ handle;
        Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "desk-composer"; drop ]
          [ attach;
            Vdom.Node.textarea
              ~attrs:
                [ Vdom.Attr.class_ "desk-input"; Vdom.Attr.id input_id; Vdom.Attr.create "rows" "1"; Vdom.Attr.create "aria-label" (Msg.desk_placeholder locale);
                  Vdom.Attr.placeholder (if dragging then Msg.desk_drop locale else Msg.desk_placeholder locale); Vdom.Attr.value_prop draft;
                  (if opened then Vdom.Attr.empty else Vdom.Attr.create "tabindex" "-1");
                  Vdom.Attr.on_input (fun _ v -> set_draft v);
                  Vdom.Attr.on_keydown (fun event ->
                      let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
                      if String.equal key "Enter" && not (Js.to_bool event##.shiftKey) then Effect.Many [ Effect.Prevent_default; send () ]
                      else if String.equal key "Escape" then close_box
                      else Effect.Ignore) ]
              [];
            Ui.div ~cls:"desk-bottom" [ send_button ] ] ]
  in
  { view; drop; dragging; files; attach }
