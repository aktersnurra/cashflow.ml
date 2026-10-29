(* The accountant's desk: the one place things are handed over. Type a question and it goes to the advisor;
   drop statements on the desk (or anywhere on the page, or pick them with the paperclip) and each is read,
   matched to its account and filed, the way an accountant files what lands on their desk: without asking
   when nothing needs the person, with the questions it must ask when something does (Import_widget, one
   per file). There is no separate import page to find. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

type t = {
  view : Vdom.Node.t;
  drop : Vdom.Attr.t;  (** for the page around the desk: dropping files anywhere hands them over *)
  dragging : bool;
  files : Vdom.Node.t;  (** what was handed over and what became of it, for the advisor page, which has its own composer *)
}

let files_of (list : File.fileList Js.t) = List.filter_map (List.init list##.length ~f:Fn.id) ~f:(fun i -> Js.Opt.to_option (list##item i))

let component ~(ctx : Ctx.t) ~(on_question : (string -> unit Effect.t) Bonsai.t) graph =
  let files, set_files = Bonsai.state Int.Map.empty graph in
  let next, set_next = Bonsai.state 0 graph in
  let draft, set_draft = Bonsai.state "" graph in
  let dragging, set_dragging = Bonsai.state false graph in
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
  let%arr imports and draft and set_draft and dragging and set_dragging and hand_over and on_question and locale = ctx.locale in
  let send () = if String.is_empty (String.strip draft) then Effect.Ignore else Effect.Many [ on_question (String.strip draft); set_draft "" ] in
  let dropped event =
    let files = Option.value_map (Js.Optdef.to_option (Js.Unsafe.coerce event)##.dataTransfer) ~default:[] ~f:(fun dt -> files_of dt##.files) in
    Effect.Many ([ Effect.Prevent_default; set_dragging false ] @ if List.is_empty files then [] else [ hand_over files ])
  in
  let drop =
    Vdom.Attr.many
      [ Vdom.Attr.on_dragover (fun _ -> Effect.Many [ Effect.Prevent_default; (if dragging then Effect.Ignore else set_dragging true) ]);
        Vdom.Attr.on_dragleave (fun event ->
            (* Leaving for a child is not leaving the page. *)
            if Js.Opt.test (Js.Unsafe.coerce event)##.relatedTarget then Effect.Ignore else set_dragging false);
        Vdom.Attr.on_drop dropped ]
  in
  let attach =
    Vdom.Node.label
      ~attrs:[ Vdom.Attr.class_ "desk-attach"; Vdom.Attr.title (Msg.desk_attach locale); Vdom.Attr.create "aria-label" (Msg.desk_attach locale) ]
      [ Feather_icon.svg ~size:(`Px 20) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Paperclip;
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
      [ Feather_icon.svg ~size:(`Px 20) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Arrow_up ]
  in
  let files = if Map.is_empty imports then Vdom.Node.none else Ui.div ~cls:"desk-files" (Map.data imports) in
  let view =
    Ui.div ~cls:("desk" ^ if dragging then " dragging" else "")
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "desk-composer"; drop ]
          [ Vdom.Node.textarea
              ~attrs:
                [ Vdom.Attr.class_ "desk-input"; Vdom.Attr.create "rows" "1"; Vdom.Attr.create "aria-label" (Msg.desk_placeholder locale);
                  Vdom.Attr.placeholder (if dragging then Msg.desk_drop locale else Msg.desk_placeholder locale); Vdom.Attr.value_prop draft;
                  Vdom.Attr.on_input (fun _ v -> set_draft v);
                  Vdom.Attr.on_keydown (fun event ->
                      let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
                      if String.equal key "Enter" && not (Js.to_bool event##.shiftKey) then Effect.Many [ Effect.Prevent_default; send () ] else Effect.Ignore) ]
              [];
            Ui.div ~cls:"desk-bottom" [ attach; send_button ] ];
        files ]
  in
  { view; drop; dragging; files }
