(* Dropping a bank statement onto an account: a small state machine,

     Idle -> Reading -> Previewing -> Preview -> Importing -> Done
                                          \-> Failed (from anywhere)

   The preview is the server's dry run (new rows, duplicates, entries it confirms, whether the
   balance will agree); nothing is posted until Import is pressed. *)
open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

type phase =
  | Idle
  | Reading of string
  | Previewing of string
  | Preview of { file : string; content : string; preview : J.t }
  | Importing of string
  | Done of J.t
  | Failed of string

(* Bank exports are often Latin-1; send the server valid UTF-8 either way. *)
let to_utf8 bytes =
  if Stdlib.String.is_valid_utf_8 bytes then bytes
  else
    let buffer = Buffer.create (String.length bytes * 2) in
    String.iter bytes ~f:(fun c -> Stdlib.Buffer.add_utf_8_uchar buffer (Uchar.of_scalar_exn (Char.to_int c)));
    Buffer.contents buffer

let read_file (file : File.file Js.t) =
  let ivar = Ivar.create () in
  let reader = new%js File.fileReader in
  reader##.onload :=
    Dom.handler (fun _ ->
        let bytes =
          match Js.Opt.to_option (File.CoerceTo.arrayBuffer reader##.result) with Some buffer -> Typed_array.String.of_arrayBuffer buffer | None -> ""
        in
        Ivar.fill_if_empty ivar (to_utf8 bytes);
        Js._false);
  reader##readAsArrayBuffer file;
  Ivar.read ivar

let read_effect = Effect.of_deferred_fun read_file

let first_file (files : File.fileList Js.t) = Js.Opt.to_option (files##item 0)

let component ~(ctx : Ctx.t) ~(account_id : string Bonsai.t) graph =
  let phase, set_phase = Bonsai.state Idle graph in
  let dragging, set_dragging = Bonsai.state false graph in
  let start =
    let%arr set_phase and account_id in
    fun (file : File.file Js.t) ->
      let name = Js.to_string file##.name in
      let%bind.Effect () = set_phase (Reading name) in
      let%bind.Effect content = read_effect file in
      let%bind.Effect () = set_phase (Previewing name) in
      let args = `Assoc [ ("account_id", `String account_id); ("content", `String content); ("file_name", `String name) ] in
      match%bind.Effect Api.run "import_statement" args with
      | Ok preview -> set_phase (Preview { file = name; content; preview })
      | Error message -> set_phase (Failed message)
  in
  let confirm =
    let%arr set_phase and account_id and changed = ctx.changed in
    fun ~file ~content ->
      let%bind.Effect () = set_phase (Importing file) in
      let args = `Assoc [ ("account_id", `String account_id); ("content", `String content); ("file_name", `String file); ("confirm", `Bool true) ] in
      match%bind.Effect Api.run "import_statement" args with
      | Ok result -> Effect.Many [ set_phase (Done result); changed ]
      | Error message -> set_phase (Failed message)
  in
  let%arr phase and set_phase and dragging and set_dragging and start and confirm and locale = ctx.locale and session = ctx.session and navigate = ctx.navigate in
  let currency = J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
  let drop_zone label =
    Vdom.Node.label
      ~attrs:
        [ Vdom.Attr.class_ ("dropzone" ^ if dragging then " dragging" else "");
          Vdom.Attr.on_dragover (fun _ -> Effect.Many [ Effect.Prevent_default; set_dragging true ]);
          Vdom.Attr.on_dragleave (fun _ -> set_dragging false);
          Vdom.Attr.on_drop (fun event ->
              let file = Option.bind (Js.Optdef.to_option (Js.Unsafe.coerce event)##.dataTransfer) ~f:(fun dt -> first_file dt##.files) in
              Effect.Many ([ Effect.Prevent_default; set_dragging false ] @ match file with Some f -> [ start f ] | None -> [])) ]
      [ Ui.span ~cls:"dropzone-label" [ Ui.text label ];
        Vdom.Node.input
          ~attrs:
            [ Vdom.Attr.type_ "file"; Vdom.Attr.create "accept" ".csv,text/csv,text/plain";
              Vdom.Attr.on_file_input (fun _ files -> match first_file files with Some f -> start f | None -> Effect.Ignore) ]
          () ]
  in
  match phase with
  | Idle -> drop_zone (Msg.card_import_drop locale)
  | Reading file | Previewing file | Importing file -> Ui.div ~cls:"import-progress" [ Ui.div ~cls:"shimmer" []; Ui.muted (Msg.card_import_reading ~file locale) ]
  | Failed message ->
      Ui.div ~cls:"import-result error" [ Ui.p (Msg.error_generic ~message locale); Ui.button (Msg.action_close locale) (set_phase Idle) ]
  | Preview { file; content; preview } ->
      let rows = J.list (J.field "rows" preview) in
      let count status = List.count rows ~f:(fun r -> String.equal (J.str (J.field "status" r)) status) in
      let balance = J.field "balance_check" preview in
      let already = String.equal (J.str (J.field "status" preview)) "already_imported" in
      Ui.div ~cls:"import-preview"
        ([ Ui.div ~cls:"import-counts"
             [ Ui.kpi "" (Msg.card_import_new ~count:(count "new") locale); Ui.kpi "" (Msg.card_import_duplicates ~count:(count "duplicate") locale);
               Ui.kpi "" (Msg.card_import_matched ~count:(count "confirms_pending") locale) ] ]
        @ (if J.is_null balance then []
           else
             let money json = Fmt.money_json ~code:currency json in
             let statement = money (J.field "statement_balance" balance) and ledger = money (J.field "ledger_balance" balance) in
             if J.bool (J.field "agrees" balance) then [ Ui.div ~cls:"ok" [ Ui.text (Msg.card_import_balance_ok ~balance:statement locale) ] ]
             else
               [ Ui.div ~cls:"warning"
                   [ Ui.text (Msg.card_import_balance_off ~difference:{ statement with minor = Int64.(statement.minor - ledger.minor) } ~balance:statement locale) ] ])
        @ [ (if already then Ui.muted (Msg.card_import_already locale) else Vdom.Node.none);
            Ui.div ~cls:"actions"
              [ Ui.button ~kind:"primary" ~disabled:already (Msg.action_import locale) (confirm ~file ~content); Ui.button (Msg.action_cancel locale) (set_phase Idle) ] ])
  | Done result ->
      let inbox = J.int (J.field "inbox" result) in
      Ui.div ~cls:"import-result ok"
        [ Ui.p (Msg.card_import_done ~count:inbox locale);
          Ui.div ~cls:"actions"
            [ (if inbox > 0 then Ui.button ~kind:"primary" (Msg.action_review locale) (navigate Route.Inbox) else Vdom.Node.none);
              Ui.button (Msg.action_close locale) (set_phase Idle) ] ]
