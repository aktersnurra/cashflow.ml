(* Dropping a bank statement: a small state machine,

     Idle -> Reading -> Previewing -> Preview -> Importing -> Done
                                          \-> Failed (from anywhere)

   Without a fixed account, the file says where it goes: the server reads the account number from
   it and picks the account with those digits, or else proposes a new one named from the file. The
   preview shows that choice and lets it be changed; the name is asked for only when the file
   doesn't give one.

   The preview is the server's dry run (new rows, duplicates, entries it confirms, whether the
   balance will agree); nothing is posted, and no account opened, until Import is pressed. Rows the
   server can't place (ambiguous: identical to rows already imported, on a day the file may not
   cover in full) need a decision each before Import is enabled. *)
open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

type new_account = { name : string; kind : string; owner : string }
type target = Existing of string | Create of new_account

type phase =
  | Idle
  | Reading of string
  | Previewing of string
  | Preview of {
      file : string;
      content : string;
      identity : J.t;  (** what identify_statement said; null when the account is fixed *)
      target : target;
      preview : J.t;
      decisions : (int * string) list;  (** row -> duplicate | new *)
    }
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
        Ivar.fill_if_empty ivar bytes;
        Js._false);
  reader##readAsArrayBuffer file;
  Ivar.read ivar

let read_effect = Effect.of_deferred_fun read_file

(* A spreadsheet (a zip) goes as base64; a CSV as text, converted to UTF-8 if it was Latin-1. *)
let content_args bytes =
  if String.is_prefix bytes ~prefix:"PK\003\004" then [ ("content_base64", `String (Js.to_string (Dom_html.window##btoa (Js.bytestring bytes)))) ]
  else [ ("content", `String (to_utf8 bytes)) ]

let first_file (files : File.fileList Js.t) = Js.Opt.to_option (files##item 0)

(* The new account the file proposes: named as the file names it, or after the bank and number. *)
let proposed_name identity =
  match J.str_opt (J.field "account_name" identity), J.str_opt (J.field "institution" identity), J.str_opt (J.field "number_last4" identity) with
  | Some name, _, _ -> name
  | None, Some bank, Some last4 -> bank ^ " ****" ^ last4
  | None, _, _ -> ""

let target_args identity = function
  | Existing id -> [ ("account_id", `String id) ]
  | Create { name; kind; owner } ->
      (* The name doesn't change the preview; a placeholder stands in until it is typed. *)
      let name = if String.is_empty (String.strip name) then "-" else name in
      [ ( "new_account",
          `Assoc
            [ ("name", `String name); ("kind", `String kind); ("owner", `String owner); ("institution", J.field "institution" identity);
              ("number_last4", J.field "number_last4" identity) ] ) ]

let component ~(ctx : Ctx.t) ~(account_id : string option Bonsai.t) graph =
  let phase, set_phase = Bonsai.state Idle graph in
  let dragging, set_dragging = Bonsai.state false graph in
  let run_preview =
    let%arr set_phase in
    fun ~name ~content ~identity ~target ->
      let%bind.Effect () = set_phase (Previewing name) in
      let args = `Assoc ([ ("file_name", `String name) ] @ target_args identity target @ content_args content) in
      match%bind.Effect Api.run "import_statement" args with
      | Ok preview -> set_phase (Preview { file = name; content; identity; target; preview; decisions = [] })
      | Error message -> set_phase (Failed message)
  in
  let start =
    let%arr set_phase and run_preview and account_id in
    fun (file : File.file Js.t) ->
      let name = Js.to_string file##.name in
      let%bind.Effect () = set_phase (Reading name) in
      let%bind.Effect content = read_effect file in
      match account_id with
      | Some id -> run_preview ~name ~content ~identity:`Null ~target:(Existing id)
      | None -> (
          match%bind.Effect Api.run "identify_statement" (`Assoc (content_args content)) with
          | Error message -> set_phase (Failed message)
          | Ok identity ->
              let target =
                match J.field "account" identity with
                | `Null -> Create { name = proposed_name identity; kind = "asset"; owner = "personal" }
                | account -> Existing (J.str (J.field "id" account))
              in
              run_preview ~name ~content ~identity ~target)
  in
  (* Sets the opening balance the bank's figures imply, then previews again so the check agrees. *)
  let set_opening =
    let%arr set_phase and run_preview in
    fun ~name ~content ~identity ~account_id ~date ~balance ->
      match%bind.Effect Api.run "set_opening_balance" (`Assoc [ ("account_id", `String account_id); ("date", date); ("balance", balance) ]) with
      | Ok _ -> run_preview ~name ~content ~identity ~target:(Existing account_id)
      | Error message -> set_phase (Failed message)
  in
  let confirm =
    let%arr set_phase and changed = ctx.changed in
    fun ~file ~content ~identity ~target ~decisions ->
      let%bind.Effect () = set_phase (Importing file) in
      let decisions = `List (List.map decisions ~f:(fun (row, decision) -> `Assoc [ ("row", `Int row); ("decision", `String decision) ])) in
      let args =
        `Assoc ([ ("file_name", `String file); ("decisions", decisions); ("confirm", `Bool true) ] @ target_args identity target @ content_args content)
      in
      match%bind.Effect Api.run "import_statement" args with
      | Ok result -> Effect.Many [ set_phase (Done result); changed ]
      | Error message -> set_phase (Failed message)
  in
  let%arr phase and set_phase and dragging and set_dragging and start and run_preview and confirm and set_opening and account_id
  and locale = ctx.locale and session = ctx.session and navigate = ctx.navigate in
  let currency = J.str ~default:"SEK" (J.path [ "user"; "default_currency" ] session) in
  (* A tap opens the phone's file picker; on a computer a file can also be dropped on it. *)
  let drop_zone =
    Vdom.Node.label
      ~attrs:
        [ Vdom.Attr.class_ ("dropzone" ^ if dragging then " dragging" else "");
          Vdom.Attr.on_dragover (fun _ -> Effect.Many [ Effect.Prevent_default; set_dragging true ]);
          Vdom.Attr.on_dragleave (fun _ -> set_dragging false);
          Vdom.Attr.on_drop (fun event ->
              let file = Option.bind (Js.Optdef.to_option (Js.Unsafe.coerce event)##.dataTransfer) ~f:(fun dt -> first_file dt##.files) in
              Effect.Many ([ Effect.Prevent_default; set_dragging false ] @ match file with Some f -> [ start f ] | None -> [])) ]
      [ Ui.span ~cls:"dropzone-icon" [ Feather_icon.svg ~size:(`Px 22) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Upload ];
        Ui.span ~cls:"dropzone-label" [ Ui.text (Msg.import_choose locale) ];
        Ui.span ~cls:"dropzone-hint" [ Ui.text (Msg.import_drop locale) ];
        Vdom.Node.input
          ~attrs:
            [ Vdom.Attr.type_ "file"; Vdom.Attr.create "accept" ".csv,.xlsx,text/csv,text/plain,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
              Vdom.Attr.on_file_input (fun _ files -> match first_file files with Some f -> start f | None -> Effect.Ignore) ]
          () ]
  in
  match phase with
  | Idle -> drop_zone
  | Reading file | Previewing file | Importing file -> Ui.div ~cls:"import-progress" [ Ui.div ~cls:"shimmer" []; Ui.muted (Msg.card_import_reading ~file locale) ]
  | Failed message ->
      Ui.div ~cls:"import-result error" [ Ui.p (Msg.error_generic ~message locale); Ui.button (Msg.action_close locale) (set_phase Idle) ]
  | Preview { file; content; identity; target; preview; decisions } ->
      let rows = J.list (J.field "rows" preview) in
      let ambiguous = List.filter rows ~f:(fun r -> String.equal (J.str (J.field "status" r)) "ambiguous") in
      let undecided = List.exists ambiguous ~f:(fun r -> not (List.Assoc.mem decisions ~equal:Int.equal (J.int (J.field "row" r)))) in
      let decide row decision =
        set_phase (Preview { file; content; identity; target; preview; decisions = (row, decision) :: List.Assoc.remove decisions ~equal:Int.equal row })
      in
      let choice row decision label =
        let chosen = Option.equal String.equal (List.Assoc.find decisions ~equal:Int.equal row) (Some decision) in
        Ui.button ~kind:(if chosen then "active" else "") label (decide row decision)
      in
      let count status = List.count rows ~f:(fun r -> String.equal (J.str (J.field "status" r)) status) in
      let balance = J.field "balance_check" preview in
      let already = String.equal (J.str (J.field "status" preview)) "already_imported" in
      let creating = match target with Create _ -> true | Existing _ -> false in
      let unnamed = match target with Create { name; _ } -> String.is_empty (String.strip name) | Existing _ -> false in
      (* Where the rows go: chosen from the file, and changeable. Changing the account (or what
         kind a new one is) previews again; the name only labels it. *)
      let destination =
        if Option.is_some account_id then []
        else
          let accounts = J.list (J.field "accounts" identity) in
          let label a =
            J.str (J.field "name" a) ^ Option.value_map (J.str_opt (J.field "number" a)) ~default:"" ~f:(fun n -> " ****" ^ n)
          in
          let options = List.map accounts ~f:(fun a -> (J.str (J.field "id" a), label a)) @ [ ("new", Msg.import_new_account locale) ] in
          let chosen = match target with Existing id -> id | Create _ -> "new" in
          let fresh = Create { name = proposed_name identity; kind = "asset"; owner = "personal" } in
          let choose value =
            run_preview ~name:file ~content ~identity ~target:(if String.equal value "new" then fresh else Existing value)
          in
          let edit f = match target with Create c -> set_phase (Preview { file; content; identity; target = f c; preview; decisions }) | Existing _ -> Effect.Ignore in
          [ Ui.div ~cls:"import-destination"
              ([ Ui.field (Msg.import_into locale) (Ui.select ~value:chosen ~options ~on_change:choose) ]
              @
              match target with
              | Existing _ -> []
              | Create { name; kind; owner } ->
                  [ Ui.field (Msg.label_name locale)
                      (Ui.input ~value:name ~on_input:(fun v -> edit (fun c -> Create { c with name = v })) ());
                    Ui.field (Msg.label_kind locale)
                      (Ui.select ~value:kind
                         ~options:[ ("asset", Msg.label_kind_asset locale); ("liability", Msg.label_kind_liability locale) ]
                         ~on_change:(fun v -> run_preview ~name:file ~content ~identity ~target:(Create { name; kind = v; owner })));
                    (if Ctx.has_household session then
                       Ui.field (Msg.label_owner locale)
                         (Ui.select ~value:owner
                            ~options:[ ("personal", Msg.label_personal locale); ("household", Msg.label_household locale) ]
                            ~on_change:(fun v -> edit (fun c -> Create { c with owner = v })))
                     else Vdom.Node.none) ]);
            (if creating && String.is_empty (proposed_name identity) then Ui.div ~cls:"warning" [ Ui.text (Msg.import_name_prompt locale) ] else Vdom.Node.none) ]
      in
      Ui.div ~cls:"import-preview"
        (destination
        @ [ Ui.div ~cls:"import-counts"
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
        @ (match J.field "suggested_opening_balance" preview with
          | `Null -> []
          | opening ->
              let date = J.field "date" opening and balance = J.field "balance" opening in
              match target with
              | Create _ -> [ Ui.div ~cls:"opening" [ Ui.muted (Msg.import_opens_with ~balance:(Fmt.money_json ~code:currency balance) ~date:(Fmt.date_json date) locale) ] ]
              | Existing account_id ->
                  [ Ui.div ~cls:"opening"
                      [ Ui.muted (Msg.card_import_opening ~balance:(Fmt.money_json ~code:currency balance) ~date:(Fmt.date_json date) locale);
                        Ui.button (Msg.action_set_opening_balance locale) (set_opening ~name:file ~content ~identity ~account_id ~date ~balance) ] ])
        @ (if List.is_empty ambiguous || already then []
           else
             [ Ui.div ~cls:"warning" [ Ui.text (Msg.card_import_ambiguous ~count:(List.length ambiguous) locale) ];
               Ui.div ~cls:"list"
                 (List.map ambiguous ~f:(fun r ->
                      let row = J.int (J.field "row" r) in
                      Ui.div ~cls:"row"
                        [ Ui.div
                            [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "description" r)) ];
                              Ui.div ~cls:"row-sub" [ Ui.text (Fmt.format_date locale (J.field "date" r) ^ " · " ^ Fmt.format_money locale ~code:currency (J.field "amount" r)) ] ];
                          Ui.div ~cls:"segmented"
                            [ choice row "duplicate" (Msg.action_already_imported locale); choice row "new" (Msg.action_new_row locale) ] ])) ])
        @ [ (if already then Ui.muted (Msg.card_import_already locale) else Vdom.Node.none);
            Ui.div ~cls:"actions"
              [ Ui.button ~kind:"primary" ~disabled:(already || undecided || unnamed) (Msg.action_import locale) (confirm ~file ~content ~identity ~target ~decisions);
                Ui.button (Msg.action_cancel locale) (set_phase Idle) ] ])
  | Done result ->
      let inbox = J.int (J.field "inbox" result) in
      Ui.div ~cls:"import-result ok"
        [ Ui.p (Msg.card_import_done ~count:inbox locale);
          Ui.div ~cls:"actions"
            [ (if inbox > 0 then Ui.button ~kind:"primary" (Msg.action_review locale) (navigate Route.Inbox) else Vdom.Node.none);
              (match J.str_opt (J.path [ "account"; "id" ] result) with
              | Some id when Option.is_none account_id -> Ui.button (J.str (J.path [ "account"; "name" ] result)) (navigate (Route.Account id))
              | _ -> Vdom.Node.none);
              Ui.button (Msg.action_close locale) (set_phase Idle) ] ]
