(* Quick entry, mostly for cash and for spending you want logged before the statement arrives. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type draft = { from_account : string; to_account : string; amount : string; date : string; description : string }

let today () = let d = Js_of_ocaml.(new%js Js.date_now) in Printf.sprintf "%04d-%02d-%02d" d##getFullYear (d##getMonth + 1) d##getDate

let page ~(ctx : Ctx.t) graph =
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let draft, set_draft = Bonsai.state None graph in
  let%arr accounts and draft and set_draft and locale = ctx.locale and navigate = ctx.navigate and notify = ctx.notify and changed = ctx.changed in
  let accounts = match accounts with Loaded json -> List.filter (Accounts_data.of_list json) ~f:(fun a -> not a.closed && Option.is_none a.role) | _ -> [] in
  let sources = List.filter accounts ~f:(fun a -> Accounts_data.is_bank a || String.equal a.kind "income") in
  let destinations = List.filter accounts ~f:(fun a -> Accounts_data.is_bank a || String.equal a.kind "expense") in
  let first l = match l with (a : Accounts_data.account) :: _ -> a.id | [] -> "" in
  let d =
    Option.value draft
      ~default:
        { from_account = first (List.filter sources ~f:Accounts_data.is_bank); to_account = first (List.filter destinations ~f:(fun a -> String.equal a.kind "expense"));
          amount = ""; date = today (); description = "" }
  in
  let set f = set_draft (Some (f d)) in
  let options l = List.map l ~f:(fun (a : Accounts_data.account) -> (a.id, a.name)) in
  let pending = List.exists accounts ~f:(fun a -> a.statement_backed && (String.equal a.id d.from_account || String.equal a.id d.to_account)) in
  let save =
    let args =
      `Assoc
        [ ("idempotency_key", `String (Api.fresh_key "add")); ("from_account_id", `String d.from_account); ("to_account_id", `String d.to_account);
          ("amount", `String (String.substr_replace_all d.amount ~pattern:"," ~with_:".")); ("date", `String d.date); ("description", `String d.description) ]
    in
    match%bind.Effect Api.run "add_transaction" args with
    | Ok json when String.equal (J.str (J.field "status" json)) "error" -> notify (J.str (J.field "error" json))
    | Ok _ -> Effect.Many [ set_draft None; changed; notify (if pending then Msg.add_saved_pending locale else Msg.add_saved locale); navigate Route.Home ]
    | Error message -> notify message
  in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
    [ Ui.div ~cls:"page-head" [ Ui.div [ Vdom.Node.h1 [ Ui.text (Msg.add_title locale) ]; Ui.div ~cls:"subtitle" [ Ui.text (Msg.add_subtitle locale) ] ] ];
      Ui.div ~cls:"panel add-form"
        [ Ui.div ~cls:"amount-field"
            [ Ui.field (Msg.label_amount locale) (Ui.input ~type_:"text" ~value:d.amount ~placeholder:"0,00" ~on_input:(fun v -> set (fun d -> { d with amount = v })) ()) ];
          Ui.field (Msg.label_description locale) (Ui.input ~value:d.description ~on_input:(fun v -> set (fun d -> { d with description = v })) ());
          Ui.div ~cls:"form-row"
            [ Ui.field (Msg.label_from locale) (Ui.select ~value:d.from_account ~options:(options sources) ~on_change:(fun v -> set (fun d -> { d with from_account = v })));
              Ui.field (Msg.label_to locale) (Ui.select ~value:d.to_account ~options:(options destinations) ~on_change:(fun v -> set (fun d -> { d with to_account = v })));
              Ui.field (Msg.label_date locale) (Ui.input ~type_:"date" ~value:d.date ~on_input:(fun v -> set (fun d -> { d with date = v })) ()) ];
          (if pending then Ui.div ~cls:"provisional" [ Ui.text (Msg.add_pending_note locale) ] else Vdom.Node.none);
          Ui.div ~cls:"actions" [ Ui.button ~kind:"primary" ~disabled:(String.is_empty (String.strip d.amount)) (Msg.action_add locale) save ] ] ]
