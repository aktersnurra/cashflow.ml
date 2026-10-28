(* Per bank account: how sure the app must be of a category before it files an imported row without
   asking. The server owns the rules; this lists the bank accounts and what each is set to. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let component ~(ctx : Ctx.t) graph =
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let settings = Remote.load ~name:"list_auto_apply" ~ctx graph in
  let%arr accounts and settings and locale = ctx.locale and changed = ctx.changed and notify = ctx.notify in
  let levels = [ ("off", Msg.settings_autoapply_off locale); ("90", Msg.settings_autoapply_90 locale); ("80", Msg.settings_autoapply_80 locale); ("70", Msg.settings_autoapply_70 locale) ] in
  let banks = match accounts with Remote.Loaded json -> List.filter (Accounts_data.of_list ~locale json) ~f:(fun a -> a.statement_backed && not a.closed) | _ -> [] in
  let current id =
    match settings with
    | Remote.Loaded json ->
        List.find_map (J.list (J.field "accounts" json)) ~f:(fun s ->
            if String.equal (J.str (J.field "account_id" s)) id then Some (Int.to_string (J.int (J.field "min_confidence" s))) else None)
        |> Option.value ~default:"off"
    | _ -> "off"
  in
  let options_for id =
    let value = current id in
    if List.Assoc.mem levels value ~equal:String.equal then levels else levels @ [ (value, value ^ "%") ]
  in
  let choose id level =
    let args = [ ("account_id", `String id) ] @ if String.equal level "off" then [] else [ ("min_confidence", `Int (Int.of_string level)) ] in
    match%bind.Effect Api.run "set_auto_apply" (`Assoc args) with Ok _ -> changed | Error message -> notify message
  in
  let row (a : Accounts_data.account) =
    Ui.div ~cls:"row setting-row"
      [ Ui.div [ Ui.div ~cls:"row-title" [ Ui.text a.name ] ];
        Ui.div ~cls:"setting-control" [ Ui.select ~value:(current a.id) ~options:(options_for a.id) ~on_change:(choose a.id) ] ]
  in
  Ui.div
    [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.settings_autoapply_title locale) ] ];
      Ui.div ~cls:"table"
        (Ui.div ~cls:"row" [ Ui.div ~cls:"row-sub" [ Ui.text (Msg.settings_autoapply_hint locale) ] ]
         :: (if List.is_empty banks then [ Ui.div ~cls:"row" [ Ui.muted (Msg.settings_autoapply_empty locale) ] ] else List.map banks ~f:row)) ]
