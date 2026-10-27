(* Bringing in a bank statement: one file box. The file says which account it is for, so there is
   nothing to pick first; adding a transaction by hand is the way round when there is no file. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let page ~(ctx : Ctx.t) graph =
  let import = Import_widget.component ~ctx ~account_id:(Bonsai.return None) graph in
  let%arr import and locale = ctx.locale and navigate = ctx.navigate in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
    [ Ui.div ~cls:"page-head" [ Ui.div [ Vdom.Node.h1 [ Ui.text (Msg.import_title locale) ]; Ui.div ~cls:"subtitle" [ Ui.text (Msg.import_subtitle locale) ] ] ];
      Ui.div ~cls:"panel import-panel" [ import ];
      Ui.div ~cls:"import-manual"
        [ Vdom.Node.a
            ~attrs:[ Vdom.Attr.href (Route.to_path Route.Add); Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate Route.Add ]) ]
            [ Ui.text (Msg.import_manual locale) ] ] ]
