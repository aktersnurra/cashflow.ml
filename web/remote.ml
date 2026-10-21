(* Server data in a component: runs a command when the component appears and again whenever its
   arguments change or anything was written (the version), keeping the last result on screen
   while it reloads. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type t = Loading | Loaded of J.t | Failed of string

let load ?(args = Bonsai.return (`Assoc [])) ~name ~(ctx : Ctx.t) graph =
  let result, set_result = Bonsai.state Loading graph in
  let key = Bonsai.both args ctx.version in
  let callback =
    let%arr set_result in
    fun (args, _) ->
      let%bind.Effect response = Api.run name args in
      set_result (match response with Ok json -> Loaded json | Error message -> Failed message)
  in
  Bonsai.Edge.on_change ~equal:(fun (a, v) (b, w) -> Yojson.Safe.equal a b && v = w) key ~callback graph;
  result

let view ~locale result ~f =
  match result with
  | Loading -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "loading" ] [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "shimmer" ] [] ]
  | Failed message -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "error" ] [ Vdom.Node.text (Msg.error_generic ~message locale) ]
  | Loaded json -> f json
