(* Server data in a component: runs a command when the component appears and again whenever its
   arguments change or anything was written (the version), keeping the last result on screen
   while it reloads. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type t = Loading | Loaded of J.t | Failed of string

(* [active] says whether anyone can see the result. A page that stays mounted behind others (Home) passes whether it is
   showing, so a write elsewhere does not recompute its figures while hidden; it reloads when it appears again. *)
let load ?(args = Bonsai.return (`Assoc [])) ?(active = Bonsai.return true) ~name ~(ctx : Ctx.t) graph =
  let result, set_result = Bonsai.state Loading graph in
  let key = let%arr args and version = ctx.version and showing = active in (args, version, showing) in
  (* Monotonic outside the resettable model: an old callback must not become current
     again after a privacy reset or an unchanged-arguments reactivation. *)
  let generation = ref 0 and active = ref false and requested = ref None in
  let equal_key (a, v, x) (b, w, y) = Yojson.Safe.equal a b && Int.equal v w && Bool.equal x y in
  let request =
    let%arr set_result in
    fun key ->
      let%bind.Effect () = Effect.of_thunk ignore in
      let _, _, showing = key in
      if not !active || (not showing) || Option.value_map !requested ~default:false ~f:(equal_key key) then Effect.Ignore
      else
        let%bind.Effect ticket = Effect.of_thunk (fun () ->
          incr generation; requested := Some key; !generation) in
        let args, _, _ = key in
        let%bind.Effect response = Api.run name args in
        if !active && ticket = !generation then
          set_result (match response with Ok json -> Loaded json | Error message -> Failed message)
        else Effect.Ignore
  in
  let activate = let%arr request and key in
    Effect.Many [Effect.of_thunk (fun () -> active := true); request key] in
  let deactivate = Effect.of_thunk (fun () ->
    active := false; incr generation; requested := None) in
  Bonsai.Edge.lifecycle ~on_activate:activate ~on_deactivate:(Bonsai.return deactivate) graph;
  Bonsai.Edge.on_change ~equal:equal_key key ~callback:request graph;
  result

let view ~locale result ~f =
  match result with
  | Loading -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "loading" ] [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "shimmer" ] [] ]
  | Failed message -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "error" ] [ Vdom.Node.text (Msg.error_generic ~message locale) ]
  | Loaded json -> f json
