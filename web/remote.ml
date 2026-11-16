(* Server data in a component: runs a command when the component appears and again whenever its
   arguments change or anything was written (the version), keeping the last result on screen
   while it reloads. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type t = Loading | Loaded of J.t | Failed of { failure : Api.failure; retry : unit Effect.t }

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
    let%arr set_result and changed = ctx.changed in
    (* Trying again shows that it is asking, and reloads everything showing: whatever failed beside this failed too. *)
    let retry = Effect.Many [ set_result Loading; changed ] in
    fun key ->
      let%bind.Effect () = Effect.of_thunk ignore in
      let _, _, showing = key in
      if not !active || (not showing) || Option.value_map !requested ~default:false ~f:(equal_key key) then Effect.Ignore
      else
        let%bind.Effect ticket = Effect.of_thunk (fun () ->
          incr generation; requested := Some key; !generation) in
        let args, _, _ = key in
        let%bind.Effect response = Api.read_effect (name, args) in
        if !active && ticket = !generation then
          set_result (match response with Ok json -> Loaded json | Error failure -> Failed { failure; retry })
        else Effect.Ignore
  in
  let activate = let%arr request and key in
    Effect.Many [Effect.of_thunk (fun () -> active := true); request key] in
  let deactivate = Effect.of_thunk (fun () ->
    active := false; incr generation; requested := None) in
  Bonsai.Edge.lifecycle ~on_activate:activate ~on_deactivate:(Bonsai.return deactivate) graph;
  Bonsai.Edge.on_change ~equal:equal_key key ~callback:request graph;
  result

(* Something didn't load: said once, in plain words that say which part failed, with the way to try again. *)
let failed ~locale ~(broken : Api.broken) ~retry =
  let says = match broken with Unreachable -> Msg.load_unreachable locale | Server_error -> Msg.load_server_error locale in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "load-failed"; Vdom.Attr.create "role" "alert" ]
    [ Vdom.Node.p [ Vdom.Node.text says ]; Ui.button (Msg.load_retry locale) retry ]

(* The server said no in words of its own: there is nothing to try again. *)
let refused message = Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "load-failed" ] [ Vdom.Node.p [ Vdom.Node.text (String.capitalize message) ] ]

let view ~locale result ~f =
  match result with
  | Loading -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "loading" ] [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "shimmer" ] [] ]
  | Failed { failure = Broken broken; retry } -> failed ~locale ~broken ~retry
  | Failed { failure = Refused message; _ } -> refused message
  | Loaded json -> f json
