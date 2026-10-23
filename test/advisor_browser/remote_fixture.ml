(* Injected-response lifecycle proof for the production Remote helper, not authorization proof.
   Built in the existing web-only advisor_browser executable. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let app graph =
  let active, set_active = Bonsai.state true graph in
  let version, set_version = Bonsai.state 0 graph in
  let ctx : Ctx.t = {
    session = Bonsai.return `Null; locale = Bonsai.return "en"; currency = Bonsai.return "SEK";
    version; changed = Bonsai.return Effect.Ignore; navigate = Bonsai.return (fun _ -> Effect.Ignore);
    notify = Bonsai.return (fun _ -> Effect.Ignore); theme = Bonsai.return Theme.System;
    set_theme = Bonsai.return (fun _ -> Effect.Ignore);
  } in
  let surface, reset = Bonsai.with_model_resetter ~f:(fun graph ->
    match%sub active with
    | false -> Bonsai.return Vdom.Node.none
    | true ->
        let remote = Remote.load ~name:"remote_lifetime_fixture" ~ctx graph in
        let%arr remote in
        match remote with
        | Remote.Loading -> Ui.text "waiting"
        | Loaded json -> Ui.text (J.str (J.field "label" json))
        | Failed message -> Ui.text message) graph in
  let clear = let%arr reset in fun active -> if active then Effect.Ignore else reset in
  Bonsai.Edge.on_change ~equal:Bool.equal active ~callback:clear graph;
  let%arr surface and set_active and version and set_version in
  Ui.div [Ui.div ~cls:"remote-result" [surface];
    Ui.button "Deactivate remote" (set_active false);
    Ui.button "Activate remote" (set_active true);
    Ui.button "Refresh remote" (set_version (version + 1))]
