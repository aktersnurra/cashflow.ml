(* Synthetic shell fixture; the controller and API are unmodified production modules. *)
open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

let shell graph =
  let locale = Bonsai.return "en" in
  let ctx : Ctx.t =
    {
      session = Bonsai.return `Null;
      locale;
      currency = Bonsai.return "SEK";
      version = Bonsai.return 0;
      changed = Bonsai.return Effect.Ignore;
      navigate = Bonsai.return (fun _ -> Effect.Ignore);
      notify = Bonsai.return (fun _ -> Effect.Ignore);
      theme = Bonsai.return Theme.System;
      set_theme = Bonsai.return (fun _ -> Effect.Ignore);
    }
  in
  let controller = Advisor.component ~ctx graph in
  let home, set_home = Bonsai.state true graph in
  let initialize =
    let%arr controller in
    let%bind.Effect _ = Effect.of_deferred_thunk Api.session in
    controller.open_empty ~scope:W.Personal ~currency:{ code = "SEK"; scale = 2 } ~locale:"en"
  in
  Bonsai.Edge.lifecycle ~on_activate:initialize graph;
  let%arr controller and home and set_home in
  let button id label action_effect =
    Vdom.Node.button ~attrs:[ Vdom.Attr.id id; Vdom.Attr.on_click (fun _ -> action_effect) ] [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    [
      button "toggle"
        (if home then "Home" else "Advisor")
        (Effect.Many [ controller.leave_surface; set_home (not home) ]);
      button "resync" "Reconnect" (controller.dispatch C.Reconnect);
      button "delete-other" "Delete unrelated"
        (controller.dispatch (C.Delete_owned "00000000-0000-4000-8000-000000000099"));
      button "history" "History probe" (controller.dispatch (C.Load_history 0));
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.id "surface" ]
        [ Advisor_view.render ~locale:"en" ~view:controller.view ~dispatch:controller.dispatch ];
    ]

let app graph =
  let enabled, set_enabled = Bonsai.state true graph in
  let surface = match%sub enabled with true -> shell graph | false -> Bonsai.return Vdom.Node.none in
  let%arr surface and set_enabled in
  Vdom.Node.div
    [
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.id "deactivate"; Vdom.Attr.on_click (fun _ -> set_enabled false) ]
        [ Vdom.Node.text "Deactivate shell" ];
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.id "activate"; Vdom.Attr.on_click (fun _ -> set_enabled true) ]
        [ Vdom.Node.text "Activate shell" ];
      surface;
    ]

let () =
  Async_js.init ();
  Bonsai_web.Start.start (if String.equal (Js_of_ocaml.Js.to_string Js_of_ocaml.Dom_html.window##.location##.search) "?remote" then Remote_fixture.app else app)
