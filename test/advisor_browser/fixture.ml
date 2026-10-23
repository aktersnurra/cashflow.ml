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

let test_advisor_evidence_text () =
  let module E = Advisor_evidence_text in
  let equal actual expected = if not (String.equal actual expected) then failwith ("evidence presentation: " ^ actual) in
  List.iter
    [ "cycle_summary"; "category_spending"; "category_trend"; "relevant_transactions";
      "recurring"; "budget_status"; "goals"; "forecast" ]
    ~f:(fun tool -> assert (not (String.equal tool (E.title "sv" tool))));
  equal (E.assumption "sv" "stopped_series_excluded") "Avslutade betalningsserier ingår inte.";
  equal (E.assumption "sv" "other_currency_accounts_excluded") "Konton i andra valutor ingår inte.";
  equal (E.assumption "sv" "no_income_detected") "Inga inkomster upptäcktes.";
  equal (E.assumption "sv" "no_range_yet") "Inget prognosintervall finns ännu.";
  equal (E.assumption "sv" "stale_import: 2026-10-22") "Inaktuell import: 2026-10-22";
  equal (E.assumption "sv" "short_history: 12") "Kort historik: 12 dagar";
  equal (E.assumption "sv" "range_from: 3 forecasts since 2026-10-22") "Intervall från 3 prognoser sedan 2026-10-22";
  equal (E.assumption "sv" "seasonal: 2026-02 -12 percent") "Säsongsjustering: 2026-02, -12 procent";
  equal (E.assumption "sv" "usual_spending: 9007199254740993 minor units; 2026-10-01 to 2026-10-22; 21 days")
    "Vanliga utgifter: 9007199254740993 i minsta valutaenhet; 2026-10-01 till 2026-10-22; 21 dagar";
  equal (E.assumption "en" "short_history: 12") "Short history: 12 days";
  List.iter ["<img src=x onerror=alert(1)>"; "stale_import: <script>"; "unknown_tool"; "seasonal: user content"]
    ~f:(fun unknown -> equal (E.title "sv" unknown) unknown; equal (E.label "sv" unknown) unknown;
      equal (E.assumption "sv" unknown) unknown);
  equal (E.value "sv" { W.label = "Truncated"; value = W.Text "false" }) "Nej";
  equal (E.value "sv" { W.label = "Merchant"; value = W.Text "false" }) "false";
  equal (E.value "sv" { W.label = "Largest returned category";
    value = W.Money { decimal = "90071992547409.93"; currency = {code="SEK"; scale=2} } }) "90071992547409.93 SEK"

let () =
  test_advisor_evidence_text ();
  Async_js.init ();
  Bonsai_web.Start.start (if String.equal (Js_of_ocaml.Js.to_string Js_of_ocaml.Dom_html.window##.location##.search) "?remote" then Remote_fixture.app else app)
