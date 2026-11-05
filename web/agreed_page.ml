(* What we agreed: the record of what was settled in each matter, newest first, and what it changed. A change can be undone
   from here for as long as it is as it was left; the record then says so rather than forgetting it. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
module W = Cashflow_advisor_protocol.Wire

type listed = Loading | Failed | Loaded of W.agreed_entry list * int option

let page_size = 20

(* A day in the reader's own time zone, as decisions late at night belong to the day they were made. *)
let day locale stamp =
  let date = new%js Js.date_fromTimeValue (Js.date##parse (Js.string stamp)) in
  let options =
    Js.Unsafe.obj
      [| ("day", Js.Unsafe.inject (Js.string "numeric")); ("month", Js.Unsafe.inject (Js.string "short"));
         ("year", Js.Unsafe.inject (Js.string "numeric")) |]
  in
  let formatted : Js.js_string Js.t =
    Js.Unsafe.meth_call date "toLocaleDateString" [| Js.Unsafe.inject (Js.string locale); Js.Unsafe.inject options |]
  in
  Js.to_string formatted

(* The card asked; the record says what was done. *)
let change_title locale = function
  | "propose_set_budget" -> Msg.agreed_change_budget locale
  | "propose_categorize" -> Msg.agreed_change_categorize locale
  | "propose_add_rule" -> Msg.agreed_change_rule locale
  | "propose_save_goal" -> Msg.agreed_change_goal locale
  | "propose_update_goal" -> Msg.agreed_change_update_goal locale
  | "propose_new_category" -> Msg.agreed_change_category locale
  | "propose_entry" -> Msg.agreed_change_entry locale
  | other -> Advisor_evidence_text.title locale other

let fact_label locale = function
  | "Current budget" -> Msg.agreed_budget_was locale
  | "Proposed budget" -> Msg.agreed_budget_now locale
  | other -> Advisor_evidence_text.label locale other

let page ~(ctx : Ctx.t) ~(advisor : Advisor.controller Bonsai.t) ~(decide : (string -> string -> unit Effect.t) Bonsai.t)
    ~(select : (string -> unit Effect.t) Bonsai.t) graph =
  let listed, set_listed = Bonsai.state ~equal:phys_equal Loading graph in
  (* The latest request wins, so a late answer can't bring back what a newer one has replaced. *)
  let latest = ref 0 in
  let fetch =
    let%arr set_listed and listed in
    fun ~offset ->
      let%bind.Effect request = Effect.of_thunk (fun () -> incr latest; !latest) in
      let%bind.Effect result = Effect.of_deferred_thunk (fun () -> Advisor_api.agreed ~limit:page_size ~offset) in
      if request <> !latest then Effect.Ignore
      else
        match result, listed with
        | Ok page, Loaded (shown, _) when offset > 0 -> set_listed (Loaded (shown @ page.entries, page.next_offset))
        | Ok page, _ -> set_listed (Loaded (page.entries, page.next_offset))
        | Error _, _ -> set_listed Failed
  in
  let fresh = let%arr fetch and set_listed in Effect.Many [ set_listed Loading; fetch ~offset:0 ] in
  let refetch = let%arr fetch in fetch ~offset:0 in
  Bonsai.Edge.lifecycle ~on_activate:fresh graph;
  (* Anything written elsewhere may change the record: nothing is shown from before the page came back. *)
  Bonsai.Edge.on_change' ~equal:Int.equal ctx.version
    ~callback:(let%arr refetch in fun previous _ -> if Option.is_some previous then refetch else Effect.Ignore)
    graph;
  let consent =
    Bonsai.cutoff ~equal:[%equal: bool option]
      (let%arr advisor in
       if advisor.view.access_denied then Some false else Option.map advisor.view.status ~f:(fun (s : Advisor.C.status) -> s.consent))
  in
  let%arr listed and consent and locale = ctx.locale and fetch and decide and select in
  let head =
    Ui.div ~cls:"page-head" [ Vdom.Node.h1 [ Ui.text (Msg.nav_agreed locale) ] ]
  in
  let change (c : W.agreed_change) =
    let e = c.evidence in
    let effect_ =
      match c.undone_at with
      | Some at -> Msg.agreed_undone ~date:(day locale at) locale
      | None -> Msg.agreed_in_effect ~date:(day locale c.applied_at) locale
    in
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "agreed-change"; Vdom.Attr.id ("agreed-" ^ e.id) ]
      [ Ui.div ~cls:"agreed-change-head"
          [ Ui.span ~cls:"agreed-change-title" [ Ui.text (change_title locale e.title) ];
            Ui.span ~cls:"agreed-effect" [ Ui.text effect_ ];
            (if Option.is_some c.undone_at then Vdom.Node.none else Ui.button (Msg.advisor_proposal_undo locale) (decide e.id "undo")) ];
        Vdom.Node.create "dl" ~attrs:[ Vdom.Attr.class_ "advisor-facts" ]
          (List.map e.facts ~f:(fun (f : W.fact) ->
               Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "advisor-fact" ]
                 [ Vdom.Node.create "dt" [ Ui.text (fact_label locale f.label) ];
                   Vdom.Node.create "dd" [ Ui.text (Advisor_evidence_text.value locale f) ] ])) ]
  in
  let entry (m : W.agreed_entry) =
    let target = Route.Advisor (Some m.conversation_id) in
    let title = match m.title with Some t when not (String.is_empty t) -> t | _ -> Msg.agreed_untitled locale in
    let status =
      match m.matter, m.decided_at with
      | W.Decided, Some at -> Msg.agreed_decided ~date:(day locale at) locale
      | W.Decided, None -> Msg.matter_group_decided locale
      | W.Open, _ -> Msg.agreed_open locale
      | W.Parked, _ -> Msg.agreed_parked locale
    in
    Vdom.Node.section ~key:m.conversation_id ~attrs:[ Vdom.Attr.class_ "agreed-matter" ]
      ([ Ui.div ~cls:"agreed-matter-head"
           [ Vdom.Node.a
               ~attrs:
                 [ Vdom.Attr.class_ "agreed-title"; Vdom.Attr.href (Route.to_path target); Vdom.Attr.create "dir" "auto";
                   Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; select m.conversation_id ]) ]
               [ Ui.text title ];
             (match m.scope with W.Household _ -> Ui.span ~cls:"agreed-scope" [ Ui.text (Msg.label_household locale) ] | W.Personal -> Vdom.Node.none);
             Ui.span ~cls:"agreed-status" [ Ui.text status ] ] ]
      @
      if List.is_empty m.changes then [ Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "agreed-nothing" ] [ Ui.text (Msg.agreed_no_change locale) ] ]
      else List.map m.changes ~f:change)
  in
  let body =
    match consent, listed with
    | Some false, _ -> [ Ui.empty (Msg.agreed_off locale) ]
    (* Until access is known, and while loading afresh, no title is shown. *)
    | None, _ | Some true, Loading -> []
    | Some true, Failed -> [ Ui.empty (Msg.agreed_failed locale) ]
    | Some true, Loaded ([], _) -> [ Ui.empty (Msg.agreed_empty locale) ]
    | Some true, Loaded (entries, next) ->
        [ Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "agreed-intro" ] [ Ui.text (Msg.agreed_intro locale) ] ]
        @ List.map entries ~f:entry
        @
        (match next with
         | Some offset -> [ Ui.div ~cls:"actions" [ Ui.button (Msg.agreed_more locale) (fetch ~offset) ] ]
         | None -> [])
  in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page agreed-page" ] (head :: body)
