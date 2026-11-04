(* The matters still open, for the sidebar and the opening. The conversation menu holds the whole list; this short one
   lives outside it, as opening a conversation empties that list. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

let limit = 5

let component ~(advisor : Advisor.controller Bonsai.t) graph =
  let listed, set_listed = Bonsai.state ~equal:phys_equal ([] : C.metadata list) graph in
  (* Titles are private: nothing shows until access is known, or once it has been lost. *)
  let readable = Bonsai.cutoff (let%arr advisor in if advisor.view.access_denied then None else advisor.view.status) ~equal:Poly.equal in
  (* A new turn, a new status or a deleted conversation changes the list; so does losing consent or a household. *)
  let changes =
    Bonsai.cutoff ~equal:Poly.equal
      (let%arr advisor in
       let view = advisor.view in
       ( view.authenticated && not view.access_denied,
         Option.map view.snapshot ~f:(fun s -> (s.conversation_id, s.matter, List.length s.turns)),
         Option.map view.status ~f:(fun s -> (s.consent, s.household)) ))
  in
  let latest = ref 0 in
  let refresh =
    let%arr set_listed in
    fun (allowed, _, consent) ->
      let%bind.Effect request = Effect.of_thunk (fun () -> incr latest; !latest) in
      match allowed, consent with
      | true, Some (true, _) -> (
          let%bind.Effect result =
            Effect.of_deferred_thunk (fun () -> Advisor_api.history ~matter:W.Open ~limit ~offset:0 ())
          in
          if request <> !latest then Effect.Ignore
          else match result with Ok page -> set_listed page.conversations | Error _ -> set_listed [])
      | _ -> set_listed []
  in
  Bonsai.Edge.on_change ~equal:Poly.equal changes ~callback:refresh graph;
  let%arr listed and readable in
  match readable with
  | None -> []
  | Some _ ->
      List.filter_map listed ~f:(fun (entry : C.metadata) ->
          match entry.preview with
          | Some { title; scope } when (not (String.is_empty title)) && Advisor_navigation.can_read_scope readable scope ->
              Some (entry.conversation_id, title, entry.updated_at)
          | _ -> None)
