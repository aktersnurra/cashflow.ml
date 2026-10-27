(* The user's notes for the advisor: a short list in Settings to add, change and forget. The server owns
   the rules (length, count, whitespace); this only shows what it holds and whether a write worked. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let limit = 20

let labelled ~label ~note ?(kind = "secondary") ?(disabled = false) effect =
  Vdom.Node.button
    ~attrs:
      ([ Vdom.Attr.class_ ("button " ^ kind); Vdom.Attr.type_ "button"; Vdom.Attr.create "aria-label" (label ^ ": " ^ note);
         Vdom.Attr.on_click (fun _ -> effect) ]
      @ if disabled then [ Vdom.Attr.disabled ] else [])
    [ Ui.text label ]

let status text = Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "row"; Vdom.Attr.create "role" "status" ] [ Ui.muted text ]

let component ~(locale : string Bonsai.t) graph =
  let notes, set_notes = Bonsai.state None graph in
  let draft, set_draft = Bonsai.state "" graph in
  let editing, set_editing = Bonsai.state None graph in
  let failed, set_failed = Bonsai.state false graph in
  let busy, set_busy = Bonsai.state false graph in
  (* Only the newest load may update the list, whatever order the replies arrive in. *)
  let generation = ref 0 in
  let reload =
    let%arr set_notes and set_failed in
    let%bind.Effect ticket = Effect.of_thunk (fun () -> incr generation; !generation) in
    let%bind.Effect result = Effect.of_deferred_thunk Advisor_api.memories in
    if ticket <> !generation then Effect.Ignore
    else match result with Ok notes -> set_notes (Some notes) | Error _ -> set_failed true
  in
  Bonsai.Edge.lifecycle ~on_activate:reload graph;
  let%arr notes and draft and set_draft and editing and set_editing and failed and set_failed and busy and set_busy and reload and locale in
  (* One write at a time: a second click while one is pending would repeat it. *)
  let write call ~on_success =
    let%bind.Effect () = Effect.Many [ set_busy true; set_failed false ] in
    let%bind.Effect result = Effect.of_deferred_thunk call in
    match result with
    | Ok () -> Effect.Many [ on_success; set_busy false; reload ]
    | Error _ -> Effect.Many [ set_busy false; set_failed true ]
  in
  let clear = Effect.Many [ set_draft ""; set_editing None ] in
  let save =
    match editing with
    | Some id -> write (fun () -> Advisor_api.update_memory ~id draft) ~on_success:clear
    | None -> write (fun () -> Advisor_api.add_memory draft) ~on_success:clear
  in
  let forget id =
    write (fun () -> Advisor_api.forget_memory id)
      ~on_success:(if Option.equal String.equal editing (Some id) then clear else Effect.Ignore)
  in
  let held = Option.value_map notes ~default:[] ~f:Fn.id in
  let full = List.length held >= limit && Option.is_none editing in
  let row (note : Advisor_api.memory) =
    Ui.div ~cls:"row"
      [ Ui.div ~cls:"row-title memory-note" [ Ui.text note.text ];
        Ui.div ~cls:"actions"
          [ labelled ~label:(Msg.advisor_memory_edit locale) ~note:note.text ~disabled:busy
              (Effect.Many [ set_editing (Some note.id); set_draft note.text; set_failed false ]);
            labelled ~label:(Msg.advisor_memory_forget locale) ~note:note.text ~disabled:busy (forget note.id) ] ]
  in
  let blank = String.is_empty (String.strip draft) in
  Ui.div
    [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.advisor_memory_title locale) ] ];
      Ui.div ~cls:"table"
        ([ Ui.div ~cls:"row" [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "row-sub"; Vdom.Attr.id "advisor-memory-hint" ] [ Ui.text (Msg.advisor_memory_hint locale) ] ] ]
         @ (match notes with
            | Some [] -> [ Ui.div ~cls:"row" [ Ui.muted (Msg.advisor_memory_empty locale) ] ]
            | _ -> List.map held ~f:row)
         @ [ Ui.div ~cls:"row memory-form"
               [ Vdom.Node.input
                   ~attrs:
                     ([ Vdom.Attr.type_ "text"; Vdom.Attr.value_prop draft; Vdom.Attr.create "aria-label" (Msg.advisor_memory_title locale);
                        Vdom.Attr.create "aria-describedby" "advisor-memory-hint"; Vdom.Attr.placeholder (Msg.advisor_memory_placeholder locale);
                        Vdom.Attr.on_input (fun _ v -> set_draft v) ]
                      @ if full || busy then [ Vdom.Attr.disabled ] else [])
                   ();
                 Ui.button ~kind:"primary" ~disabled:(blank || full || busy) (if Option.is_some editing then Msg.action_save locale else Msg.action_add locale) save;
                 (if Option.is_some editing then Ui.button ~disabled:busy (Msg.action_cancel locale) clear else Vdom.Node.none) ] ]
         @ (if full then [ status (Msg.advisor_memory_full locale) ] else [])
         @ if failed then [ status (Msg.advisor_memory_failed locale) ] else []) ]
