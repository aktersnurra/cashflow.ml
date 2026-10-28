(* The command palette: type to filter pages, accounts and actions; enter runs the first. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

let component ~(ctx : Ctx.t) ~(ask : (string -> unit Effect.t) Bonsai.t) ~(open_ : bool Bonsai.t) ~(close : unit Effect.t Bonsai.t) graph =
  let query, set_query = Bonsai.state "" graph in
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let focus = match%sub open_ with true -> Bonsai_web.Effect.Focus.on_activate () graph | false -> Bonsai.return Vdom.Attr.empty in
  let%arr focus and ask and open_ and close and query and set_query and accounts and locale = ctx.locale and navigate = ctx.navigate and theme = ctx.theme
  and set_theme = ctx.set_theme in
  if not open_ then Vdom.Node.none
  else
    let pages = List.map Route.[ Home; Cashflow; Transactions { offset = 0; category = None }; Inbox; Accounts; Budgets; Forecast; Household; Insights; Settings ] ~f:(fun r -> (Msg.command_go_to ~page:(Route.title locale r) locale, navigate r)) in
    let accounts =
      match accounts with
      | Loaded json ->
          List.filter_map (Accounts_data.of_list ~locale json) ~f:(fun a ->
              if Accounts_data.is_bank a then Some (Msg.palette_account ~account:a.name locale, navigate (Route.Account a.id)) else None)
      | _ -> []
    in
    let entries = ((Msg.palette_add locale, navigate Route.Add) :: pages) @ accounts @ [ (Msg.palette_flip_theme locale, set_theme (Theme.flipped theme)) ] in
    let filter query =
      let q = String.lowercase (String.strip query) in
      let matching = List.filter entries ~f:(fun (label, _) -> String.is_empty q || String.is_substring (String.lowercase label) ~substring:q) in
      (* Anything that names no page or action is a question: it goes to the advisor, last so a page match still wins Enter. *)
      let question = String.strip query in
      if String.is_empty question || String.length question > Advisor_chat_hooks.input_bytes_limit then matching
      else List.take matching 9 @ [ (Msg.palette_ask ~question locale, ask question) ]
    in
    let entries = filter query in
    let run effect = Effect.Many [ set_query ""; effect ] in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "overlay"; Vdom.Attr.on_click (fun _ -> close) ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "palette"; Vdom.Attr.on_click (fun _ -> Effect.Stop_propagation) ]
          [ Vdom.Node.input
              ~attrs:
                [ focus; Vdom.Attr.placeholder (Msg.command_placeholder locale); Vdom.Attr.value_prop query;
                  Vdom.Attr.on_input (fun _ v -> set_query v);
                  Vdom.Attr.on_keydown (fun e ->
                      match Js.Optdef.case e##.key (fun () -> "") Js.to_string with
                      | "Enter" -> (match filter (Ui.event_value e) with (_, effect) :: _ -> run effect | [] -> Effect.Ignore)
                      | "Escape" -> Effect.Many [ set_query ""; close ]
                      | _ -> Effect.Ignore) ]
              ();
            Ui.div ~cls:"list" (List.map (List.take entries 10) ~f:(fun (label, effect) ->
                 Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "row clickable"; Vdom.Attr.on_click (fun _ -> run effect) ] [ Ui.div ~cls:"row-title" [ Ui.text label ] ])) ] ]
