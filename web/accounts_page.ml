open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let kinds locale = [ ("asset", Msg.label_kind_asset locale); ("liability", Msg.label_kind_liability locale); ("expense", Msg.label_kind_expense locale); ("income", Msg.label_kind_income locale) ]

type draft = { name : string; kind : string; owner : string; last4 : string; statement_backed : bool; no_overdraft : bool }

let empty_draft = { name = ""; kind = "asset"; owner = "personal"; last4 = ""; statement_backed = true; no_overdraft = false }

let account_row ~locale ~navigate (a : Accounts_data.account) =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "row clickable"; Vdom.Attr.on_click (fun _ -> navigate (Route.Account a.id)) ]
    [ Ui.div
        [ Ui.div ~cls:"row-title" [ Ui.text a.name ];
          Ui.div ~cls:"row-sub"
            [ Ui.text
                (String.concat ~sep:" · "
                   (List.filter_opt [ Some (if String.equal a.owner "household" then Msg.label_household locale else Msg.label_personal locale); a.number ])) ] ];
      Ui.div ~cls:"amount" [ Ui.text (Fmt.format_money locale ~code:a.currency a.balance) ] ]

let list ~(ctx : Ctx.t) graph =
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let draft, set_draft = Bonsai.state None graph in
  let%arr accounts and draft and set_draft and locale = ctx.locale and navigate = ctx.navigate and changed = ctx.changed and notify = ctx.notify
  and session = ctx.session in
  let create (d : draft) =
    let args =
      `Assoc
        ([ ("idempotency_key", `String (Api.fresh_key "open")); ("kind", `String d.kind); ("name", `String d.name); ("owner", `String d.owner);
           ("statement_backed", `Bool (d.statement_backed && (String.equal d.kind "asset" || String.equal d.kind "liability")));
           ("no_overdraft", `Bool d.no_overdraft) ]
        @ if String.is_empty d.last4 then [] else [ ("number_last4", `String d.last4) ])
    in
    match%bind.Effect Api.run "open_account" args with
    | Ok json -> Effect.Many [ set_draft None; changed; navigate (Route.Account (J.str (J.path [ "account"; "id" ] json))) ]
    | Error message -> notify message
  in
  let form (d : draft) =
    let set f = set_draft (Some (f d)) in
    Ui.div ~cls:"overlay"
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "sheet" ]
          [ Vdom.Node.h2 [ Ui.text (Msg.action_open_account locale) ];
            Ui.field (Msg.label_name locale) (Ui.input ~value:d.name ~on_input:(fun v -> set (fun d -> { d with name = v })) ());
            Ui.div ~cls:"form-row"
              [ Ui.field (Msg.label_kind locale) (Ui.select ~value:d.kind ~options:(kinds locale) ~on_change:(fun v -> set (fun d -> { d with kind = v })));
                (if Ctx.has_household session then
                   Ui.field (Msg.label_owner locale)
                     (Ui.select ~value:d.owner ~options:[ ("personal", Msg.label_personal locale); ("household", Msg.label_household locale) ]
                        ~on_change:(fun v -> set (fun d -> { d with owner = v })))
                 else Vdom.Node.none) ];
            (if String.equal d.kind "asset" || String.equal d.kind "liability" then
               Ui.div
                 [ Ui.field (Msg.label_last4 locale) (Ui.input ~value:d.last4 ~placeholder:"1234" ~on_input:(fun v -> set (fun d -> { d with last4 = v })) ());
                   Vdom.Node.label
                     ~attrs:[ Vdom.Attr.class_ "checkbox" ]
                     [ Vdom.Node.input ~attrs:([ Vdom.Attr.type_ "checkbox"; Vdom.Attr.on_change (fun _ _ -> set (fun d -> { d with statement_backed = not d.statement_backed })) ] @ if d.statement_backed then [ Vdom.Attr.checked ] else []) ();
                       Ui.text (Msg.label_statement_backed locale) ];
                   Vdom.Node.label
                     ~attrs:[ Vdom.Attr.class_ "checkbox" ]
                     [ Vdom.Node.input ~attrs:([ Vdom.Attr.type_ "checkbox"; Vdom.Attr.on_change (fun _ _ -> set (fun d -> { d with no_overdraft = not d.no_overdraft })) ] @ if d.no_overdraft then [ Vdom.Attr.checked ] else []) ();
                       Ui.text (Msg.label_no_overdraft locale) ] ]
             else Vdom.Node.none);
            Ui.div ~cls:"actions"
              [ Ui.button ~kind:"primary" ~disabled:(String.is_empty (String.strip d.name)) (Msg.action_open_account locale) (create d);
                Ui.button (Msg.action_cancel locale) (set_draft None) ] ] ]
  in
  Vdom.Node.div
    [ Ui.page ~title:(Msg.accounts_title locale)
        [ Ui.div ~cls:"actions" [ Ui.button ~kind:"primary" (Msg.action_open_account locale) (set_draft (Some empty_draft)) ];
          Remote.view ~locale accounts ~f:(fun json ->
              let accounts = List.filter (Accounts_data.of_list json) ~f:(fun a -> not a.closed) in
              let banks = List.filter accounts ~f:Accounts_data.is_bank and categories = List.filter accounts ~f:Accounts_data.is_category in
              Ui.div
                [ Ui.div ~cls:"section-title" [ Ui.text (Msg.accounts_bank locale) ];
                  (if List.is_empty banks then Ui.empty (Msg.accounts_empty locale) else Ui.div ~cls:"list" (List.map banks ~f:(account_row ~locale ~navigate)));
                  Ui.div ~cls:"section-title" [ Ui.text (Msg.accounts_categories locale) ];
                  Ui.div ~cls:"list" (List.map categories ~f:(account_row ~locale ~navigate)) ]) ];
      (match draft with Some d -> form d | None -> Vdom.Node.none) ]

type tool = No_tool | Reconcile of { date : string; balance : string } | Opening of { date : string; balance : string } | Market of string

let detail ~(ctx : Ctx.t) ~(account_id : string Bonsai.t) graph =
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let args = let%arr account_id in `Assoc [ ("account_id", `String account_id); ("limit", `Int 200) ] in
  let transactions = Remote.load ~name:"query_transactions" ~args ~ctx graph in
  let imports = Remote.load ~name:"list_imports" ~args:(let%arr account_id in `Assoc [ ("account_id", `String account_id) ]) ~ctx graph in
  let import = Import_widget.component ~ctx ~account_id graph in
  let tool, set_tool = Bonsai.state No_tool graph in
  let%arr accounts and transactions and imports and import and account_id and tool and set_tool and locale = ctx.locale and changed = ctx.changed
  and notify = ctx.notify in
  let today = let d = Js_of_ocaml.(new%js Js.date_now) in Printf.sprintf "%04d-%02d-%02d" d##getFullYear (d##getMonth + 1) d##getDate in
  let account = match accounts with Loaded json -> Accounts_data.find (Accounts_data.of_list json) account_id | _ -> None in
  match account with
  | None -> Ui.page ~title:(Msg.accounts_title locale) [ Remote.view ~locale accounts ~f:(fun _ -> Ui.empty (Msg.accounts_empty locale)) ]
  | Some a ->
      let run name extra =
        match%bind.Effect Api.run name (`Assoc (("account_id", `String a.id) :: extra)) with
        | Ok _ -> Effect.Many [ set_tool No_tool; changed ]
        | Error message -> notify message
      in
      let tool_form =
        match tool with
        | No_tool -> Vdom.Node.none
        | Reconcile { date; balance } ->
            Ui.card ~title:(Msg.action_reconcile locale)
              ~body:
                [ Ui.div ~cls:"form-row"
                    [ Ui.field (Msg.label_date locale) (Ui.input ~type_:"date" ~value:date ~on_input:(fun date -> set_tool (Reconcile { date; balance })) ());
                      Ui.field (Msg.label_statement_balance locale) (Ui.input ~value:balance ~placeholder:"0.00" ~on_input:(fun balance -> set_tool (Reconcile { date; balance })) ()) ] ]
              ~actions:
                [ Ui.button ~kind:"primary" (Msg.action_adjust locale) (run "reconcile" [ ("date", `String date); ("statement_balance", `String balance); ("adjust", `Bool true) ]);
                  Ui.button (Msg.action_reconcile locale) (run "reconcile" [ ("date", `String date); ("statement_balance", `String balance) ]);
                  Ui.button (Msg.action_cancel locale) (set_tool No_tool) ] ()
        | Opening { date; balance } ->
            Ui.card ~title:(Msg.label_opening_balance locale)
              ~body:
                [ Ui.div ~cls:"form-row"
                    [ Ui.field (Msg.label_date locale) (Ui.input ~type_:"date" ~value:date ~on_input:(fun date -> set_tool (Opening { date; balance })) ());
                      Ui.field (Msg.label_balance locale) (Ui.input ~value:balance ~placeholder:"0.00" ~on_input:(fun balance -> set_tool (Opening { date; balance })) ()) ] ]
              ~actions:[ Ui.button ~kind:"primary" (Msg.action_save locale) (run "set_opening_balance" [ ("date", `String date); ("balance", `String balance) ]); Ui.button (Msg.action_cancel locale) (set_tool No_tool) ] ()
        | Market value ->
            Ui.card ~title:(Msg.label_market_value locale)
              ~body:[ Ui.field (Msg.label_market_value locale) (Ui.input ~value ~placeholder:"0.00" ~on_input:(fun v -> set_tool (Market v)) ()) ]
              ~actions:[ Ui.button ~kind:"primary" (Msg.action_save locale) (run "set_market_value" [ ("value", `String value) ]); Ui.button (Msg.action_cancel locale) (set_tool No_tool) ] ()
      in
      let transaction_row t =
        let from_ = J.field "from" t and to_ = J.field "to" t in
        let incoming = String.equal (J.str (J.field "id" to_)) a.id in
        let other = if incoming then from_ else to_ in
        let amount = Fmt.money_json ~code:(J.str ~default:a.currency (J.field "currency" t)) (J.field "amount" t) in
        let signed = if incoming then amount else { amount with minor = Int64.neg amount.minor } in
        let pending = String.equal (J.str (J.field "kind" t)) "pending" in
        Ui.div ~cls:"row"
          [ Ui.div
              [ Ui.div ~cls:"row-title" [ Ui.text (J.str (J.field "description" t)) ];
                Ui.div ~cls:"row-sub"
                  [ Ui.text
                      (String.concat ~sep:" · "
                         (List.filter_opt
                            [ Some (Fmt.format_date locale (J.field "date" t)); Some (J.str ~default:(J.str (J.field "owner" other)) (J.field "name" other));
                              (if pending then Some (Msg.label_pending locale) else None) ])) ] ];
            Ui.div ~cls:("amount " ^ if incoming then "positive" else "") [ Ui.text (Mf2_runtime.format_money locale signed) ] ]
      in
      let import_row b =
        let undo =
          match%bind.Effect Api.run "undo_import" (`Assoc [ ("import_id", J.field "id" b) ]) with Ok _ -> changed | Error message -> notify message
        in
        Ui.div ~cls:"row"
          [ Ui.div
              [ Ui.div ~cls:"row-title" [ Ui.text (J.str ~default:"CSV" (J.field "file_name" b)) ];
                Ui.div ~cls:"row-sub"
                  [ Ui.text (Msg.card_import_new ~count:(J.int (J.field "new" b)) locale ^ " · " ^ Msg.card_import_duplicates ~count:(J.int (J.field "duplicates" b)) locale) ] ];
            (if J.bool (J.field "undone" b) then Ui.span ~cls:"muted" [ Ui.text (Msg.label_undone locale) ] else Ui.button ~kind:"danger" (Msg.action_undo locale) undo) ]
      in
      Ui.page ~title:a.name
        [ Ui.div ~cls:"kpis" [ Ui.kpi (Msg.label_balance locale) (Fmt.format_money locale ~code:a.currency a.balance) ];
          Ui.div ~cls:"actions"
            (List.filter_opt
               [ (if Accounts_data.is_bank a then Some (Ui.button (Msg.action_reconcile locale) (set_tool (Reconcile { date = today; balance = "" }))) else None);
                 (if Accounts_data.is_bank a then Some (Ui.button (Msg.label_opening_balance locale) (set_tool (Opening { date = today; balance = "" })))
                  else None);
                 (if List.mem a.tags "investment" ~equal:String.equal then Some (Ui.button (Msg.label_market_value locale) (set_tool (Market ""))) else None) ]);
          tool_form;
          (if Accounts_data.is_bank a && a.statement_backed then import else Vdom.Node.none);
          Ui.div ~cls:"section-title" [ Ui.text (Msg.label_transactions locale) ];
          Remote.view ~locale transactions ~f:(fun json ->
              match J.list (J.field "transactions" json) with
              | [] -> Ui.empty (Msg.accounts_no_transactions locale)
              | ts -> Ui.div ~cls:"list" (List.map ts ~f:transaction_row));
          (match imports with
          | Loaded json when not (List.is_empty (J.list (J.field "imports" json))) ->
              Ui.div [ Ui.div ~cls:"section-title" [ Ui.text (Msg.label_imports locale) ]; Ui.div ~cls:"list" (List.map (J.list (J.field "imports" json)) ~f:import_row) ]
          | _ -> Vdom.Node.none) ]
