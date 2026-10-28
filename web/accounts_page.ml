open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type draft = { name : string; kind : string; owner : string; last4 : string; statement_backed : bool; no_overdraft : bool }

let empty_draft = { name = ""; kind = "asset"; owner = "personal"; last4 = ""; statement_backed = true; no_overdraft = false }

(* A bank account or card says where it is; a category is plain. With a household, each row says
   whose it is. *)
let account_row ~locale ~navigate ~household ~amount (a : Accounts_data.account) =
  let owner = if String.equal a.owner "household" then Msg.label_household locale else Msg.label_personal locale in
  let sub = List.filter_opt [ a.institution; Option.map a.number ~f:(fun n -> "•••• " ^ n); (if household then Some owner else None) ] in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "row clickable"; Vdom.Attr.on_click (fun _ -> navigate (Route.Account a.id)) ]
    [ Ui.div ~cls:"category-cell"
        [ Ui.span ~cls:"tile" [ Ui.text (Ui.initial a.name) ];
          Ui.div
            [ Ui.div ~cls:"row-title" [ Ui.text a.name ];
              (if List.is_empty sub then Vdom.Node.none else Ui.div ~cls:"row-sub" [ Ui.text (String.concat ~sep:" · " sub) ]) ] ];
      amount ]

let list ~(ctx : Ctx.t) graph =
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let worth = Remote.load ~name:"net_worth_series" ~args:(Bonsai.return (`Assoc [ ("cycles", `Int 12) ])) ~ctx graph in
  let draft, set_draft = Bonsai.state None graph in
  let%arr accounts and worth and draft and set_draft and locale = ctx.locale and navigate = ctx.navigate and changed = ctx.changed
  and notify = ctx.notify and session = ctx.session in
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
              [ Ui.field (Msg.label_kind locale) (Ui.select ~value:d.kind ~options:(Accounts_data.kinds locale) ~on_change:(fun v -> set (fun d -> { d with kind = v })));
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
  let household = Ctx.has_household session in
  (* Net worth now, its change since the last pay cycle closed, and the year behind it. *)
  let worth_panel =
    match worth with
    | Remote.Loaded json -> (
        let code = J.str ~default:"SEK" (J.field "currency" json) in
        (* Cycles before the first recorded money are zero, not history. *)
        let values =
          List.map (J.list (J.field "points" json)) ~f:(fun p -> Fmt.to_float (J.field "net_worth" p)) |> List.drop_while ~f:(fun v -> Float.(v = 0.))
        in
        let whole v = Fmt.whole locale ~code (`String (Float.to_string v)) in
        match List.rev values with
        | [] -> Vdom.Node.none
        | last :: rest ->
            Ui.div ~cls:"panel"
              [ Ui.div ~cls:"figures"
                  [ Ui.div ~cls:"figures-main"
                      [ Ui.div ~cls:"figure-label" [ Ui.text (Msg.label_net_worth locale) ]; Ui.div ~cls:"hero" [ Ui.text (whole last) ];
                        (match rest with
                        | previous :: _ ->
                            let change = last -. previous in
                            Ui.div
                              ~cls:("trend " ^ if Float.(change >= 0.) then "positive" else "negative")
                              [ Ui.text ((if Float.(change > 0.) then "+" else "") ^ whole change); Ui.span [ Ui.text (Msg.accounts_this_month locale) ] ]
                        | [] -> Vdom.Node.none) ];
                    Ui.div ~cls:"figures-chart" [ Chart.mini_line ~tone:(if Float.(last >= List.hd_exn values) then "green" else "red") values ] ] ])
    | _ -> Vdom.Node.none
  in
  let table ~title ~columns:(first, last) rows =
    Ui.div
      [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text title ] ];
        Ui.div ~cls:"table" (Ui.div ~cls:"table-head row" [ Ui.span [ Ui.text first ]; Ui.span ~cls:"num" [ Ui.text last ] ] :: rows) ]
  in
  Vdom.Node.div
    [ Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
        [ Ui.div ~cls:"page-head"
            [ Vdom.Node.h1 [ Ui.text (Msg.accounts_title locale) ];
              Ui.div ~cls:"page-controls" [ Ui.button ~kind:"primary" (Msg.action_open_account locale) (set_draft (Some empty_draft)) ] ];
          worth_panel;
          Remote.view ~locale accounts ~f:(fun json ->
              let accounts = List.filter (Accounts_data.of_list ~locale json) ~f:(fun a -> not a.closed) in
              let banks = List.filter accounts ~f:Accounts_data.is_bank in
              let categories kind = List.filter accounts ~f:(fun a -> Accounts_data.is_category a && String.equal a.kind kind) in
              let bank_row (a : Accounts_data.account) =
                account_row ~locale ~navigate ~household a ~amount:(Ui.span ~cls:"amount" [ Ui.text (Fmt.format_money locale ~code:a.currency a.balance) ])
              in
              let category_row (a : Accounts_data.account) =
                account_row ~locale ~navigate ~household a ~amount:(Ui.span ~cls:"amount muted" [ Ui.text (if Float.(Fmt.to_float a.balance = 0.) then "–" else Fmt.whole locale ~code:a.currency a.balance) ])
              in
              let group label kind = match categories kind with [] -> [] | l -> Ui.div ~cls:"group-head" [ Ui.text label ] :: List.map l ~f:category_row in
              Ui.div
                [ (if List.is_empty banks then Ui.div [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text (Msg.accounts_bank locale) ] ]; Ui.empty (Msg.accounts_empty locale) ]
                   else table ~title:(Msg.accounts_bank locale) ~columns:(Msg.accounts_col_account locale, Msg.label_balance locale) (List.map banks ~f:bank_row));
                  table ~title:(Msg.accounts_categories locale) ~columns:(Msg.budgets_col_category locale, Msg.accounts_col_total locale)
                    (group (Msg.label_spending locale) "expense" @ group (Msg.label_income locale) "income") ]) ];
      (match draft with Some d -> form d | None -> Vdom.Node.none) ]

type tool = No_tool | Reconcile of { date : string; balance : string } | Opening of { date : string; balance : string } | Market of string

let detail ~(ctx : Ctx.t) ~(account_id : string Bonsai.t) graph =
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  (* A bank account's imported rows come folded, with the category each has now. A category's own
     rows are the transfers triage posted into it, which folding would leave out. *)
  let args =
    let%arr account_id and accounts and locale = ctx.locale in
    let fold = match accounts with Remote.Loaded json -> Option.exists (Accounts_data.find (Accounts_data.of_list ~locale json) account_id) ~f:Accounts_data.is_bank | _ -> false in
    `Assoc [ ("account_id", `String account_id); ("limit", `Int 200); ("fold_triage", `Bool fold) ]
  in
  let transactions = Remote.load ~name:"query_transactions" ~args ~ctx graph in
  let imports = Remote.load ~name:"list_imports" ~args:(let%arr account_id in `Assoc [ ("account_id", `String account_id) ]) ~ctx graph in
  let import = Import_widget.component ~ctx ~account_id:(let%arr account_id in Some account_id) graph in
  let tool, set_tool = Bonsai.state No_tool graph in
  let%arr accounts and transactions and imports and import and account_id and tool and set_tool and locale = ctx.locale and changed = ctx.changed
  and notify = ctx.notify and navigate = ctx.navigate and session = ctx.session in
  let today = let d = Js_of_ocaml.(new%js Js.date_now) in Printf.sprintf "%04d-%02d-%02d" d##getFullYear (d##getMonth + 1) d##getDate in
  let account = match accounts with Loaded json -> Accounts_data.find (Accounts_data.of_list ~locale json) account_id | _ -> None in
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
      let bank = Accounts_data.is_bank a in
      let transaction_row t =
        let from_ = J.field "from" t and to_ = J.field "to" t in
        let incoming = String.equal (J.str (J.field "id" to_)) a.id in
        let other = if incoming then from_ else to_ in
        let amount = Fmt.money_json ~code:(J.str ~default:a.currency (J.field "currency" t)) (J.field "amount" t) in
        (* A bank account shows money in and out; a category shows its own direction (spent, or
           earned) as plain amounts, so only a refund or a correction reads negative. *)
        let forward = if bank then incoming else Bool.equal incoming (String.equal a.kind "expense") in
        let signed = if forward then amount else { amount with minor = Int64.neg amount.minor } in
        let pending = String.equal (J.str (J.field "kind" t)) "pending" in
        let name =
          match J.field "category" t with
          | `Null -> if J.is_null (J.field "name" other) then J.str (J.field "owner" other) else Accounts_data.name ~locale other
          | c -> Accounts_data.name ~locale c
        in
        let category =
          match J.str_opt (J.field "triage" t) with
          | Some "paired" -> Ui.span ~cls:"muted" [ Ui.text (Msg.transactions_transfer locale) ]
          | Some "open" ->
              Vdom.Node.a
                ~attrs:[ Vdom.Attr.class_ "review-link"; Vdom.Attr.href (Route.to_path Route.Inbox); Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate Route.Inbox ]) ]
                [ Ui.text name; Ui.span ~cls:"review-tag" [ Ui.text (Msg.transactions_review locale) ] ]
          | _ -> Ui.span [ Ui.text name ]
        in
        Ui.div ~cls:"row statement-row"
          [ Ui.span ~cls:"txn-date" [ Ui.text (Fmt.format_date locale (J.field "date" t)) ];
            Ui.div ~cls:"category-cell"
              [ Ui.span ~cls:"row-title" [ Ui.text (J.str (J.field "description" t)) ];
                (if pending then Ui.span ~cls:"review-tag" [ Ui.text (Msg.transactions_pending locale) ] else Vdom.Node.none) ];
            (if bank then Ui.div ~cls:"txn-category" [ category ] else Vdom.Node.none);
            Ui.span ~cls:("amount" ^ if bank && incoming then " positive" else "") [ Ui.text (Mf2_runtime.format_money locale signed) ] ]
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
      let household = Ctx.has_household session in
      let subtitle =
        List.filter_opt
          [ (if Accounts_data.is_bank a then a.institution else Some (if String.equal a.kind "income" then Msg.label_kind_income locale else Msg.label_kind_expense locale));
            Option.map a.number ~f:(fun n -> "•••• " ^ n);
            (if household then Some (if String.equal a.owner "household" then Msg.label_household locale else Msg.label_personal locale) else None) ]
      in
      let pending = Fmt.to_float a.pending in
      let section title body = Ui.div [ Ui.div ~cls:"section-head" [ Vdom.Node.h2 [ Ui.text title ] ]; body ] in
      Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page" ]
        [ Ui.div ~cls:"page-head"
            [ Ui.div
                [ Vdom.Node.h1 [ Ui.text a.name ];
                  (if List.is_empty subtitle then Vdom.Node.none else Ui.div ~cls:"subtitle" [ Ui.text (String.concat ~sep:" · " subtitle) ]) ];
              Ui.div ~cls:"page-controls"
                (List.filter_opt
                   [ (if Accounts_data.is_bank a then Some (Ui.button (Msg.action_reconcile locale) (set_tool (Reconcile { date = today; balance = "" }))) else None);
                     (if Accounts_data.is_bank a then Some (Ui.button (Msg.label_opening_balance locale) (set_tool (Opening { date = today; balance = "" })))
                      else None);
                     (if List.mem a.tags "investment" ~equal:String.equal then Some (Ui.button (Msg.label_market_value locale) (set_tool (Market ""))) else None) ]) ];
          Ui.div ~cls:"panel"
            [ Ui.div ~cls:"figures"
                [ Ui.div ~cls:"figures-main"
                    [ Ui.div ~cls:"figure-label" [ Ui.text (if bank then Msg.label_balance locale else Msg.accounts_col_total locale) ]; Ui.div ~cls:"hero" [ Ui.text (Fmt.format_money locale ~code:a.currency a.balance) ] ];
                  (if Float.(pending = 0.) then Vdom.Node.none
                   else
                     Ui.div ~cls:"figures-side"
                       [ Ui.div ~cls:"figure-line"
                           [ Ui.span ~cls:"figure-label" [ Ui.text (Msg.label_pending locale) ];
                             Ui.span ~cls:"figure-value" [ Ui.text (Fmt.format_money locale ~code:a.currency a.pending) ] ] ]) ] ];
          tool_form;
          (if Accounts_data.is_bank a && a.statement_backed then import else Vdom.Node.none);
          section (Msg.label_transactions locale)
            (Remote.view ~locale transactions ~f:(fun json ->
                 match J.list (J.field "transactions" json) with
                 | [] -> Ui.empty (Msg.accounts_no_transactions locale)
                 | ts ->
                     Ui.div ~cls:("table statement" ^ if bank then "" else " plain")
                       (Ui.div ~cls:"table-head statement-row"
                          [ Ui.span [ Ui.text (Msg.transactions_col_date locale) ]; Ui.span [ Ui.text (Msg.transactions_col_description locale) ];
                            (if bank then Ui.span [ Ui.text (Msg.budgets_col_category locale) ] else Vdom.Node.none);
                            Ui.span ~cls:"num" [ Ui.text (Msg.cashflow_col_amount locale) ] ]
                       :: List.map ts ~f:transaction_row)));
          (match imports with
          | Loaded json when not (List.is_empty (J.list (J.field "imports" json))) ->
              section (Msg.label_imports locale) (Ui.div ~cls:"table" (List.map (J.list (J.field "imports" json)) ~f:import_row))
          | _ -> Vdom.Node.none) ]
