(* Transactions: every transaction in a pay cycle as one dense table, grouped by day, with search
   and filters; and Needs review, the inbox's keyboard triage. The cycle and category filters live
   in the route, so Back and links (from a Sankey flow, say) keep them; the search and account
   filter are local. Imported rows come folded (each once, with the category it has now); their
   category changes by categorize, like triage. A manual entry's changes by correct_transaction,
   which reverses it and posts the corrected one, linked. Rows still in the inbox link there. *)
open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

let iso_day ~days_ago =
  let d = new%js Js.date_now in
  ignore (d##setDate (d##getDate - days_ago) : float);
  sprintf "%04d-%02d-%02d" d##getFullYear (d##getMonth + 1) d##getDate

(* Which way a transaction moved money, seen from your own bank accounts and cards. *)
type direction = Out of Accounts_data.account option | In of Accounts_data.account option | Between

let page ~(ctx : Ctx.t) ~review ~(offset : int Bonsai.t) ~(category : string option Bonsai.t) graph =
  let scope_args = Bonsai.return (`Assoc [ ("cycles", `Int 12) ]) in
  let summary = Remote.load ~name:"cashflow_summary" ~args:scope_args ~ctx graph in
  let accounts = Remote.load ~name:"list_accounts" ~ctx graph in
  let inbox = if review then Inbox_page.page ~embedded:true ~ctx graph else Bonsai.return Vdom.Node.none in
  let range =
    let%arr summary and offset in
    match summary with
    | Remote.Loaded json -> (
        let cycles = J.list (J.field "cycles" json) in
        match List.nth cycles (List.length cycles - 1 + offset) with Some c -> Some (c, List.length cycles) | None -> None)
    | _ -> None
  in
  let transactions =
    let args =
      let%arr range in
      match range with
      | Some (c, _) -> `Assoc [ ("from_date", J.field "first" c); ("to_date", J.field "last" c); ("limit", `Int 500); ("fold_triage", `Bool true) ]
      | None -> `Assoc [ ("limit", `Int 1) ]
    in
    if review then Bonsai.return Remote.Loading else Remote.load ~name:"query_transactions" ~args ~ctx graph
  in
  let search, set_search = Bonsai.state "" graph in
  let account_filter, set_account_filter = Bonsai.state "" graph in
  let%arr accounts and inbox and range and transactions and search and set_search and account_filter and set_account_filter
  and offset and category and locale = ctx.locale and session = ctx.session and navigate = ctx.navigate and changed = ctx.changed
  and notify = ctx.notify in
  let accounts = match accounts with Remote.Loaded json -> Accounts_data.of_list json | _ -> [] in
  let find id = Accounts_data.find accounts id in
  let inbox_count = J.int (J.field "inbox_count" session) in
  let go ?(offset = offset) ?(category = category) () = navigate (Route.Transactions { offset; category }) in
  let tab active label route =
    Vdom.Node.a
      ~attrs:
        [ Vdom.Attr.class_ ("tab-button" ^ if active then " active" else ""); Vdom.Attr.href (Route.to_path route);
          Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate route ]) ]
      label
  in
  let head =
    Ui.div ~cls:"page-head"
      [ Ui.div [ Vdom.Node.h1 [ Ui.text (Msg.nav_transactions locale) ]; Ui.div ~cls:"subtitle" [ Ui.text (Msg.transactions_subtitle locale) ] ];
        Ui.div ~cls:"page-controls"
          [ (if review then Vdom.Node.none
             else
               match range with
               | Some (c, n) ->
                   Ui.period ~label:(Fmt.month_label locale (J.str (J.field "cycle" c)))
                     ~previous:(Msg.overview_previous locale, n - 1 + offset <= 0, go ~offset:(offset - 1) ())
                     ~next:(Msg.overview_next locale, offset >= 0, go ~offset:(offset + 1) ())
               | None -> Vdom.Node.none);
            Ui.button ~kind:"primary" (Msg.transactions_add locale) (navigate Route.Add) ] ]
  in
  let tabs =
    Ui.div ~cls:"tabs"
      [ tab (not review) [ Ui.text (Msg.transactions_tab_all locale) ] (Route.Transactions { offset = 0; category = None });
        tab review
          ([ Ui.text (Msg.transactions_tab_review locale) ] @ if inbox_count > 0 then [ Ui.span ~cls:"count tab-count" [ Ui.text (Int.to_string inbox_count) ] ] else [])
          Route.Inbox ]
  in
  let table json =
    let direction t =
      let from = find (J.str (J.path [ "from"; "id" ] t)) and to_ = find (J.str (J.path [ "to"; "id" ] t)) in
      let bank a = Option.exists a ~f:Accounts_data.is_bank in
      (* A folded import says where its amount sits now. *)
      let now other = match J.field "category" t with `Null -> other | c -> find (J.str (J.field "id" c)) in
      match bank from, bank to_ with true, true -> Between | true, false -> Out (now to_) | false, _ -> In (now from)
    in
    let triage t = J.str_opt (J.field "triage" t) in
    let bank_side t = match direction t with Out _ | Between -> find (J.str (J.path [ "from"; "id" ] t)) | In _ -> find (J.str (J.path [ "to"; "id" ] t)) in
    let category_of t = match direction t with Out c | In c -> c | Between -> None in
    let query = String.lowercase (String.strip search) in
    let rows =
      J.list (J.field "transactions" json)
      |> List.filter ~f:(fun t ->
             (String.is_empty query
             || String.is_substring (String.lowercase (J.str (J.field "description" t) ^ " " ^ J.str ~default:"" (J.field "counterparty" t))) ~substring:query)
             && (String.is_empty account_filter || Option.exists (bank_side t) ~f:(fun a -> String.equal a.id account_filter))
             && Option.for_all category ~f:(fun id -> Option.exists (category_of t) ~f:(fun c -> String.equal c.id id)))
      |> List.sort ~compare:(fun a b -> String.compare (J.str (J.field "date" b)) (J.str (J.field "date" a)))
    in
    let today = iso_day ~days_ago:0 and yesterday = iso_day ~days_ago:1 in
    let day_label d = if String.equal d today then Msg.transactions_today locale else if String.equal d yesterday then Msg.transactions_yesterday locale else Fmt.format_date locale (`String d) in
    let categories kind = List.filter accounts ~f:(fun a -> Accounts_data.is_category a && String.equal a.kind kind && not a.closed) in
    let recategorize t ~side (target : string) =
      let id = J.str (J.field "id" t) in
      let%bind.Effect result =
        match triage t with
        | Some _ -> Api.run "categorize" (`Assoc [ ("id", `String id); ("category_id", `String target) ])
        | None ->
            Api.run "correct_transaction" (`Assoc [ ("idempotency_key", `String ("recategorize:" ^ id ^ ":" ^ target)); ("id", `String id); (side, `String target) ])
      in
      match result with Ok _ -> Effect.Many [ changed; notify (Msg.transactions_recategorized locale) ] | Error message -> notify message
    in
    let category_cell t =
      let pending = not (String.equal (J.str (J.field "kind" t)) "posted") in
      match direction t with
      | Between -> Ui.span ~cls:"muted" [ Ui.text (Msg.transactions_transfer locale) ]
      | _ when Option.equal String.equal (triage t) (Some "paired") -> Ui.span ~cls:"muted" [ Ui.text (Msg.transactions_transfer locale) ]
      | (Out c | In c) when Option.equal String.equal (triage t) (Some "open") || Option.exists c ~f:(fun c -> Option.is_some c.role) ->
          let name = Option.value_map c ~default:"" ~f:(fun c -> c.name) in
          Vdom.Node.a
            ~attrs:[ Vdom.Attr.class_ "review-link"; Vdom.Attr.href (Route.to_path Route.Inbox); Vdom.Attr.on_click (fun _ -> Effect.Many [ Effect.Prevent_default; navigate Route.Inbox ]) ]
            [ Ui.text name; Ui.span ~cls:"review-tag" [ Ui.text (Msg.transactions_review locale) ] ]
      | Out None | In None -> Ui.span ~cls:"muted" [ Ui.text "–" ]
      | (Out (Some c) | In (Some c)) when pending -> Ui.span [ Ui.text c.name ]
      | (Out (Some c) as d) | (In (Some c) as d) ->
          let kind, side = match d with Out _ -> ("expense", "to_account_id") | _ -> ("income", "from_account_id") in
          Vdom.Node.select
            ~attrs:[ Vdom.Attr.class_ "inline-select"; Vdom.Attr.on_change (fun _ v -> if String.equal v c.id then Effect.Ignore else recategorize t ~side v) ]
            (List.map (categories kind) ~f:(fun (a : Accounts_data.account) ->
                 Vdom.Node.option ~attrs:([ Vdom.Attr.value a.id ] @ if String.equal a.id c.id then [ Vdom.Attr.create "selected" "" ] else []) [ Ui.text a.name ]))
    in
    let row t =
      let description = J.str (J.field "description" t) in
      let code = J.str ~default:"SEK" (J.field "currency" t) and amount = J.str (J.field "amount" t) in
      let dir = direction t in
      let tile = sprintf "tile c%d" ((String.hash description % 5 + 5) % 5 + 1) in
      Ui.div ~cls:"row txn-row"
        [ Ui.span ~cls:"txn-date" [ Ui.text (Fmt.format_date locale (J.field "date" t)) ];
          Ui.div ~cls:"category-cell"
            [ Ui.span ~cls:tile [ Ui.text (Ui.initial description) ]; Ui.span ~cls:"row-title" [ Ui.text description ];
              (if String.equal (J.str (J.field "kind" t)) "posted" then Vdom.Node.none else Ui.span ~cls:"review-tag" [ Ui.text (Msg.transactions_pending locale) ]) ];
          Ui.div ~cls:"txn-category" [ category_cell t ];
          Ui.span ~cls:"txn-account muted" [ Ui.text (Option.value_map (bank_side t) ~default:"" ~f:(fun a -> a.name)) ];
          Ui.span
            ~cls:("amount" ^ match dir with In _ -> " positive" | _ -> "")
            [ Ui.text (match dir with Out _ -> Fmt.format_money locale ~code (`String ("-" ^ amount)) | In _ | Between -> Fmt.format_money locale ~code (`String amount)) ] ]
    in
    let groups = List.group rows ~break:(fun a b -> not (String.equal (J.str (J.field "date" a)) (J.str (J.field "date" b)))) in
    if List.is_empty (J.list (J.field "transactions" json)) then Ui.empty (Msg.transactions_empty locale)
    else if List.is_empty rows then Ui.empty (Msg.transactions_no_match locale)
    else
      Ui.div ~cls:"table txn-table"
        (Ui.div ~cls:"table-head txn-row"
           [ Ui.span [ Ui.text (Msg.transactions_col_date locale) ]; Ui.span [ Ui.text (Msg.transactions_col_description locale) ];
             Ui.span [ Ui.text (Msg.budgets_col_category locale) ]; Ui.span [ Ui.text (Msg.transactions_col_account locale) ];
             Ui.span ~cls:"num" [ Ui.text (Msg.cashflow_col_amount locale) ] ]
        :: List.concat_map groups ~f:(fun group ->
               match group with
               | first :: _ -> Ui.div ~cls:"day-head" [ Ui.text (day_label (J.str (J.field "date" first))) ] :: List.map group ~f:row
               | [] -> []))
  in
  let toolbar =
    let banks = List.filter accounts ~f:(fun a -> Accounts_data.is_bank a && not a.closed) in
    let cats = List.filter accounts ~f:(fun a -> Accounts_data.is_category a && not a.closed) in
    Ui.div ~cls:"toolbar"
      [ Ui.div ~cls:"search"
          [ Feather_icon.svg ~size:(`Px 16) ~stroke:(`Name "currentColor") ~stroke_width:(`Px 2) Search;
            Ui.input ~type_:"search" ~placeholder:(Msg.transactions_search locale) ~value:search ~on_input:set_search () ];
        Ui.select ~value:account_filter ~options:(("", Msg.transactions_all_accounts locale) :: List.map banks ~f:(fun a -> (a.id, a.name))) ~on_change:set_account_filter;
        Ui.select ~value:(Option.value category ~default:"")
          ~options:(("", Msg.transactions_all_categories locale) :: List.map cats ~f:(fun a -> (a.id, a.name)))
          ~on_change:(fun v -> go ~category:(if String.is_empty v then None else Some v) ()) ]
  in
  Vdom.Node.main ~attrs:[ Vdom.Attr.class_ "page page-wide" ]
    ([ head; tabs ] @ if review then [ inbox ] else [ toolbar; Remote.view ~locale transactions ~f:table ])
