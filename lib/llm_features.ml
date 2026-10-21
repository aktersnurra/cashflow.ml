(* What the LLM is used for (§12): category suggestions for imported rows, a few sentences for
   the month review, and a sentence on what drove unusual spending. Neither ever posts a transfer or decides what is shown. The model sees
   merchant text, amounts, dates and category names for suggestions, and computed totals for
   summaries: never account names, numbers, balances or another member's data. *)
let max_rows = 60

let suggest_categories (service : Service.t) access ~(bank_account : Account_info.t) (rows : (int * Statement_format.row) list) =
  match service.assistant, rows with
  | None, _ | _, [] -> []
  | Some llm, rows ->
      let rows = List.filteri (fun i _ -> i < max_rows) rows in
      let categories =
        List.filter
          (fun (i : Account_info.t) ->
            (i.account.kind = Expense || i.account.kind = Income) && i.details.role = None && not i.account.closed
            && Owner.equal i.account.owner bank_account.account.owner)
          (Account_store.for_owner service.db bank_account.account.owner)
      in
      let scale = Currency.scale bank_account.account.currency in
      let user =
        Yojson.Safe.to_string
          (`Assoc
             [ ( "categories",
                 `List (List.map (fun (i : Account_info.t) -> `Assoc [ ("id", `String (Account_id.to_string i.account.id)); ("name", `String i.account.name); ("kind", `String (Account.kind_to_string i.account.kind)) ]) categories) );
               ( "rows",
                 `List
                   (List.map
                      (fun (index, (r : Statement_format.row)) ->
                        `Assoc
                          [ ("row", `Int index); ("date", `String (Date.to_string r.booking_date)); ("text", `String r.description);
                            ("counterparty", Json.option (fun s -> `String s) r.counterparty); ("amount", `String (Json.decimal ~scale r.amount)) ])
                      rows) ) ])
      in
      let system =
        "You categorise bank transactions for a personal finance ledger. Negative amounts are spending, positive ones income. For each \
         row choose the best category id from the list (expense categories for spending, income ones for income), or skip the row if \
         none fits. Reply with JSON only: {\"suggestions\": [{\"row\": <row>, \"category_id\": \"<id>\", \"confidence\": <0-100>}]}."
      in
      let result = llm.complete ~system ~user in
      Service.log_llm_call service access ~purpose:"categorise" ~items:(List.length rows) ~ok:(Result.is_ok result);
      let valid id = List.find_opt (fun (i : Account_info.t) -> Account_id.to_string i.account.id = id) categories in
      match Result.map Llm.json_of_reply result with
      | Ok (Some (`Assoc fields)) -> (
          match List.assoc_opt "suggestions" fields with
          | Some (`List items) ->
              List.filter_map
                (function
                  | `Assoc s -> (
                      match List.assoc_opt "row" s, List.assoc_opt "category_id" s, List.assoc_opt "confidence" s with
                      | Some (`Int row), Some (`String id), confidence when List.mem_assoc row rows -> (
                          match valid id with
                          | Some category ->
                              let confidence = match confidence with Some (`Int c) -> max 0 (min 80 c) | _ -> 50 in
                              Some (row, { Suggest.category_id = category.account.id; tags = []; shared = false; source = Llm; confidence; auto_apply = false })
                          | None -> None)
                      | _ -> None)
                  | _ -> None)
                items
          | _ -> [])
      | _ -> []

let cache_get = Sql.(stmt "SELECT content FROM llm_cache WHERE cache_key = ?" (text @-> nil) (opt (row (text @-> nil) Fun.id)))
let cache_put = Sql.(stmt "INSERT OR REPLACE INTO llm_cache (cache_key, content, model, created_at) VALUES (?, ?, ?, ?)" Codec.(text @-> text @-> text @-> time @-> nil) exec)

(* A few sentences on a finished cycle, written from the computed figures alone, once per user,
   cycle and language. *)
let month_summary (service : Service.t) access ~(cycle : Cycle.t) ~currency ~(summary : Analytics.cycle_summary) ~(previous : Analytics.cycle_summary option) ~(top : Analytics.category_total list) =
  match service.assistant with
  | None -> None
  | Some llm -> (
      let user = Access.user access in
      let key = Printf.sprintf "month:%s:%s:%s" (User_id.to_string user.id) (Cycle.label cycle) user.locale in
      match Sql.run service.db cache_get key with
      | Some text -> Some text
      | None -> (
          let m = Json.money currency in
          let flows (s : Analytics.cycle_summary) = `Assoc [ ("income", m s.flows.income); ("spending", m s.flows.spending); ("savings", m s.savings) ] in
          let figures =
            `Assoc
              [ ("cycle", `String (Cycle.label cycle)); ("currency", `String (Currency.code currency)); ("this_cycle", flows summary);
                ("previous_cycle", Json.option flows previous);
                ( "top_categories",
                  `List (List.map (fun (c : Analytics.category_total) -> `Assoc [ ("name", `String c.category.account.name); ("total", m c.total); ("previous", Json.option m c.previous) ]) top) ) ]
          in
          let language = if String.starts_with ~prefix:"sv" user.locale then "Swedish" else "English" in
          let system =
            Printf.sprintf
              "Write two or three short, plain sentences in %s summarising a household's month from the figures given: how spending \
               and savings compare with the previous month and which categories explain it. No advice, no greetings, no markdown." language
          in
          let result = llm.complete ~system ~user:(Yojson.Safe.to_string figures) in
          Service.log_llm_call service access ~purpose:"month_summary" ~items:1 ~ok:(Result.is_ok result);
          match result with
          | Ok text when String.trim text <> "" ->
              let text = String.trim text in
              Sql.run service.db cache_put key text llm.model (service.now ());
              Some text
          | _ -> None))

(* One sentence on what drove a category's unusual spending, from that category's transactions
   this cycle (merchant text, dates and amounts). Cached per user, category, cycle, amount and
   language, so it is written again only when the spending changes. *)
let unusual_spending (service : Service.t) access ~(cycle : Cycle.t) ~currency ~(unusual : Analytics.unusual) ~(transactions : Transfer.t list) =
  match service.assistant with
  | None -> None
  | Some llm -> (
      let user = Access.user access in
      let key =
        Printf.sprintf "unusual:%s:%s:%s:%Ld:%s" (User_id.to_string user.id) (Account_id.to_string unusual.unusual_category.account.id) (Cycle.label cycle)
          unusual.spent_so_far user.locale
      in
      match Sql.run service.db cache_get key with
      | Some text -> Some text
      | None -> (
          let m = Json.money currency in
          let figures =
            `Assoc
              [ ("category", `String unusual.unusual_category.account.name); ("currency", `String (Currency.code currency));
                ("spent_this_cycle", m unusual.spent_so_far); ("usual_whole_cycle", m unusual.usual);
                ( "transactions",
                  `List
                    (List.map
                       (fun (t : Transfer.t) ->
                         `Assoc [ ("date", Json.date t.value_date); ("text", `String t.memo.description); ("amount", m (Amount.to_int64 t.amount)) ])
                       (List.filteri (fun i _ -> i < 15) transactions)) ) ]
          in
          let language = if String.starts_with ~prefix:"sv" user.locale then "Swedish" else "English" in
          let system =
            Printf.sprintf
              "Write one short, plain sentence in %s saying what made this category's spending higher than usual, naming the \
               transactions or merchants that explain most of it. Use only the figures given. No advice, no greetings, no markdown." language
          in
          let result = llm.complete ~system ~user:(Yojson.Safe.to_string figures) in
          Service.log_llm_call service access ~purpose:"unusual_spending" ~items:1 ~ok:(Result.is_ok result);
          match result with
          | Ok text when String.trim text <> "" ->
              let text = String.trim text in
              Sql.run service.db cache_put key text llm.model (service.now ());
              Some text
          | _ -> None))
