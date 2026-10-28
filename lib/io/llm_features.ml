(* What the LLM is used for (§12): category suggestions for imported rows, a few sentences for
   the month review, and a sentence on what drove unusual spending. Neither ever posts a transfer or decides what is shown. The model sees
   merchant text, amounts, dates and category names for suggestions, and computed totals for
   summaries: never account names, numbers, balances or another member's data. *)
module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Csv = Cashflow_core.Csv
module Amount = Cashflow_core.Amount
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Cycle = Cashflow_core.Cycle
module Date = Cashflow_core.Date
module Owner = Cashflow_core.Owner
module Statement_format = Cashflow_core.Statement_format
module Suggest = Cashflow_core.Suggest
module Transfer = Cashflow_core.Transfer
module User_id = Cashflow_core.User_id

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
      let locale = (Access.user access).locale in
      let user =
        Yojson.Safe.to_string
          (`Assoc
             [ ( "categories",
                 `List (List.map (fun (i : Account_info.t) -> `Assoc [ ("id", `String (Account_id.to_string i.account.id)); ("name", `String (Starter.display_name ~locale i)); ("kind", `String (Account.kind_to_string i.account.kind)) ]) categories) );
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
      let result = llm.complete ~system ~user () in
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

(* The layout of a statement no format knows, read by the model from the file's first rows with
   every digit masked: it sees the shape and the words (headers, merchant text), never an amount,
   balance or account number. Its answer is structured output, checked here and then used to read
   the whole file, so every figure comes from the file. Cached by the masked rows, so a preview and
   its import ask once, and never inside the import's transaction. [Error None]: no model. *)
let layout_rows = 25

let layout_schema =
  let typ t = `Assoc [ ("type", t) ] in
  let nullable t = `List [ `String t; `String "null" ] in
  let obj ?(null = false) props =
    `Assoc
      [ ("type", if null then nullable "object" else `String "object"); ("properties", `Assoc props);
        ("required", `List (List.map (fun (k, _) -> `String k) props)); ("additionalProperties", `Bool false) ]
  in
  let cell = obj ~null:true [ ("row", typ (`String "integer")); ("column", typ (`String "integer")) ] in
  obj
    [ ("header_row", typ (`String "integer"));
      ( "columns",
        obj
          (List.map (fun k -> (k, typ (nullable "integer")))
             [ "booking_date"; "transaction_date"; "description"; "counterparty"; "amount"; "money_out"; "money_in"; "balance"; "reference" ]) );
      ("date_order", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "YMD"; `String "DMY"; `String "MDY" ]) ]);
      ("spending_positive", typ (`String "boolean")); ("institution", typ (nullable "string")); ("account_name_cell", cell); ("account_number_cell", cell) ]

let layout_system =
  "You read the layout of a bank statement export for a personal finance ledger. Every digit in it has been replaced by 9, so you \
   see the shape of the file, not its figures. Rows are numbered from 0, and so are the cells in a row. Give the row holding the \
   column headers above the transactions, and the column of each field, or null when the file has none: booking_date (the day the \
   bank booked the row; when there is only one date, that one), transaction_date (the purchase day, if given separately), \
   description, counterparty, amount (one signed column), money_out and money_in (separate columns instead of amount), balance (the \
   running balance after each row) and reference (the bank's own transaction id). date_order is the order of year, month and day \
   in the dates, judged from the headers' language and the bank. spending_positive is true only when purchases are written as \
   positive amounts, as some card exports do. institution is the bank's name if the file gives it. account_name_cell and \
   account_number_cell are the cells above the headers naming the account and holding its number, or null."

let layout_of_json json : (Statement_format.layout, string) result =
  let open Yojson.Safe.Util in
  try
    let int_opt = function `Int i -> Some i | _ -> None in
    let cell = function `Assoc _ as c -> Some (to_int (member "row" c), to_int (member "column" c)) | _ -> None in
    let columns = member "columns" json in
    let column (name, kind) = Option.map (fun i -> (kind, i)) (int_opt (member name columns)) in
    Ok
      { Statement_format.header_row = to_int (member "header_row" json);
        columns =
          List.filter_map column
            [ ("booking_date", `Booking); ("transaction_date", `Transaction); ("description", `Description); ("counterparty", `Counterparty);
              ("amount", `Amount); ("money_out", `Out); ("money_in", `In); ("balance", `Balance); ("reference", `Reference) ];
        date_order = (match member "date_order" json with `String "DMY" -> Statement_format.Dmy | `String "MDY" -> Statement_format.Mdy | _ -> Statement_format.Ymd);
        spending_positive = (match member "spending_positive" json with `Bool b -> b | _ -> false);
        institution = (match member "institution" json with `String s when String.trim s <> "" -> Some (String.trim s) | _ -> None);
        account_name_cell = cell (member "account_name_cell" json);
        account_number_cell = cell (member "account_number_cell" json) }
  with Type_error (reason, _) -> Error reason

(* Rows that might each be one half of a move between the user's own accounts, with the rows that might
   be the other half. The model sees dates, texts and amounts, never account names or balances. It answers
   only for the pairs it is sure of; the caller still checks them. Returns (row id, counterpart id). *)
type move_case = { id : string; date : Date.t; text : string; amount : string; options : (string * Date.t * string) list }

let min_move_confidence = 80

let pair_moves (service : Service.t) access (cases : move_case list) =
  match service.assistant, cases with
  | None, _ | _, [] -> []
  | Some llm, cases ->
      let cases = List.filteri (fun i _ -> i < max_rows) cases in
      let user =
        Yojson.Safe.to_string
          (`List
             (List.map
                (fun c ->
                  `Assoc
                    [ ("id", `String c.id); ("date", `String (Date.to_string c.date)); ("text", `String c.text); ("amount", `String c.amount);
                      ( "options",
                        `List (List.map (fun (id, date, text) -> `Assoc [ ("id", `String id); ("date", `String (Date.to_string date)); ("text", `String text) ]) c.options) ) ])
                cases))
      in
      let system =
        "You match bank transactions that are one transfer of the user's own money between two of their accounts: the same money leaving \
         one account and arriving in another, a few days apart. For each case choose the option that is the other half of the transfer, \
         judging from the texts and dates, or leave it out if none clearly is. A purchase and an unrelated deposit of the same amount are not \
         a transfer. Reply with JSON only: {\"pairs\": [{\"id\": <id>, \"option\": <option id>, \"confidence\": <0-100>}]}."
      in
      let result = llm.complete ~system ~user () in
      Service.log_llm_call service access ~purpose:"pair_moves" ~items:(List.length cases) ~ok:(Result.is_ok result);
      (match Result.map Llm.json_of_reply result with
      | Ok (Some (`Assoc fields)) -> (
          match List.assoc_opt "pairs" fields with
          | Some (`List items) ->
              List.filter_map
                (function
                  | `Assoc p -> (
                      match List.assoc_opt "id" p, List.assoc_opt "option" p, List.assoc_opt "confidence" p with
                      | Some (`String id), Some (`String option), Some (`Int c) when c >= min_move_confidence -> (
                          match List.find_opt (fun case -> case.id = id) cases with
                          | Some case when List.exists (fun (o, _, _) -> o = option) case.options -> Some (id, option)
                          | _ -> None)
                      | _ -> None)
                  | _ -> None)
                items
          | _ -> [])
      | _ -> [])

let statement_layout (service : Service.t) access (table : Csv.table) =
  match service.assistant with
  | None -> Error None
  | Some llm -> (
      let mask cell =
        let cell = if String.length cell > 80 then String.sub cell 0 80 else cell in
        String.map (fun c -> if c >= '0' && c <= '9' then '9' else c) cell
      in
      let sample = List.filteri (fun i _ -> i < layout_rows) table in
      let user =
        Yojson.Safe.to_string
          (`Assoc [ ("rows", `List (List.mapi (fun i row -> `Assoc [ ("row", `Int i); ("cells", `List (List.map (fun c -> `String (mask c)) row)) ]) sample)) ])
      in
      let key = "layout:" ^ Digestif.SHA256.(to_hex (digest_string user)) in
      let check reply =
        match Llm.json_of_reply reply with
        | None -> Error "the reply wasn't JSON"
        | Some json ->
            let ( let* ) = Result.bind in
            let* layout = layout_of_json json in
            let* () = Statement_format.layout_valid layout table in
            let (module F) = Statement_format.of_layout layout in
            (match F.parse ~scale:2 table with
            | Ok (_ :: _) -> Ok layout
            | Ok [] -> Error "no transactions under the header it found"
            | Error reason -> Error reason)
      in
      match Sql.run service.db cache_get key with
      | Some reply -> Result.map_error Option.some (check reply)
      | None -> (
          let result = llm.complete ~schema:("statement_layout", layout_schema) ~system:layout_system ~user () in
          Service.log_llm_call service access ~purpose:"statement_layout" ~items:(List.length sample) ~ok:(Result.is_ok result);
          match result with
          | Error reason -> Error (Some reason)
          | Ok reply -> (
              match check reply with
              | Ok layout ->
                  Sql.run service.db cache_put key reply llm.model (service.now ());
                  Ok layout
              | Error reason -> Error (Some reason))))

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
                  `List (List.map (fun (c : Analytics.category_total) -> `Assoc [ ("name", `String (Starter.display_name ~locale:user.locale c.category)); ("total", m c.total); ("previous", Json.option m c.previous) ]) top) ) ]
          in
          let language = if String.starts_with ~prefix:"sv" user.locale then "Swedish" else "English" in
          let system =
            Printf.sprintf
              "Write two or three short, plain sentences in %s summarising a household's month from the figures given: how spending \
               and savings compare with the previous month and which categories explain it. No advice, no greetings, no markdown." language
          in
          let result = llm.complete ~system ~user:(Yojson.Safe.to_string figures) () in
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
              [ ("category", `String (Starter.display_name ~locale:user.locale unusual.unusual_category)); ("currency", `String (Currency.code currency));
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
          let result = llm.complete ~system ~user:(Yojson.Safe.to_string figures) () in
          Service.log_llm_call service access ~purpose:"unusual_spending" ~items:1 ~ok:(Result.is_ok result);
          match result with
          | Ok text when String.trim text <> "" ->
              let text = String.trim text in
              Sql.run service.db cache_put key text llm.model (service.now ());
              Some text
          | _ -> None))

(* Two or three sentences on the last full week, written from its computed figures alone. Cached per user, week, language
   and a digest of the figures, so the text is written again only when the facts behind it change, and never per
   transaction. With [write] false only a text already written is returned. A failed attempt is remembered for the same
   figures, so a provider that is down is not asked again on every refresh. *)
let week_summary (service : Service.t) access ~write ~(last : Date.t) ~figures =
  match service.assistant with
  | None -> `Off
  | Some llm -> (
      let user = Access.user access in
      let digest = String.sub (Digest.to_hex (Digest.string (Yojson.Safe.to_string figures))) 0 12 in
      let key = Printf.sprintf "week:%s:%s:%s:%s" (User_id.to_string user.id) (Date.to_string last) digest user.locale in
      let failed_key = key ^ ":failed" in
      match Sql.run service.db cache_get key with
      | Some text -> `Text text
      | None when Option.is_some (Sql.run service.db cache_get failed_key) -> `Failed
      | None when not write -> `Unwritten
      | None -> (
          let language = if String.starts_with ~prefix:"sv" user.locale then "Swedish" else "English" in
          let system =
            Printf.sprintf
              "Write two or three short, plain sentences in %s summarising a household's last week from the figures given: how \
               spending compares with the week before and which categories explain it. Use only the figures given. No advice, no \
               greetings, no markdown." language
          in
          let result = llm.complete ~system ~user:(Yojson.Safe.to_string figures) () in
          Service.log_llm_call service access ~purpose:"week_summary" ~items:1 ~ok:(Result.is_ok result);
          match result with
          | Ok text when String.trim text <> "" ->
              let text = String.trim text in
              Sql.run service.db cache_put key text llm.model (service.now ());
              `Text text
          | _ ->
              Sql.run service.db cache_put failed_key "failed" llm.model (service.now ());
              `Failed))
