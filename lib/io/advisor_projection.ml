open Cashflow_core
module A = Advisor
module T = Cashflow_workflow.Advisor_tools

let ( let* ) = Result.bind
let error code = Error { A.code; context = A.No_context }
let limit kind = Error { A.code = A.Run_limit_reached; context = A.Limit kind }

exception Invalid

let require = function Some x -> x | None -> raise Invalid
let get = function Ok x -> x | Error _ -> raise Invalid

let unique fields =
  let keys = List.map fst fields in
  if List.length keys <> List.length (List.sort_uniq String.compare keys) then raise Invalid

let fields allowed = function
  | `Assoc f ->
      unique f;
      if List.exists (fun (k, _) -> not (List.mem k allowed)) f then raise Invalid;
      f
  | _ -> raise Invalid

let text f k =
  match List.assoc_opt k f with
  | None -> None
  | Some (`String s) when String.length s <= 512 -> Some s
  | _ -> raise Invalid

let integer f k default lo hi =
  match List.assoc_opt k f with None -> default | Some (`Int n) when n >= lo && n <= hi -> n | _ -> raise Invalid

let boolean f k = match List.assoc_opt k f with None -> false | Some (`Bool b) -> b | _ -> raise Invalid

let items f k max =
  match List.assoc_opt k f with None -> [] | Some (`List xs) when List.length xs <= max -> xs | _ -> raise Invalid

let parse_call ~name ~arguments =
  let allowed =
    match name with
    | "cycle_summary" -> Some [ "cycles" ]
    | "category_spending" | "budget_status" -> Some [ "cycle_offset" ]
    | "category_trend" -> Some [ "category"; "cycles" ]
    | "relevant_transactions" -> Some [ "cycle_offset"; "category"; "limit" ]
    | "recurring" | "goals" -> Some []
    | "forecast" -> Some [ "cycles"; "scenario" ]
    | "propose_set_budget" -> Some [ "category"; "monthly_amount"; "rollover" ]
    | "inbox_rows" | "categories" | "period_outlook" -> Some []
    | "explain_month" -> Some [ "cycle_offset" ]
    | "propose_categorize" -> Some [ "rows" ]
    | "propose_add_rule" -> Some [ "pattern"; "category"; "direction" ]
    | "bank_accounts" -> Some []
    | "propose_entry" -> Some [ "from_account"; "to_account"; "amount"; "date"; "description" ]
    | "propose_save_goal" -> Some [ "name"; "target_amount"; "target_date"; "monthly_amount" ]
    | "propose_update_goal" -> Some [ "goal"; "name"; "target_amount"; "target_date"; "monthly_amount" ]
    | "propose_new_category" -> Some [ "name"; "kind" ]
    | "file_inbox" -> Some [ "keep_on" ]
    | _ -> None
  in
  match allowed with
  | None -> error A.Tool_not_allowed
  | Some allowed -> (
      try
        if String.length arguments > 8192 then raise Invalid;
        (* A proposal, or filing, never takes a currency: it acts in the conversation's own. *)
        let proposing = String.starts_with ~prefix:"propose_" name || name = "file_inbox" in
        let f = fields ((if proposing then [] else [ "currency" ]) @ allowed) (Yojson.Safe.from_string arguments) in
        let currency = Option.map (fun s -> require (T.currency_code s)) (text f "currency") in
        let c = { T.currency } in
        let cycles () = integer f "cycles" 6 1 6 in
        let period () = T.Cycle_offset (integer f "cycle_offset" 0 (-5) 0) in
        let category () = Option.map (fun s -> require (T.account_reference s)) (text f "category") in
        Ok
          (match name with
          | "cycle_summary" -> T.Cycle_summary (c, cycles ())
          | "category_spending" -> T.Category_spending (c, period ())
          | "category_trend" -> T.Category_trend (c, require (category ()), cycles ())
          | "relevant_transactions" -> T.Relevant_transactions (c, period (), category (), integer f "limit" 20 1 20)
          | "recurring" -> T.Recurring c
          | "budget_status" -> T.Budget_status (c, integer f "cycle_offset" 0 (-5) 0)
          | "goals" -> T.Goals c
          | "inbox_rows" -> T.Inbox_rows c
          | "period_outlook" -> T.Period_outlook c
          | "explain_month" -> T.Explain_cycle (c, integer f "cycle_offset" (-1) (-5) (-1))
          | "file_inbox" -> T.File_inbox { keep_on = boolean f "keep_on" }
          | "categories" -> T.Categories c
          | "bank_accounts" -> T.Bank_accounts c
          | "propose_entry" ->
              let text f k = match List.assoc_opt k f with Some `Null -> None | _ -> text f k in
              let amount = require (text f "amount") in
              if String.length amount > 30 then raise Invalid;
              T.Propose_entry
                { from_account = require (T.account_reference (require (text f "from_account"))); to_account = require (T.account_reference (require (text f "to_account")));
                  amount; date = text f "date"; description = require (text f "description") }
          | "propose_add_rule" ->
              (* Models often send null for an optional field; that means the same as leaving it out. *)
              let text f k = match List.assoc_opt k f with Some `Null -> None | _ -> text f k in
              let direction = match text f "direction" with None -> None | Some "in" -> Some `In | Some "out" -> Some `Out | Some _ -> raise Invalid in
              T.Propose_rule { pattern = require (text f "pattern"); category = require (T.account_reference (require (text f "category"))); direction }
          | "propose_save_goal" ->
              let text f k = match List.assoc_opt k f with Some `Null -> None | _ -> text f k in
              let short k = Option.map (fun s -> if String.length s > 30 then raise Invalid else s) (text f k) in
              T.Propose_goal { name = require (text f "name"); target_amount = require (short "target_amount"); target_date = short "target_date"; monthly_amount = short "monthly_amount" }
          | "propose_update_goal" ->
              let text f k = match List.assoc_opt k f with Some `Null -> None | _ -> text f k in
              let short k = Option.map (fun s -> if String.length s > 30 then raise Invalid else s) (text f k) in
              T.Propose_update_goal
                { goal = require (T.goal_reference (require (text f "goal"))); name = text f "name"; target_amount = short "target_amount"; target_date = short "target_date";
                  monthly_amount = short "monthly_amount" }
          | "propose_new_category" ->
              let kind = match require (text f "kind") with "expense" -> `Expense | "income" -> `Income | _ -> raise Invalid in
              T.Propose_category { name = require (text f "name"); kind }
          | "propose_categorize" ->
              let rows =
                List.map
                  (fun x ->
                    let o = fields [ "row"; "category" ] x in
                    (require (T.row_reference (require (text o "row"))), require (T.account_reference (require (text o "category")))))
                  (items f "rows" 20)
              in
              T.Propose_categorize { rows }
          | "propose_set_budget" ->
              let amount = require (text f "monthly_amount") in
              if String.length amount > 30 then raise Invalid;
              T.Propose_budget { category = require (category ()); monthly_amount = amount; rollover = boolean f "rollover" }
          | "forecast" ->
              let horizon = integer f "cycles" 3 1 6 in
              if not (List.mem horizon [ 1; 3; 6 ]) then raise Invalid;
              let s =
                match List.assoc_opt "scenario" f with
                | None -> []
                | Some x -> fields [ "leave_out"; "one_off"; "follow_budgets" ] x
              in
              let leave_out =
                List.map
                  (function `String s -> require (T.series_reference s) | _ -> raise Invalid)
                  (items s "leave_out" 20)
              in
              let one_off =
                List.map
                  (fun x ->
                    let o = fields [ "date"; "amount"; "description" ] x in
                    let d = get (Date.of_string (require (text o "date"))) in
                    let a = require (text o "amount") in
                    if String.length a > 30 then raise Invalid;
                    (d, a, Option.value (text o "description") ~default:""))
                  (items s "one_off" 10)
              in
              T.Forecast
                ( c,
                  horizon,
                  {
                    T.leave_out;
                    input =
                      {
                        Cashflow_workflow.Forecast_request.leave_out = [];
                        one_off;
                        follow_budgets = boolean s "follow_budgets";
                      };
                  } )
          | _ -> raise Invalid)
      with Invalid | Yojson.Json_error _ | Stack_overflow -> error A.Invalid_tool_arguments)

type aliases = {
  service : Service.t;
  principal : Advisor_access.principal;
  scope : A.scope;
  conversation : A.Conversation_id.t;
  owner : User_id.t;
  nonce : string;
  accounts : (Account_id.t, string) Hashtbl.t;
  series : (string, string) Hashtbl.t;
  rows : (string, Transfer_id.t) Hashtbl.t;  (** inbox row aliases issued to this run, never persisted *)
  goals : (string, string) Hashtbl.t;  (** goal aliases issued to this run, never persisted *)
  evidence : (A.Evidence_id.t, T.evidence) Hashtbl.t;
  selected : (A.Evidence_id.t, int) Hashtbl.t;
}

let store service = Advisor_store.create ~clock:service.Service.now service.db

let create_aliases (service : Service.t) ~principal ~scope ~conversation =
  let* () = Advisor_access.authorize_history service principal conversation in
  let* user = Advisor_access.user service principal in
  let* metadata = Advisor_store.metadata (store service) ~owner:user.id conversation in
  if metadata.scope <> scope then error A.Scope_mismatch
  else
    Ok
      {
        service;
        principal;
        scope;
        conversation;
        owner = user.id;
        nonce =
          String.concat ""
            (String.split_on_char '-' (Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ())));
        accounts = Hashtbl.create 32;
        series = Hashtbl.create 20;
        rows = Hashtbl.create 32;
        goals = Hashtbl.create 20;
        evidence = Hashtbl.create 8;
        selected = Hashtbl.create 8;
      }

let authorize service ~principal ~scope aliases =
  if service != aliases.service || principal != aliases.principal || scope <> aliases.scope then error A.Access_denied
  else
    let* () = Advisor_access.authorize_scope service principal scope in
    Advisor_access.authorize_history service principal aliases.conversation

(* Categories are named as the user sees them; bank accounts are never named to the model. *)
let account ~locale (i : Account_info.t) : T.account =
  let name = match i.account.kind with Account.Expense | Income -> Some (Starter.display_name ~locale i) | Asset | Liability | Equity -> None in
  { id = i.account.id; kind = i.account.kind; name }

let scoped_book (ctx : Insights.ctx) =
  let book = Insights.book ctx in
  fun id -> Option.map (account ~locale:(Access.user ctx.access).locale) (book.inside id)

let take n xs = List.filteri (fun i _ -> i < n) xs

let service_result = function
  | Ok x -> Ok x
  | Error (`Not_found _) -> error A.Not_found
  | Error (`Forbidden _) -> error A.Access_denied
  | Error (`Invalid _) -> error A.Invalid_tool_arguments
  | Error (`Storage_unavailable _) -> error A.Storage_unavailable

let resolve_category service aliases ctx reference =
  let* id =
    Advisor_store.resolve_account_alias (store service) ~owner:aliases.owner ~conversation:aliases.conversation
      ~alias:(T.account_reference_text reference)
  in
  match scoped_book ctx id with
  | Some a when a.kind = Account.Expense || a.kind = Account.Income -> Ok a
  | _ -> error A.Not_found

(* Any account in the pinned book that this conversation has been shown the alias of. *)
let resolve_account service aliases ctx reference =
  let* id =
    Advisor_store.resolve_account_alias (store service) ~owner:aliases.owner ~conversation:aliases.conversation
      ~alias:(T.account_reference_text reference)
  in
  match scoped_book ctx id with Some a -> Ok a | None -> error A.Not_found

(* An account's name with every digit and asterisk taken out: enough for "my savings", never part of a number. *)
let name_hint name =
  let kept = String.concat "" (List.map (String.make 1) (List.filter (fun c -> not (Char.equal c '*' || (c >= '0' && c <= '9'))) (List.init (String.length name) (String.get name)))) in
  let words = List.filter (fun w -> w <> "") (String.split_on_char ' ' kept) in
  let hint = String.concat " " words in
  if hint = "" then "account"
  else if String.length hint <= 40 then hint
  else (
    let cut = ref 40 in
    while !cut > 0 && Char.code hint.[!cut] land 0xC0 = 0x80 do decr cut done;
    String.sub hint 0 !cut)

(* What the user sees for an account: categories in their language, other accounts as they named them. *)
let shown_name ~locale (i : Account_info.t) =
  match i.account.kind with Account.Expense | Income -> Starter.display_name ~locale i | Asset | Liability | Equity -> i.account.name

let period (T.Cycle_offset n) = Insights.Cycle_offset n

(* A name or pattern the user will see and later search for: trimmed, short, one line. *)
let one_line ~max ~min s =
  let s = String.trim s in
  (* Valid UTF-8 only, counted in characters (as the schema's maxLength is), and no control, format, line or paragraph
     separator characters: nothing that could make the card show something other than what is stored. *)
  let hidden cp =
    cp < 0x20 || (cp >= 0x7f && cp <= 0x9f) || (cp >= 0x200b && cp <= 0x200f) || (cp >= 0x2028 && cp <= 0x202e)
    || (cp >= 0x2060 && cp <= 0x206f) || cp = 0xfeff || cp = 0x061c || cp = 0x180e
  in
  let rec scan i count =
    if i >= String.length s then Some count
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) || hidden (Uchar.to_int (Uchar.utf_decode_uchar d)) then None
      else scan (i + Uchar.utf_decode_length d) (count + 1)
  in
  match scan 0 0 with Some count when count >= min && count <= max -> Some s | _ -> None

let refs = function
  | T.Category_spending_result xs -> List.map (fun (x : T.category) -> x.account) xs
  | T.Category_trend_result (a, _) -> [ a ]
  | T.Relevant_transactions_result xs ->
      List.concat_map (fun (x : T.transaction) -> List.filter_map Fun.id [ x.debit; x.credit ]) xs
  | T.Recurring_result xs ->
      List.concat_map (fun (x : T.payment) -> List.filter_map Fun.id [ x.category; x.paid_from ]) xs
  | T.Budget_status_result (_, xs) -> List.map (fun (x : T.budget) -> x.category) xs
  | T.Forecast_result f ->
      List.concat_map (fun (x : T.forecast_item) -> List.filter_map Fun.id [ x.account; x.category ]) f.items
  | T.Budget_proposal_result p -> [ p.category ]
  | T.Inbox_rows_result rows -> List.filter_map (fun (r : T.inbox_row) -> Option.map (fun (a, _, _) -> a) r.suggestion) rows
  | T.Categories_result xs -> xs
  | T.Categorize_proposal_result rows -> List.map (fun (r : T.categorize_row) -> r.category) rows
  | T.Proposal_card_result c -> c.accounts
  | T.Bank_accounts_result xs -> List.map (fun (b : T.bank_account) -> b.account) xs
  | T.Filed_inbox_result { filed; unsure; _ } ->
      List.filter_map (fun (r : T.inbox_row) -> Option.map (fun (a, _, _) -> a) r.suggestion) (filed @ unsure)
  | T.Explain_result { categories; _ } -> categories
  | T.Cycle_summary_result _ | T.Goals_result _ | T.Period_outlook_result _ -> []

let assumptions (f : Forecast.t) =
  List.map
    (function
      | Forecast.Usual_spending { per_cycle; first; last; days } ->
          Printf.sprintf "usual_spending: %Ld minor units; %s to %s; %d days" per_cycle (Date.to_string first)
            (Date.to_string last) days
      | Stopped _ -> "stopped_series_excluded"
      | Stale_import d -> "stale_import: " ^ Date.to_string d
      | Other_currency _ -> "other_currency_accounts_excluded"
      | Short_history d -> "short_history: " ^ string_of_int d
      | No_income_detected -> "no_income_detected"
      | Range_from { forecasts; since } ->
          Printf.sprintf "range_from: %d forecasts since %s" forecasts (Date.to_string since)
      | No_range_yet -> "no_range_yet"
      | Seasonal { month; year; percent } -> Printf.sprintf "seasonal: %d-%02d %d percent" year month percent)
    f.assumptions

(* [filed] is what file_inbox already did (Autofile.file, in the caller's transaction): read only reports it. *)
let read ?filed (service : Service.t) ~principal ~scope ~aliases ~currency:pinned request =
  try
    let* detached =
      Sql.read_snapshot service.db (fun () ->
          let* () = authorize service ~principal ~scope aliases in
          let* () = if Hashtbl.length aliases.evidence >= A.limits.tool_calls then limit A.Tool_calls else Ok () in
          let* access = Advisor_access.access service principal in
          let common = T.common request in
          (* A conversation is pinned to one currency for its whole life: an omitted argument means the
             pinned one, never the user's current default, and any other currency is rejected before reading. *)
          let* currency =
            match Option.map T.currency_code_text common.currency with
            | Some code when not (String.equal code (Currency.code pinned)) -> error A.Invalid_tool_arguments
            | _ -> Ok (Some (Currency.code pinned))
          in
          let query_scope = match scope with A.Personal -> Analytics.Me | Household _ -> Analytics.Household in
          let qc = { Query.scope = query_scope; currency } in
          let* ctx = service_result (Insights.context service access ~scope:query_scope ?currency ()) in
          let scoped = scoped_book ctx in
          let run q = Result.map fst (service_result (Query.run service access q)) in
          let cycle_ok n = n >= 1 && n <= 6 in
          let offset_ok n = n >= -5 && n <= 0 in
          let valid =
            match request with
            | T.Cycle_summary (_, n) | T.Category_trend (_, _, n) -> cycle_ok n
            | Category_spending (_, Cycle_offset n) | Budget_status (_, n) -> offset_ok n
            | Relevant_transactions (_, Cycle_offset n, _, count) -> offset_ok n && count >= 1 && count <= 20
            | Forecast (_, n, s) ->
                List.mem n [ 1; 3; 6 ]
                && s.input.leave_out = []
                && List.length s.leave_out <= 20
                && List.length s.input.one_off <= 10
            | Recurring _ | Goals _ | Propose_budget _ | Inbox_rows _ | Categories _ | Propose_rule _ | Propose_goal _ | Propose_update_goal _ | Propose_category _ | Bank_accounts _ | Propose_entry _ | File_inbox _ | Period_outlook _ -> true
            | Explain_cycle (_, n) -> n >= -5 && n <= -1
            | Propose_categorize { rows } ->
                let n = List.length rows in
                n >= 1 && n <= 20 && List.length (List.sort_uniq String.compare (List.map (fun (r, _) -> T.row_reference_text r) rows)) = n
          in
          if not valid then error A.Invalid_tool_arguments
          else
            let* result, total_count, returned_count, extra_assumptions =
              match request with
              | T.Cycle_summary (_, n) ->
                  let* xs = run (Query.Cashflow_summary (qc, n)) in
                  Ok (T.Cycle_summary_result xs, List.length xs, List.length xs, [])
              | Explain_cycle (_, offset) -> (
                  let cycle = Insights.cycle_at ctx offset in
                  (* Only a cycle the statements cover to its end: half a cycle is not explained. *)
                  let covered = match Insights.covered_through ctx with Some d -> Date.compare cycle.last d <= 0 | None -> true in
                  let floor = Int64.mul 300L (Int64.of_float (10. ** float_of_int (Currency.scale ctx.currency))) in
                  match
                    if covered then
                      Explain.explain (Insights.book ctx) ~setting:ctx.setting ~floor ~cycle
                        (Insights.transfers ctx ~first:(Date.add_days cycle.first (-400)) ~last:cycle.last)
                    else None
                  with
                  | None -> error A.Not_found
                  | Some e ->
                      let categories =
                        List.filter_map
                          (function
                            | Explain.Yearly { category; _ } | One_off { category; _ } | Habit { category; _ } | Less { category; _ } ->
                                scoped category.account.id
                            | Income_change _ -> None)
                          e.drivers
                      in
                      Ok (T.Explain_result { explanation = e; categories }, 1, 1, [ "compared with the average of compared_with earlier cycles" ]))
              | Period_outlook _ ->
                  let* o = run (Query.Period_outlook qc) in
                  Ok
                    ( T.Period_outlook_result o,
                      1,
                      1,
                      [ "expected is a projection from the recurring payments still due and usual spending, not observed; so_far runs only through covered_through" ] )
              | Category_spending (_, p) ->
                  let* xs = run (Query.Spending_by_category (qc, period p, true)) in
                  let ys =
                    take 20 xs
                    |> List.map (fun (x : Analytics.category_total) ->
                        { T.account = account ~locale:(Access.user ctx.access).locale x.category; total = x.total; previous = x.previous })
                  in
                  Ok (T.Category_spending_result ys, List.length xs, List.length ys, [])
              | Category_trend (_, reference, n) ->
                  let* a = resolve_category service aliases ctx reference in
                  let* _, xs = run (Query.Category_trend (qc, a.id, n)) in
                  Ok (T.Category_trend_result (a, xs), List.length xs, List.length xs, [])
              | Relevant_transactions (_, p, category, count) ->
                  let* category =
                    match category with
                    | None -> Ok None
                    | Some r ->
                        let* a = resolve_category service aliases ctx r in
                        if a.kind = Account.Expense then Ok (Some a) else error A.Not_found
                  in
                  let* xs = run (Query.Largest_expenses (qc, period p, max_int)) in
                  let xs =
                    List.filter
                      (fun (v : Service.transfer_view) ->
                        match category with None -> true | Some a -> Account_id.equal v.transfer.debit_account_id a.id)
                      xs
                  in
                  let ys =
                    take count xs
                    |> List.map (fun (v : Service.transfer_view) ->
                        let t = v.transfer in
                        (* The pinned book is stricter than the UI's combined membership visibility. *)
                        let debit = scoped t.debit_account_id and credit = scoped t.credit_account_id in
                        let description =
                          match (debit, credit) with Some _, Some _ -> t.memo.description | _ -> "masked counterpart"
                        in
                        { T.date = t.value_date; amount = Amount.to_int64 t.amount; description; debit; credit })
                  in
                  Ok
                    ( T.Relevant_transactions_result ys,
                      List.length xs,
                      List.length ys,
                      [ "partial samples do not establish a complete explanation" ] )
              | Inbox_rows _ ->
                  let open_rows =
                    Inbox.list service access ~limit:500
                    |> List.filter (fun (v : Inbox.item_view) -> v.item.status = "open" && Option.is_some (scoped v.item.bank_account_id))
                  in
                  let alias_for id =
                    match Hashtbl.fold (fun a t found -> if Transfer_id.equal t id then Some a else found) aliases.rows None with
                    | Some a -> a
                    | None ->
                        let a = Printf.sprintf "row_%s_%d" aliases.nonce (Hashtbl.length aliases.rows + 1) in
                        Hashtbl.add aliases.rows a id;
                        a
                  in
                  let shown =
                    take 20 open_rows
                    |> List.map (fun (v : Inbox.item_view) ->
                        let t = v.transfer.transfer in
                        let suggestion =
                          match (v.suggested, v.item.suggestion_confidence) with
                          | Some info, confidence -> (
                              match scoped info.account.id with
                              | Some a when a.kind = Account.Expense || a.kind = Account.Income ->
                                  Some (a, Option.value confidence ~default:0, Option.value v.item.suggestion_source ~default:"unknown")
                              | _ -> None)
                          | None, _ -> None
                        in
                        { T.reference = require (T.row_reference (alias_for t.id)); date = t.value_date; amount = Amount.to_int64 t.amount;
                          outflow = Inbox.outflow v.item t; description = t.memo.description; suggestion })
                  in
                  Ok (T.Inbox_rows_result shown, List.length open_rows, List.length shown, [])
              | File_inbox _ -> (
                  match (filed : Autofile.report option) with
                  | None -> error A.Invalid_request
                  | Some report ->
                      let alias_for id =
                        match Hashtbl.fold (fun a t found -> if Transfer_id.equal t id then Some a else found) aliases.rows None with
                        | Some a -> a
                        | None ->
                            let a = Printf.sprintf "row_%s_%d" aliases.nonce (Hashtbl.length aliases.rows + 1) in
                            Hashtbl.add aliases.rows a id;
                            a
                      in
                      let row (r : Autofile.row) =
                        let t = r.transfer in
                        let suggestion =
                          Option.bind r.guess (fun ((c : Account_info.t), confidence, source) ->
                              match scoped c.account.id with
                              | Some a when a.kind = Account.Expense || a.kind = Account.Income -> Some (a, confidence, source)
                              | _ -> None)
                        in
                        { T.reference = require (T.row_reference (alias_for t.id)); date = t.value_date; amount = Amount.to_int64 t.amount;
                          outflow = r.outflow; description = t.memo.description; suggestion }
                      in
                      (* Every filed row is reported (the model must say what changed); unsure ones up to twenty, the
                         rest wait in the inbox where the user sees them all. *)
                      let unsure = List.map row report.unsure in
                      let shown = take 20 unsure in
                      Ok
                        ( T.Filed_inbox_result { filed = List.map row report.filed; unsure = shown; kept_on = report.kept_on },
                          List.length report.filed + List.length unsure,
                          List.length report.filed + List.length shown,
                          [ Printf.sprintf "filed without asking at %d percent confidence or more; each can be recategorised" Autofile.threshold ] ))
              | Categories _ ->
                  let all =
                    Insights.accounts ctx
                    |> List.filter (fun (i : Account_info.t) ->
                        (i.account.kind = Account.Expense || i.account.kind = Account.Income)
                        && Option.is_none i.details.role && (not i.account.closed) && Option.is_some (scoped i.account.id))
                    |> List.sort (fun (a : Account_info.t) (b : Account_info.t) -> String.compare a.account.name b.account.name)
                  in
                  let shown = take 60 all |> List.map (account ~locale:(Access.user ctx.access).locale) in
                  Ok (T.Categories_result shown, List.length all, List.length shown, [])
              | Bank_accounts _ ->
                  let all =
                    Insights.accounts ctx
                    |> List.filter (fun (i : Account_info.t) ->
                        (i.account.kind = Account.Asset || i.account.kind = Account.Liability)
                        && Option.is_none i.details.role && (not i.account.closed) && Option.is_some (scoped i.account.id))
                    |> List.sort (fun (a : Account_info.t) (b : Account_info.t) -> String.compare a.account.name b.account.name)
                  in
                  let shown =
                    take 30 all
                    |> List.map (fun (i : Account_info.t) ->
                        { T.account = account ~locale:(Access.user ctx.access).locale i; institution = Option.map name_hint i.details.institution; hint = name_hint i.account.name;
                          statement_backed = i.details.statement_backed })
                  in
                  Ok (T.Bank_accounts_result shown, List.length all, List.length shown, [])
              | Propose_entry { from_account; to_account; amount; date; description } ->
                  let* from_ = resolve_account service aliases ctx from_account in
                  let* to_ = resolve_account service aliases ctx to_account in
                  let* description = match one_line ~min:1 ~max:80 description with Some d -> Ok d | None -> error A.Invalid_tool_arguments in
                  let* minor =
                    match Json.parse_decimal ~scale:(Currency.scale ctx.currency) amount with
                    | Some n when Int64.compare n 0L > 0 -> Ok n
                    | _ -> error A.Invalid_tool_arguments
                  in
                  let* on =
                    match date with
                    | None -> Ok ctx.today
                    | Some d -> (
                        (* Within the last year or the next month: an entry is something that just happened or is about to. *)
                        match Date.of_string d with
                        | Ok d when Date.compare d (Date.add_days ctx.today (-366)) >= 0 && Date.compare d (Date.add_days ctx.today 31) <= 0 -> Ok d
                        | _ -> error A.Invalid_tool_arguments)
                  in
                  let* from_info = match Service.find_account service access from_.id with Ok i -> Ok i | Error _ -> error A.Not_found in
                  let* to_info = match Service.find_account service access to_.id with Ok i -> Ok i | Error _ -> error A.Not_found in
                  let balance_sheet (k : Account.kind) = k = Account.Asset || k = Account.Liability in
                  (* A purchase, income, or a move between the user's own accounts; never a system account. *)
                  let sensible =
                    (balance_sheet from_.kind && to_.kind = Account.Expense) || (from_.kind = Account.Income && balance_sheet to_.kind)
                    || (balance_sheet from_.kind && balance_sheet to_.kind)
                  in
                  if (not sensible) || Account_id.equal from_.id to_.id || Option.is_some from_info.details.role || Option.is_some to_info.details.role
                     || from_info.account.closed || to_info.account.closed
                  then error A.Invalid_tool_arguments
                  else
                    let pending = from_info.details.statement_backed || to_info.details.statement_backed in
                    Ok
                      ( T.Proposal_card_result
                          { tool = "propose_entry"; kind = "entry";
                            facts =
                              [ { A.label = "From"; value = A.Text (shown_name ~locale:(Access.user ctx.access).locale from_info) }; { A.label = "To"; value = A.Text (shown_name ~locale:(Access.user ctx.access).locale to_info) };
                                { A.label = "Amount"; value = A.Money { minor_units = minor; currency = ctx.currency } };
                                { A.label = "Date"; value = A.Date on }; { A.label = "Description"; value = A.Text description };
                                { A.label = "Status"; value = A.Text (if pending then "pending" else "posted") } ];
                            model =
                              [ ("from", T.Account_value from_); ("to", T.Account_value to_); ("amount", T.Number_value minor);
                                ("date", T.Text_value (Date.to_string on)); ("description", T.Text_value description);
                                ("posting", T.Text_value (if pending then "pending_until_statement_confirms" else "at_once")) ];
                            accounts = [ from_; to_ ];
                            arguments =
                              Yojson.Safe.to_string
                                (`Assoc [ ("from_account_id", `String (Account_id.to_string from_.id)); ("to_account_id", `String (Account_id.to_string to_.id));
                                          ("amount", `String (Int64.to_string minor)); ("date", `String (Date.to_string on)); ("description", `String description) ]);
                            precondition = Yojson.Safe.to_string (`Assoc [ ("pending", `Bool pending) ]) },
                        1, 1, [] )
              | Propose_rule { pattern; category; direction } ->
                  let* pattern = match one_line ~min:3 ~max:60 pattern with Some p -> Ok p | None -> error A.Invalid_tool_arguments in
                  let* a = resolve_category service aliases ctx category in
                  let* target = match Service.find_account service access a.id with Ok i -> Ok i | Error _ -> error A.Not_found in
                  if Option.is_some target.details.role || target.account.closed
                     || (match (direction, a.kind) with Some `In, Account.Expense | Some `Out, Account.Income -> true | _ -> false)
                  then error A.Invalid_tool_arguments
                  else
                    let direction_text = match direction with Some `In -> "in" | Some `Out -> "out" | None -> "any" in
                    Ok
                      ( T.Proposal_card_result
                          { tool = "propose_add_rule"; kind = "add_rule";
                            facts =
                              [ { A.label = "Pattern"; value = A.Text pattern }; { A.label = "Direction"; value = A.Text direction_text };
                                { A.label = "Category"; value = A.Text (Option.value a.name ~default:"") } ];
                            model = [ ("pattern", T.Text_value pattern); ("direction", T.Text_value direction_text); ("category", T.Account_value a) ];
                            accounts = [ a ];
                            arguments =
                              Yojson.Safe.to_string
                                (`Assoc [ ("pattern", `String pattern); ("category_id", `String (Account_id.to_string a.id));
                                          ("direction", match direction with Some `In -> `String "in" | Some `Out -> `String "out" | None -> `Null) ]);
                            precondition = "{}" },
                        1, 1, [] )
              | Propose_goal { name; target_amount; target_date; monthly_amount } ->
                  let scale = Currency.scale ctx.currency in
                  let* name = match one_line ~min:1 ~max:60 name with Some n -> Ok n | None -> error A.Invalid_tool_arguments in
                  let* target = match Json.parse_decimal ~scale target_amount with Some n when Int64.compare n 0L > 0 -> Ok n | _ -> error A.Invalid_tool_arguments in
                  let* monthly =
                    match monthly_amount with
                    | None -> Ok None
                    | Some m -> (match Json.parse_decimal ~scale m with Some n when Int64.compare n 0L >= 0 -> Ok (Some n) | _ -> error A.Invalid_tool_arguments)
                  in
                  let* date =
                    match target_date with
                    | None -> Ok None
                    | Some d ->
                        (* In the future, and within fifty years: a far date makes every goals page build thousands of cycles. *)
                        (match Date.of_string d with
                         | Ok d when Date.compare d ctx.today > 0 && Date.compare d (Date.add_days ctx.today 18_250) <= 0 -> Ok (Some d)
                         | _ -> error A.Invalid_tool_arguments)
                  in
                  (* Goal names are shown to the model, so a name that exists is refused here, and again when applied. *)
                  let exists = List.exists (fun (o : Planning_store.goal) -> String.equal (String.lowercase_ascii o.name) (String.lowercase_ascii name)) (Planning_store.goals service.Service.db (Insights.budget_owner ctx)) in
                  if exists || (match monthly with Some m -> Int64.compare m target > 0 | None -> false) then error A.Invalid_tool_arguments
                  else
                    let owner_text = match Insights.budget_owner ctx with Owner.Household _ -> "household" | Owner.User _ -> "personal" in
                    let money label n = { A.label; value = A.Money { minor_units = n; currency = ctx.currency } } in
                    let none label = { A.label; value = A.Text "none" } in
                    Ok
                      ( T.Proposal_card_result
                          { tool = "propose_save_goal"; kind = "save_goal";
                            facts =
                              [ { A.label = "Name"; value = A.Text name }; money "Target" target;
                                (match date with Some d -> { A.label = "Target date"; value = A.Date d } | None -> none "Target date");
                                (match monthly with Some m -> money "Monthly saving" m | None -> none "Monthly saving");
                                { A.label = "Owner"; value = A.Text owner_text } ];
                            model =
                              [ ("name", T.Text_value name); ("target", T.Number_value target);
                                ("target_date", (match date with Some d -> T.Text_value (Date.to_string d) | None -> T.Null_value));
                                ("monthly_saving", (match monthly with Some m -> T.Number_value m | None -> T.Null_value)) ];
                            accounts = [];
                            arguments =
                              Yojson.Safe.to_string
                                (`Assoc [ ("name", `String name); ("target_amount", `String (Int64.to_string target));
                                          ("target_date", (match date with Some d -> `String (Date.to_string d) | None -> `Null));
                                          ("monthly_amount", (match monthly with Some m -> `String (Int64.to_string m) | None -> `Null)) ]);
                            precondition = "{}" },
                        1, 1, [] )
              | Propose_update_goal { goal; name; target_amount; target_date; monthly_amount } ->
                  let scale = Currency.scale ctx.currency in
                  let* current =
                    match Hashtbl.find_opt aliases.goals (T.goal_reference_text goal) with
                    | None -> error A.Not_found
                    | Some id -> (
                        match Planning_store.find_goal service.Service.db id with
                        | Some g
                          when Access.can_use ctx.access g.goal_owner && Owner.equal g.goal_owner (Insights.budget_owner ctx) && String.equal g.currency (Currency.code ctx.currency) -> Ok g
                        | _ -> error A.Not_found)
                  in
                  let positive text = match Json.parse_decimal ~scale text with Some n when Int64.compare n 0L > 0 -> Ok n | _ -> error A.Invalid_tool_arguments in
                  let* new_name = match name with None -> Ok None | Some n -> (match one_line ~min:1 ~max:60 n with Some n -> Ok (Some n) | None -> error A.Invalid_tool_arguments) in
                  let* new_target = match target_amount with None -> Ok None | Some t -> let* n = positive t in Ok (Some n) in
                  let* new_monthly =
                    match monthly_amount with
                    | None -> Ok None
                    | Some m -> (match Json.parse_decimal ~scale m with Some n when Int64.compare n 0L >= 0 -> Ok (Some n) | _ -> error A.Invalid_tool_arguments)
                  in
                  let* new_date =
                    match target_date with
                    | None -> Ok None
                    | Some d -> (
                        match Date.of_string d with
                        | Ok d when Date.compare d ctx.today > 0 && Date.compare d (Date.add_days ctx.today 18_250) <= 0 -> Ok (Some d)
                        | _ -> error A.Invalid_tool_arguments)
                  in
                  (* Only what actually changes counts; asking for what is already there is not a proposal. *)
                  let new_name = Option.bind new_name (fun n -> if String.equal n current.name then None else Some n) in
                  let new_target = Option.bind new_target (fun n -> if Int64.equal n current.target_amount then None else Some n) in
                  let new_monthly = Option.bind new_monthly (fun n -> if Option.equal Int64.equal (Some n) current.monthly_amount then None else Some n) in
                  let new_date = Option.bind new_date (fun d -> if Option.equal Date.equal (Some d) current.target_date then None else Some d) in
                  let others = Planning_store.goals service.Service.db (Insights.budget_owner ctx) in
                  let renamed_to_existing =
                    match new_name with
                    | Some n -> List.exists (fun (o : Planning_store.goal) -> o.goal_id <> current.goal_id && String.equal (String.lowercase_ascii o.name) (String.lowercase_ascii n)) others
                    | None -> false
                  in
                  let target_after = Option.value new_target ~default:current.target_amount in
                  let monthly_after = match new_monthly with Some m -> Some m | None -> current.monthly_amount in
                  if (new_name, new_target, new_monthly, new_date) = (None, None, None, None)
                     || (match monthly_after with Some m -> Int64.compare m target_after > 0 | None -> false)
                     || renamed_to_existing
                  then error A.Invalid_tool_arguments
                  else
                    let current_name = Option.value (one_line ~min:1 ~max:60 current.name) ~default:"goal" in
                    let money label n = { A.label; value = A.Money { minor_units = n; currency = ctx.currency } } in
                    let was_money label = function Some n -> money label n | None -> { A.label; value = A.Text "none" } in
                    let facts =
                      [ { A.label = "Goal"; value = A.Text current_name } ]
                      @ (match new_name with Some n -> [ { A.label = "Name"; value = A.Text n }; { A.label = "Name was"; value = A.Text current_name } ] | None -> [])
                      @ (match new_target with Some n -> [ money "Target" n; money "Target was" current.target_amount ] | None -> [])
                      @ (match new_date with
                         | Some d ->
                             [ { A.label = "Target date"; value = A.Date d };
                               (match current.target_date with Some o -> { A.label = "Target date was"; value = A.Date o } | None -> { A.label = "Target date was"; value = A.Text "none" }) ]
                         | None -> [])
                      @ (match new_monthly with Some n -> [ money "Monthly saving" n; was_money "Monthly saving was" current.monthly_amount ] | None -> [])
                    in
                    let opt_text f = function Some v -> `String (f v) | None -> `Null in
                    let opt_json_date = function Some d -> `String (Date.to_string d) | None -> `Null in
                    Ok
                      ( T.Proposal_card_result
                          { tool = "propose_update_goal"; kind = "update_goal"; facts;
                            model =
                              [ ("goal", T.Text_value current_name);
                                ("new_name", (match new_name with Some n -> T.Text_value n | None -> T.Null_value));
                                ("new_target", (match new_target with Some n -> T.Number_value n | None -> T.Null_value));
                                ("new_target_date", (match new_date with Some d -> T.Text_value (Date.to_string d) | None -> T.Null_value));
                                ("new_monthly_saving", (match new_monthly with Some n -> T.Number_value n | None -> T.Null_value)) ];
                            accounts = [];
                            arguments =
                              Yojson.Safe.to_string
                                (`Assoc [ ("goal_id", `String current.goal_id); ("name", opt_text Fun.id new_name); ("target_amount", opt_text Int64.to_string new_target);
                                          ("target_date", opt_json_date new_date); ("monthly_amount", opt_text Int64.to_string new_monthly) ]);
                            (* What the goal is now: applying it to a goal that has changed since is out of date, and undoing puts this back. *)
                            precondition =
                              Yojson.Safe.to_string
                                (`Assoc [ ("name", `String current.name); ("target_amount", `String (Int64.to_string current.target_amount));
                                          ("target_date", opt_json_date current.target_date); ("monthly_amount", opt_text Int64.to_string current.monthly_amount) ]) },
                        1, 1, [] )
              | Propose_category { name; kind } ->
                  let* name = match one_line ~min:1 ~max:40 name with Some n -> Ok n | None -> error A.Invalid_tool_arguments in
                  let kind_account = match kind with `Expense -> Account.Expense | `Income -> Account.Income in
                  let owner = Insights.budget_owner ctx in
                  let taken =
                    List.exists
                      (fun (i : Account_info.t) ->
                        i.account.kind = kind_account && (not i.account.closed) && Owner.equal i.account.owner owner
                        && List.exists (fun n -> String.equal (String.lowercase_ascii n) (String.lowercase_ascii name)) (Starter.names i))
                      (Insights.accounts ctx)
                  in
                  if taken then error A.Invalid_tool_arguments
                  else
                    let kind_text = match kind with `Expense -> "expense" | `Income -> "income" in
                    let owner_text = match owner with Owner.Household _ -> "household" | Owner.User _ -> "personal" in
                    Ok
                      ( T.Proposal_card_result
                          { tool = "propose_new_category"; kind = "new_category";
                            facts = [ { A.label = "Name"; value = A.Text name }; { A.label = "Kind"; value = A.Text kind_text }; { A.label = "Owner"; value = A.Text owner_text } ];
                            model = [ ("name", T.Text_value name); ("kind", T.Text_value kind_text); ("owner", T.Text_value owner_text) ];
                            accounts = [];
                            arguments = Yojson.Safe.to_string (`Assoc [ ("name", `String name); ("kind", `String kind_text) ]);
                            precondition = "{}" },
                        1, 1, [] )
              | Propose_categorize { rows } ->
                  let* rows =
                    List.fold_left
                      (fun acc (row, category) ->
                        let* acc = acc in
                        let* id = match Hashtbl.find_opt aliases.rows (T.row_reference_text row) with Some id -> Ok id | None -> error A.Not_found in
                        let* item, bank =
                          match Inbox.find_open service access id with Ok x -> Ok x | Error _ -> error A.Not_found
                        in
                        if item.status <> "open" || Option.is_none (scoped item.bank_account_id) then error A.Not_found
                        else
                          let* view = match Inbox.view_item service access item with Ok v -> Ok v | Error _ -> error A.Not_found in
                          let t = view.transfer.transfer in
                          let outflow = Inbox.outflow item t in
                          let* a = resolve_category service aliases ctx category in
                          let* target = match Service.find_account service access a.id with Ok i -> Ok i | Error _ -> error A.Not_found in
                          (* Money out belongs in an expense category and money in in an income one, in the bank account's
                             own owner: what the inbox itself would accept, never a move between accounts. *)
                          if (outflow && a.kind <> Account.Expense) || ((not outflow) && a.kind <> Account.Income)
                             || not (Owner.equal target.account.owner bank.account.owner)
                             (* Never a system category (Uncategorized, Untracked cash), a closed one, or where the row already is. *)
                             || Option.is_some target.details.role || target.account.closed
                             || Account_id.equal target.account.id item.current_account_id
                          then error A.Invalid_tool_arguments
                          else
                            Ok
                              ({ T.transfer_id = t.id; date = t.value_date; amount = Amount.to_int64 t.amount; outflow;
                                 description = t.memo.description; category = a; revision = item.revision;
                                 current = item.current_account_id }
                              :: acc))
                      (Ok []) rows
                  in
                  let rows = List.rev rows in
                  Ok (T.Categorize_proposal_result rows, List.length rows, List.length rows, [])
              | Propose_budget { category; monthly_amount; rollover } ->
                  let* a = resolve_category service aliases ctx category in
                  if a.kind <> Account.Expense then error A.Invalid_tool_arguments
                  else
                    (match Json.parse_decimal ~scale:(Currency.scale ctx.currency) monthly_amount with
                    | Some proposed when Int64.compare proposed 0L >= 0 ->
                        Ok
                          ( T.Budget_proposal_result
                              { category = a; current = Option.map (fun (b : Analytics.budget) -> b.monthly_amount) (Insights.current_budget ctx a.id);
                                current_rollover = Option.map (fun (b : Analytics.budget) -> b.rollover) (Insights.current_budget ctx a.id);
                                proposed; rollover; from_cycle = Insights.current_cycle_label ctx },
                            1, 1, [] )
                    | _ -> error A.Invalid_tool_arguments)
              | Recurring _ ->
                  let* xs = run (Query.Recurring qc) in
                  let ys =
                    take 20 xs
                    |> List.map (fun (x : Analytics.recurring) ->
                        {
                          T.merchant =
                            (if Option.is_none (scoped x.paid_from) then "masked counterpart" else x.merchant_name);
                          cadence = x.cadence;
                          typical = x.typical_amount;
                          latest = x.last_amount;
                          changed = Analytics.price_changed x;
                          last_date = x.last_date;
                          next_expected = x.next_expected;
                          occurrences = x.occurrences;
                          category = Option.bind x.category (fun a -> scoped a.account.id);
                          paid_from = scoped x.paid_from;
                        })
                  in
                  Ok (T.Recurring_result ys, List.length xs, List.length ys, [ "next_expected dates are estimates" ])
              | Budget_status (_, n) ->
                  let* r = run (Query.Budget_status (qc, n)) in
                  let ys =
                    take 20 r.lines
                    |> List.filter_map (fun (x : Analytics.budget_line) ->
                        Option.map
                          (fun category ->
                            {
                              T.category;
                              budgeted = x.budgeted;
                              carried = x.carried;
                              rollover = x.rollover;
                              spent = x.spent;
                              expected = x.expected_by_now;
                              status = x.status;
                            })
                          (scoped x.line_category.account.id))
                  in
                  Ok (T.Budget_status_result (r.report_cycle, ys), List.length r.lines, List.length ys, [])
              | Goals _ ->
                  let* xs = run (Query.Goals qc) in
                  let ys =
                    take 20 xs
                    |> List.map (fun (x : Insights.goal_progress) ->
                        let reference =
                          match Hashtbl.fold (fun a id found -> if String.equal id x.goal.goal_id then Some a else found) aliases.goals None with
                          | Some a -> a
                          | None ->
                              let a = Printf.sprintf "goal_%s_%d" aliases.nonce (Hashtbl.length aliases.goals + 1) in
                              Hashtbl.add aliases.goals a x.goal.goal_id;
                              a
                        in
                        {
                          T.reference = require (T.goal_reference reference);
                          name = Option.value (one_line ~min:1 ~max:60 x.goal.name) ~default:"goal";
                          target = x.goal.target_amount;
                          saved = x.saved;
                          remaining = x.remaining;
                          target_date = x.goal.target_date;
                          monthly = x.goal.monthly_amount;
                          required = x.required_monthly;
                          on_track = x.on_track;
                        })
                  in
                  Ok (T.Goals_result ys, List.length xs, List.length ys, [])
              | Forecast (_, horizon, s) ->
                  (* Eligibility is recomputed even for a previously supplied series. *)
                  let* baseline = run (Query.Forecast (qc, horizon, Cashflow_workflow.Forecast_request.no_scenario)) in
                  let eligible id =
                    List.exists (fun (x : Forecast.item) -> x.item_id = id && Option.is_some x.series) baseline.items
                  in
                  let* leave_out =
                    List.fold_right
                      (fun r acc ->
                        let* acc = acc in
                        match Hashtbl.find_opt aliases.series (T.series_reference_text r) with
                        | Some id when eligible id -> Ok (id :: acc)
                        | _ -> error A.Not_found)
                      s.leave_out (Ok [])
                  in
                  let scenario = { s.input with leave_out } in
                  let* f =
                    if scenario = Cashflow_workflow.Forecast_request.no_scenario then Ok baseline
                    else run (Query.Forecast (qc, horizon, scenario))
                  in
                  let items =
                    take 20 f.items
                    |> List.map (fun (x : Forecast.item) ->
                        {
                          T.series_id = Option.map (fun (s : Series.t) -> s.id) x.series;
                          date = x.date;
                          kind = x.kind;
                          amount = x.amount;
                          account = scoped x.account;
                          category =
                            Option.bind x.series (fun s -> Option.bind s.category (fun a -> scoped a.account.id));
                        })
                  in
                  let projected : T.forecast =
                    {
                      today = f.today;
                      imported_through = f.imported_through;
                      horizon_end = f.horizon_end;
                      lowest = f.lowest;
                      baseline = take 20 f.baseline;
                      scenario = Option.map (take 20) f.scenario;
                      range = take 20 f.range;
                      chance_positive = f.chance_positive;
                      items;
                      item_count = List.length f.items;
                      baseline_count = List.length f.baseline;
                      range_count = List.length f.range;
                      scenario_count = Option.fold ~none:0 ~some:List.length f.scenario;
                    }
                  in
                  Ok
                    ( T.Forecast_result projected,
                      List.length f.items,
                      List.length items,
                      assumptions f
                      @
                      if scenario = Cashflow_workflow.Forecast_request.no_scenario then []
                      else [ "user-specified scenario; not observed facts" ] )
            in
            let accounts =
              Insights.accounts ctx
              |> List.filter (fun (a : Account_info.t) ->
                  Option.is_some (scoped a.account.id) && (a.account.kind = Asset || a.account.kind = Liability))
            in
            let dates =
              List.filter_map
                (fun (a : Account_info.t) -> Import_store.imported_through service.db a.account.id)
                accounts
            in
            let imported_through = match dates with [] -> None | x :: xs -> Some (List.fold_left Date.min x xs) in
            let import_status =
              if dates = [] then A.Unknown
              else if List.length dates < List.length accounts then A.Incomplete
              else match imported_through with Some d when Date.compare d ctx.today < 0 -> A.Stale | _ -> A.Current
            in
            let reconciled =
              match result with
              | T.Cycle_summary_result xs -> List.for_all (fun (x : Analytics.cycle_summary) -> x.reconciled) xs
              | _ -> false
            in
            let has_more =
              total_count > returned_count
              ||
              match result with
              | T.Forecast_result f ->
                  f.baseline_count > List.length f.baseline
                  || f.range_count > List.length f.range
                  || f.scenario_count > Option.fold ~none:0 ~some:List.length f.scenario
              | _ -> false
            in
            if List.length extra_assumptions > 20 then limit A.Tool_result_bytes
            else
              let id =
                require (A.Evidence_id.of_string (Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ())))
              in
              Ok
                {
                  T.id;
                  scope;
                  sampled_at = service.now ();
                  currency = ctx.currency;
                  result;
                  total_count;
                  returned_count;
                  has_more;
                  import_status;
                  imported_through;
                  reconciled;
                  assumptions = extra_assumptions;
                  source_references = [ T.name result ];
                })
    in
    (* Alias metadata is distinct from financial state. No serialization under either boundary. *)
    let* () =
      Sql.transaction_result service.db (fun () ->
          let* () = authorize service ~principal ~scope aliases in
          let* access = Advisor_access.access service principal in
          let query_scope = match scope with A.Personal -> Analytics.Me | Household _ -> Analytics.Household in
          let* ctx =
            service_result
              (Insights.context service access ~scope:query_scope ~currency:(Currency.code detached.currency) ())
          in
          let scoped = scoped_book ctx in
          List.fold_left
            (fun acc (a : T.account) ->
              let* () = acc in
              match scoped a.id with
              | Some current when current.kind = a.kind ->
                  let* alias = Advisor_store.account_alias (store service) ~conversation:aliases.conversation a.id in
                  Hashtbl.replace aliases.accounts a.id alias;
                  Ok ()
              | _ -> error A.Not_found)
            (Ok ()) (refs detached.result))
    in
    (match detached.result with
    | T.Forecast_result f ->
        List.iter
          (fun (x : T.forecast_item) ->
            Option.iter
              (fun id ->
                if not (Hashtbl.fold (fun _ v found -> found || v = id) aliases.series false) then
                  let alias = Printf.sprintf "series_%s_%d" aliases.nonce (Hashtbl.length aliases.series + 1) in
                  if Hashtbl.length aliases.series < 160 then Hashtbl.add aliases.series alias id)
              x.series_id)
          f.items
    | _ -> ());
    Hashtbl.add aliases.evidence detached.id detached;
    Ok detached
  with Sql.Sql_error _ -> error A.Storage_unavailable

(* At most [n] bytes, never cutting a UTF-8 character. A card shows a merchant's text as the row's label, not in full. *)
let shorten s n =
  if String.length s <= n then s
  else
    let cut = ref n in
    while !cut > 0 && Char.code s.[!cut] land 0xC0 = 0x80 do decr cut done;
    String.sub s 0 !cut ^ "…"

let display (e : T.evidence) : A.evidence =
  let money label amount = { A.label; value = A.Money { minor_units = amount; currency = e.currency } } in
  let financial_facts =
    match e.result with
    | T.Cycle_summary_result (x :: _) ->
        [
          money "Income (first returned cycle)" x.flows.income; money "Spending (first returned cycle)" x.flows.spending;
        ]
    | Category_spending_result (x :: _) -> [ money "Largest returned category" x.total ]
    | Category_trend_result (_, (_, n) :: _) -> [ money "First returned cycle" n ]
    | Relevant_transactions_result (x :: _) -> [ money "Largest returned expense" x.amount ]
    | Recurring_result (x :: _) -> [ money "Largest returned typical payment" x.typical ]
    | Budget_status_result (_, x :: _) -> [ money "First returned budget: spent" x.spent ]
    | Goals_result (x :: _) -> [ money "First returned goal: remaining" x.remaining ]
    | Forecast_result f ->
        [
          money "Forecast lowest cash" (snd f.lowest);
          { A.label = "Forecast lowest date"; value = A.Date (fst f.lowest) };
        ]
    | Period_outlook_result o ->
        [ money "Expected savings this cycle" o.expected; money "Savings so far" (Analytics.savings o.so_far) ]
        @ (match o.covered_through with Some d -> [ { A.label = "Covered through"; value = A.Date d } ] | None -> [])
    | Filed_inbox_result { filed; unsure; kept_on } ->
        let line (r : T.inbox_row) =
          Date.to_string r.date ^ " · " ^ shorten r.description 60
          ^ match r.suggestion with Some (a, confidence, _) -> Printf.sprintf " → %s (%d%%)" (Option.value a.name ~default:"") confidence | None -> ""
        in
        List.map (fun r -> { A.label = "Filed"; value = A.Text (line r) }) filed
        @ List.map (fun r -> { A.label = "Needs review"; value = A.Text (line r) }) unsure
        @ [ { A.label = "Future imports filed automatically"; value = A.Text (string_of_bool kept_on) } ]
    | _ -> []
  in
  let proposal_facts =
    match e.result with
    | T.Budget_proposal_result p ->
        Some
          [
            { A.label = "Category"; value = A.Text (Option.value p.category.name ~default:"") };
            (match p.current with Some n -> money "Current budget" n | None -> { A.label = "Current budget"; value = A.Text "none" });
            money "Proposed budget" p.proposed;
            { A.label = "From cycle"; value = A.Text p.from_cycle };
            { A.label = "Carry over"; value = A.Text (string_of_bool p.rollover) };
          ]
    | T.Proposal_card_result card -> Some card.facts
    | T.Categorize_proposal_result rows ->
        Some
          (List.concat_map
             (fun (r : T.categorize_row) ->
               [
                 { A.label = "Transaction"; value = A.Text (Date.to_string r.date ^ " · " ^ shorten r.description 80) };
                 money "Amount" (if r.outflow then Int64.neg r.amount else r.amount);
                 { A.label = "New category"; value = A.Text (Option.value r.category.name ~default:"") };
               ])
             rows)
    | _ -> None
  in
  {
    id = e.id;
    scope = e.scope;
    currency = e.currency;
    sampled_at = e.sampled_at;
    title = T.name e.result;
    facts =
      (match proposal_facts with
      | Some facts -> facts
      | None ->
          [
            { A.label = "Returned rows"; value = A.Text (string_of_int e.returned_count) };
            { A.label = "Total rows"; value = A.Text (string_of_int e.total_count) };
            { A.label = "Truncated"; value = A.Text (string_of_bool e.has_more) };
          ]
          @ financial_facts);
    source_references = e.source_references;
    import_status = e.import_status;
    reconciled = e.reconciled;
    assumptions = e.assumptions;
    proposal = (match e.result with T.Budget_proposal_result _ | T.Categorize_proposal_result _ | T.Proposal_card_result _ -> Some A.Awaiting | _ -> None);
  }

let model_json ~aliases (e : T.evidence) =
  let* () = authorize aliases.service ~principal:aliases.principal ~scope:aliases.scope aliases in
  match Hashtbl.find_opt aliases.evidence e.id with
  | Some original when original == e -> (
      try
        let obj x = `Assoc x and arr f xs = `List (List.map f xs) in
        let str s = `String s and num n = `String (Int64.to_string n) in
        let date d = `String (Date.to_string d) in
        let opt f = function None -> `Null | Some x -> f x in
        let acct (a : T.account) =
          let alias = require (Hashtbl.find_opt aliases.accounts a.id) in
          obj
            ([ ("reference", str alias); ("role", str (Account.kind_to_string a.kind)) ]
            @ match a.name with Some name -> [ ("name", str name) ] | None -> [])
        in
        let series id =
          Hashtbl.fold (fun alias value acc -> if value = id then Some alias else acc) aliases.series None |> require
        in
        let point (p : Forecast.point) =
          obj [ ("date", date p.date); ("cash", num p.cash); ("earmarked", num p.earmarked) ]
        in
        let payload =
          match e.result with
          | T.Cycle_summary_result xs ->
              arr
                (fun (x : Analytics.cycle_summary) ->
                  obj
                    [
                      ("cycle", str (Cycle.label x.cycle));
                      ("first_day", date x.cycle.first);
                      ("last_day", date x.cycle.last);
                      ("income", num x.flows.income);
                      ("spending", num x.flows.spending);
                      ("transfers_in", num x.flows.transfers_in);
                      ("transfers_out", num x.flows.transfers_out);
                      ("savings", num x.savings);
                      ("savings_rate_bp", opt (fun n -> `Int n) x.savings_rate_bp);
                      ("reconciled", `Bool x.reconciled);
                      ("complete", `Bool x.complete);
                      ("covered_through", opt date x.covered_through);
                    ])
                xs
          | Category_spending_result xs ->
              arr
                (fun (x : T.category) ->
                  obj [ ("category", acct x.account); ("total", num x.total); ("previous", opt num x.previous) ])
                xs
          | Category_trend_result (a, xs) ->
              obj
                [
                  ("category", acct a);
                  ("cycles", arr
                      (fun ((c : Cycle.t), n) ->
                        obj
                          [
                            ("cycle", str (Cycle.label c));
                            ("first_day", date c.first);
                            ("last_day", date c.last);
                            ("total", num n);
                          ])
                      xs);
                ]
          | Relevant_transactions_result xs ->
              arr
                (fun (x : T.transaction) ->
                  obj
                    [
                      ("date", date x.date);
                      ("amount", num x.amount);
                      ("description", str x.description);
                      ("debit", opt acct x.debit);
                      ("credit", opt acct x.credit);
                    ])
                xs
          | Recurring_result xs ->
              arr
                (fun (x : T.payment) ->
                  obj
                    [
                      ("merchant", str x.merchant);
                      ("cadence", str (Analytics.cadence_to_string x.cadence));
                      ("typical", num x.typical);
                      ("latest", num x.latest);
                      ("price_changed", `Bool x.changed);
                      ("last_date", date x.last_date);
                      ("next_expected", date x.next_expected);
                      ("occurrences", `Int x.occurrences);
                      ("category", opt acct x.category);
                      ("paid_from", opt acct x.paid_from);
                    ])
                xs
          | Budget_status_result (c, xs) ->
              obj
                [
                  ("cycle", str (Cycle.label c));
                  ("first_day", date c.first);
                  ("last_day", date c.last);
                  ( "lines",
                    arr
                      (fun (x : T.budget) ->
                        obj
                          [
                            ("category", acct x.category);
                            ("budgeted", num x.budgeted);
                            ("carried", num x.carried);
                            ("rollover", `Bool x.rollover);
                            ("spent", num x.spent);
                            ("expected", num x.expected);
                            ("status", str (Query.status_string x.status));
                          ])
                      xs );
                ]
          | Goals_result xs ->
              arr
                (fun (x : T.goal) ->
                  obj
                    [
                      ("reference", str (T.goal_reference_text x.reference));
                      ("name", str x.name);
                      ("target", num x.target);
                      ("saved", num x.saved);
                      ("remaining", num x.remaining);
                      ("target_date", opt date x.target_date);
                      ("monthly", opt num x.monthly);
                      ("required", opt num x.required);
                      ("on_track", `Bool x.on_track);
                    ])
                xs
          | Inbox_rows_result rows ->
              arr
                (fun (r : T.inbox_row) ->
                  obj
                    [
                      ("reference", str (T.row_reference_text r.reference));
                      ("date", date r.date);
                      ("amount", num r.amount);
                      ("direction", str (if r.outflow then "out" else "in"));
                      (* Bounded: a merchant's text is data, and twenty long ones must not exceed the result limit. *)
                      ("description", str (shorten r.description 200));
                      ( "suggestion",
                        opt
                          (fun ((a : T.account), confidence, source) ->
                            obj [ ("category", acct a); ("confidence", `Int confidence); ("source", str source) ])
                          r.suggestion );
                    ])
                rows
          | Explain_result { explanation = e; categories } ->
              let category (info : Account_info.t) =
                match List.find_opt (fun (a : T.account) -> Account_id.equal a.id info.account.id) categories with Some a -> acct a | None -> `Null
              in
              let driver : Explain.driver -> Yojson.Safe.t = function
                | Yearly { category = c; description; amount; date = d } ->
                    obj [ ("kind", str "yearly"); ("category", category c); ("description", str (shorten description 80)); ("amount", num amount); ("date", date d) ]
                | One_off { category = c; description; amount; date = d } ->
                    obj [ ("kind", str "one_off"); ("category", category c); ("description", str (shorten description 80)); ("amount", num amount); ("date", date d) ]
                | Habit { category = c; difference; cycles_up } -> obj [ ("kind", str "habit"); ("category", category c); ("amount", num difference); ("cycles_up", `Int cycles_up) ]
                | Less { category = c; difference } -> obj [ ("kind", str "less"); ("category", category c); ("amount", num difference) ]
                | Income_change { difference } -> obj [ ("kind", str "income"); ("amount", num difference) ]
              in
              obj
                [
                  ("cycle", str (Cycle.label e.cycle)); ("first_day", date e.cycle.first); ("last_day", date e.cycle.last);
                  ("normal", `Bool e.normal); ("spending", num e.spending); ("usual_spending", num e.usual_spending);
                  ("income", num e.income); ("usual_income", num e.usual_income); ("compared_with", `Int e.compared_with);
                  ("drivers", arr driver e.drivers);
                ]
          | Period_outlook_result o ->
              let flows (f : Analytics.flows) = obj [ ("income", num f.income); ("spending", num f.spending); ("savings", num (Analytics.savings f)) ] in
              obj
                [
                  ("cycle", str (Cycle.label o.cycle));
                  ("first_day", date o.cycle.first);
                  ("last_day", date o.cycle.last);
                  ("complete", `Bool (o.days_covered >= o.days_total));
                  ("covered_through", opt date o.covered_through);
                  ("days_covered", `Int o.days_covered);
                  ("days_total", `Int o.days_total);
                  ("so_far", flows o.so_far);
                  ( "usual_by_now",
                    opt
                      (fun (u : Outlook.usual) ->
                        obj [ ("income", num u.income); ("spending", num u.spending); ("savings", num u.savings); ("cycles", `Int u.cycles) ])
                      o.usual_by_now );
                  ( "still_due",
                    arr
                      (fun (i : Forecast.item) ->
                        obj [ ("date", date i.date); ("kind", str (Forecast.kind_to_string i.kind)); ("name", str (shorten i.name 80)); ("amount", num i.amount) ])
                      (take 20 o.still_due) );
                  ("usual_rest", num o.usual_rest);
                  ("expected", num o.expected);
                  ("safe_to_spend", num o.safe_to_spend);
                  ("days_to_payday", `Int o.days_to_payday);
                  ("goals_set_aside", num o.goals_set_aside);
                  ("unseen_spending", num o.unseen_spending);
                  ("expected_range", opt (fun (low, high) -> obj [ ("low", num low); ("high", num high) ]) o.expected_range);
                  ( "track_record",
                    opt
                      (fun (t : Cashflow_core.Outlook.track) -> obj [ ("miss", num t.miss); ("forecasts", `Int t.forecasts); ("since", date t.since) ])
                      o.track_record );
                ]
          | Filed_inbox_result { filed; unsure; kept_on } ->
              let row (r : T.inbox_row) =
                obj
                  [
                    ("reference", str (T.row_reference_text r.reference));
                    ("date", date r.date);
                    ("amount", num r.amount);
                    ("direction", str (if r.outflow then "out" else "in"));
                    ("description", str (shorten r.description 200));
                    ( "category",
                      opt (fun ((a : T.account), confidence, source) -> obj [ ("category", acct a); ("confidence", `Int confidence); ("source", str source) ]) r.suggestion );
                  ]
              in
              obj
                [
                  ("filed", arr row filed);
                  ("needs_review", arr row unsure);
                  ("future_imports_filed_automatically", `Bool kept_on);
                  ("threshold_percent", `Int Autofile.threshold);
                ]
          | Categories_result xs -> arr acct xs
          | Bank_accounts_result xs ->
              arr
                (fun (b : T.bank_account) ->
                  obj
                    [ ("reference", str (match acct b.account with `Assoc fs -> (match List.assoc_opt "reference" fs with Some (`String r) -> r | _ -> "") | _ -> ""));
                      ("role", str (Account.kind_to_string b.account.kind)); ("hint", str b.hint);
                      ("institution", opt str b.institution); ("statement_backed", `Bool b.statement_backed) ])
                xs
          | Proposal_card_result card ->
              obj
                (("status", str "awaiting_user_confirmation")
                :: List.map
                     (fun (key, value) ->
                       ( key,
                         match value with
                         | T.Text_value s -> str s
                         | Number_value n -> num n
                         | Bool_value b -> `Bool b
                         | Account_value a -> acct a
                         | Null_value -> `Null ))
                     card.model)
          | Categorize_proposal_result rows ->
              let reference id =
                Hashtbl.fold (fun alias t found -> if Transfer_id.equal t id then Some alias else found) aliases.rows None
              in
              obj
                [
                  ("status", str "awaiting_user_confirmation");
                  ( "rows",
                    arr
                      (fun (r : T.categorize_row) ->
                        obj [ ("row", opt str (reference r.transfer_id)); ("category", acct r.category) ])
                      rows );
                ]
          | Budget_proposal_result p ->
              (* Never "done": the user's own click is what applies it, and the card shows the outcome. *)
              obj
                [
                  ("status", str "awaiting_user_confirmation");
                  ("category", acct p.category);
                  ("current_monthly", opt num p.current);
                  ("proposed_monthly", num p.proposed);
                  ("carry_over", `Bool p.rollover);
                  ("from_cycle", str p.from_cycle);
                ]
          | Forecast_result f ->
              obj
                [
                  ("today", date f.today);
                  ("imported_through", opt date f.imported_through);
                  ("horizon_end", date f.horizon_end);
                  ("lowest", obj [ ("date", date (fst f.lowest)); ("cash", num (snd f.lowest)) ]);
                  ("baseline", arr point f.baseline);
                  ("baseline_count", `Int f.baseline_count);
                  ("scenario", opt (arr point) f.scenario);
                  ("scenario_count", `Int f.scenario_count);
                  ("range", arr (fun (d, lo, hi) -> obj [ ("date", date d); ("low", num lo); ("high", num hi) ]) f.range);
                  ("range_count", `Int f.range_count);
                  ("chance_positive", opt (fun n -> `Int n) f.chance_positive);
                  ( "items",
                    arr
                      (fun (x : T.forecast_item) ->
                        obj
                          [
                            ("series", opt (fun id -> str (series id)) x.series_id);
                            ("date", date x.date);
                            ("kind", str (Forecast.kind_to_string x.kind));
                            ("amount", num x.amount);
                            ("account", opt acct x.account);
                            ("category", opt acct x.category);
                          ])
                      f.items );
                  ("item_count", `Int f.item_count);
                ]
        in
        let json =
          obj
            [
              ("evidence_id", str (A.Evidence_id.to_string e.id));
              ("tool", str (T.name e.result));
              ("sampled_at", str (Time.to_utc_string e.sampled_at));
              ("currency", obj [ ("code", str (Currency.code e.currency)); ("scale", `Int (Currency.scale e.currency)) ]);
              ("amount_encoding", str "signed minor units as decimal integer strings");
              ("total_count", `Int e.total_count);
              ("returned_count", `Int e.returned_count);
              ("has_more", `Bool e.has_more);
              ( "import_status",
                str
                  (match e.import_status with
                  | A.Current -> "current"
                  | Stale -> "stale"
                  | Incomplete -> "incomplete"
                  | Unknown -> "unknown") );
              ("imported_through", opt date e.imported_through);
              ("reconciled", `Bool e.reconciled);
              ("assumptions", arr str e.assumptions);
              ("source_references", arr str e.source_references);
              ("data", payload);
            ]
        in
        let bytes = String.length (Yojson.Safe.to_string json) in
        if bytes > A.limits.tool_result_bytes then limit A.Tool_result_bytes
        else
          let previous = Option.value (Hashtbl.find_opt aliases.selected e.id) ~default:0 in
          let total = Hashtbl.fold (fun _ n sum -> sum + n) aliases.selected 0 - previous + bytes in
          if total > A.limits.evidence_bytes then limit A.Evidence_bytes
          else (
            Hashtbl.replace aliases.selected e.id bytes;
            Ok json)
      with Invalid -> error A.Not_found)
  | _ -> error A.Not_found

let resolve_finding (service : Service.t) ~principal ~scope ~key =
  if String.length key > 512 then error A.Invalid_tool_arguments
  else
    try
      Sql.read_snapshot service.db (fun () ->
          let* () = Advisor_access.authorize_scope service principal scope in
          let* access = Advisor_access.access service principal in
          let owner = match scope with A.Personal -> Access.personal access | Household h -> Owner.Household h in
          let owns (a : Account_info.t) = Owner.equal a.account.owner owner in
          let scope_is s =
            match (scope, s) with A.Personal, Analytics.Me | Household _, Analytics.Household -> true | _ -> false
          in
          let cards = Attention.feed ~narrative:false service access in
          match List.find_opt (fun (Attention.Card c) -> c.key = key) cards with
          | None -> error A.Not_found
          | Some (Attention.Card c) -> (
              let money currency n =
                (Advisor_codec.money_out { A.minor_units = n; currency }).decimal ^ " " ^ Currency.code currency
              in
              let found =
                match (c.kind, c.payload) with
                | Attention.Overview, p when scope = A.Personal ->
                    Option.map
                      (fun (s : Analytics.cycle_summary) ->
                        ("Cycle overview", "Spending: " ^ money p.currency s.flows.spending))
                      p.summary
                | Budget_alert, (p, currency) when owns p.line_category ->
                    Some ("Budget status", "Spent: " ^ money currency p.spent ^ "; budget: " ^ money currency p.budgeted)
                | Unusual_spend, p when scope_is p.scope && owns p.unusual.unusual_category ->
                    Some
                      ( "Unusual spending",
                        "Spent: "
                        ^ money p.unusual_currency p.unusual.spent_so_far
                        ^ "; usual: "
                        ^ money p.unusual_currency p.unusual.usual )
                | Shortfall, (p, currency) when owns p.short_account ->
                    Some
                      ("Cash shortfall", "Available: " ^ money currency p.available ^ "; due: " ^ money currency p.due)
                | Goal_off_track, (p, currency) when Owner.equal p.goal.goal_owner owner ->
                    Some
                      ( "Goal progress",
                        "Saved: " ^ money currency p.saved ^ "; remaining: " ^ money currency p.remaining )
                | _ -> None
              in
              match found with
              | None -> error A.Not_found
              | Some (title, summary) ->
                  Ok { A.kind = Attention.kind_name c.kind; key; scope; title; summary; sampled_at = service.now () }))
    with Sql.Sql_error _ -> error A.Storage_unavailable
