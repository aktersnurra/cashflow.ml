(* The attention engine behind the landing page (§11.3): a ranked feed of cards worked out from
   facts about the user's data. Ranking is deterministic, so it is fast, testable and predictable;
   an LLM only ever writes text for a card, never decides what is shown.

   Card kinds are a GADT indexed by their payload, so a card can only be built with the right data
   and [payload_json] must handle every kind. What a user did with a card (snoozed it, resolved it)
   is a small state machine whose transitions are typed the same way. *)

module Access = Cashflow_core.Access
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Cycle = Cashflow_core.Cycle
module Date = Cashflow_core.Date
module Household = Cashflow_core.Household
module Household_id = Cashflow_core.Household_id
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id
module User_id = Cashflow_core.User_id

open Sql

type import_due = { account : Account_info.t; last_import : Ptime.t option; days : int option }
type mismatch = { account : Account_info.t; checkpoint : Planning_store.checkpoint }
type count_cash = { cash : Account_info.t; since : Date.t }
type overview = { cycle : Cycle.t; summary : Analytics.cycle_summary option; currency : Currency.t }
type month_review = { reviewed : Cycle.t; review_summary : Analytics.cycle_summary option; review_currency : Currency.t; text : string option }

type unusual = { unusual : Analytics.unusual; unusual_currency : Currency.t; scope : Analytics.scope; explanation : string option }

(* Rows waiting in the inbox, and how many of them have a suggested category. *)
type inbox = { waiting : int; suggested : int }

type _ kind =
  | Invitation : Household.t kind
  | Reconciliation_mismatch : mismatch kind
  | Import_due : import_due kind
  | Inbox : inbox kind
  | Month_review : month_review kind
  | Budget_alert : (Analytics.budget_line * Currency.t) kind
  | Shortfall : (Analytics.shortfall * Currency.t) kind
  | Unusual_spend : unusual kind
  | Contributions_due : (Insights.member_plan * Currency.t) kind
  | Settle_up : (Insights.owed list * Currency.t) kind
  | Unmatched_entry : Service.transfer_view kind
  | Count_cash : count_cash kind
  | Goal_off_track : (Insights.goal_progress * Currency.t) kind
  | Recurring_change : (Analytics.recurring * Currency.t) kind
  | Overview : overview kind

let kind_name : type a. a kind -> string = function
  | Invitation -> "invitation"
  | Reconciliation_mismatch -> "reconciliation_mismatch"
  | Import_due -> "import_due"
  | Inbox -> "inbox"
  | Month_review -> "month_review"
  | Budget_alert -> "budget_alert"
  | Shortfall -> "shortfall"
  | Unusual_spend -> "unusual_spend"
  | Contributions_due -> "contributions_due"
  | Settle_up -> "settle_up"
  | Unmatched_entry -> "unmatched_entry"
  | Count_cash -> "count_cash"
  | Goal_off_track -> "goal_off_track"
  | Recurring_change -> "recurring_change"
  | Overview -> "overview"

(* Lower comes first. *)
let priority : type a. a kind -> int = function
  | Invitation -> 0
  | Reconciliation_mismatch -> 10
  | Import_due -> 20
  | Inbox -> 30
  | Unmatched_entry -> 35
  | Shortfall -> 32
  | Month_review -> 40
  | Budget_alert -> 50
  | Unusual_spend -> 55
  | Contributions_due -> 60
  | Settle_up -> 70
  | Count_cash -> 75
  | Goal_off_track -> 80
  | Recurring_change -> 90
  | Overview -> 100

(* Lifecycle: a card is shown until it is snoozed (it comes back after the date) or resolved
   (with a note saying what was acknowledged). *)
(* Distinct nominal types, so the checker knows a [shown state] can't be a [snoozed state]. *)
type shown = Shown_tag
type snoozed = Snoozed_tag
type resolved = Resolved_tag

type _ state = Shown : shown state | Snoozed : Date.t -> snoozed state | Resolved : string -> resolved state
type (_, _) transition = Snooze : Date.t -> (shown, snoozed) transition | Resolve : string -> (shown, resolved) transition | Wake : (snoozed, shown) transition
type any_state = State : 'a state -> any_state

let step : type a b. a state -> (a, b) transition -> b state =
 fun state transition ->
  match state, transition with
  | Shown, Snooze until -> Snoozed until
  | Shown, Resolve note -> Resolved note
  | Snoozed _, Wake -> Shown
  | Resolved _, _ -> .

type card = Card : { kind : 'a kind; key : string; payload : 'a; resolvable : bool } -> card

(* Persistence of the lifecycle *)

let state_row =
  row Codec.(text @-> nullable date @-> nullable text @-> nil) (fun state until note ->
      match state, until, note with
      | "snoozed", Some until, _ -> State (Snoozed until)
      | "resolved", _, note -> State (Resolved (Option.value note ~default:""))
      | _ -> State Shown)

let find_state = stmt "SELECT state, until_date, note FROM attention_cards WHERE user_id = ? AND card_key = ?" Codec.(user_id @-> text @-> nil) (opt state_row)

let save_state =
  stmt
    "INSERT INTO attention_cards (user_id, card_key, state, until_date, note, updated_at) VALUES (?, ?, ?, ?, ?, ?) \
     ON CONFLICT (user_id, card_key) DO UPDATE SET state = excluded.state, until_date = excluded.until_date, note = excluded.note, updated_at = excluded.updated_at"
    Codec.(user_id @-> text @-> text @-> nullable date @-> nullable text @-> time @-> nil)
    exec

let clear_state = stmt "DELETE FROM attention_cards WHERE user_id = ? AND card_key = ?" Codec.(user_id @-> text @-> nil) exec

(* A snoozed card whose date has passed wakes up. *)
let current_state db user_id key ~today =
  match run db find_state user_id key with
  | None -> State Shown
  | Some (State (Snoozed until)) when Date.compare until today <= 0 -> State (step (Snoozed until) Wake)
  | Some state -> state

let persist : type a. Sql.db -> User_id.t -> string -> now:Ptime.t -> a state -> unit =
 fun db user_id key ~now state ->
  match state with
  | Shown -> run db clear_state user_id key
  | Snoozed until -> run db save_state user_id key "snoozed" (Some until) None now
  | Resolved note -> run db save_state user_id key "resolved" None (Some note) now

(* Facts *)

let statement_accounts service access =
  List.filter (fun (i : Account_info.t) -> i.details.statement_backed && not i.account.closed && (i.account.kind = Asset || i.account.kind = Liability)) (Service.accounts service access)

let import_due (service : Service.t) access ~today =
  List.filter_map
    (fun (account : Account_info.t) ->
      let last = Import_store.last_import_at service.db account.account.id in
      let days = Option.map (fun t -> Date.days_between ~first:(Date.of_instant ~utc_offset_s:0 t) ~last:today) last in
      match days with Some d when d < 7 -> None | _ -> Some { account; last_import = last; days })
    (statement_accounts service access)

let mismatches (service : Service.t) access =
  List.filter_map
    (fun (account : Account_info.t) ->
      match List.rev (Planning_store.checkpoints service.db account.account.id) with
      | latest :: _ ->
          (* Against the ledger as it is now. *)
          let ledger_balance = Import_store.posted_balance_on service.db account.account latest.on_date in
          if Int64.equal latest.statement_balance ledger_balance then None else Some { account; checkpoint = { latest with ledger_balance } }
      | _ -> None)
    (statement_accounts service access)

(* Pending manual entries the bank should have confirmed by now: an import has covered their date
   plus a week and they are still pending (§6.7). *)
let unmatched (service : Service.t) access =
  let covered (account : Account_info.t) =
    List.fold_left
      (fun latest (b : Import_store.batch) -> match b.last_date, latest with Some d, Some l -> Some (Date.max d l) | Some d, None -> Some d | None, l -> l)
      None
      (List.filter (fun (b : Import_store.batch) -> not b.undone) (Import_store.batches service.db ~account_id:account.account.id ~limit:20))
  in
  List.concat_map
    (fun (account : Account_info.t) ->
      match covered account with
      | None -> []
      | Some through ->
          Store_sqlite.open_pending service.db
          |> List.filter (fun (t : Transfer.t) ->
                 (Account_id.equal t.debit_account_id account.account.id || Account_id.equal t.credit_account_id account.account.id)
                 && Date.compare (Date.add_days t.value_date 7) through <= 0)
          |> List.filter_map (fun (t : Transfer.t) -> Result.to_option (Service.get_transfer service access t.id)))
    (statement_accounts service access)

let count_cash (service : Service.t) access ~today =
  List.filter_map
    (fun (cash : Account_info.t) ->
      let recent = Transfer_query.run_filter service.db access { Transfer_query.default_filter with account_id = Some cash.account.id; limit = 50 } in
      let withdrawals = List.filter (fun (t : Transfer.t) -> Account_id.equal t.debit_account_id cash.account.id) recent in
      let spent = List.filter (fun (t : Transfer.t) -> Account_id.equal t.credit_account_id cash.account.id) recent in
      let latest ts = List.fold_left (fun acc (t : Transfer.t) -> match acc with None -> Some t.value_date | Some d -> Some (Date.max d t.value_date)) None ts in
      match latest withdrawals, latest spent with
      | Some withdrawn, last_spent ->
          let since = match last_spent with Some d -> Date.max d withdrawn | None -> withdrawn in
          if Date.days_between ~first:since ~last:today >= 30 then Some { cash; since } else None
      | None, _ -> None)
    (List.filter (fun (i : Account_info.t) -> i.details.role = Some Cash && not i.account.closed) (Service.accounts service access))

let status_rank = function `On_pace -> 0 | `Ahead_of_pace -> 1 | `Over_budget -> 2
let status_note s = match s with `On_pace -> "on_pace" | `Ahead_of_pace -> "ahead_of_pace" | `Over_budget -> "over_budget"
let note_rank = function "over_budget" -> 2 | "ahead_of_pace" -> 1 | _ -> 0

(* The feed *)

let feed ?(narrative = true) (service : Service.t) access =
  let today = Service.today service access in
  let user = Access.user access in
  let db = service.db in
  let cards = ref [] in
  (* Every calculation below reads the same ledger; load the fourteen months they need together once. *)
  let shared = Insights.new_shared () in
  Insights.preload_range shared service access ~first:(Date.add_days today (-420)) ~last:(Date.add_days today 70);
  (* Recurring payments scan a year of transfers and the shortfall and price-change cards both need them: work them
     out once per scope and currency rather than once per card kind. *)
  let recurring_memo = Hashtbl.create 4 in
  let recurring (ctx : Insights.ctx) =
    let key = (ctx.scope, Currency.code ctx.currency) in
    match Hashtbl.find_opt recurring_memo key with
    | Some payments -> payments
    | None ->
        let payments = Insights.recurring ctx in
        Hashtbl.replace recurring_memo key payments;
        payments
  in
  let add ?(resolvable = true) kind key payload = cards := Card { kind; key; payload; resolvable } :: !cards in
  List.iter (fun (h : Household.t) -> add ~resolvable:false Invitation ("invitation:" ^ Household_id.to_string h.id) h) (Service.invitations service access);
  List.iter (fun (m : mismatch) -> add Reconciliation_mismatch ("mismatch:" ^ m.checkpoint.checkpoint_id) m) (mismatches service access);
  List.iter (fun (d : import_due) -> add Import_due ("import:" ^ Account_id.to_string d.account.account.id ^ ":" ^ Date.to_string today) d) (import_due service access ~today);
  (match Inbox.count service access with
  | 0 -> ()
  | waiting -> add ~resolvable:false Inbox "inbox" { waiting; suggested = Inbox.suggested_count service access });
  List.iter (fun (v : Service.transfer_view) -> add Unmatched_entry ("unmatched:" ^ Transfer_id.to_string v.transfer.id) v) (unmatched service access);
  (match Insights.context service access ~scope:Me ~shared () with
  | Error _ -> ()
  | Ok ctx ->
      let current = Insights.current_cycle ctx in
      let previous = Cycle.previous ctx.setting current in
      if Date.days_between ~first:current.first ~last:today < 10 then (
        let summary = match Analytics.cashflow_summary (Insights.book ctx) ~cycles:[ previous ] ~reconciled_through:(Insights.reconciled_through ctx) (Insights.transfers ctx ~first:previous.first ~last:previous.last) with [ s ] -> Some s | _ -> None in
        let text =
          if not narrative then None else Option.bind summary (fun summary ->
              let before = Cycle.previous ctx.setting previous in
              let previous_summary =
                match Analytics.cashflow_summary (Insights.book ctx) ~cycles:[ before ] ~reconciled_through:None (Insights.transfers ctx ~first:before.first ~last:before.last) with
                | [ s ] -> Some s
                | _ -> None
              in
              let top = List.filteri (fun i _ -> i < 5) (Insights.spending_by_category ctx ~period:(Cycle_offset (-1)) ~compare_previous:true) in
              Llm_features.month_summary service access ~cycle:previous ~currency:ctx.currency ~summary ~previous:previous_summary ~top)
        in
        add Month_review ("month_review:" ^ Cycle.label previous) { reviewed = previous; review_summary = summary; review_currency = ctx.currency; text });
      let summary = match Insights.cashflow_summary ctx ~cycles:1 with [ s ] -> Some s | _ -> None in
      add ~resolvable:false Overview "overview" { cycle = current; summary; currency = ctx.currency };
      List.iter
        (fun scope ->
          match Insights.context service access ~scope ~shared () with
          | Error _ -> ()
          | Ok ctx ->
              let report = Insights.budget_status ctx ~cycle_offset:0 in
              List.iter
                (fun (line : Analytics.budget_line) ->
                  if line.status <> `On_pace then
                    let key = Printf.sprintf "budget:%s:%s" (Account_id.to_string line.line_category.account.id) (Cycle.label report.report_cycle) in
                    (* An acknowledged alert comes back if the category gets worse. *)
                    match current_state db user.id key ~today with
                    | State (Resolved note) when note_rank note >= status_rank line.status -> ()
                    | _ -> add Budget_alert key (line, ctx.currency))
                report.lines;
              (* Budgeted categories already have alerts of their own. *)
              let budgeted (u : Analytics.unusual) =
                List.exists (fun (l : Analytics.budget_line) -> Account_id.equal l.line_category.account.id u.unusual_category.account.id) report.lines
              in
              List.iter
                (fun (u : Analytics.unusual) ->
                  if not (budgeted u) then
                    let key = Printf.sprintf "unusual:%s:%s" (Account_id.to_string u.unusual_category.account.id) (Cycle.label report.report_cycle) in
                    let explanation =
                      match current_state db user.id key ~today with
                      | State Shown when narrative ->
                          Llm_features.unusual_spending service access ~cycle:report.report_cycle ~currency:ctx.currency ~unusual:u
                            ~transactions:(Insights.category_spending ctx u.unusual_category.account.id)
                      | State (Shown | Snoozed _ | Resolved _) -> None
                    in
                    add Unusual_spend key { unusual = u; unusual_currency = ctx.currency; scope; explanation })
                (Insights.unusual_spending ctx);
              List.iter
                (fun (s : Analytics.shortfall) ->
                  add Shortfall (Printf.sprintf "shortfall:%s:%s" (Account_id.to_string s.short_account.account.id) (Cycle.label report.report_cycle)) (s, ctx.currency))
                (Insights.shortfalls ~recurring:(recurring ctx) ctx);
              (* The feed covers every goal currency, unlike a currency-specific report. *)
              List.iter
                (fun owner ->
                  List.iter
                    (fun (goal : Planning_store.goal) ->
                      match Service.currency service goal.currency with
                      | Error _ -> ()
                      | Ok currency ->
                          let g = Insights.goal_progress { ctx with currency } goal in
                          if not g.on_track then add Goal_off_track ("goal:" ^ g.goal.goal_id ^ ":" ^ Cycle.label current) (g, currency))
                    (Planning_store.goals db owner))
                ctx.owners)
        (Analytics.Me :: if Option.is_some (Access.household access) then [ Analytics.Household ] else []);
      (match Insights.contribution_plan ctx ~target:None ~incomes:[] with
      | Ok plan when Date.days_between ~first:plan.plan_cycle.first ~last:today < 7 -> (
          match List.find_opt (fun (m : Insights.member_plan) -> User_id.equal m.member user.id) plan.members with
          | Some me when Int64.compare me.contributed me.contribution < 0 ->
              let household_ctx = Result.get_ok (Insights.context service access ~scope:Household ~shared ()) in
              add Contributions_due ("contributions:" ^ Cycle.label plan.plan_cycle) (me, household_ctx.currency)
          | _ -> ())
      | _ -> ());
      (match Insights.settle_up ctx with
      | Ok (_ :: _ as owed) -> add Settle_up "settle_up" (owed, (Result.get_ok (Insights.context service access ~scope:Household ~shared ())).currency)
      | _ -> ());
      List.iter
        (fun (r : Analytics.recurring) ->
          let fresh = r.occurrences = 3 && Date.days_between ~first:r.last_date ~last:today <= 40 in
          if Analytics.price_changed r || fresh then add Recurring_change ("recurring:" ^ r.merchant ^ ":" ^ Date.to_string r.last_date) (r, ctx.currency))
        (recurring ctx));
  List.iter (fun (c : count_cash) -> add Count_cash ("count_cash:" ^ Account_id.to_string c.cash.account.id ^ ":" ^ Date.to_string c.since) c) (count_cash service access ~today);
  !cards
  |> List.filter (fun (Card c) ->
         match current_state db user.id c.key ~today with
         | State Shown -> true
         | State (Snoozed _) -> false
         | State (Resolved _) -> (match c.kind with Budget_alert -> true | _ -> false))
  |> List.stable_sort (fun (Card a) (Card b) -> Int.compare (priority a.kind) (priority b.kind))

(* Acting on a card: only a shown card can be snoozed or resolved, which the transition types say. *)
let act (service : Service.t) access ~key ~action ~until =
  let user = Access.user access in
  let today = Service.today service access in
  (* A resolved card that is back in the feed (a budget alert that got worse) counts as shown. *)
  let state =
    match current_state service.db user.id key ~today with
    | State (Resolved _) when List.exists (fun (Card c) -> c.key = key) (feed service access) -> State Shown
    | state -> state
  in
  match state, action with
  | State Shown, `Snooze ->
      let until = Option.value until ~default:(Date.add_days today 3) in
      if Date.compare until today <= 0 then Error (`Invalid "snooze until a later date")
      else (persist service.db user.id key ~now:(service.now ()) (step Shown (Snooze until)); Ok ())
  | State Shown, `Resolve note -> persist service.db user.id key ~now:(service.now ()) (step Shown (Resolve note)); Ok ()
  | State (Snoozed _), _ -> Error (`Invalid "that card is snoozed")
  | State (Resolved _), _ -> Ok ()

(* JSON *)

let payload_json : type a. a kind -> a -> Yojson.Safe.t =
 fun kind payload ->
  match kind, payload with
  | Invitation, h -> Json.household h
  | Reconciliation_mismatch, m ->
      let c = m.account.account.currency in
      `Assoc
        [ ("account", Json.account m.account); ("date", Json.date m.checkpoint.on_date); ("statement_balance", Json.money c m.checkpoint.statement_balance);
          ("ledger_balance", Json.money c m.checkpoint.ledger_balance); ("difference", Json.money c (Int64.sub m.checkpoint.statement_balance m.checkpoint.ledger_balance)) ]
  | Import_due, d -> `Assoc [ ("account", Json.account d.account); ("last_import_at", Json.option Json.time d.last_import); ("days", Json.option (fun d -> `Int d) d.days) ]
  | Inbox, i -> `Assoc [ ("count", `Int i.waiting); ("suggested", `Int i.suggested) ]
  | Month_review, r ->
      `Assoc
        [ ("cycle", Json.cycle r.reviewed);
          ( "summary",
            Json.option
              (fun (s : Analytics.cycle_summary) ->
                let m = Json.money r.review_currency in
                `Assoc
                  [ ("income", m s.flows.income); ("spending", m s.flows.spending); ("savings", m s.savings);
                    ("savings_rate", match s.savings_rate_bp with None -> `Null | Some bp -> `String (Json.decimal ~scale:2 (Int64.of_int bp) ^ "%")) ])
              r.review_summary );
          ("text", Json.option (fun t -> `String t) r.text) ]
  | Budget_alert, (l, c) ->
      `Assoc
        [ ("category", Json.account_ref l.line_category); ("budget", Json.money c (Int64.add l.budgeted l.carried)); ("spent", Json.money c l.spent);
          ("expected_by_now", Json.money c l.expected_by_now); ("status", `String (status_note l.status)) ]
  | Shortfall, (s, c) ->
      `Assoc
        [ ("account", Json.account s.short_account); ("available", Json.money c s.available); ("due", Json.money c s.due);
          ("short_by", Json.money c (Int64.sub s.due s.available));
          ( "upcoming",
            `List
              (List.map
                 (fun (r : Analytics.recurring) -> `Assoc [ ("merchant", `String r.merchant_name); ("amount", Json.money c r.typical_amount); ("expected", Json.date r.next_expected) ])
                 s.upcoming) ) ]
  | Unusual_spend, u ->
      let m = Json.money u.unusual_currency in
      `Assoc
        [ ("category", Json.account_ref u.unusual.unusual_category); ("scope", `String (Analytics.scope_to_string u.scope));
          ("spent", m u.unusual.spent_so_far); ("usual", m u.unusual.usual); ("text", Json.option (fun t -> `String t) u.explanation) ]
  | Contributions_due, (m, c) ->
      `Assoc [ ("contribution", Json.money c m.contribution); ("transferred", Json.money c m.contributed); ("remaining", Json.money c (Int64.sub m.contribution m.contributed)) ]
  | Settle_up, (owed, c) ->
      `Assoc [ ("owed", `List (List.map (fun (o : Insights.owed) -> `Assoc [ ("name", `String o.owed_name); ("amount", Json.money c o.owed) ]) owed)) ]
  | Unmatched_entry, v -> `Assoc [ ("transaction", Json.transfer v) ]
  | Count_cash, c -> `Assoc [ ("account", Json.account c.cash); ("since", Json.date c.since) ]
  | Goal_off_track, (g, c) ->
      `Assoc
        [ ("goal", `String g.goal.name); ("saved", Json.money c g.saved); ("target", Json.money c g.goal.target_amount);
          ("required_monthly", Json.option (Json.money c) g.required_monthly); ("monthly_amount", Json.option (Json.money c) g.goal.monthly_amount) ]
  | Recurring_change, (r, c) ->
      `Assoc
        [ ("merchant", `String r.merchant_name); ("cadence", `String (Analytics.cadence_to_string r.cadence)); ("last_amount", Json.money c r.last_amount);
          ("previous_amount", Json.money c r.previous_amount); ("price_changed", `Bool (Analytics.price_changed r)); ("next_expected", Json.date r.next_expected) ]
  | Overview, o ->
      `Assoc
        [ ("cycle", Json.cycle o.cycle);
          ( "summary",
            Json.option
              (fun (s : Analytics.cycle_summary) ->
                let m = Json.money o.currency in
                `Assoc [ ("income", m s.flows.income); ("spending", m s.flows.spending); ("savings", m s.savings); ("reconciled", `Bool s.reconciled) ])
              o.summary );
          ("currency", `String (Currency.code o.currency)) ]

let card_json (Card c) =
  `Assoc [ ("kind", `String (kind_name c.kind)); ("key", `String c.key); ("resolvable", `Bool c.resolvable); ("data", payload_json c.kind c.payload) ]

let commands =
  let open Commands in
    [ command "get_attention_feed" Read
        "What needs attention now, most important first: invitations, reconciliation mismatches, statements due for import, the inbox, \
         unconfirmed manual entries, accounts the bills due before payday would overdraw, the month \
         review, budget alerts, unusual spending in categories without a budget, contributions due, settle-up, goals off track and new or \
         changed subscriptions, ending with an overview of the current pay cycle."
        [ ("narrative", `Assoc [ ("type", `String "boolean"); ("description", `String "Include optional generated explanations; defaults to true.") ]) ]
        (fun service access fields ->
          let ( let* ) = Result.bind in
          let* narrative = arg (Query.Args.bool "narrative" ~default:true fields) in
          Ok (`Assoc [ ("cards", `List (List.map card_json (feed ~narrative service access))) ]));
      command ~mcp:false "act_on_card" Write "Snooze a card until a date, or mark it done." ~required:[ "key"; "action" ]
        [ string_prop "key" "The card's key."; string_prop "action" "snooze or resolve."; date_prop "until" "For snooze; defaults to three days from now." ]
        (fun service access fields ->
          let ( let* ) = Result.bind in
          let* key = required_string "key" fields in
          let* action = required_string "action" fields in
          let* until = arg (Query.Args.date "until" fields) in
          let note =
            (* A budget alert remembers the status that was acknowledged. *)
            match List.find_opt (fun (Card c) -> c.key = key) (feed service access) with
            | Some (Card { kind = Budget_alert; payload = l, _; _ }) -> status_note l.status
            | _ -> "done"
          in
          let* action = match action with "snooze" -> Ok `Snooze | "resolve" -> Ok (`Resolve note) | _ -> invalid "action must be snooze or resolve" in
          let* () = lift (act service access ~key ~action ~until) in
          Ok (`Assoc [ ("cards", `List (List.map card_json (feed service access))) ])) ]
