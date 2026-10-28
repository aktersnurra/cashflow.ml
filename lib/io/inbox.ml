(* Triage of imported rows. Every action is a ledger transfer: categorising moves the amount from
   wherever the row sits now (at first, an uncategorized account) to the chosen account, with the
   row's own date, so the bank account's balance never changes. Keys carry the item's revision, so
   a retry is safe and a later recategorisation is a new transfer. *)

module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Currency = Cashflow_core.Currency
module Date = Cashflow_core.Date
module Household = Cashflow_core.Household
module Idempotency_key = Cashflow_core.Idempotency_key
module Ledger_core = Cashflow_core.Ledger_core
module Ledger_error = Cashflow_core.Ledger_error
module Ledger_event = Cashflow_core.Ledger_event
module Owner = Cashflow_core.Owner
module Suggest = Cashflow_core.Suggest
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id
module User_id = Cashflow_core.User_id

let ( let* ) = Result.bind

type item_view = { item : Import_store.item; transfer : Service.transfer_view; suggested : Account_info.t option; candidate : Service.transfer_view option }

let view_item (service : Service.t) access (item : Import_store.item) =
  let* transfer = Service.get_transfer service access item.transfer_id in
  let suggested = Option.bind item.suggested_account_id (fun id -> Result.to_option (Service.find_account service access id)) in
  let candidate = Option.bind item.candidate_pending_id (fun id -> Result.to_option (Service.get_transfer service access id)) in
  Ok { item; transfer; suggested; candidate }

let list (service : Service.t) access ~limit =
  List.filter_map (fun item -> Result.to_option (view_item service access item)) (Import_store.open_items service.db access ~limit)

let count (service : Service.t) access = Import_store.open_count service.db access

let find_open (service : Service.t) access ?(paired = false) transfer_id =
  match Import_store.find_item service.db transfer_id with
  | Some { status = "paired"; _ } when not paired -> Error (`Invalid "this row is paired with another as one move between accounts")
  | Some item when item.status <> "dismissed" -> (
      match Service.find_account service access item.bank_account_id with Ok bank -> Ok (item, bank) | Error _ -> Error (`Not_found "inbox item"))
  | _ -> Error (`Not_found "inbox item")

let key_for item revision =
  Result.get_ok (Idempotency_key.of_string (Printf.sprintf "categorize:%s:%d" (Transfer_id.to_string item.Import_store.transfer_id) revision))

(* Whether the row took money out of the bank account. *)
let outflow (item : Import_store.item) (t : Transfer.t) = Account_id.equal t.credit_account_id item.bank_account_id

let single_result = function Ok [ result ] -> Ok result | Ok _ -> Error (`Invalid "unexpected batch result") | Error e -> Error e

let finish (service : Service.t) (item : Import_store.item) ~current ~revision ~status =
  Import_store.save_item service.db ~now:(service.now ()) { item with current_account_id = current; revision; status }

let transfer_of = function Ok (Ledger_core.Created (v : Service.transfer_view) | Exists v) -> Some v.transfer.id | Error _ -> None

(* Remembers what a triage action posted for each row it was about (see Import.undo). *)
let record (service : Service.t) items results =
  List.iter (fun result -> Option.iter (fun t -> List.iter (fun (item : Import_store.item) -> Import_store.record_triage service.db ~item:item.transfer_id t) items) (transfer_of result)) results

let categorize (service : Service.t) access ~transfer_id ~category_id ?(tags = []) () =
  let* item, bank = find_open service access transfer_id in
  let* original = Service.get_transfer service access transfer_id in
  let* target = Service.find_account service access category_id in
  if not (Owner.equal target.account.owner bank.account.owner) then
    Error (`Invalid "choose a category of the same owner as the bank account, or mark the row as shared")
  else if not (Currency.equal target.account.currency bank.account.currency) then Error (`Invalid "the category is in another currency")
  else if Account_id.equal target.account.id item.current_account_id then Ok None
  else
    let t = original.transfer in
    let revision = item.revision + 1 in
    let debit, credit = if outflow item t then (category_id, item.current_account_id) else (item.current_account_id, category_id) in
    (* Categorising to a balance-sheet account (cash, savings) makes the row a move between accounts. *)
    let code : Transfer.code = match target.account.kind with Asset | Liability -> Internal | Equity | Income | Expense -> Reclassify in
    Sql.transaction service.db (fun () ->
        let* result =
          single_result
            (Service.submit service access ~scope:bank.account.owner
               [ { key = key_for item revision; linked = false;
                   body =
                     Transfer
                       { debit_account_id = debit; credit_account_id = credit; amount = t.amount; phase = Posted; code;
                         memo = { t.memo with tags = t.memo.tags @ tags }; value_date = t.value_date } } ])
        in
        match result with
        | Ok _ ->
            record service [ item ] [ result ];
            finish service item ~current:category_id ~revision ~status:"done";
            Ok (Some result)
        | Error _ -> Ok (Some result))

(* Takes a categorisation back: reverses the transfer it posted and puts the row back in the inbox where it was.
   Only if the row is exactly as that categorisation left it (same revision, same category); anything done to it
   since means this is no longer the change to undo. The revision moves on, so categorising it again posts a new
   transfer rather than finding the reversed one by its key. *)
let reopen (service : Service.t) access ~transfer_id ~revision ~category_id ~previous =
  let* item, bank = find_open service access transfer_id in
  if item.status <> "done" || item.revision <> revision || not (Account_id.equal item.current_account_id category_id) then
    Error (`Invalid "the transaction has changed since, so this can't be undone")
  else
    let view = Store_sqlite.view service.db in
    let key = key_for item item.revision in
    let posted =
      List.find_map
        (fun id -> match view.transfer id with Some (t : Transfer.t) when String.equal (Idempotency_key.to_string t.key) (Idempotency_key.to_string key) -> Some t | _ -> None)
        (Import_store.triage_transfers service.db transfer_id)
    in
    match posted with
    | None -> Error (`Invalid "the categorisation can't be found, so it can't be undone")
    | Some (t : Transfer.t) ->
        let reverse : Ledger_event.t =
          { key = Result.get_ok (Idempotency_key.of_string ("undo:" ^ Transfer_id.to_string t.id)); linked = false;
            body = Reverse { transfer_id = t.id; value_date = None; memo = None } }
        in
        Sql.transaction service.db (fun () ->
            match Service.submit service access ~scope:bank.account.owner [ reverse ] with
            | Ok [ (Ok _ as result) ] ->
                (* Recorded like every triage action, so reports that fold triage drop both the categorisation and its
                   reversal and show the row against where it is again. *)
                record service [ item ] [ result ];
                Import_store.save_item service.db ~now:(service.now ())
                  { item with current_account_id = previous; revision = item.revision + 1; status = "open" };
                Ok ()
            | Ok [ Error e ] -> Error (`Invalid ("undo failed: " ^ Ledger_error.transfer_to_string e))
            | Ok _ -> Error (`Invalid "undo failed")
            | Error e -> Error e)

(* A shared cost paid from a personal account (§4.6): the household's books get the expense and
   owe the member for it; the member's books swap the expense for what they are owed. *)
let role_account (service : Service.t) access owner role kind ~currency ~name ~member_id =
  match
    List.find_opt
      (fun (i : Account_info.t) ->
        i.details.role = Some role && i.account.kind = kind && Currency.code i.account.currency = currency && i.details.member_id = member_id
        && not i.account.closed)
      (Account_store.for_owner service.Service.db owner)
  with
  | Some info -> Ok info
  | None -> (
      let suffix = Option.fold ~none:"" ~some:User_id.to_string member_id in
      let key = Result.get_ok (Idempotency_key.of_string (Printf.sprintf "role:%s:%s:%s" (Account_info.role_to_string role) currency suffix)) in
      match
        Service.open_account service access ~key ~owner ~kind ~name ~currency ~flags:Account.no_flags
          ~details:{ Account_info.no_details with role = Some role; member_id }
      with
      | Ok (`Created info | `Exists info) -> Ok info
      | Error e -> Error e)

let share (service : Service.t) access ~transfer_id ~household_category_id =
  let* item, bank = find_open service access transfer_id in
  let* original = Service.get_transfer service access transfer_id in
  let* household = Option.to_result ~none:(`Invalid "you don't belong to a household") (Access.household access) in
  let* category = Service.find_account service access household_category_id in
  let me = Access.user access in
  let t = original.transfer in
  if not (Owner.equal bank.account.owner (Access.personal access)) then Error (`Invalid "only rows from your personal accounts can be shared")
  else if not (outflow item t) then Error (`Invalid "only spending can be shared")
  else if not (Owner.equal category.account.owner (Household household.id) && category.account.kind = Expense) then
    Error (`Invalid "choose one of the household's expense categories")
  else
    let currency = Currency.code bank.account.currency in
    let* receivable =
      role_account service access (Access.personal access) Receivable Asset ~currency ~name:("Owed by " ^ household.name) ~member_id:None
    in
    let* payable =
      role_account service access (Owner.Household household.id) Payable Liability ~currency ~name:("Owed to " ^ me.display_name)
        ~member_id:(Some me.id)
    in
    let revision = item.revision + 1 in
    let household_key = Result.get_ok (Idempotency_key.of_string (Idempotency_key.to_string (key_for item revision) ^ "#household")) in
    let transfer debit credit code : Ledger_event.body =
      Transfer { debit_account_id = debit; credit_account_id = credit; amount = t.amount; phase = Posted; code; memo = t.memo; value_date = t.value_date }
    in
    Sql.transaction service.db (fun () ->
        let* results =
          Service.submit service access ~scope:(Access.personal access)
            [ { key = key_for item revision; linked = true; body = transfer receivable.account.id item.current_account_id Reclassify };
              { key = household_key; linked = false; body = transfer category.account.id payable.account.id Purchase } ]
        in
        if List.for_all Result.is_ok results then (
          record service [ item ] results;
          finish service item ~current:receivable.account.id ~revision ~status:"done");
        Ok results)

(* A row can be one half of a move while it waits in the inbox or is filed in an income or expense
   category, by a person before the other half was imported or on import without asking: pairing
   moves the amount on from wherever the row is now. Not once it is shared with the household or
   moved to an account. *)
let pairable (service : Service.t) access (item : Import_store.item) =
  let* current = Service.find_account service access item.current_account_id in
  match current.account.kind with
  | Income | Expense -> Ok ()
  | Asset | Liability | Equity -> Error (`Invalid "one of these rows was shared or moved to an account, so it can't be paired")

(* Two rows that are one move between the user's own accounts (checking to savings, personal to
   household): the money left one and arrived in the other, so what each row was counted as
   (uncategorized, or the category it was filed in) cancels. Both rows are then paired: asking
   again for the same pair returns what was posted, and neither row can be paired with anything else. *)
let pair (service : Service.t) access ~out_id ~in_id =
  let* out_item, out_bank = find_open service access ~paired:true out_id in
  let* in_item, in_bank = find_open service access ~paired:true in_id in
  let shared = List.filter (fun t -> List.exists (Transfer_id.equal t) (Import_store.triage_transfers service.db in_id)) (Import_store.triage_transfers service.db out_id) in
  match out_item.status, in_item.status, shared with
  | "paired", "paired", t :: _ ->
      let* existing = Service.get_transfer service access t in
      Ok (Ok (Ledger_core.Exists existing))
  | "paired", _, _ | _, "paired", _ -> Error (`Invalid "one of the rows is already paired with another")
  | _ ->
      let* () = pairable service access out_item in
      let* () = pairable service access in_item in
      let* out_t = Service.get_transfer service access out_id in
      let* in_t = Service.get_transfer service access in_id in
      let out_t = out_t.transfer and in_t = in_t.transfer in
      if not (outflow out_item out_t && not (outflow in_item in_t)) then Error (`Invalid "pair a row that took money out with one that brought it in")
      else if not (Amount.equal out_t.amount in_t.amount) then Error (`Invalid "the two rows have different amounts")
      else if abs (Date.days_between ~first:out_t.value_date ~last:in_t.value_date) > 3 then Error (`Invalid "the two rows are more than three days apart")
      else if not (Currency.equal out_bank.account.currency in_bank.account.currency) then Error (`Invalid "the accounts have different currencies")
      else
        let code : Transfer.code = if Owner.equal out_bank.account.owner in_bank.account.owner then Internal else Contribution in
        let scope = if Owner.equal out_bank.account.owner in_bank.account.owner then out_bank.account.owner else Access.personal access in
        let revision = out_item.revision + 1 in
        Sql.transaction service.db (fun () ->
            let* result =
              single_result
                (Service.submit service access ~scope
                   [ { key = key_for out_item revision; linked = false;
                       body =
                         Transfer
                           { debit_account_id = in_item.current_account_id; credit_account_id = out_item.current_account_id; amount = out_t.amount;
                             phase = Posted; code; memo = out_t.memo; value_date = out_t.value_date } } ])
            in
            (match result with
            | Ok _ ->
                record service [ out_item; in_item ] [ result ];
                finish service out_item ~current:out_item.current_account_id ~revision ~status:"paired";
                finish service in_item ~current:in_item.current_account_id ~revision:in_item.revision ~status:"paired"
            | Error _ -> ());
            Ok result)

(* Rows that look like the other half of a move: the same amount the other way within three days. *)
let pair_candidates (service : Service.t) access ~transfer_id =
  let* item, _ = find_open service access transfer_id in
  let* t = Service.get_transfer service access transfer_id in
  let t = t.transfer in
  let others = List.filter (fun (v : item_view) -> not (Transfer_id.equal v.item.transfer_id transfer_id)) (list service access ~limit:500) in
  Ok
    (List.filter
       (fun (v : item_view) ->
         let o = v.transfer.transfer in
         Amount.equal o.amount t.amount && outflow v.item o <> outflow item t
         && abs (Date.days_between ~first:o.value_date ~last:t.value_date) <= 3)
       others)

(* The row is the bank's version of a pending manual entry with a different amount: void the
   entry, and categorise the row as the entry was. *)
let accept_candidate (service : Service.t) access ~transfer_id =
  let* item, bank = find_open service access transfer_id in
  let* pending_id = Option.to_result ~none:(`Invalid "this row has no pending entry to match") item.candidate_pending_id in
  let* pending = Service.get_transfer service access pending_id in
  let category = if Account_id.equal pending.transfer.credit_account_id bank.account.id then pending.transfer.debit_account_id else pending.transfer.credit_account_id in
  Sql.transaction_result service.db (fun () ->
      let void_key = Result.get_ok (Idempotency_key.of_string ("matched:" ^ Transfer_id.to_string pending_id)) in
      let* voided = Service.void_pending service access ~key:void_key ~pending_id ~value_date:pending.transfer.value_date in
      match voided with
      | Error e -> Error (`Invalid (Ledger_error.transfer_to_string e))
      | Ok _ -> (
          let* result = categorize service access ~transfer_id ~category_id:category () in
          match result with Some (Error e) -> Error (`Invalid (Ledger_error.transfer_to_string e)) | Some r -> Ok r | None -> Ok voided))

let dismiss (service : Service.t) access ~transfer_id =
  let* item, _ = find_open service access transfer_id in
  finish service item ~current:item.current_account_id ~revision:item.revision ~status:"dismissed";
  Ok ()

(* Reconciliation (§3.6): record what the bank says the balance was on a date, and optionally post
   the difference so the ledger agrees, against untracked cash for a cash account and against
   uncategorized otherwise. Nothing is ever adjusted without being asked. *)
let reconcile (service : Service.t) access ~account_id ~on_date ~statement_balance ~adjust =
  let* info = Service.find_account service access account_id in
  let account = info.account in
  let ledger_balance = Import_store.posted_balance_on service.db account on_date in
  let difference = Int64.sub statement_balance ledger_balance in
  let* adjustment =
    if Int64.equal difference 0L || not adjust then Ok None
    else
      let role, kind = if info.details.role = Some Cash then (Account_info.Untracked_cash, Account.Expense) else (Uncategorized, Expense) in
      let currency = Currency.code account.currency in
      let* target =
        role_account service access account.owner role kind ~currency
          ~name:(if role = Untracked_cash then "Untracked cash" else "Uncategorized") ~member_id:None
      in
      (* The account must move by [difference]; for a debit-normal account that's a debit when positive. *)
      let increase = Int64.compare difference 0L > 0 = Account.debit_normal account.kind in
      let debit, credit = if increase then (account.id, target.account.id) else (target.account.id, account.id) in
      let key = Result.get_ok (Idempotency_key.of_string (Printf.sprintf "reconcile:%s:%s:%Ld" (Account_id.to_string account.id) (Date.to_string on_date) statement_balance)) in
      let* results =
        Service.submit service access ~scope:account.owner
          [ { key; linked = false;
              body =
                Transfer
                  { debit_account_id = debit; credit_account_id = credit; amount = Option.get (Amount.of_int64 (Int64.abs difference)); phase = Posted;
                    code = Adjustment; memo = { Transfer.empty_memo with description = "Reconciliation adjustment" }; value_date = on_date } } ]
      in
      match results with [ Ok _ ] -> Ok (Some difference) | [ Error e ] -> Error (`Invalid (Ledger_error.transfer_to_string e)) | _ -> Ok None
  in
  let ledger_balance = Import_store.posted_balance_on service.db account on_date in
  Planning_store.add_checkpoint service.db ~now:(service.now ()) ~user_id:(Access.user access).id ~source:"manual"
    { checkpoint_id = Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()); account_id; on_date; statement_balance; ledger_balance; source = "manual" };
  Ok (`Difference difference, `Adjusted adjustment, `Ledger_balance ledger_balance)

(* Rules *)

let add_rule_as (service : Service.t) access ~rule_id ~owner ~pattern ~direction ~min_amount ~max_amount ~bank_account_id ~category_id ~tags ~shared ~auto_apply =
  if not (Access.can_use access owner) then Error (`Forbidden "you can't add rules for that owner")
  else if String.trim pattern = "" then Error (`Invalid "a rule needs a pattern")
  else
    let* category = Service.find_account service access category_id in
    if not (Owner.equal category.account.owner owner) && not shared then Error (`Invalid "the category must belong to the rule's owner")
    else
      let rule : Suggest.rule =
        { rule_id = (match rule_id with Some id -> id | None -> Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ())); owner; pattern = String.trim pattern; direction;
          min_amount; max_amount; bank_account_id; category_id; tags; shared; auto_apply }
      in
      Import_store.add_rule service.db ~now:(service.now ()) rule;
      Ok rule

let add_rule service access ~owner ~pattern ~direction ~min_amount ~max_amount ~bank_account_id ~category_id ~tags ~shared ~auto_apply =
  add_rule_as service access ~rule_id:None ~owner ~pattern ~direction ~min_amount ~max_amount ~bank_account_id ~category_id ~tags ~shared ~auto_apply

let rules (service : Service.t) access = List.concat_map (Import_store.rules service.db) (Access.owners access)

let remove_rule (service : Service.t) access ~rule_id =
  if List.exists (fun owner -> Import_store.remove_rule service.db owner rule_id) (Access.owners access) then Ok () else Error (`Not_found "rule")
(* Auto-apply: a bank account's opt-in to applying confident history and LLM suggestions on import. *)

let auto_apply_settings (service : Service.t) access =
  let usable = List.filter_map (fun (i : Account_info.t) -> if i.details.statement_backed then Some i.account.id else None) (Service.accounts service access) in
  List.filter (fun (id, _) -> List.exists (Account_id.equal id) usable) (Import_store.auto_apply_thresholds service.db)

let set_auto_apply (service : Service.t) access ~account_id ~min_confidence =
  let* bank = Service.find_account service access account_id in
  let* () = Service.may_open access bank.account.owner bank.account.kind in
  if not bank.details.statement_backed then Error (`Invalid "only bank accounts with statements import rows")
  else (
    Import_store.set_auto_apply_threshold service.db account_id min_confidence;
    Ok ())

(* Which of these rows are still waiting in the inbox. *)
let still_open (service : Service.t) ids =
  List.filter (fun id -> match Import_store.find_item service.db id with Some { status = "open"; _ } -> true | _ -> false) ids

(* Finding moves between the user's own accounts across the whole inbox, in memory: two years of
   imports from several accounts is thousands of rows, too many to ask the database about one by one.
   A row's counterparts are the pairable rows of the same amount the other way within three days on
   another bank account. *)
type move_row = { view : item_view; out : bool }

let counterparts_of rows =
  let by_amount = Hashtbl.create 1024 in
  List.iter (fun r -> Hashtbl.add by_amount (Amount.to_int64 r.view.transfer.transfer.amount) r) rows;
  fun r ->
    List.filter
      (fun o ->
        o.out <> r.out && (not (Account_id.equal o.view.item.bank_account_id r.view.item.bank_account_id))
        && abs (Date.days_between ~first:o.view.transfer.transfer.value_date ~last:r.view.transfer.transfer.value_date) <= 3)
      (Hashtbl.find_all by_amount (Amount.to_int64 r.view.transfer.transfer.amount))

(* The rows waiting in the inbox and those filed in a category (see [pairable]): a move filed as spending before its
   other half was imported is still a move. *)
let move_rows (service : Service.t) access =
  List.filter_map
    (fun item -> Result.to_option (Result.map (fun (view : item_view) -> { view; out = outflow view.item view.transfer.transfer }) (view_item service access item)))
    (Import_store.pairable_items service.db access)

let pair_rows (service : Service.t) access ~out ~in_ =
  match pair service access ~out_id:out.view.item.transfer_id ~in_id:in_.view.item.transfer_id with Ok (Ok _) -> true | _ -> false

let same_row a b = Transfer_id.equal a.view.item.transfer_id b.view.item.transfer_id

(* A row with exactly one counterpart, which has no other counterpart either, is the same move seen from both
   accounts, so it is paired without asking. Pairing a move can make its neighbours unambiguous, so it repeats
   until nothing more pairs. A move [pair] refuses stays among the rows, so it still counts as a rival, but is
   not tried again. Returns how many moves it paired. *)
let pair_obvious (service : Service.t) access =
  let refused = Hashtbl.create 16 in
  let rec go rows paired =
    let counterparts = counterparts_of rows in
    let moves =
      List.filter_map
        (fun r ->
          if (not r.out) || Hashtbl.mem refused r.view.item.transfer_id then None
          else match counterparts r with [ o ] when (match counterparts o with [ back ] -> same_row back r | _ -> false) -> Some (r, o) | _ -> None)
        rows
    in
    let made = List.filter (fun (r, o) -> pair_rows service access ~out:r ~in_:o || (Hashtbl.replace refused r.view.item.transfer_id (); false)) moves in
    if made = [] then paired
    else go (List.filter (fun r -> not (List.exists (fun (a, b) -> same_row a r || same_row b r) made)) rows) (paired + List.length made)
  in
  go (move_rows service access) 0

(* What is still unsure goes to the LLM, a batch at a time: each outflow row with its possible counterparts.
   [among] limits it to the rows involving those transfers, so an import doesn't ask again about old leftovers.
   [pair] still checks amount, dates and currency of what the model picks. Returns how many moves it paired. *)
let max_llm_batches = 20

let unsure_cases rows ?among () =
  let counterparts = counterparts_of rows in
  let concerns r = match among with None -> true | Some ids -> List.exists (Transfer_id.equal r.view.item.transfer_id) ids in
  List.filter_map
    (fun r ->
      if not r.out then None
      else match counterparts r with [] -> None | options -> if concerns r || List.exists concerns options then Some (r, options) else None)
    rows

let rec chunks n = function
  | [] -> []
  | items -> let batch = List.filteri (fun i _ -> i < n) items in batch :: chunks n (List.filteri (fun i _ -> i >= n) items)

let pair_unsure ?among (service : Service.t) access =
  let rows = move_rows service access in
  let scale_of r = match Service.find_account service access r.view.item.bank_account_id with Ok bank -> Currency.scale bank.account.currency | Error _ -> 2 in
  let describe r = (Transfer_id.to_string r.view.item.transfer_id, r.view.transfer.transfer.value_date, r.view.transfer.transfer.memo.description) in
  let find id = List.find_opt (fun r -> String.equal (Transfer_id.to_string r.view.item.transfer_id) id) rows in
  let paired = ref 0 in
  if Option.is_some service.assistant then
    List.iter
      (fun batch ->
        let cases =
          List.map
            (fun (r, options) ->
              let id, date, text = describe r in
              { Llm_features.id; date; text; amount = Json.decimal ~scale:(scale_of r) (Amount.to_int64 r.view.transfer.transfer.amount); options = List.map describe options })
            batch
        in
        List.iter
          (fun (id, option) -> match find id, find option with Some out, Some in_ -> if pair_rows service access ~out ~in_ then incr paired | _ -> ())
          (Llm_features.pair_moves service access cases))
      (List.filteri (fun i _ -> i < max_llm_batches) (chunks Llm_features.max_rows (unsure_cases rows ?among ())));
  !paired

(* Everything at once, for rows imported before this existed or left over: returns (paired by rule, paired by the
   model, rows with a possible counterpart still left to a person). *)
let pair_all service access =
  let obvious = pair_obvious service access in
  let by_llm = pair_unsure service access in
  (obvious, by_llm, List.length (unsure_cases (move_rows service access) ()))
