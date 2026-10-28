(* Applying and dismissing the changes the advisor proposed. The click is the only way a proposal does anything:
   it runs here, with the clicking user's own access and the same code as the manual change, after checking
   the proposal is still undecided, unexpired and based on what is true now. *)
open Cashflow_core
module A = Advisor
module S = Advisor_store

let ( let* ) = Result.bind
let error code = Error { A.code; context = A.No_context }
let store (service : Service.t) = S.create ~clock:service.now service.db

let field fields name = match fields with `Assoc fs -> List.assoc_opt name fs | _ -> None
let string_field fields name = match field fields name with Some (`String s) -> Some s | _ -> None

(* The conversation's own scope and currency, and the clicking user's current access to it. *)
let scoped_context service ~principal ~owner (p : S.proposal) =
  let* meta = S.metadata (store service) ~owner p.conversation_id in
  let* () = Advisor_access.authorize_scope service principal meta.scope in
  let* access = Advisor_access.access service principal in
  let query_scope = match meta.scope with A.Personal -> Analytics.Me | Household _ -> Analytics.Household in
  match Insights.context service access ~scope:query_scope ~currency:(Currency.code meta.currency) () with
  | Error _ -> error A.Access_denied
  | Ok ctx -> Ok (access, ctx)

(* Budget: the category is still an expense category in the conversation's scope, it is still the pay cycle the card
   named, and the budget it would replace (amount and carry-over) is the one the user saw. [`Out_of_date] is a stale
   proposal, not a failure. *)
let ready_budget service ~principal ~owner (p : S.proposal) =
  let* _, ctx = scoped_context service ~principal ~owner p in
  let arg = field p.arguments and pre = field p.precondition in
  match
    ( Option.bind (string_field p.arguments "category_id") Account_id.of_string,
      Option.bind (string_field p.arguments "monthly_amount") Int64.of_string_opt,
      arg "rollover",
      string_field p.precondition "from_cycle" )
  with
  | Some category_id, Some monthly_amount, Some (`Bool rollover), Some from_cycle -> (
      let recorded_amount = Option.bind (string_field p.precondition "current_monthly") Int64.of_string_opt in
      let recorded_rollover = match pre "current_rollover" with Some (`Bool b) -> Some b | _ -> None in
      let current = Insights.current_budget ctx category_id in
      match (Insights.book ctx).inside category_id with
      | Some info
        when info.account.kind = Account.Expense
             && String.equal (Insights.current_cycle_label ctx) from_cycle
             && Option.equal Int64.equal (Option.map (fun (b : Analytics.budget) -> b.monthly_amount) current) recorded_amount
             && Option.equal Bool.equal (Option.map (fun (b : Analytics.budget) -> b.rollover) current) recorded_rollover ->
          Ok (`Ready (ctx, category_id, monthly_amount, rollover))
      | _ -> Ok `Out_of_date)
  | _ -> error A.Invalid_request

(* Categorize: every row is still open, untouched since it was proposed (same revision, same place) and its category
   is still an expense or income category in the conversation's scope. *)
let ready_categorize service ~principal ~owner (p : S.proposal) =
  let* access, ctx = scoped_context service ~principal ~owner p in
  let list_of json name = match field json name with Some (`List xs) -> xs | _ -> [] in
  let rows = list_of p.arguments "rows" and recorded = list_of p.precondition "rows" in
  let book = Insights.book ctx in
  let parse row =
    match (Option.bind (string_field row "transfer_id") Transfer_id.of_string, Option.bind (string_field row "category_id") Account_id.of_string) with
    | Some transfer, Some category -> Some (transfer, category)
    | _ -> None
  in
  let parsed = List.map parse rows in
  if rows = [] || List.length rows > 20 || List.exists Option.is_none parsed || List.length recorded <> List.length rows then error A.Invalid_request
  else
    let pairs = List.filter_map Fun.id parsed in
    let unchanged ((transfer, category), before) =
      let revision = match field before "revision" with Some (`Int n) -> Some n | _ -> None in
      let current = Option.bind (string_field before "current") Account_id.of_string in
      match Import_store.find_item service.Service.db transfer with
      | Some item ->
          item.status = "open" && Some item.revision = revision && Option.equal Account_id.equal (Some item.current_account_id) current
          && (match book.inside category with
              | Some info ->
                  (info.account.kind = Account.Expense || info.account.kind = Account.Income)
                  && Option.is_none info.details.role && (not info.account.closed)
                  && not (Account_id.equal category item.current_account_id)
              | None -> false)
      | None -> false
    in
    if List.for_all unchanged (List.combine pairs recorded) then Ok (`Ready_rows (access, pairs)) else Ok `Out_of_date

(* The simple kinds: each holds only typed values from the proposal, rechecked against the conversation's scope. *)
let usable_category ctx category_id direction =
  match (Insights.book ctx).inside category_id with
  | Some info
    when (info.account.kind = Account.Expense || info.account.kind = Account.Income)
         && Option.is_none info.details.role && (not info.account.closed)
         && (match (direction, info.account.kind) with Some "in", Account.Expense | Some "out", Account.Income -> false | _ -> true) ->
      Some info
  | _ -> None

let plan_rule service ~principal ~owner ~id (p : S.proposal) =
  let* access, ctx = scoped_context service ~principal ~owner p in
  match (string_field p.arguments "pattern", Option.bind (string_field p.arguments "category_id") Account_id.of_string) with
  | Some pattern, Some category_id -> (
      let direction = string_field p.arguments "direction" in
      match usable_category ctx category_id direction with
      | None -> Ok `Out_of_date
      | Some info ->
          let direction = match direction with Some "in" -> Some `In | Some "out" -> Some `Out | _ -> None in
          Ok
            (`Do
              (fun () ->
                match
                  Inbox.add_rule_as service access ~rule_id:(Some id) ~owner:info.account.owner ~pattern ~direction ~min_amount:None ~max_amount:None
                    ~bank_account_id:None ~category_id ~tags:[] ~shared:false ~auto_apply:false
                with
                | Ok _ -> Ok ()
                | Error _ -> Error ())))
  | _ -> error A.Invalid_request

let plan_goal service ~principal ~owner ~id (p : S.proposal) =
  let* _, ctx = scoped_context service ~principal ~owner p in
  let minor key = Option.bind (string_field p.arguments key) Int64.of_string_opt in
  match (string_field p.arguments "name", minor "target_amount") with
  | Some name, Some target_amount ->
      let target_date = Option.bind (string_field p.arguments "target_date") (fun d -> Result.to_option (Date.of_string d)) in
      let taken =
        List.exists (fun (g : Planning_store.goal) -> String.equal (String.lowercase_ascii g.name) (String.lowercase_ascii name))
          (Planning_store.goals service.Service.db (Insights.budget_owner ctx))
      in
      if taken || (match target_date with Some d -> Date.compare d ctx.today <= 0 | None -> false) then Ok `Out_of_date
      else
        Ok
          (`Do
            (fun () ->
              match Insights.save_goal ctx ~create_with:id ~name ~target_amount ~target_date ~monthly_amount:(minor "monthly_amount") () with
              | Ok _ -> Ok ()
              | Error _ -> Error ()))
  | _ -> error A.Invalid_request

let category_key id = Result.get_ok (Idempotency_key.of_string ("proposal:" ^ id))

let plan_category service ~principal ~owner ~id (p : S.proposal) =
  let* access, ctx = scoped_context service ~principal ~owner p in
  match (string_field p.arguments "name", string_field p.arguments "kind") with
  | Some name, Some (("expense" | "income") as kind_text) ->
      let kind = if kind_text = "expense" then Account.Expense else Account.Income in
      let scope_owner = Insights.budget_owner ctx in
      let taken =
        List.exists
          (fun (i : Account_info.t) ->
            i.account.kind = kind && (not i.account.closed) && Owner.equal i.account.owner scope_owner
            && List.exists (fun n -> String.equal (String.lowercase_ascii n) (String.lowercase_ascii name)) (Starter.names i))
          (Insights.accounts ctx)
      in
      if taken then Ok `Out_of_date
      else
        Ok
          (`Do
            (fun () ->
              match
                Service.open_account service access ~key:(category_key id) ~owner:scope_owner ~kind ~name ~currency:(Currency.code ctx.currency)
                  ~flags:Account.no_flags ~details:Account_info.no_details
              with
              | Ok (`Created _ | `Exists _) -> Ok ()
              | Error _ -> Error ()))
  | _ -> error A.Invalid_request

(* A manual entry: the same call as entering it by hand, under a key made from the proposal, so a repeat is the same entry. *)
let plan_entry service ~principal ~owner ~id (p : S.proposal) =
  let* access, ctx = scoped_context service ~principal ~owner p in
  let book = Insights.book ctx in
  match
    ( Option.bind (string_field p.arguments "from_account_id") Account_id.of_string, Option.bind (string_field p.arguments "to_account_id") Account_id.of_string,
      Option.bind (string_field p.arguments "amount") Int64.of_string_opt,
      Option.bind (string_field p.arguments "date") (fun d -> Result.to_option (Date.of_string d)), string_field p.arguments "description" )
  with
  | Some from_id, Some to_id, Some minor, Some on, Some description -> (
      match (book.inside from_id, book.inside to_id, Amount.of_int64 minor) with
      | Some from_, Some to_, Some amount ->
          let balance_sheet (k : Account.kind) = k = Account.Asset || k = Account.Liability in
          let sensible =
            (balance_sheet from_.account.kind && to_.account.kind = Account.Expense) || (from_.account.kind = Account.Income && balance_sheet to_.account.kind)
            || (balance_sheet from_.account.kind && balance_sheet to_.account.kind)
          in
          let pending = from_.details.statement_backed || to_.details.statement_backed in
          let promised = match field p.precondition "pending" with Some (`Bool b) -> b | _ -> pending in
          if Account_id.equal from_id to_id || pending <> promised || (not sensible) || from_.account.closed || to_.account.closed || Option.is_some from_.details.role || Option.is_some to_.details.role
             || Date.compare on (Date.add_days ctx.today (-366)) < 0 || Date.compare on (Date.add_days ctx.today 31) > 0
          then Ok `Out_of_date
          else
            Ok
              (`Do
                (fun () ->
                  match
                    Service.add_entry service access
                      { key = category_key id; from_account = from_id; to_account = to_id; amount; value_date = on;
                        memo = { Transfer.empty_memo with description }; code = None; scope = Some (Insights.budget_owner ctx) }
                  with
                  | Ok (Ok _) -> Ok ()
                  | _ -> Error ()))
      | _ -> Ok `Out_of_date)
  | _ -> error A.Invalid_request

let decide service ~owner ~id state =
  let* decided = S.decide_proposal (store service) ~owner ~id state in
  if decided then Ok state else (S.proposal (store service) ~owner id |> Result.map (fun (p : S.proposal) -> p.state))

let with_proposal service ~principal ~id f =
  let* user = Sql.read_snapshot service.Service.db (fun () -> Advisor_access.user service principal) in
  let* p = S.proposal (store service) ~owner:user.id id in
  match p.state with
  | A.Awaiting -> f user.id p
  | A.Lapsed ->
      (* Expiry is derived when read; record it the first time anyone acts on it. *)
      decide service ~owner:user.id ~id A.Lapsed
  | decided -> Ok decided

(* Checking the proposal still holds, claiming it and making the change are one transaction, so nothing can change
   in between: a row categorized in another tab, a second click or a retry cannot be overwritten or applied twice.
   [Error ()] from a write means the service rejected the change; it undoes the claim and the proposal is recorded as
   not applied. *)
let apply service ~principal ~id =
  with_proposal service ~principal ~id (fun owner p ->
      let rejected = { A.code = A.Invalid_request; context = A.No_context } in
      let plan () =
        match p.kind with
        | "set_budget" ->
            let* ready = ready_budget service ~principal ~owner p in
            Ok
              (match ready with
              | `Out_of_date -> `Out_of_date
              | `Ready (ctx, category_id, monthly_amount, rollover) ->
                  `Do
                    (fun () ->
                      match Insights.set_budget ctx ~category_id ~monthly_amount ~rollover ~effective_from:None with
                      | Ok _ -> Ok ()
                      | Error _ -> Error ()))
        | "categorize" ->
            let* ready = ready_categorize service ~principal ~owner p in
            Ok
              (match ready with
              | `Out_of_date -> `Out_of_date
              | `Ready_rows (access, pairs) ->
                  (* The same call as the Inbox's own button, row by row; any rejection undoes all of them. *)
                  `Do
                    (fun () ->
                      List.fold_left
                        (fun acc (transfer_id, category_id) ->
                          match acc with
                          | Error () -> acc
                          | Ok () -> (
                              match Inbox.categorize service access ~transfer_id ~category_id () with
                              | Ok (Some (Error _)) | Error _ -> Error ()
                              | Ok _ -> Ok ()))
                        (Ok ()) pairs))
        | "add_rule" -> plan_rule service ~principal ~owner ~id p
        | "save_goal" -> plan_goal service ~principal ~owner ~id p
        | "new_category" -> plan_category service ~principal ~owner ~id p
        | "entry" -> plan_entry service ~principal ~owner ~id p
        | _ -> error A.Invalid_request
      in
      match
        Sql.transaction_result service.Service.db (fun () ->
            let* plan = plan () in
            match plan with
            | `Out_of_date ->
                let* _ = S.decide_proposal (store service) ~owner ~id A.Out_of_date in
                Ok `Decided
            | `Do write -> (
                let* claimed = S.decide_proposal (store service) ~owner ~id A.Applied in
                if not claimed then Ok `Decided else match write () with Ok () -> Ok `Applied | Error () -> Error rejected))
      with
      | Ok `Applied -> Ok A.Applied
      | Ok `Decided -> S.proposal (store service) ~owner id |> Result.map (fun (p : S.proposal) -> p.state)
      | Error e when e == rejected -> decide service ~owner ~id A.Not_applied
      | Error e -> Error e)

let dismiss service ~principal ~id = with_proposal service ~principal ~id (fun owner _ -> decide service ~owner ~id A.Declined)

(* Taking an applied proposal back, only if what it did is still exactly as it left it: a budget the user has since
   changed, or a row they have since categorised again, is theirs now and is not touched. Claiming the undo and
   reversing the change are one transaction. A change that no longer matches is [Version_conflict] and nothing moves. *)
let undo_budget service ~principal ~owner (p : S.proposal) =
  let* _, ctx = scoped_context service ~principal ~owner p in
  match
    ( Option.bind (string_field p.arguments "category_id") Account_id.of_string,
      Option.bind (string_field p.arguments "monthly_amount") Int64.of_string_opt,
      field p.arguments "rollover",
      string_field p.precondition "from_cycle" )
  with
  | Some category_id, Some applied, Some (`Bool rollover), Some from_cycle -> (
      let previous = Option.bind (string_field p.precondition "current_monthly") Int64.of_string_opt in
      let previous_rollover = match field p.precondition "current_rollover" with Some (`Bool b) -> b | _ -> false in
      let current = Insights.current_budget ctx category_id in
      let unchanged =
        String.equal (Insights.current_cycle_label ctx) from_cycle
        && Option.equal Int64.equal (Option.map (fun (b : Analytics.budget) -> b.monthly_amount) current) (Some applied)
        && Option.equal Bool.equal (Option.map (fun (b : Analytics.budget) -> b.rollover) current) (Some rollover)
      in
      if not unchanged then Ok `Changed
      else
        (* No budget before means a budget of nothing now. *)
        match
          Insights.set_budget ctx ~category_id ~monthly_amount:(Option.value previous ~default:0L) ~rollover:previous_rollover
            ~effective_from:(Some from_cycle)
        with
        | Ok _ -> Ok `Undone
        | Error (`Not_found _) -> Ok `Changed
        | Error (`Invalid _) -> Ok `Changed
        | Error (`Forbidden _) -> error A.Access_denied
        | Error (`Storage_unavailable _) -> error A.Storage_unavailable)
  | _ -> error A.Invalid_request

let undo_categorize service ~principal ~owner (p : S.proposal) =
  let* access, _ = scoped_context service ~principal ~owner p in
  let list_of json name = match field json name with Some (`List xs) -> xs | _ -> [] in
  let rows = list_of p.arguments "rows" and recorded = list_of p.precondition "rows" in
  if rows = [] || List.length rows <> List.length recorded then error A.Invalid_request
  else
    (* Latest first, as the categorisations were made. *)
    List.fold_left
      (fun acc (row, before) ->
        match acc with
        | Ok `Undone -> (
            let revision = match field before "revision" with Some (`Int n) -> Some n | _ -> None in
            match
              ( Option.bind (string_field row "transfer_id") Transfer_id.of_string, Option.bind (string_field row "category_id") Account_id.of_string,
                Option.bind (string_field before "current") Account_id.of_string, revision )
            with
            | Some transfer_id, Some category_id, Some previous, Some revision -> (
                match Inbox.reopen service access ~transfer_id ~revision:(revision + 1) ~category_id ~previous with
                | Ok () -> Ok `Undone
                | Error (`Invalid _ | `Not_found _) -> Ok `Changed
                | Error (`Forbidden _) -> error A.Access_denied
                | Error (`Storage_unavailable _) -> error A.Storage_unavailable)
            | _ -> error A.Invalid_request)
        | other -> other)
      (Ok `Undone) (List.rev (List.combine rows recorded))

(* The simple kinds undo by removing what was made, if the user has not changed it since. *)
let undo_rule service ~principal ~owner ~id (p : S.proposal) =
  let* access, _ = scoped_context service ~principal ~owner p in
  match Inbox.remove_rule service access ~rule_id:id with
  | Ok () -> Ok `Undone
  | Error (`Not_found _) -> Ok `Changed
  | Error (`Forbidden _) -> error A.Access_denied
  | Error _ -> Ok `Changed

let undo_goal service ~principal ~owner ~id (p : S.proposal) =
  let* _, ctx = scoped_context service ~principal ~owner p in
  match Planning_store.find_goal service.Service.db id with
  | Some goal when Access.can_use ctx.access goal.goal_owner ->
      let minor key = Option.bind (string_field p.arguments key) Int64.of_string_opt in
      let target_date = Option.bind (string_field p.arguments "target_date") (fun d -> Result.to_option (Date.of_string d)) in
      let unchanged =
        Some goal.name = string_field p.arguments "name" && Some goal.target_amount = minor "target_amount"
        && Option.equal Date.equal goal.target_date target_date && Option.equal Int64.equal goal.monthly_amount (minor "monthly_amount")
        && goal.earmarks = []
      in
      if unchanged then (
        Planning_store.archive_goal service.Service.db id;
        Ok `Undone)
      else Ok `Changed
  | _ -> Ok `Changed

let undo_category service ~principal ~owner ~id (p : S.proposal) =
  let* access, ctx = scoped_context service ~principal ~owner p in
  let view = Store_sqlite.view service.Service.db in
  match view.account_by_key (Insights.budget_owner ctx) (category_key id) with
  | Some account ->
      (* In use: money moved through it, a budget, a rule or an inbox suggestion points at it. *)
      let used =
        Sql.(run service.db
               (stmt
                  "SELECT (SELECT count(*) FROM transfers WHERE debit_account_id = ?1 OR credit_account_id = ?1) \
                   + (SELECT count(*) FROM budget_versions WHERE category_account_id = ?1) \
                   + (SELECT count(*) FROM categorisation_rules WHERE category_account_id = ?1) \
                   + (SELECT count(*) FROM inbox_items WHERE suggested_account_id = ?1 OR current_account_id = ?1)"
                  (Codec.account_id @-> nil) (one (row (int @-> nil) Fun.id))))
          account.id
        > 0
      in
      if used || (not (Some account.name = string_field p.arguments "name")) || account.closed then Ok `Changed
      else (match Service.close_account service access account.id with Ok _ -> Ok `Undone | Error _ -> Ok `Changed)
  | None -> Ok `Changed

(* An entry is taken back as it would be by hand: voided if it is still waiting for its statement, reversed if posted,
   and left alone once a statement has confirmed it or it has been reversed or corrected. *)
let undo_entry service ~principal ~owner ~id (p : S.proposal) =
  let* access, ctx = scoped_context service ~principal ~owner p in
  let view = Store_sqlite.view service.Service.db in
  match view.transfer_by_key (Insights.budget_owner ctx) (category_key id) with
  | None -> Ok `Changed
  | Some t when Option.is_some (view.reversal t.id) || Option.is_some (view.resolution t.id) -> Ok `Changed
  | Some t -> (
      let key = Result.get_ok (Idempotency_key.of_string ("undo-proposal:" ^ id)) in
      match t.kind with
      | Transfer.Pending _ -> (
          match Service.void_pending service access ~key ~pending_id:t.id ~value_date:ctx.today with Ok (Ok _) -> Ok `Undone | _ -> Ok `Changed)
      | Transfer.Single -> (
          match Service.reverse service access ~key ~transfer_id:t.id () with Ok (Ok _) -> Ok `Undone | _ -> Ok `Changed)
      | _ -> Ok `Changed)

let undo service ~principal ~id =
  let* user = Sql.read_snapshot service.Service.db (fun () -> Advisor_access.user service principal) in
  let owner = user.id in
  let* p = S.proposal (store service) ~owner id in
  match p.state with
  | A.Reverted -> Ok A.Reverted
  | A.Applied -> (
      let conflict = { A.code = A.Version_conflict; context = A.No_context } in
      match
        Sql.transaction_result service.Service.db (fun () ->
            let* claimed = S.undo_proposal (store service) ~owner ~id in
            if not claimed then Ok ()
            else
              let* outcome =
                match p.kind with
                | "set_budget" -> undo_budget service ~principal ~owner p
                | "categorize" -> undo_categorize service ~principal ~owner p
                | "add_rule" -> undo_rule service ~principal ~owner ~id p
                | "save_goal" -> undo_goal service ~principal ~owner ~id p
                | "new_category" -> undo_category service ~principal ~owner ~id p
                | "entry" -> undo_entry service ~principal ~owner ~id p
                | _ -> error A.Invalid_request
              in
              match outcome with `Undone -> Ok () | `Changed -> Error conflict)
      with
      | Ok () -> Ok A.Reverted
      | Error e -> Error e)
  | _ -> error A.Invalid_request
