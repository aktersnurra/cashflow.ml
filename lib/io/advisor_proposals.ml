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
