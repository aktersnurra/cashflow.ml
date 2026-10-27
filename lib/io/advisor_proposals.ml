(* Applying and dismissing the changes the advisor proposed. The click is the only way a proposal does anything:
   it runs here, with the clicking user's own access and the same code as the manual command, after checking
   the proposal is still undecided, unexpired and based on what is true now. *)
open Cashflow_core
module A = Advisor
module S = Advisor_store

let ( let* ) = Result.bind
let error code = Error { A.code; context = A.No_context }
let store (service : Service.t) = S.create ~clock:service.now service.db

(* What the proposal assumed still holds: the category is still an expense category in the conversation's scope,
   it is still the pay cycle the card named, and the budget it would replace (amount and carry-over) is the one the
   user saw. [`Out_of_date] is a stale proposal, not a failure. *)
let ready service ~principal ~owner (p : S.proposal) =
  let* meta = S.metadata (store service) ~owner p.conversation_id in
  let* () = Advisor_access.authorize_scope service principal meta.scope in
  let* access = Advisor_access.access service principal in
  let query_scope = match meta.scope with A.Personal -> Analytics.Me | Household _ -> Analytics.Household in
  match Insights.context service access ~scope:query_scope ~currency:(Currency.code meta.currency) () with
  | Error _ -> error A.Access_denied
  | Ok ctx -> (
      let field fields name = match fields with `Assoc fs -> List.assoc_opt name fs | _ -> None in
      let arg = field p.arguments and pre = field p.precondition in
      match
        ( Option.bind (match arg "category_id" with Some (`String s) -> Some s | _ -> None) Account_id.of_string,
          (match arg "monthly_amount" with Some (`String s) -> Int64.of_string_opt s | _ -> None),
          arg "rollover",
          pre "from_cycle" )
      with
      | Some category_id, Some monthly_amount, Some (`Bool rollover), Some (`String from_cycle) -> (
          let recorded_amount = match pre "current_monthly" with Some (`String s) -> Int64.of_string_opt s | _ -> None in
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
      | _ -> error A.Invalid_request)

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

let apply service ~principal ~id =
  with_proposal service ~principal ~id (fun owner p ->
      match Sql.read_snapshot service.Service.db (fun () -> ready service ~principal ~owner p) with
      | Error e -> Error e
      | Ok `Out_of_date -> decide service ~owner ~id A.Out_of_date
      | Ok (`Ready (ctx, category_id, monthly_amount, rollover)) -> (
          (* Claiming the proposal and writing the budget commit together or not at all, so a click, a second
             tab or a retry can never write without the proposal being recorded as applied. *)
          let rejected = { A.code = A.Invalid_request; context = A.No_context } in
          match
            Sql.transaction_result service.Service.db (fun () ->
                let* claimed = S.decide_proposal (store service) ~owner ~id A.Applied in
                if not claimed then Ok `Already
                else
                  match Insights.set_budget ctx ~category_id ~monthly_amount ~rollover ~effective_from:None with
                  | Ok _ -> Ok `Applied
                  | Error _ -> Error rejected)
          with
          | Ok `Applied -> Ok A.Applied
          | Ok `Already -> decide service ~owner ~id A.Applied
          | Error e when e == rejected -> decide service ~owner ~id A.Not_applied
          | Error e -> Error e))

let dismiss service ~principal ~id = with_proposal service ~principal ~id (fun owner _ -> decide service ~owner ~id A.Declined)
