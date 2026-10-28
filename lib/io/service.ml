(* Use cases, shared by the MCP tools and the web API. Every function takes an Access.t and checks
   what it allows before touching anything. *)
module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Chart = Cashflow_core.Chart
module Currency = Cashflow_core.Currency
module Date = Cashflow_core.Date
module Household = Cashflow_core.Household
module Household_id = Cashflow_core.Household_id
module I18n_locale = Cashflow_core.I18n_locale
module Idempotency_key = Cashflow_core.Idempotency_key
module Ledger_core = Cashflow_core.Ledger_core
module Ledger_error = Cashflow_core.Ledger_error
module Ledger_event = Cashflow_core.Ledger_event
module Owner = Cashflow_core.Owner
module Time = Cashflow_core.Time
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id
module User = Cashflow_core.User
module User_id = Cashflow_core.User_id

let ( let* ) = Result.bind

type error = [ `Forbidden of string | `Not_found of string | `Invalid of string | `Storage_unavailable of Cashflow_workflow.Reporting_error.operation ]

let error_to_string : [< error ] -> string = function
  | `Forbidden reason -> "forbidden: " ^ reason
  | `Not_found what -> what ^ " not found"
  | `Invalid reason -> reason
  | `Storage_unavailable _ -> "internal error"

type t = { db : Sql.db; now : unit -> Ptime.t; assistant : Llm.t option; logo_network : Logo_fetch.network option; schedule : (unit -> unit) -> unit }

let make ?(now = fun () -> Ptime_clock.now ()) ?assistant ?logo_network ?(schedule = fun _ -> ()) db =
  { db; now; assistant; logo_network; schedule }

(* Every LLM call is logged with its purpose and size, never its content, for the settings page. *)
let log_llm_call service (access : Access.t) ~purpose ~items ~ok =
  match service.assistant with
  | None -> ()
  | Some llm ->
      Sql.(run service.db (stmt "INSERT INTO llm_calls (user_id, purpose, item_count, model, ok, created_at) VALUES (?, ?, ?, ?, ?, ?)"
             Codec.(user_id @-> text @-> int @-> text @-> bool @-> time @-> nil) exec))
        (Access.user access).id purpose items llm.model ok (service.now ())

let env service : Ledger_core.env =
  { now = service.now; fresh_account_id = Id_generator.account; fresh_transfer_id = Id_generator.transfer }

let today service (access : Access.t) =
  let zone = Result.value (Time.parse_zone (Access.user access).timezone) ~default:Time.utc in
  Time.local_date zone (service.now ())

(* The ledger store as one user sees it: accounts they may not use don't exist. *)
module Scoped = struct
  type t = { db : Sql.db; access : Access.t }

  let view store = Access.restrict store.access (Store_sqlite.view store.db)
  let transaction store f = Sql.transaction store.db f
  let write store = Store_sqlite.write store.db
  let save_account store = Store_sqlite.save_account store.db
  let accounts store = Store_sqlite.accounts store.db
  let transfers store = Store_sqlite.transfers store.db
  let open_pending store = Store_sqlite.open_pending store.db
end

module L = Ledger.Make (Scoped)

let scoped service access = { Scoped.db = service.db; access }

(* Users and households *)

let access_for service (user : User.t) = Access.make ~user ~household:(Directory.active_household service.db user.id)

let seed_chart service access owner ~currency =
  let scope = match owner with Owner.User _ -> `Personal | Household _ -> `Shared in
  List.iter
    (fun (entry : Chart.entry) ->
      if Chart.applies scope entry then
        let spec : Account.spec = { owner; kind = entry.kind; currency; name = Starter.name ~locale:"en" { entry; currency = None }; flags = Account.no_flags } in
        match L.open_account (scoped service access) (env service) (Chart.key entry) spec with
        | Ok (Created account) -> Account_store.save_details service.db account.id { Account_info.no_details with role = entry.role }
        | Ok (Exists _) -> ()
        | Error error -> invalid_arg (Ledger_error.account_to_string error))
    Chart.entries

let currency service code =
  match Sql.(run service.db (stmt "SELECT scale FROM currencies WHERE code = ?" (text @-> nil) (opt (row (int @-> nil) Fun.id)))) code with
  | Some scale -> Result.map_error (fun (`Invalid_currency reason) -> `Invalid reason) (Currency.make ~code ~scale)
  | None -> Error (`Invalid ("unknown currency " ^ code))

let sign_in service (claims : Directory.claims) =
  Sql.transaction service.db (fun () ->
      let user, status = Directory.resolve_user service.db ~now:(service.now ()) claims in
      let access = access_for service user in
      (if status = `Created then
         match currency service user.default_currency with
         | Ok currency -> seed_chart service access (Access.personal access) ~currency
         | Error _ -> ());
      access)

let update_settings service access (settings : User.settings) =
  let* _ = currency service settings.default_currency in
  let* _ = Result.map_error (fun _ -> `Invalid ("unsupported timezone " ^ settings.timezone)) (Time.parse_zone settings.timezone) in
  if not (List.mem settings.locale I18n_locale.supported) then Error (`Invalid ("unsupported locale " ^ settings.locale))
  else (
    Sql.transaction service.db (fun () -> Directory.save_settings service.db (Access.user access) settings);
    Ok (access_for service (Option.get (Directory.find_user service.db (Access.user access).id))))

let require_owner access = if Access.is_owner access then Ok () else Error (`Forbidden "only the household's owner can do this")

let create_household service access ~name ~default_currency =
  let name = String.trim name in
  if name = "" then Error (`Invalid "household name must not be empty")
  else if Option.is_some (Access.household access) then Error (`Invalid "you already belong to a household")
  else
    let* currency = currency service default_currency in
    Sql.transaction service.db (fun () ->
        let user = Access.user access in
        let household = Directory.create_household service.db ~now:(service.now ()) ~creator:user ~name ~default_currency in
        let access = access_for service user in
        seed_chart service access (Owner.Household household.id) ~currency;
        Ok household)

let own_household access = Option.to_result ~none:(`Invalid "you don't belong to a household") (Access.household access)

let invite_member service access ~email =
  let* household = own_household access in
  let* () = require_owner access in
  match Directory.find_user_by_email service.db email with
  | None -> Error (`Not_found ("a user with email " ^ email ^ " (they need to sign in once first)"))
  | Some invitee ->
      if Option.is_some (Directory.active_household service.db invitee.id) then Error (`Invalid "that user already belongs to a household")
      else (
        Directory.invite service.db ~now:(service.now ()) ~household_id:household.id ~invited_by:(Access.user access)
          ~user_id:invitee.id;
        Ok invitee)

let invitations service access = Directory.invitations service.db (Access.user access).id

let respond_to_invitation service access ~household_id ~accept =
  let user = Access.user access in
  if not (List.exists (fun (h : Household.t) -> Household_id.equal h.id household_id) (invitations service access)) then
    Error (`Not_found "invitation")
  else if accept && Option.is_some (Access.household access) then Error (`Invalid "leave your current household first")
  else (
    Directory.set_membership service.db ~now:(service.now ()) ~household_id ~user_id:user.id (if accept then Active else Declined);
    Ok (access_for service user))

let leave_household service access =
  let* household = own_household access in
  let user = Access.user access in
  Directory.set_membership service.db ~now:(service.now ()) ~household_id:household.id ~user_id:user.id Left;
  Ok (access_for service user)

let members service access =
  let* household = own_household access in
  Ok (Directory.members service.db household.id)

let update_household service access ~name ~report_split_bp ~pay_cycle =
  let* household = own_household access in
  let* () = require_owner access in
  if report_split_bp < 0 || report_split_bp > 10_000 then Error (`Invalid "report split must be between 0 and 10000 basis points")
  else
    let household = { household with name = Option.value name ~default:household.name; report_split_bp; pay_cycle } in
    Directory.save_household service.db household;
    Ok household

(* Accounts *)

let owner_name service owner =
  match owner with
  | Owner.User id -> Option.fold ~none:"someone" ~some:(fun (u : User.t) -> u.display_name) (Directory.find_user service.db id)
  | Household id -> Option.fold ~none:"the household" ~some:(fun (h : Household.t) -> h.name) (Directory.find_household service.db id)

let accounts service access = List.concat_map (Account_store.for_owner service.db) (Access.owners access)

let find_account service access id =
  match (Scoped.view (scoped service access)).account id with
  | Some account -> Ok (Account_store.info service.db account)
  | None -> Error (`Not_found "account")

(* Anyone may open accounts of their own. In a household, any member may add categories, but only
   its owner adds the household's bank accounts and liabilities, except the one a member needs for
   sharing a cost: what the household owes them (§4.6). *)
let may_open ?(details = Account_info.no_details) access owner (kind : Account.kind) =
  let owed_to_me = details.role = Some Payable && Option.equal User_id.equal details.member_id (Some (Access.user access).id) in
  if not (Access.can_use access owner) then Error (`Forbidden "you can't open accounts for that owner")
  else
    match owner, kind with
    | Owner.Household _, Liability when owed_to_me -> Ok ()
    | Owner.Household _, (Asset | Liability) when not (Access.is_owner access) ->
        Error (`Forbidden "only the household's owner can add or close its bank accounts")
    | _ -> Ok ()

let open_account service access ~key ~owner ~kind ~name ~currency:code ~flags ~(details : Account_info.details) =
  let* () = may_open ~details access owner kind in
  let* currency = currency service code in
  let* number_last4 = Account_info.validate_last4 details.number_last4 in
  let spec : Account.spec = { owner; kind; currency; name; flags } in
  Sql.transaction service.db (fun () ->
      match L.open_account (scoped service access) (env service) key spec with
      | Ok (Created account) ->
          Account_store.save_details service.db account.id { details with number_last4 };
          Ok (`Created (Account_store.info service.db account))
      | Ok (Exists account) -> Ok (`Exists (Account_store.info service.db account))
      | Error error -> Error (`Invalid (Ledger_error.account_to_string error)))

let update_account service access id ~name ~(details : Account_info.details) =
  let* info = find_account service access id in
  let* () = may_open access info.account.owner info.account.kind in
  let* number_last4 = Account_info.validate_last4 details.number_last4 in
  let name = String.trim (Option.value name ~default:info.account.name) in
  if name = "" then Error (`Invalid "account name must not be empty")
  else
    Sql.transaction service.db (fun () ->
        Store_sqlite.save_account service.db { info.account with name };
        Account_store.save_details service.db id { details with number_last4; role = info.details.role };
        find_account service access id)

let close_account service access id =
  let* info = find_account service access id in
  let* () = may_open access info.account.owner info.account.kind in
  match L.close_account (scoped service access) id with
  | Ok account -> Ok (Account_store.info service.db account)
  | Error error -> Error (`Invalid (Ledger_error.account_to_string error))

let role_account service owner role kind ~currency =
  Option.map (Account_store.info service.db) (Account_store.find_role service.db owner role kind ~currency)

(* Transfers *)

type side = Own of Account_info.t | Hidden of { owner_name : string }

type transfer_view = {
  transfer : Transfer.t;
  debit : side;
  credit : side;
  resolved_by : Transfer.t option;  (** for a pending transfer, the post or void that resolved it *)
  reversed_by : Transfer_id.t option;
}

let side service access id =
  match Store_sqlite.view service.db |> fun view -> view.account id with
  | Some account when Access.can_use access account.owner -> Own (Account_store.info service.db account)
  | Some account -> Hidden { owner_name = owner_name service account.owner }
  | None -> Hidden { owner_name = "unknown" }

let transfer_view service access (transfer : Transfer.t) =
  let view = Store_sqlite.view service.db in
  { transfer; debit = side service access transfer.debit_account_id; credit = side service access transfer.credit_account_id;
    resolved_by = (match transfer.kind with Pending _ -> view.resolution transfer.id | _ -> None);
    reversed_by = Option.map (fun (t : Transfer.t) -> t.id) (view.reversal transfer.id) }

let visible service access (transfer : Transfer.t) =
  match side service access transfer.debit_account_id, side service access transfer.credit_account_id with
  | Hidden _, Hidden _ -> false
  | _ -> true

let get_transfer service access id =
  match (Store_sqlite.view service.db).transfer id with
  | Some transfer when visible service access transfer -> Ok (transfer_view service access transfer)
  | _ -> Error (`Not_found "transfer")

let query_transfers service access filter =
  List.map (transfer_view service access) (Transfer_query.run_filter service.db access filter)

let check_scope access scope = if Access.can_use access scope then Ok () else Error (`Forbidden "you can't write in that scope")

type outcome = (transfer_view Ledger_core.outcome, Ledger_error.transfer) result

let submit service access ~scope events : (outcome list, [> error ]) result =
  let* () = check_scope access scope in
  match L.submit (scoped service access) (env service) scope events with
  | Error error -> Error (`Invalid (Ledger_error.batch_to_string error))
  | Ok results ->
      Ok
        (List.map
           (Result.map (function
             | Ledger_core.Created t -> Ledger_core.Created (transfer_view service access t)
             | Exists t -> Exists (transfer_view service access t)))
           results)

(* A person-friendly transfer: money moves from one account to another. The ledger debits the
   receiving account and credits the sending one. *)
type entry = {
  key : Idempotency_key.t;
  from_account : Account_id.t;
  to_account : Account_id.t;
  amount : Amount.t;
  value_date : Date.t;
  memo : Transfer.memo;
  code : Transfer.code option;
  scope : Owner.t option;
}

let infer_code (from_ : Account_info.t) (to_ : Account_info.t) : Transfer.code =
  match from_.account.kind, to_.account.kind with
  | _, Expense -> Purchase
  | Income, _ -> Income
  | (Asset | Liability), (Asset | Liability) ->
      if Owner.equal from_.account.owner to_.account.owner then Internal else Contribution
  | _ -> Adjustment

(* Writes go in the household's key space when every account involved is the household's, so two
   members entering the same thing deduplicate; otherwise in the writer's own. *)
let default_scope access (accounts : Account_info.t list) =
  match Access.household access with
  | Some household when List.for_all (fun (i : Account_info.t) -> Owner.equal i.account.owner (Household household.id)) accounts ->
      Owner.Household household.id
  | _ -> Access.personal access

let entry_event service access (entry : entry) =
  let* from_ = find_account service access entry.from_account in
  let* to_ = find_account service access entry.to_account in
  (* A manual entry on an account a bank statement confirms waits, pending, for the statement (§6.7). *)
  let phase : Ledger_event.phase =
    if from_.details.statement_backed || to_.details.statement_backed then Pending { expires_on = None } else Posted
  in
  let code = Option.value entry.code ~default:(infer_code from_ to_) in
  let event : Ledger_event.t =
    { key = entry.key; linked = false;
      body =
        Transfer
          { debit_account_id = entry.to_account; credit_account_id = entry.from_account; amount = entry.amount; phase; code;
            memo = entry.memo; value_date = entry.value_date } }
  in
  Ok (event, Option.value entry.scope ~default:(default_scope access [ from_; to_ ]))

let add_entry service access entry =
  let* event, scope = entry_event service access entry in
  let* results = submit service access ~scope [ event ] in
  match results with [ result ] -> Ok result | _ -> assert false

let derived_key key suffix =
  Result.map_error (fun (`Invalid_idempotency_key reason) -> `Invalid reason)
    (Idempotency_key.of_string (Idempotency_key.to_string key ^ suffix))

let reverse service access ~key ~transfer_id ?value_date () =
  let* view = get_transfer service access transfer_id in
  let event : Ledger_event.t = { key; linked = false; body = Reverse { transfer_id; value_date; memo = None } } in
  let* results = submit service access ~scope:view.transfer.scope [ event ] in
  match results with [ result ] -> Ok result | _ -> assert false

type correction = {
  amount : Amount.t option;
  from_account : Account_id.t option;
  to_account : Account_id.t option;
  value_date : Date.t option;
  memo : Transfer.memo option;
}

(* A correction reverses the original and posts the corrected transfer, linked so that both
   happen or neither does. *)
let correct service access ~key ~transfer_id (c : correction) =
  let* view = get_transfer service access transfer_id in
  let original = view.transfer in
  let* reverse_key = derived_key key "#reverse" in
  let* repost_key = derived_key key "#repost" in
  let events : Ledger_event.t list =
    [ { key = reverse_key; linked = true; body = Reverse { transfer_id; value_date = None; memo = None } };
      { key = repost_key; linked = false;
        body =
          Transfer
            { debit_account_id = Option.value c.to_account ~default:original.debit_account_id;
              credit_account_id = Option.value c.from_account ~default:original.credit_account_id;
              amount = Option.value c.amount ~default:original.amount; phase = Posted; code = original.code;
              memo = Option.value c.memo ~default:original.memo;
              value_date = Option.value c.value_date ~default:original.value_date } } ]
  in
  submit service access ~scope:original.scope events

let post_pending service access ~key ~pending_id ?amount ~value_date () =
  let* view = get_transfer service access pending_id in
  let* results =
    submit service access ~scope:view.transfer.scope
      [ { key; linked = false; body = Post_pending { pending_id; amount; value_date } } ]
  in
  match results with [ result ] -> Ok result | _ -> assert false

let void_pending service access ~key ~pending_id ~value_date =
  let* view = get_transfer service access pending_id in
  let* results =
    submit service access ~scope:view.transfer.scope [ { key; linked = false; body = Void_pending { pending_id; value_date } } ]
  in
  match results with [ result ] -> Ok result | _ -> assert false

(* Sets an account's starting balance against the owner's opening-balances account. *)
let set_opening_balance service access ~account_id ~balance ~value_date =
  let* info = find_account service access account_id in
  let account = info.account in
  match role_account service account.owner Opening Equity ~currency:(Currency.code account.currency) with
  | None -> Error (`Not_found "opening balances account")
  | Some opening ->
      if Int64.equal balance 0L then Error (`Invalid "an opening balance must not be zero")
      else
        let positive = Int64.compare balance 0L > 0 in
        let amount = Option.get (Amount.of_int64 (Int64.abs balance)) in
        (* A positive balance on a debit-normal account (an asset) is a debit to it. *)
        let debit_side = positive = Account.debit_normal account.kind in
        let debit, credit = if debit_side then (account.id, opening.account.id) else (opening.account.id, account.id) in
        let* key = derived_key (Result.get_ok (Idempotency_key.of_string "opening:")) (Account_id.to_string account.id) in
        let* results =
          submit service access ~scope:account.owner
            [ { key; linked = false;
                body =
                  Transfer
                    { debit_account_id = debit; credit_account_id = credit; amount; phase = Posted; code = Opening;
                      memo = { Transfer.empty_memo with description = "Opening balance" }; value_date } } ]
        in
        match results with [ result ] -> Ok result | _ -> assert false

let void_expired service access ~today = L.void_expired (scoped service access) (env service) ~today
let verify service access = L.verify (scoped service access)
