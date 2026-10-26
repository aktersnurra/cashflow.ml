open Cashflow_core
module A = Advisor

(* Only identity is retained. Current authority is loaded under each gate's snapshot. *)
type principal = { owner : User_id.t; reference : A.Session_ref.t }

let ( let* ) = Result.bind
let error code = { A.code; context = A.No_context }
let denied = Error (error A.Access_denied)
let store (service : Service.t) = Advisor_store.create ~clock:service.now service.db
let read (service : Service.t) f = try Sql.read_snapshot service.db f with Sql.Sql_error _ -> Error (error A.Storage_unavailable)
let tx (service : Service.t) f = try Sql.transaction_outer_result service.db f with Sql.Sql_error _ -> Error (error A.Storage_unavailable)

let current_at (service : Service.t) ~now p =
  match Web_auth.inspect_session service ~now p.reference with
  | Some identity when User_id.equal identity.user_id p.owner -> (
      match Directory.find_user service.db p.owner with Some user -> Ok (identity, Service.access_for service user) | None -> denied)
  | _ -> denied

let current (service : Service.t) p = current_at service ~now:(service.now ()) p

let authenticate service ~now ~token =
  read service (fun () ->
      let reference = Web_auth.session_ref token in
      match Web_auth.inspect_session service ~now reference with
      | None -> denied
      | Some identity ->
          let p = { owner = identity.user_id; reference } in
          let* _ = current_at service ~now p in
          Ok p)

let access service p =
  read service (fun () ->
      let* _, a = current service p in
      Ok a)

let user service p = Result.map Access.user (access service p)

let expiry service p =
  read service (fun () ->
      let* i, _ = current service p in
      Ok i.Web_auth.expires_at)

let session service p =
  read service (fun () ->
      let* i, _ = current service p in
      Ok i.Web_auth.reference)

let scope store access scope =
  let* enabled = Advisor_store.consent store (User (Access.user access).id) in
  if not enabled then Error (error A.Consent_required)
  else
    match scope with
    | A.Personal -> Ok ()
    | A.Household id ->
        if not (Access.can_use access (Owner.Household id)) then denied
        else
          let* enabled = Advisor_store.consent store (Household id) in
          if enabled then Ok () else Error (error A.Consent_required)

let authorize_scope service p requested =
  read service (fun () ->
      let* _, a = current service p in
      scope (store service) a requested)

let history store a conversation =
  let* metadata = Advisor_store.metadata store ~owner:(Access.user a).id conversation in
  scope store a metadata.scope

let authorize_history service p conversation =
  read service (fun () ->
      let* _, a = current service p in
      history (store service) a conversation)

let authorize_run service ~subscriber ~run =
  read service (fun () ->
      let* _, a = current service subscriber in
      let store = store service in
      let* run = Advisor_store.find_run store ~owner:(Access.user a).id run in
      let* () = history store a run.ids.conversation_id in
      if A.is_terminal run.state then Ok run
      else
        let origin = { owner = run.owner_id; reference = run.originating_session } in
        let* _ = current service origin in
        Ok run)

let cancel store runs =
  let rec loop ids = function
    | [] -> Ok (List.rev ids)
    | (run : A.run) :: rest ->
        let* () =
          if run.state = A.Cancel_requested then Ok ()
          else Result.map (fun _ -> ()) (Advisor_store.transition store ~run:run.ids.run_id A.Request_cancel)
        in
        loop (run.ids.run_id :: ids) rest
  in
  loop [] runs

let runs_in_scope store runs requested =
  let rec loop matching = function
    | [] -> Ok (List.rev matching)
    | (run : A.run) :: rest ->
        let* metadata = Advisor_store.metadata store ~owner:run.owner_id run.ids.conversation_id in
        loop (if metadata.scope = requested then run :: matching else matching) rest
  in
  loop [] runs

let revoke_session service ~now reference =
  (* Explicit revocation invalidates even an otherwise unexpired session. The caller of an
     expiry timer re-inspects its actual expiry before deciding to invoke this operation. *)
  let timed = { service with Service.now = (fun () -> now) } in
  match
    tx timed (fun () ->
        let store = store timed in
        let* runs = Advisor_store.active_runs store in
        Sql.run service.Service.db Web_auth.delete_session (A.Session_ref.to_hash reference);
        cancel store (List.filter (fun (r : A.run) -> r.originating_session = reference) runs))
  with
  | Ok ids -> ids
  | Error _ -> raise (Sql.Sql_error "advisor session revocation failed")

let set_user_consent service p ~enabled =
  tx service (fun () ->
      let* _, a = current service p in
      let store = store service in
      let owner = (Access.user a).id in
      let* () = Advisor_store.set_consent store (User owner) ~enabled in
      if enabled then Ok []
      else
        let* runs = Advisor_store.active_runs_for_owner store ~owner in
        cancel store runs)

let set_household_consent service p id ~enabled =
  tx service (fun () ->
      let* _, a = current service p in
      if not (Access.is_admin a && Access.can_use a (Owner.Household id)) then denied
      else
        let store = store service in
        let* () = Advisor_store.set_consent store (Household id) ~enabled in
        if enabled then Ok []
        else
          let* runs = Advisor_store.active_runs store in
          let* runs = runs_in_scope store runs (A.Household id) in
          cancel store runs)

let leave_household service p =
  tx service (fun () ->
      let* _, a = current service p in
      match Access.household a with
      | None -> denied
      | Some household ->
          let store = store service in
          let* runs = Advisor_store.active_runs_for_owner store ~owner:(Access.user a).id in
          let* runs = runs_in_scope store runs (A.Household household.id) in
          let* _ = Result.map_error (fun _ -> error A.Access_denied) (Service.leave_household service a) in
          cancel store runs)

let delete_conversation service p conversation =
  tx service (fun () ->
      let* _, a = current service p in
      Advisor_store.delete (store service) ~owner:(Access.user a).id ~now:(service.Service.now ()) conversation)
