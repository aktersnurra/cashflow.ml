open Cashflow_core
open Cashflow_io
open Advisor_test_support
module X = Advisor_access
module T = Test_service_support

let denied = function Error _ -> true | Ok _ -> false
let check label b = Alcotest.(check bool) label true b
let code expected = function Error e -> check "error code" (e.A.code = expected) | Ok _ -> Alcotest.fail "expected denial"

let fixture f =
  let clock = ref now in
  let db = Store_sqlite.open_memory () in
  let service = Service.make ~now:(fun () -> !clock) db in
  let store = Advisor_store.create ~clock:(fun () -> !clock) db in
  Fun.protect ~finally:(fun () -> Sql.close db) (fun () -> f service store clock)

let login ?(admin = false) service name =
  let access = T.sign_in service ~admin name in
  let token = Web_auth.create_session service.Service.db ~now:(service.now ()) (Access.user access) in
  let principal = ok (X.authenticate service ~now:(service.now ()) ~token) in
  (access, token, principal)

let start service store principal (request : A.submit) =
  ok (X.authorize_scope service principal request.A.scope);
  ok
    (Advisor_store.accept store ~owner:(ok (X.user service principal)).User.id
       ~session:(ok (X.session service principal))
       ~now:(service.Service.now ()) ~model:"synthetic" ~source_finding:None request)

let state store owner run = (ok (Advisor_store.find_run store ~owner run)).A.state
let advance clock seconds = clock := Option.get (Ptime.add_span !clock (Ptime.Span.of_int_s seconds))

let origin_session_and_scope_permissions () =
  fixture @@ fun service store clock ->
  let alice, ta, a = login service "alice" in
  let _, tb, b = login service "alice" in
  let bob, _, bp = login service "bob" in
  let owner = (Access.user alice).id in
  code A.Consent_required (X.authorize_scope service a A.Personal);
  ignore (ok (X.set_user_consent service a ~enabled:true));
  let accepted = start service store a (submit "origin") in
  let run = accepted.ids.run_id and conversation = accepted.ids.conversation_id in
  ignore (ok (Advisor_store.transition store ~run A.Start));
  ignore
    (ok (Advisor_store.append store ~run ~expected_sequence:1L [ A.Text_appended { segment_id = segment 1; text = "synthetic partial" } ]));
  check "B subscribes to A" (Result.is_ok (X.authorize_run service ~subscriber:b ~run));
  code A.Not_found (Advisor_store.find_run store ~owner:(Access.user bob).id run);
  let unknown = Option.get (A.Run_id.of_string "00000000-0000-4000-8000-000000999999") in
  code A.Not_found (Advisor_store.find_run store ~owner unknown);
  check "Bob cannot read private household/personal history" (denied (X.authorize_history service bp conversation));
  let expiry = ok (X.expiry service a) in
  advance clock 60;
  ignore (ok (X.authorize_history service a conversation));
  ignore (ok (X.authorize_run service ~subscriber:b ~run));
  check "read does not touch expiry" (Ptime.equal expiry (ok (X.expiry service a)));
  check "B logout returns no A run" (X.revoke_session service ~now:!clock (Web_auth.session_ref tb) = []);
  check "stale B rejected" (denied (X.authorize_run service ~subscriber:b ~run));
  check "B logout leaves A run valid" (Result.is_ok (X.authorize_run service ~subscriber:a ~run));
  let _, _, b2 = login service "alice" in
  check "A logout returns origin ID" (X.revoke_session service ~now:!clock (Web_auth.session_ref ta) = [ run ]);
  check "A logout cancels despite B" (state store owner run = A.Cancel_requested);
  check "stale A rejected" (denied (X.authorize_scope service a A.Personal));
  check "B cannot continue A generation" (denied (X.authorize_run service ~subscriber:b2 ~run));
  check "pending cancellation history remains readable" (Result.is_ok (X.authorize_history service b2 conversation));
  check "repeated revoke returns pending ID" (X.revoke_session service ~now:!clock (Web_auth.session_ref ta) = [ run ]);
  ignore (ok (Advisor_store.transition store ~run A.Confirm_cancel));
  check "terminal replay authorized after origin logout" (Result.is_ok (X.authorize_run service ~subscriber:b2 ~run));
  check "owner-scoped lookup permits terminal" (A.is_terminal (state store owner run));
  check "B can read saved partial after A logout" (Result.is_ok (X.authorize_history service b2 conversation));
  let snapshot = ok (Advisor_store.snapshot store ~owner conversation) in
  let identity = Option.get (Web_auth.inspect_session service ~now:!clock (ok (X.session service b2))) in
  let contains text secret =
    let n = String.length secret in
    let rec scan i = i + n <= String.length text && (String.sub text i n = secret || scan (i + 1)) in
    scan 0
  in
  let module W = Cashflow_advisor_protocol.Wire in
  let public =
    W.encode_snapshot (Advisor_codec.snapshot snapshot)
    ^ W.encode_accepted (Advisor_codec.accepted accepted)
    ^
    match ok (Advisor_store.replay store ~run ~after:0L ~limit:100) with
    | Events es -> String.concat "" (List.map (fun e -> W.encode_event (Advisor_codec.event e)) es)
    | Resync_required _ -> Alcotest.fail "unexpected replay pruning"
  in
  List.iter
    (fun secret -> check "public projections exclude authentication material" (not (contains public secret)))
    [ ta; tb; Web_auth.hash ta; Web_auth.hash tb; identity.csrf_token; A.Session_ref.to_hash identity.reference ];
  check "partial retained"
    (match (List.hd (List.hd snapshot.turns).attempts).segments with [ A.Text_segment d ] -> d.text = "synthetic partial" | _ -> false);
  let retry =
    ok
      (Advisor_store.retry store ~owner
         ~session:(ok (X.session service b2))
         ~now:!clock ~model:"synthetic"
         { A.turn_id = accepted.ids.turn_id; expected_version = snapshot.version; idempotency_key = T.key "retry" })
  in
  check "explicit retry binds B"
    ((ok (Advisor_store.find_run store ~owner retry.ids.run_id)).originating_session = ok (X.session service b2))

let expiry_and_refresh () =
  fixture @@ fun service store clock ->
  let alice, ta, a = login service "expiry" in
  ignore (ok (X.set_user_consent service a ~enabled:true));
  let accepted = start service store a (submit "expiry") in
  let initial_expiry = ok (X.expiry service a) in
  advance clock 120;
  ignore (Web_auth.session service ~now:!clock ta);
  check "ordinary session still extends expiry" (Ptime.compare (ok (X.expiry service a)) initial_expiry > 0);
  clock := initial_expiry;
  check "gate rereads extended expiry" (Result.is_ok (X.authorize_run service ~subscriber:a ~run:accepted.ids.run_id));
  let _, _, b = login service "expiry" in
  clock := ok (X.expiry service a);
  check "origin expired even with B" (denied (X.authorize_run service ~subscriber:b ~run:accepted.ids.run_id));
  check "expired principal rejected" (denied (X.authorize_scope service a A.Personal));
  check "B history survives expiry" (Result.is_ok (X.authorize_history service b accepted.ids.conversation_id));
  check "expiry cancellation IDs" (X.revoke_session service ~now:!clock (Web_auth.session_ref ta) = [ accepted.ids.run_id ]);
  check "expiry cancellation committed" (state store (Access.user alice).id accepted.ids.run_id = A.Cancel_requested);
  ignore (ok (Advisor_store.transition store ~run:accepted.ids.run_id A.Confirm_cancel));
  check "terminal replay survives origin expiry" (Result.is_ok (X.authorize_run service ~subscriber:b ~run:accepted.ids.run_id))

let subscriber_expiry () =
  fixture @@ fun service store clock ->
  let alice, ta, a = login service "subscriber-expiry" in
  let _, _, b = login service "subscriber-expiry" in
  ignore (ok (X.set_user_consent service a ~enabled:true));
  let accepted = start service store a (submit "subscriber-expiry") in
  let b_expiry = ok (X.expiry service b) in
  advance clock 120;
  ignore (Web_auth.session service ~now:!clock ta);
  clock := b_expiry;
  check "expired subscriber denied" (denied (X.authorize_run service ~subscriber:b ~run:accepted.ids.run_id));
  check "subscriber expiry does not cancel origin" (Result.is_ok (X.authorize_run service ~subscriber:a ~run:accepted.ids.run_id));
  check "origin still queued" (state store (Access.user alice).id accepted.ids.run_id = A.Queued)

let household_and_consent () =
  fixture @@ fun service store _ ->
  let admin = T.sign_in service ~admin:true "admin" in
  let h = T.ok (Service.create_household service admin ~name:"synthetic" ~default_currency:"SEK") in
  let _, _, ap = login ~admin:true service "admin" in
  let alice, _, a = login service "member" in
  ignore
    (T.ok
       (Service.invite_member service
          (Service.access_for service (Access.user (T.sign_in service ~admin:true "admin")))
          ~email:"member@example.com"));
  ignore (T.ok (Service.respond_to_invitation service alice ~household_id:h.id ~accept:true));
  let other_admin = T.sign_in service ~admin:true "other-admin" in
  let other = T.ok (Service.create_household service other_admin ~name:"unrelated" ~default_currency:"SEK") in
  ignore (ok (X.set_user_consent service a ~enabled:true));
  code A.Consent_required (X.authorize_scope service a (A.Household h.id));
  check "member cannot enable household" (denied (X.set_household_consent service a h.id ~enabled:true));
  check "admin cannot enable unrelated household" (denied (X.set_household_consent service ap other.id ~enabled:true));
  ignore (ok (X.set_household_consent service ap h.id ~enabled:true));
  check "unrelated scope denied" (denied (X.authorize_scope service a (A.Household other.id)));
  let request = { (submit "household") with scope = A.Household h.id } in
  let accepted = start service store a request in
  check "admin cannot read member private conversation" (denied (X.authorize_history service ap accepted.ids.conversation_id));
  let owner = (Access.user alice).id in
  ignore (ok (X.set_household_consent service ap h.id ~enabled:false));
  check "household optout cancels" (state store owner accepted.ids.run_id = A.Cancel_requested);
  check "stale consent denied" (denied (X.authorize_history service a accepted.ids.conversation_id));
  ignore (ok (Advisor_store.transition store ~run:accepted.ids.run_id A.Confirm_cancel));
  ignore (ok (X.set_household_consent service ap h.id ~enabled:true));
  let household_run = start service store a { request with idempotency_key = T.key "leave"; conversation_id = None } in
  (* Another owner can have a personal run while the member's household run is active. *)
  ignore (ok (X.set_user_consent service ap ~enabled:true));
  let personal_run = start service store ap (submit "personal-unaffected") in
  let ids = ok (X.leave_household service a) in
  check "leave cancels matching household" (ids = [ household_run.ids.run_id ]);
  check "other owner personal untouched" (state store (ok (X.user service ap)).id personal_run.ids.run_id = A.Queued);
  check "leave failure" (denied (X.leave_household service a));
  check "leave failure preserves runs" (state store (ok (X.user service ap)).id personal_run.ids.run_id = A.Queued);
  check "scope stays pinned" ((ok (Advisor_store.metadata store ~owner household_run.ids.conversation_id)).scope = A.Household h.id);
  check "history inaccessible after leave" (denied (X.authorize_history service a household_run.ids.conversation_id));
  check "other owner cannot delete" (denied (X.delete_conversation service ap household_run.ids.conversation_id));
  ignore (ok (X.delete_conversation service a household_run.ids.conversation_id));
  code A.Not_found (Advisor_store.metadata store ~owner household_run.ids.conversation_id);
  let personal = start service store a (submit "personal-after-leave") in
  check "leave failure leaves personal" (denied (X.leave_household service a));
  check "personal unaffected" (state store owner personal.ids.run_id = A.Queued);
  check "user optout affects only own unfinished" (ok (X.set_user_consent service a ~enabled:false) = [ personal.ids.run_id ]);
  check "stale user consent rejected" (denied (X.authorize_scope service a A.Personal));
  ignore (ok (X.delete_conversation service a personal.ids.conversation_id));
  (* External membership changes are also observed without new authentication. *)
  Directory.set_membership service.db ~now:(service.now ()) ~household_id:h.id ~user_id:owner Active;
  ignore (ok (X.set_user_consent service a ~enabled:true));
  check "rejoin observed" (Result.is_ok (X.authorize_scope service a (A.Household h.id)));
  Directory.set_membership service.db ~now:(service.now ()) ~household_id:h.id ~user_id:owner Left;
  check "external loss observed" (denied (X.authorize_scope service a (A.Household h.id)));
  ignore (T.sign_in service ~admin:false "admin");
  check "stale admin cannot toggle" (denied (X.set_household_consent service ap h.id ~enabled:false))

let atomic_mutations () =
  fixture @@ fun service store clock ->
  let alice, token, a = login service "atomic" in
  let owner = (Access.user alice).id in
  ignore (ok (X.set_user_consent service a ~enabled:true));
  let accepted = start service store a (submit "atomic") in
  let run = accepted.ids.run_id in
  Sql.transaction service.db (fun () ->
      code A.Storage_unavailable (X.set_user_consent service a ~enabled:false);
      check "nested session revoke rejected"
        (try
           ignore (X.revoke_session service ~now:!clock (Web_auth.session_ref token));
           false
         with Sql.Sql_error _ -> true));
  check "nested wrappers leave consent and session unchanged" (Result.is_ok (X.authorize_scope service a A.Personal));
  check "nested wrappers leave run unchanged" (state store owner run = A.Queued);
  Sql.exec_script service.db
    "CREATE TABLE commit_parent(id INTEGER PRIMARY KEY); CREATE TABLE commit_child(id INTEGER REFERENCES commit_parent(id) DEFERRABLE \
     INITIALLY DEFERRED); CREATE TRIGGER deny_consent_commit AFTER UPDATE ON advisor_user_consent BEGIN INSERT INTO commit_child \
     VALUES(42); END";
  code A.Storage_unavailable (X.set_user_consent service a ~enabled:false);
  check "failed outer commit restores consent" (ok (Advisor_store.consent store (User owner)));
  check "failed outer commit restores run" (state store owner run = A.Queued);
  Sql.exec_script service.db "DROP TRIGGER deny_consent_commit";
  Sql.exec_script service.db
    "CREATE TRIGGER deny_cancel BEFORE UPDATE OF state ON advisor_runs BEGIN SELECT RAISE(ABORT, 'synthetic cancellation failure'); END";
  code A.Storage_unavailable (X.set_user_consent service a ~enabled:false);
  check "failed cancel rolls consent back" (ok (Advisor_store.consent store (User owner)));
  check "failed cancel leaves run unchanged" (state store owner run = A.Queued);
  check "failed revocation raises without returning IDs"
    (try
       ignore (X.revoke_session service ~now:!clock (Web_auth.session_ref token));
       false
     with Sql.Sql_error _ -> true);
  check "failed revocation rolls session deletion back" (Result.is_ok (X.authorize_scope service a A.Personal));
  Sql.exec_script service.db "DROP TRIGGER deny_cancel";
  ignore (ok (X.set_user_consent service a ~enabled:false));
  check "IDs returned outside transaction" (not (Sql.in_transaction service.db));
  check "pending cancel is idempotent" (ok (X.set_user_consent service a ~enabled:false) = [ run ]);
  ignore (ok (X.set_user_consent service a ~enabled:true));
  check "delete returns pending run for signaling" (ok (X.delete_conversation service a accepted.ids.conversation_id) = [ run ]);
  code A.Not_found (Advisor_store.find_run store ~owner run);
  check "late worker writes cannot resurrect deleted run" (denied (Advisor_store.transition store ~run A.Start));
  let admin = T.sign_in service ~admin:true "leave-atomic" in
  let h = T.ok (Service.create_household service admin ~name:"synthetic" ~default_currency:"SEK") in
  ignore (T.ok (Service.invite_member service (Service.access_for service (Access.user admin)) ~email:"atomic@example.com"));
  ignore (T.ok (Service.respond_to_invitation service alice ~household_id:h.id ~accept:true));
  ignore (ok (Advisor_store.set_consent store (Household h.id) ~enabled:true));
  let accepted = start service store a { (submit "leave-atomic") with scope = A.Household h.id } in
  Sql.exec_script service.db
    "CREATE TRIGGER deny_cancel BEFORE UPDATE OF state ON advisor_runs BEGIN SELECT RAISE(ABORT, 'synthetic cancellation failure'); END";
  code A.Storage_unavailable (X.leave_household service a);
  check "leave cancellation failure rolls membership back" (Result.is_ok (X.authorize_scope service a (A.Household h.id)));
  check "leave cancellation failure rolls run back" (state store owner accepted.ids.run_id = A.Queued);
  Sql.exec_script service.db "DROP TRIGGER deny_cancel";
  ignore (ok (X.leave_household service a));
  ignore (ok (X.delete_conversation service a accepted.ids.conversation_id));
  Directory.set_membership service.db ~now:!clock ~household_id:h.id ~user_id:owner Active;
  let personal = start service store a (submit "personal-leave") in
  check "leave returns no Personal run" (ok (X.leave_household service a) = []);
  check "same-owner Personal run preserved" (state store owner personal.ids.run_id = A.Queued)

let () =
  Alcotest.run "advisor access"
    [
      ( "authorization",
        [
          Alcotest.test_case "origin_session_and_scope_permissions" `Quick origin_session_and_scope_permissions;
          Alcotest.test_case "expiry and ordinary refresh" `Quick expiry_and_refresh;
          Alcotest.test_case "non-origin subscriber expiry" `Quick subscriber_expiry;
          Alcotest.test_case "household, consent, ownership-only deletion" `Quick household_and_consent;
          Alcotest.test_case "atomic mutation rollback and idempotence" `Quick atomic_mutations;
        ] );
    ]
