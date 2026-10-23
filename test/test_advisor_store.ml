open Cashflow_core
open Cashflow_io
open Advisor_test_support

let accept store owner request =
  ok (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None request)

let attempt_idempotency_and_replay () =
  let db, store, owner, _ = fixture () in
  Fun.protect ~finally:(fun () -> Sql.close db) @@ fun () ->
  let first = accept store owner (submit "first") in
  let repeated = accept store owner (submit "first") in
  Alcotest.(check bool) "same key is same result" true (first.ids = repeated.ids && repeated.idempotent_repeat);
  ignore (ok (Advisor_store.transition store ~run:first.ids.run_id A.Start));
  ignore
    (ok
       (Advisor_store.transition store ~run:first.ids.run_id
          (A.Fail { code = Provider_unavailable; context = No_context })));
  let retry key version =
    ok
      (Advisor_store.retry store ~owner ~session ~now ~model:"synthetic"
         { A.turn_id = first.ids.turn_id; expected_version = version; idempotency_key = Test_service_support.key key })
  in
  let second = retry "retry-1" 1L in
  ignore (ok (Advisor_store.transition store ~run:second.ids.run_id A.Start));
  ignore
    (ok
       (Advisor_store.transition store ~run:second.ids.run_id
          (A.Fail { code = Provider_unavailable; context = No_context })));
  let third = retry "retry-2" 2L in
  let repeated_retry = retry "retry-1" 1L in
  Alcotest.(check bool)
    "retry key resolves before stale version and busy" true
    (repeated_retry.ids = second.ids && repeated_retry.idempotent_repeat);
  Alcotest.(check bool)
    "attempt/run/message identities are fresh" true
    (first.ids.attempt_id <> second.ids.attempt_id
    && second.ids.attempt_id <> third.ids.attempt_id
    && first.ids.run_id <> second.ids.run_id && second.ids.run_id <> third.ids.run_id
    && first.ids.assistant_message_id <> second.ids.assistant_message_id
    && second.ids.assistant_message_id <> third.ids.assistant_message_id
    && first.ids.user_message_id = third.ids.user_message_id);
  let snapshot = ok (Advisor_store.snapshot store ~owner first.ids.conversation_id) in
  Alcotest.(check int) "one user message after two retries" 1 (List.length snapshot.turns);
  Alcotest.(check int) "three distinct attempts" 3 (List.length (List.hd snapshot.turns).attempts)

let with_fixture f =
  let db, store, owner, clock = fixture () in
  Fun.protect ~finally:(fun () -> Sql.close db) (fun () -> f db store owner clock)

let error_code expected = function
  | Error e -> Alcotest.(check bool) "expected error code" true (e.A.code = expected)
  | Ok _ -> Alcotest.fail "expected rejection"

let fail store id =
  ignore (ok (Advisor_store.transition store ~run:id A.Start));
  ignore (ok (Advisor_store.transition store ~run:id (A.Fail { code = Provider_unavailable; context = No_context })))

let advance clock seconds = clock := Option.get (Ptime.add_span !clock (Ptime.Span.of_int_s seconds))
let count db table = Sql.(run db (stmt ("SELECT count(*) FROM " ^ table) nil (one (row (int @-> nil) Fun.id))))

let append store run sequence text =
  Advisor_store.append store ~run ~expected_sequence:sequence [ A.Text_appended { segment_id = segment 1; text } ]

let replay_events store run after =
  match ok (Advisor_store.replay store ~run ~after ~limit:100) with
  | Events es -> es
  | Resync_required _ -> Alcotest.fail "unexpected resync"

let partials_replay_retention () =
  with_fixture @@ fun db store owner clock ->
  let first = accept store owner (submit "partial") in
  let run = first.ids.run_id in
  ignore (ok (Advisor_store.transition store ~run A.Start));
  ignore (ok (append store run 1L "hello "));
  ignore (ok (append store run 2L "world"));
  error_code A.Cursor_gap (append store run 1L "stale");
  error_code A.Invalid_transition (Advisor_store.append store ~run ~expected_sequence:3L [ A.Run_completed ]);
  let before = ok (Advisor_store.snapshot store ~owner first.ids.conversation_id) in
  let attempt = List.hd (List.hd before.turns).attempts in
  Alcotest.(check bool)
    "persisted before return/broadcast" true
    (match attempt.segments with [ Text_segment d ] -> d.text = "hello world" | _ -> false);
  ignore (ok (Advisor_store.transition store ~run (A.Fail { code = Provider_unavailable; context = No_context })));
  error_code A.Invalid_transition (append store run 4L "late");
  error_code A.Invalid_transition (Advisor_store.transition store ~run A.Finish);
  Alcotest.(check int)
    "exactly one terminal event" 1
    (List.length
       (List.filter
          (fun (e : A.event) -> match e.payload with Run_failed _ -> true | _ -> false)
          (replay_events store run 0L)));
  advance clock 86399;
  Advisor_store.prune store ~now:!clock;
  Alcotest.(check int) "retained before 24h" 4 (List.length (replay_events store run 0L));
  advance clock 1;
  Advisor_store.prune store ~now:!clock;
  Alcotest.(check bool)
    "old cursor needs snapshot" true
    (match ok (Advisor_store.replay store ~run ~after:0L ~limit:20) with Resync_required 4L -> true | _ -> false);
  Alcotest.(check int) "journal pruned" 0 (count db "advisor_events");
  let after = ok (Advisor_store.snapshot store ~owner first.ids.conversation_id) in
  Alcotest.(check bool)
    "partial transcript survives failure/prune" true
    ((List.hd (List.hd after.turns).attempts).segments = attempt.segments)

let concurrent_retry () =
  Eio_main.run @@ fun _ ->
  with_fixture @@ fun db store owner _ ->
  Sql.set_guard db (Db_guard.make ());
  let first = accept store owner (submit "race") in
  fail store first.ids.run_id;
  let results = ref [] in
  let retry key () =
    Eio.Fiber.yield ();
    let result =
      Advisor_store.retry store ~owner ~session ~now ~model:"synthetic"
        { A.turn_id = first.ids.turn_id; expected_version = 1L; idempotency_key = Test_service_support.key key }
    in
    results := result :: !results
  in
  Eio.Fiber.both (retry "race-a") (retry "race-b");
  Alcotest.(check int) "one winner for concurrent retry" 1 (List.length (List.filter Result.is_ok !results));
  Alcotest.(check int) "one active run" 1 (List.length (ok (Advisor_store.active_runs store)));
  Alcotest.(check int) "two persisted attempts" 2 (count db "advisor_attempts")

let ownership_idempotency_and_old_turn () =
  with_fixture @@ fun db store owner _ ->
  let other_service = Service.make db in
  let other = (Access.user (Test_service_support.sign_in other_service "other")).User.id in
  let request = submit "identity" in
  let first = accept store owner request in
  let other_session = Option.get (A.Session_ref.of_hash (String.make 64 'b')) in
  let repeated =
    ok (Advisor_store.accept store ~owner ~session:other_session ~now ~model:"changed" ~source_finding:None request)
  in
  Alcotest.(check bool)
    "repeat does not rebind origin" true
    (repeated.ids = first.ids && (List.hd (ok (Advisor_store.active_runs store))).originating_session = session);
  error_code A.Idempotency_conflict
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None
       { request with user_text = "different" });
  error_code A.Idempotency_conflict
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None
       { request with user_text = String.make 8193 'x' });
  error_code A.Not_found (Advisor_store.snapshot store ~owner:other first.ids.conversation_id);
  error_code A.Not_found (Advisor_store.metadata store ~owner:other first.ids.conversation_id);
  error_code A.Not_found (Advisor_store.delete store ~owner:other ~now first.ids.conversation_id);
  error_code A.Not_found
    (Advisor_store.accept store ~owner:other ~session ~now ~model:"synthetic" ~source_finding:None
       (submit ~conversation:first.ids.conversation_id ~version:1L "steal"));
  error_code A.Run_busy
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None (submit "second-active"));
  fail store first.ids.run_id;
  let second = accept store owner (submit ~conversation:first.ids.conversation_id ~version:1L "followup") in
  fail store second.ids.run_id;
  error_code A.Version_conflict
    (Advisor_store.retry store ~owner ~session ~now ~model:"synthetic"
       { A.turn_id = first.ids.turn_id; expected_version = 2L; idempotency_key = Test_service_support.key "old-turn" });
  error_code A.Version_conflict
    (Advisor_store.retry store ~owner ~session ~now ~model:"synthetic"
       { A.turn_id = second.ids.turn_id; expected_version = 1L; idempotency_key = Test_service_support.key "stale" })

let deletion_and_tombstones () =
  with_fixture @@ fun db store owner clock ->
  let request = submit "deleted" in
  let first = accept store owner request in
  let deleted = ok (Advisor_store.delete store ~owner ~now first.ids.conversation_id) in
  Alcotest.(check bool) "returns active work to cancel" true (deleted = [ first.ids.run_id ]);
  error_code A.Not_found (append store first.ids.run_id 0L "late");
  error_code A.Not_found (Advisor_store.transition store ~run:first.ids.run_id A.Recover_interrupted);
  error_code A.Not_found
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None request);
  Alcotest.(check bool) "deleted turn cannot reappear" false (count db "advisor_turns" > 0);
  Alcotest.(check int) "tombstone retained" 1 (count db "advisor_requests");
  let content = Sql.(run db (stmt "SELECT accepted FROM advisor_requests" nil (one (row (text @-> nil) Fun.id)))) in
  Alcotest.(check bool)
    "tombstone is only accepted IDs" true
    (Result.is_ok (Cashflow_advisor_protocol.Wire.decode_accepted content));
  advance clock 86399;
  Advisor_store.prune store ~now:!clock;
  Alcotest.(check int) "tombstone before boundary" 1 (count db "advisor_requests");
  advance clock 1;
  Advisor_store.prune store ~now:!clock;
  Alcotest.(check int) "tombstone at boundary" 0 (count db "advisor_requests");
  List.iter
    (fun table -> Alcotest.(check int) table 0 (count db table))
    [
      "advisor_conversations";
      "advisor_turns";
      "advisor_attempts";
      "advisor_runs";
      "advisor_segments";
      "advisor_evidence";
      "advisor_events";
      "advisor_account_aliases";
    ]

let recovery_and_capacity () =
  with_fixture @@ fun db store owner clock ->
  let first = accept store owner (submit "recover") in
  let service = Service.make db in
  for i = 2 to 8 do
    let owner = (Access.user (Test_service_support.sign_in service ("capacity-" ^ string_of_int i))).User.id in
    ignore (accept store owner (submit ("capacity-" ^ string_of_int i)))
  done;
  let ninth = (Access.user (Test_service_support.sign_in service "ninth")).User.id in
  error_code A.Capacity_exceeded
    (Advisor_store.accept store ~owner:ninth ~session ~now ~model:"synthetic" ~source_finding:None (submit "ninth"));
  Alcotest.(check int) "eight globally" 8 (List.length (ok (Advisor_store.active_runs store)));
  ignore (ok (Advisor_store.transition store ~run:first.ids.run_id A.Request_cancel));
  error_code A.Run_busy
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None
       (submit "cancel-still-active"));
  advance clock 10;
  Advisor_store.recover store ~now:!clock;
  Alcotest.(check int) "all unfinished recovered" 0 (List.length (ok (Advisor_store.active_runs store)));
  let events = count db "advisor_events" in
  Advisor_store.recover store ~now:!clock;
  Alcotest.(check int) "recovery once" events (count db "advisor_events");
  let s = ok (Advisor_store.snapshot store ~owner first.ids.conversation_id) in
  Alcotest.(check bool)
    "restart reason" true
    (match (List.hd (List.hd s.turns).attempts).state with Failed { code = Server_restarted; _ } -> true | _ -> false)

let commit_failure () =
  with_fixture @@ fun db store owner _ ->
  Sql.exec_script db
    "CREATE TABLE failure_parent(id INTEGER PRIMARY KEY); CREATE TABLE failure_child(id INTEGER REFERENCES \
     failure_parent(id) DEFERRABLE INITIALLY DEFERRED); CREATE TRIGGER fail_advisor_commit AFTER INSERT ON \
     advisor_requests BEGIN INSERT INTO failure_child VALUES (1); END";
  error_code A.Storage_unavailable
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None (submit "commit-failure"));
  List.iter
    (fun table -> Alcotest.(check int) ("rollback " ^ table) 0 (count db table))
    [ "advisor_requests"; "advisor_runs"; "advisor_attempts"; "advisor_turns"; "advisor_conversations" ];
  Sql.exec_script db "DROP TRIGGER fail_advisor_commit";
  let a = accept store owner (submit "commit-failure") in
  Alcotest.(check int) "connection reusable" 1 (count db "advisor_conversations");
  let fail_events () =
    Sql.exec_script db
      "CREATE TRIGGER fail_event_commit AFTER INSERT ON advisor_events BEGIN INSERT INTO failure_child VALUES (1); END"
  in
  fail_events ();
  error_code A.Storage_unavailable (Advisor_store.transition store ~run:a.ids.run_id A.Start);
  let state () =
    (List.hd (List.hd (ok (Advisor_store.snapshot store ~owner a.ids.conversation_id)).turns).attempts).state
  in
  Alcotest.(check bool) "failed start commit retains queued" true (state () = A.Queued);
  Sql.exec_script db "DROP TRIGGER fail_event_commit";
  ignore (ok (Advisor_store.transition store ~run:a.ids.run_id A.Start));
  fail_events ();
  error_code A.Storage_unavailable (append store a.ids.run_id 1L "uncommitted text");
  Alcotest.(check int) "failed checkpoint leaves no segment" 0 (count db "advisor_segments");
  error_code A.Storage_unavailable (Advisor_store.transition store ~run:a.ids.run_id A.Finish);
  Alcotest.(check bool) "failed terminal commit retains running" true (state () = A.Running A.Awaiting_model);
  Alcotest.(check int) "failed terminal commit leaves no terminal event" 1 (count db "advisor_events");
  Sql.exec_script db "DROP TRIGGER fail_event_commit";
  ignore (ok (Advisor_store.transition store ~run:a.ids.run_id A.Finish));
  Alcotest.(check bool)
    "terminal state committed with event" true
    (state () = A.Completed && count db "advisor_events" = 2)

let migration_v5 () =
  let db = Sql.open_db ":memory:" in
  Fun.protect ~finally:(fun () -> Sql.close db) @@ fun () ->
  List.iter
    (fun (version, sql) ->
      if version <= 5 then (
        Sql.exec_script db sql;
        Sql.(
          run db (stmt "INSERT INTO schema_migrations(version,applied_at) VALUES (?,'synthetic')" (int @-> nil) exec))
          version))
    Schema.migrations;
  let service = Service.make db in
  ignore (Test_service_support.sign_in service "populated-v5");
  Alcotest.(check int) "v5 populated" 1 (count db "users");
  Schema.migrate db;
  Schema.migrate db;
  Alcotest.(check int) "existing data retained" 1 (count db "users");
  Alcotest.(check int) "migration applied once" 6 (count db "schema_migrations");
  let violations =
    Sql.(
      run db
        (stmt "PRAGMA foreign_key_check" nil
           (many (row (text @-> int @-> text @-> int @-> nil) (fun a b c d -> (a, b, c, d))))))
  in
  Alcotest.(check int) "FK consistency" 0 (List.length violations)

let consent_metadata_aliases () =
  with_fixture @@ fun db store owner _ ->
  Alcotest.(check bool) "consent disabled by default" false (ok (Advisor_store.consent store (User owner)));
  ignore (ok (Advisor_store.set_consent store (User owner) ~enabled:true));
  Alcotest.(check bool) "user consent enabled" true (ok (Advisor_store.consent store (User owner)));
  let service = Service.make db in
  let access = Test_service_support.sign_in service ~admin:true "advisor-fixture" in
  let household =
    Test_service_support.ok
      (Service.create_household service access ~name:"Synthetic household" ~default_currency:"SEK")
  in
  Alcotest.(check bool)
    "household disabled separately" false
    (ok (Advisor_store.consent store (Household household.Household.id)));
  ignore (ok (Advisor_store.set_consent store (Household household.id) ~enabled:true));
  let finding : A.finding =
    {
      kind = "budget";
      key = "synthetic-finding";
      scope = Personal;
      title = "Captured title";
      summary = "Captured summary";
      sampled_at = now;
    }
  in
  let request = { (submit "finding") with source_finding_key = Some finding.key } in
  error_code A.Invalid_request
    (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None request);
  let first =
    ok (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:(Some finding) request)
  in
  Alcotest.(check bool)
    "immutable captured finding" true
    ((ok (Advisor_store.snapshot store ~owner first.ids.conversation_id)).source_finding = Some finding);
  let account = Test_service_support.open_checking service access "Synthetic checking" in
  let alias = ok (Advisor_store.account_alias store ~conversation:first.ids.conversation_id account) in
  Alcotest.(check string) "pseudonym" "account_1" alias;
  Alcotest.(check string)
    "alias stable" alias
    (ok (Advisor_store.account_alias store ~conversation:first.ids.conversation_id account));
  let meta = ok (Advisor_store.metadata store ~owner first.ids.conversation_id) in
  Alcotest.(check bool) "metadata has active run" true (meta.active_run = Some first.ids.run_id);
  fail store first.ids.run_id;
  for i = 2 to 22 do
    let a = accept store owner (submit ("page-" ^ string_of_int i)) in
    fail store a.ids.run_id
  done;
  Alcotest.(check int) "default page" 20 (List.length (ok (Advisor_store.list store ~owner ())));
  Alcotest.(check int) "next page" 2 (List.length (ok (Advisor_store.list store ~owner ~offset:20 ())));
  Alcotest.(check int) "large page clamped" 22 (List.length (ok (Advisor_store.list store ~owner ~limit:1000 ())));
  error_code A.Invalid_request (Advisor_store.list store ~owner ~offset:(-1) ())

let evidence_bounds_and_cancel () =
  with_fixture @@ fun db store owner _ ->
  let first = accept store owner (submit "bounds") in
  let run = first.ids.run_id in
  ignore (ok (Advisor_store.transition store ~run A.Start));
  let evidence : A.evidence =
    {
      id = Option.get (A.Evidence_id.of_string "00000000-0000-4000-8000-000000000001");
      scope = Personal;
      currency = (submit "unused").currency;
      sampled_at = now;
      title = "Synthetic evidence";
      facts =
        [ { label = "Balance"; value = Money { minor_units = Int64.min_int; currency = (submit "unused").currency } } ];
      source_references = [ "synthetic" ];
      import_status = Current;
      reconciled = true;
      assumptions = [];
    }
  in
  ignore (ok (Advisor_store.append store ~run ~expected_sequence:1L [ A.Evidence_added evidence ]));
  let s = ok (Advisor_store.snapshot store ~owner first.ids.conversation_id) in
  Alcotest.(check bool)
    "evidence round trip signed int64" true
    (match (List.hd (List.hd s.turns).attempts).segments with [ Evidence_segment e ] -> e = evidence | _ -> false);
  error_code A.Invalid_request (Advisor_store.append store ~run ~expected_sequence:2L [ A.Evidence_added evidence ]);
  error_code A.Run_limit_reached (append store run 2L (String.make 65537 'x'));
  Alcotest.(check int) "rejected output rolled back" 2 (count db "advisor_events");
  ignore (ok (Advisor_store.transition store ~run A.Request_cancel));
  error_code A.Invalid_transition (Advisor_store.transition store ~run A.Finish);
  ignore (ok (Advisor_store.transition store ~run A.Confirm_cancel));
  Alcotest.(check int) "cancel terminal event persisted" 3 (count db "advisor_events")

let durable_visibility () =
  let path = Filename.temp_file "advisor-synthetic-" ".sqlite3" in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let p = path ^ suffix in
          if Sys.file_exists p then Sys.remove p)
        [ ""; "-wal"; "-shm" ])
  @@ fun () ->
  let db = Sql.open_db path in
  let owner, conversation, run =
    Fun.protect
      ~finally:(fun () -> Sql.close db)
      (fun () ->
        Schema.migrate db;
        let service = Service.make db in
        let owner = (Access.user (Test_service_support.sign_in service "durable")).User.id in
        let store = Advisor_store.create ~clock:(fun () -> now) db in
        let a = accept store owner (submit "durable") in
        ignore (ok (Advisor_store.transition store ~run:a.ids.run_id A.Start));
        ignore (ok (append store a.ids.run_id 1L "visible after commit"));
        let reader = Sql.open_db path in
        Fun.protect
          ~finally:(fun () -> Sql.close reader)
          (fun () ->
            Alcotest.(check int) "another connection sees committed checkpoint" 2 (count reader "advisor_events");
            let reader_store = Advisor_store.create reader in
            let s = ok (Advisor_store.snapshot reader_store ~owner a.ids.conversation_id) in
            Alcotest.(check int64) "state/event same checkpoint" 2L (List.hd (List.hd s.turns).attempts).last_sequence);
        (owner, a.ids.conversation_id, a.ids.run_id))
  in
  let db = Sql.open_db path in
  Fun.protect ~finally:(fun () -> Sql.close db) @@ fun () ->
  let store = Advisor_store.create ~clock:(fun () -> now) db in
  Advisor_store.recover store ~now;
  let s = ok (Advisor_store.snapshot store ~owner conversation) in
  Alcotest.(check bool)
    "reopened transcript survives recovery" true
    (match (List.hd (List.hd s.turns).attempts).segments with
    | [ Text_segment d ] -> d.text = "visible after commit"
    | _ -> false);
  Alcotest.(check int) "reopened run has restart terminal" 3 (List.length (replay_events store run 0L))

let fractional_retention () =
  with_fixture @@ fun db store owner clock ->
  clock := Option.get (Ptime.add_span now (Option.get (Ptime.Span.of_float_s 0.75)));
  let a = accept store owner (submit "fractional") in
  ignore (ok (Advisor_store.transition store ~run:a.ids.run_id A.Start));
  ignore (ok (Advisor_store.transition store ~run:a.ids.run_id A.Finish));
  let terminal = !clock in
  clock := Option.get (Ptime.add_span terminal (Option.get (Ptime.Span.of_float_s 86399.5)));
  Advisor_store.prune store ~now:!clock;
  Alcotest.(check int) "not pruned half a second early" 2 (count db "advisor_events");
  clock := Option.get (Ptime.add_span terminal (Ptime.Span.of_int_s 86400));
  Advisor_store.prune store ~now:!clock;
  Alcotest.(check int) "pruned at exact fractional boundary" 0 (count db "advisor_events")

let bounded_journal_and_counters () =
  with_fixture @@ fun db store owner _ ->
  let a = accept store owner (submit "journal") in
  let run = a.ids.run_id in
  ignore (ok (Advisor_store.transition store ~run A.Start));
  let oversized = A.Tool_finished { label = String.make 262144 'x'; outcome = Succeeded } in
  error_code A.Run_limit_reached (Advisor_store.append store ~run ~expected_sequence:1L [ oversized ]);
  let large = A.Tool_finished { label = String.make 250000 'x'; outcome = Succeeded } in
  ignore (ok (Advisor_store.append store ~run ~expected_sequence:1L [ large; large; large; large ]));
  error_code A.Run_limit_reached (Advisor_store.append store ~run ~expected_sequence:5L [ large ]);
  Alcotest.(check int) "journal bounded and atomic" 5 (count db "advisor_events");
  ignore (ok (Advisor_store.transition store ~run A.Finish));
  Alcotest.(check int) "journal reserves terminal event" 6 (count db "advisor_events");
  let a = accept store owner (submit "counters") in
  let run = a.ids.run_id in
  ignore (ok (Advisor_store.transition store ~run A.Start));
  for _ = 2 to 4 do
    ignore (ok (Advisor_store.transition store ~run (A.Set_phase Running_tools)));
    ignore (ok (Advisor_store.transition store ~run (A.Set_phase Awaiting_model)))
  done;
  ignore (ok (Advisor_store.transition store ~run (A.Set_phase Running_tools)));
  error_code A.Run_limit_reached (Advisor_store.transition store ~run (A.Set_phase Awaiting_model));
  let tools = List.init 8 (fun _ -> A.Tool_started "synthetic read") in
  ignore (ok (Advisor_store.append store ~run ~expected_sequence:1L tools));
  error_code A.Run_limit_reached (Advisor_store.append store ~run ~expected_sequence:9L [ A.Tool_started "ninth" ]);
  let current = List.hd (ok (Advisor_store.active_runs store)) in
  Alcotest.(check int) "four model rounds" 4 current.counters.model_rounds;
  Alcotest.(check int) "eight read tools" 8 current.counters.tool_calls

let () =
  Alcotest.run "advisor_store"
    [
      ( "atomic",
        List.map
          (fun (name, test) -> Alcotest.test_case name `Quick test)
          [
            ("attempt_idempotency_and_replay", attempt_idempotency_and_replay);
            ("partials_replay_retention", partials_replay_retention);
            ("concurrent_retry", concurrent_retry);
            ("ownership_idempotency_and_old_turn", ownership_idempotency_and_old_turn);
            ("deletion_and_tombstones", deletion_and_tombstones);
            ("recovery_and_capacity", recovery_and_capacity);
            ("commit_failure", commit_failure);
            ("migration_v5", migration_v5);
            ("consent_metadata_aliases", consent_metadata_aliases);
            ("evidence_bounds_and_cancel", evidence_bounds_and_cancel);
            ("durable_visibility", durable_visibility);
            ("fractional_retention", fractional_retention);
            ("bounded_journal_and_counters", bounded_journal_and_counters);
          ] );
    ]
