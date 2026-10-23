open Cashflow_core
open Cashflow_io
open Advisor_test_support
module R = Advisor_runtime

let durable_delivery_and_runtime_deadlines () =
  Eio_main.run (fun env ->
      let db = Store_sqlite.open_memory () in
      Fun.protect
        ~finally:(fun () -> Sql.close db)
        (fun () ->
          Eio.Switch.run (fun sw ->
              Sql.set_guard db (Db_guard.make ());
              let service = Service.make ~now:(fun () -> now) db in
              let access = Test_service_support.sign_in service "runtime" in
              let token = Web_auth.create_session db ~now (Access.user access) in
              let principal = ok (Advisor_access.authenticate service ~now ~token) in
              ignore (ok (Advisor_access.set_user_consent service principal ~enabled:true));
              let calls = ref 0 in
              let post ~uri:_ ~headers:_ ~body:_ ~on_chunk =
                incr calls;
                Alcotest.(check bool) "actual runtime DB guard free" false ((Sql.unguarded db).held ());
                on_chunk
                  "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"live\"},\"finish_reason\":null}]}\n\n";
                on_chunk
                  "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n";
                Ok ()
              in
              let provider =
                Advisor_provider.create
                  ~limits:
                    {
                      context_tokens = 262144;
                      completion_tokens = 1024;
                      whole_completion_cap_supported = true;
                      request_tokens_per_byte = 1;
                      request_token_overhead = 0;
                    }
                  ~post ~api_key:"fake" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
              in
              let clock : R.clock =
                {
                  now = (fun () -> now);
                  monotonic_s = (fun () -> Eio.Time.now (Eio.Stdenv.clock env));
                  sleep_s = Eio.Time.sleep (Eio.Stdenv.clock env);
                }
              in
              let runtime = R.create ~sw ~service ~provider ~clock in
              let a = ok (R.submit runtime ~principal (submit "runtime")) in
              R.start runtime a;
              let sub = ok (R.subscribe runtime ~principal ~run:a.ids.run_id ~after:0L) in
              let seen = ref [] in
              let rec read () =
                match ok (R.next sub) with
                | R.Events es ->
                    let journal =
                      match
                        ok (Advisor_store.replay (Advisor_store.create db) ~run:a.ids.run_id ~after:0L ~limit:100)
                      with
                      | Events es -> es
                      | _ -> []
                    in
                    List.iter (fun e -> Alcotest.(check bool) "persisted before observed" true (List.mem e journal)) es;
                    seen := !seen @ es;
                    read ()
                | R.End -> ()
                | R.Heartbeat -> read ()
                | _ -> Alcotest.fail "unexpected resync"
              in
              read ();
              R.close sub;
              Alcotest.(check int) "one-shot provider" 1 !calls;
              Alcotest.(check int)
                "one terminal" 1
                (List.length (List.filter (fun e -> e.A.payload = A.Run_completed) !seen));
              ())))

type fake_clock = {
  mutable elapsed : float;
  mutable wall : Ptime.t;
  mutable sleepers : (float * unit Eio.Promise.u) list;
}

let drain () =
  for _ = 1 to 40 do
    Eio.Fiber.yield ()
  done

let advance f seconds =
  f.elapsed <- f.elapsed +. seconds;
  f.wall <- Option.get (Ptime.add_span now (Option.get (Ptime.Span.of_float_s f.elapsed)));
  let due, remaining = List.partition (fun (at, _) -> at <= f.elapsed) f.sleepers in
  f.sleepers <- remaining;
  List.iter (fun (_, u) -> Eio.Promise.resolve u ()) due;
  drain ()

let fake () =
  let f = { elapsed = 0.; wall = now; sleepers = [] } in
  let clock : R.clock =
    {
      now = (fun () -> f.wall);
      monotonic_s = (fun () -> f.elapsed);
      sleep_s =
        (fun seconds ->
          let p, u = Eio.Promise.create () in
          f.sleepers <- (f.elapsed +. seconds, u) :: f.sleepers;
          Eio.Promise.await p);
    }
  in
  (f, clock)

let login service name =
  let access = Test_service_support.sign_in service name in
  let token = Web_auth.create_session service.Service.db ~now:(service.now ()) (Access.user access) in
  let principal = ok (Advisor_access.authenticate service ~now:(service.now ()) ~token) in
  ignore (ok (Advisor_access.set_user_consent service principal ~enabled:true));
  ((Access.user access).id, token, principal)

let fixture post test =
  Eio_main.run (fun _ ->
      let db = Store_sqlite.open_memory () in
      Fun.protect
        ~finally:(fun () -> Sql.close db)
        (fun () ->
          Eio.Switch.run (fun sw ->
              Sql.set_guard db (Db_guard.make ());
              let f, clock = fake () in
              let service = Service.make ~now:clock.now db in
              let provider =
                Advisor_provider.create
                  ~limits:
                    {
                      context_tokens = 262144;
                      completion_tokens = 1024;
                      whole_completion_cap_supported = true;
                      request_tokens_per_byte = 1;
                      request_token_overhead = 0;
                    }
                  ~post:(post db) ~api_key:"fake" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
              in
              let runtime = R.create ~sw ~service ~provider ~clock in
              test sw db service runtime f)))

let state db owner (a : A.accepted) =
  (ok (Advisor_store.find_run (Advisor_store.create db) ~owner a.A.ids.run_id)).state

let silent db ~uri:_ ~headers:_ ~body:_ ~on_chunk:_ =
  Alcotest.(check bool) "provider wait uses runtime guard" false ((Sql.unguarded db).held ());
  let promise, _ = Eio.Promise.create () in
  Eio.Promise.await promise

let check label value = Alcotest.(check bool) label true value

let silent_deadlines () =
  fixture silent (fun _ db service runtime f ->
      let owner, _, principal = login service "inactivity" in
      let a = ok (R.submit runtime ~principal (submit "inactivity")) in
      let sub = ok (R.subscribe runtime ~principal ~run:a.ids.run_id ~after:0L) in
      drain ();
      advance f 30.;
      check "silent upstream fails at 30s"
        (match state db owner a with A.Failed { context = A.Limit A.Upstream_inactivity; _ } -> true | _ -> false);
      R.close sub);
  fixture silent (fun _ db service runtime f ->
      let owner, _, principal = login service "abandoned" in
      let a = ok (R.submit runtime ~principal (submit "abandoned")) in
      (* No model activity cannot hide the absent-subscriber deadline; cancellation wins. *)
      drain ();
      advance f 30.;
      check "no subscriber cancels at 30s" (state db owner a = A.Cancelled))

let sessions_and_reconnect () =
  fixture silent (fun _ db service runtime f ->
      let owner, ta, a = login service "sessions" in
      let _, tb, b = login service "sessions" in
      let accepted = ok (R.submit runtime ~principal:a (submit "sessions")) in
      let repeat = ok (R.submit runtime ~principal:b (submit "sessions")) in
      check "B repeats canonical run" (repeat.ids = accepted.ids && repeat.idempotent_repeat);
      let sub = ok (R.subscribe runtime ~principal:b ~run:accepted.ids.run_id ~after:0L) in
      drain ();
      R.close sub;
      advance f 10.;
      let sub = ok (R.subscribe runtime ~principal:b ~run:accepted.ids.run_id ~after:0L) in
      advance f 10.;
      check "disconnect reconnect does not regenerate"
        (match state db owner accepted with A.Running _ -> true | _ -> false);
      let ids = Advisor_access.revoke_session service ~now:f.wall (Web_auth.session_ref tb) in
      check "B logout does not target A" (ids = []);
      R.signal_cancel runtime ids;
      let _, _, b = login service "sessions" in
      let sub2 = ok (R.subscribe runtime ~principal:b ~run:accepted.ids.run_id ~after:0L) in
      let ids = Advisor_access.revoke_session service ~now:f.wall (Web_auth.session_ref ta) in
      R.signal_cancel runtime ids;
      drain ();
      check "origin logout immediate despite B" (state db owner accepted = A.Cancelled);
      check "terminal replay survives A" (Result.is_ok (R.next sub2));
      R.close sub;
      R.close sub2)

let capacity_and_duplicate () =
  let calls = ref 0 in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      incr calls;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime _ ->
      let accepted =
        List.init 8 (fun n ->
            let owner, _, p = login service ("capacity" ^ string_of_int n) in
            let a = ok (R.submit runtime ~principal:p (submit ("capacity" ^ string_of_int n))) in
            R.start runtime a;
            (owner, p, a))
      in
      let _, _, ninth = login service "ninth" in
      check "ninth rejected"
        (match R.submit runtime ~principal:ninth (submit "ninth") with
        | Error { code = A.Capacity_exceeded; _ } -> true
        | _ -> false);
      drain ();
      Alcotest.(check int) "eight one-shot workers" 8 !calls;
      List.iter
        (fun (owner, p, (a : A.accepted)) ->
          ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
          drain ();
          check "cancel terminal" (state db owner a = A.Cancelled))
        accepted)

let expiry_and_keepalives () =
  let callback = ref (fun (_ : string) -> ()) in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime f ->
      let owner, token, p = login service "expiry" in
      let a = ok (R.submit runtime ~principal:p (submit "expiry")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      let reference = Web_auth.session_ref token in
      Sql.(run db (stmt "UPDATE sessions SET expires_at=? WHERE token_hash=?" (text @-> text @-> nil) exec))
        (Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 (Option.get (Ptime.add_span now (Ptime.Span.of_int_s 1))))
        (A.Session_ref.to_hash reference);
      advance f 1.;
      check "silent origin expiry" (state db owner a = A.Cancelled);
      R.close sub);
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime f ->
      let owner, _, p = login service "total" in
      let a = ok (R.submit runtime ~principal:p (submit "total")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      for _ = 1 to 5 do
        !callback ": keepalive\n\n";
        advance f 20.
      done;
      !callback ": keepalive\n\n";
      advance f 20.;
      check "120s despite keepalives"
        (match state db owner a with A.Failed { context = A.Limit A.Run_deadline; _ } -> true | _ -> false);
      R.close sub)

let text_chunk text =
  "data: "
  ^ Yojson.Safe.to_string
      (`Assoc
         [
           ( "choices",
             `List
               [
                 `Assoc [ ("index", `Int 0); ("delta", `Assoc [ ("content", `String text) ]); ("finish_reason", `Null) ];
               ] );
         ])
  ^ "\n\n"

let finish_chunk = "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n"

let journal db (a : A.accepted) =
  match ok (Advisor_store.replay (Advisor_store.create db) ~run:a.A.ids.run_id ~after:0L ~limit:100) with
  | Events es -> es
  | _ -> []

let texts es =
  List.filter_map (fun e -> match e.A.payload with A.Text_appended d -> Some d.text | _ -> None) es
  |> String.concat ""

let buffering_drain_delete () =
  let callback = ref (fun (_ : string) -> ()) in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime f ->
      let owner, _, p = login service "buffering" in
      let a = ok (R.submit runtime ~principal:p (submit "buffering")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      !callback (text_chunk "first");
      drain ();
      check "100ms not yet flushed" (texts (journal db a) = "");
      advance f 0.1;
      check "100ms incremental while upstream open" (texts (journal db a) = "first");
      !callback (text_chunk " last");
      drain ();
      ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
      drain ();
      check "cancel drains prior buffered text" (texts (journal db a) = "first last");
      check "cancel terminal" (state db owner a = A.Cancelled);
      (try !callback (text_chunk "LATE") with _ -> ());
      drain ();
      check "late callback cannot enter drain" (texts (journal db a) = "first last");
      let replay = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:2L) in
      check "final delta replays after disconnect"
        (match ok (R.next replay) with
        | R.Events [ { payload = A.Text_appended d; _ } ] -> d.text = " last"
        | _ -> false);
      R.close replay;
      R.close sub;
      let b = ok (R.submit runtime ~principal:p (submit "delete")) in
      drain ();
      !callback (text_chunk "secret");
      drain ();
      let ids = ok (Advisor_access.set_user_consent service p ~enabled:false) in
      R.signal_cancel runtime ids;
      ignore (ok (R.delete runtime ~principal:p b.ids.conversation_id));
      drain ();
      (try !callback (text_chunk "DELETED LATE") with _ -> ());
      drain ();
      check "opted-out owner deletes and late callback cannot recreate"
        (Result.is_error (Advisor_store.find_run (Advisor_store.create db) ~owner b.ids.run_id)))

let persistence_failure () =
  fixture
    (fun db ~uri:_ ~headers:_ ~body:_ ~on_chunk ->
      check "outside actual guarded transaction" (not ((Sql.unguarded db).held ()));
      on_chunk (text_chunk "uncommitted" ^ finish_chunk);
      Ok ())
    (fun _ db service runtime _ ->
      let owner, _, p = login service "failure" in
      Sql.exec_script db
        "CREATE TRIGGER fail_text BEFORE INSERT ON advisor_events WHEN NEW.projection LIKE '%text_appended%' BEGIN \
         SELECT RAISE(ABORT,'synthetic failure'); END;";
      let a = ok (R.submit runtime ~principal:p (submit "failure")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      check "persistence failure terminal"
        (match state db owner a with A.Failed { code = A.Storage_unavailable; _ } -> true | _ -> false);
      check "uncommitted text never delivered" (texts (journal db a) = "");
      R.close sub)

let slow_reader_and_wakeup () =
  let callback = ref (fun (_ : string) -> ()) in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun sw db service runtime f ->
      let _, _, p = login service "slow" in
      let a = ok (R.submit runtime ~principal:p (submit "slow")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      let waiting, u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
          ignore (ok (R.next sub));
          (* Run_started *)
          Eio.Promise.resolve u (ok (R.next sub)));
      drain ();
      !callback (text_chunk "wake");
      drain ();
      advance f 0.101;
      check "wakeup catches durable event after waiter registration"
        (match Eio.Promise.await waiting with R.Events [ { payload = A.Text_appended _; _ } ] -> true | _ -> false);
      for _ = 1 to 800 do
        !callback (text_chunk "x");
        drain ();
        advance f 0.101
      done;
      check "slow reader resync without blocking producer"
        (match ok (R.next sub) with R.Resync_required _ -> true | _ -> false);
      ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
      drain ();
      check "journal bounded"
        ((ok
            (Advisor_store.find_run (Advisor_store.create db) ~owner:(ok (Advisor_access.user service p)).id
               a.ids.run_id))
           .counters
           .journal_bytes <= 1048576))

let recovery_and_unknown_start () =
  let calls = ref 0 in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      incr calls;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun sw db service runtime f ->
      let owner, _, p = login service "recovery" in
      let store = Advisor_store.create db in
      let raw =
        ok
          (Advisor_store.accept store ~owner
             ~session:(ok (Advisor_access.session service p))
             ~now:f.wall ~model:"synthetic" ~source_finding:None (submit "raw"))
      in
      R.start runtime raw;
      drain ();
      check "unregistered raw acceptance starts nothing" (!calls = 0);
      let _, clock = fake () in
      let post ~uri:_ ~headers:_ ~body:_ ~on_chunk:_ =
        incr calls;
        Ok ()
      in
      let provider =
        Advisor_provider.create ~post ~api_key:"fake" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
      in
      let restarted = R.create ~sw ~service ~provider ~clock in
      R.start restarted raw;
      drain ();
      check "restart never generates" (!calls = 0);
      check "recovery fails unfinished"
        (match state db owner raw with A.Failed { code = A.Server_restarted; _ } -> true | _ -> false);
      let a = ok (R.submit restarted ~principal:p (submit "unsupported")) in
      drain ();
      check "unconfigured runtime refuses before provider" (!calls = 0);
      check "unconfigured terminal failure"
        (match state db owner a with A.Failed { code = A.Invalid_request; _ } -> true | _ -> false))

let tools_and_fresh_segments () =
  let calls = ref 0 in
  fixture
    (fun db ~uri:_ ~headers:_ ~body ~on_chunk ->
      check "tools provider outside guard" (not ((Sql.unguarded db).held ()));
      incr calls;
      let open Yojson.Safe.Util in
      let body = Yojson.Safe.from_string body in
      check "cap and required privacy every round"
        (member "max_tokens" body = `Int 1024 && member "provider" body |> member "require_parameters" = `Bool true);
      if !calls = 1 then
        on_chunk
          (text_chunk "A"
         ^ "data: \
            {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call\",\"type\":\"function\",\"function\":{\"name\":\"goals\",\"arguments\":\"{}\"}}]},\"finish_reason\":\"tool_calls\"}]}\n\n\
            data: [DONE]\n\n")
      else on_chunk (text_chunk "B" ^ finish_chunk);
      Ok ())
    (fun _ db service runtime _ ->
      let owner, _, p = login service "tools" in
      let a = ok (R.submit runtime ~principal:p (submit "tools")) in
      drain ();
      check "tool workflow completes" (state db owner a = A.Completed);
      let snapshot = ok (Advisor_store.snapshot (Advisor_store.create db) ~owner a.ids.conversation_id) in
      let segments = (List.hd (List.hd snapshot.turns).attempts).segments in
      check "real runtime fresh segments around exact evidence"
        (match segments with
        | [ A.Text_segment a; A.Evidence_segment _; A.Text_segment b ] ->
            a.text = "A" && b.text = "B" && a.segment_id <> b.segment_id
        | _ -> false))

let runtime_journal_reserve () =
  let callback = ref (fun (_ : string) -> ()) in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime f ->
      let owner, _, p = login service "journal" in
      let a = ok (R.submit runtime ~principal:p (submit "journal")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      !callback (text_chunk "committed partial");
      drain ();
      advance f 0.1;
      let store = Advisor_store.create db in
      let current () = ok (Advisor_store.find_run store ~owner a.ids.run_id) in
      let append payloads =
        ignore
          (ok
             (Advisor_store.append store ~run:a.ids.run_id ~expected_sequence:(current ()).counters.last_sequence
                payloads))
      in
      let tool s = A.Tool_finished { label = s; outcome = A.Succeeded } in
      append (List.init 4 (fun _ -> tool (String.make 240000 'x')));
      let current_run = current () in
      let event = { A.ids = a.ids; sequence = Int64.succ current_run.counters.last_sequence; payload = tool "" } in
      let overhead = String.length (Cashflow_advisor_protocol.Wire.encode_event (Advisor_codec.event event)) in
      append [ tool (String.make (1044480 - current_run.counters.journal_bytes - overhead) 'x') ];
      !callback (text_chunk "uncommitted overflow");
      drain ();
      advance f 0.1;
      check "journal rejection terminal retains committed partial"
        (match state db owner a with A.Failed { context = A.Limit A.Journal_bytes; _ } -> true | _ -> false);
      let bytes =
        Sql.(
          run db
            (stmt "SELECT sum(length(CAST(projection AS BLOB))) FROM advisor_events WHERE run_id=?" (text @-> nil)
               (one (row (int @-> nil) Fun.id))))
          (A.Run_id.to_string a.ids.run_id)
      in
      check "actual encoded cap includes bounded terminal"
        (bytes <= 1048576 && bytes > 1044480 && bytes = (current ()).counters.journal_bytes);
      check "no overflow text publication" (texts (journal db a) = "committed partial");
      R.close sub)

let runtime_reserve_admission () =
  fixture silent (fun sw db service _runtime _ ->
      let owner, _, p = login service "reserve" in
      let _, clock = fake () in
      let calls = ref 0 in
      let post ~uri:_ ~headers:_ ~body:_ ~on_chunk:_ =
        incr calls;
        Ok ()
      in
      let provider =
        Advisor_provider.create
          ~limits:
            {
              context_tokens = 16384;
              completion_tokens = 1024;
              whole_completion_cap_supported = true;
              request_tokens_per_byte = 1;
              request_token_overhead = 0;
            }
          ~post ~api_key:"fake" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
      in
      let runtime = R.create ~sw ~service ~provider ~clock in
      let a = ok (R.submit runtime ~principal:p (submit "reserve")) in
      drain ();
      check "runtime visible floor stricter than provider cap"
        (!calls = 0
        && match state db owner a with A.Failed { context = A.Limit A.Context_bytes; _ } -> true | _ -> false))

let orphan_sweep () =
  let callback = ref (fun (_ : string) -> ()) and calls = ref 0 in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      incr calls;
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime f ->
      let store = Advisor_store.create ~clock:(fun () -> f.wall) db in
      let raw key =
        let owner, _, p = login service key in
        ( owner,
          ok
            (Advisor_store.accept store ~owner
               ~session:(ok (Advisor_access.session service p))
               ~now:f.wall ~model:"synthetic" ~source_finding:None (submit key)) )
      in
      let owner, orphan = raw "orphan" in
      let ro, running = raw "raw-running" in
      ignore (ok (Advisor_store.transition store ~run:running.ids.run_id A.Start));
      let co, cancelling = raw "raw-cancelling" in
      ignore (ok (Advisor_store.transition store ~run:cancelling.ids.run_id A.Request_cancel));
      let to_, terminal = raw "raw-terminal" in
      ignore
        (ok
           (Advisor_store.transition store ~run:terminal.ids.run_id
              (A.Fail { code = A.Provider_unavailable; context = A.No_context })));
      let do_, deleted = raw "raw-deleted" in
      ignore (ok (Advisor_store.delete store ~owner:do_ ~now:f.wall deleted.ids.conversation_id));
      let registered_owner, _, p = login service "registered" in
      let registered = ok (R.submit runtime ~principal:p (submit "registered")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:registered.ids.run_id ~after:0L) in
      drain ();
      advance f 29.;
      check "younger orphan retained" (state db owner orphan = A.Queued);
      !callback ": keepalive\n\n";
      let yo, young = raw "young" in
      advance f 2.;
      check "aged orphan fails once without regeneration"
        (match state db owner orphan with A.Failed { code = A.Invalid_request; _ } -> !calls = 1 | _ -> false);
      check "orphan has no Run_started"
        (match journal db orphan with [ { payload = A.Run_failed _; _ } ] -> true | _ -> false);
      check "younger stays queued" (state db yo young = A.Queued);
      check "raw Running unchanged" (state db ro running = A.Running A.Awaiting_model);
      check "raw cancellation unchanged" (state db co cancelling = A.Cancel_requested);
      check "terminal unchanged"
        (match state db to_ terminal with A.Failed { code = A.Provider_unavailable; _ } -> true | _ -> false);
      check "deleted unchanged" (Result.is_error (Advisor_store.find_run store ~owner:do_ deleted.ids.run_id));
      check "registered run survives orphan threshold"
        (match state db registered_owner registered with A.Running _ -> true | _ -> false);
      ignore (ok (R.cancel runtime ~principal:p registered.ids.run_id));
      drain ();
      R.close sub)

let cancelled_handoff_and_sweep () =
  let calls = ref 0 in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      incr calls;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun sw db service runtime f ->
      let owner, _, p = login service "handoff" in
      let original = Sql.unguarded db in
      let gate, release = Eio.Promise.create () and committed, commit = Eio.Promise.create () in
      let armed = ref true and context = ref None in
      let store = Advisor_store.create db in
      Sql.set_guard db
        {
          original with
          exclusive =
            (fun fn ->
              let value = original.exclusive fn in
              if !armed then (
                armed := false;
                let runs = ok (Advisor_store.active_runs store) in
                if List.exists (fun (r : A.run) -> r.state = A.Queued) runs then (
                  check "handoff gate outside actual guard" (not (original.held ()));
                  Eio.Promise.resolve commit ();
                  Eio.Promise.await gate)
                else armed := true);
              value);
        };
      let done_, finished = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
          (try
             Eio.Cancel.sub (fun ctx ->
                 context := Some ctx;
                 ignore (R.submit runtime ~principal:p (submit "handoff")))
           with Exit | Eio.Cancel.Cancelled Exit -> ());
          Eio.Cancel.protect (fun () -> Eio.Promise.resolve finished ()));
      Eio.Promise.await committed;
      let current () = List.hd (ok (Advisor_store.active_runs store)) in
      advance f 31.;
      check "in-flight handoff not swept" ((current ()).state = A.Queued);
      Eio.Cancel.cancel (Option.get !context) Exit;
      Eio.Promise.resolve release ();
      Eio.Promise.await done_;
      drain ();
      let run = current () in
      check "cancelled request does not strand committed acceptance"
        (!calls = 1 && match run.state with A.Running _ -> true | _ -> false);
      ignore (ok (R.cancel runtime ~principal:p run.ids.run_id));
      drain ();
      check "handoff cancellation cleanup terminal"
        ((ok (Advisor_store.find_run store ~owner run.ids.run_id)).state = A.Cancelled))

let revoked_buffer_and_subscriber_denial () =
  let callback = ref (fun (_ : string) -> ()) in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      callback := on_chunk;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun _ db service runtime f ->
      let owner, _, p = login service "revoked-buffer" in
      let a = ok (R.submit runtime ~principal:p (submit "revoked-buffer")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      !callback (text_chunk "pre-admitted");
      drain ();
      advance f 0.099;
      ignore (ok (Advisor_access.set_user_consent service p ~enabled:false));
      (* No immediate signal: the timer itself must not consume a batch it cannot publish. *)
      advance f 0.002;
      check "revoked timer retains prior drain text" (texts (journal db a) = "pre-admitted");
      check "revocation terminal" (state db owner a = A.Cancelled);
      check "revoked next has typed denial"
        (match R.next sub with Error { code = A.Consent_required; _ } -> true | _ -> false);
      R.close sub);
  fixture silent (fun _ _db service runtime f ->
      let _, _, a = login service "subscriber-denial" in
      let _, tb, b = login service "subscriber-denial" in
      let accepted = ok (R.submit runtime ~principal:a (submit "subscriber-denial")) in
      let sub = ok (R.subscribe runtime ~principal:b ~run:accepted.ids.run_id ~after:0L) in
      drain ();
      ignore (ok (R.next sub));
      ignore (Advisor_access.revoke_session service ~now:f.wall (Web_auth.session_ref tb));
      advance f 0.05;
      check "expired subscriber is not silent End"
        (match R.next sub with Error { code = A.Access_denied; _ } -> true | _ -> false);
      ignore (ok (R.cancel runtime ~principal:a accepted.ids.run_id));
      drain ();
      R.close sub)

let replay_backlog_limit () =
  fixture silent (fun _ db service runtime _ ->
      let owner, _, p = login service "replay-backlog" in
      let a = ok (R.submit runtime ~principal:p (submit "replay-backlog")) in
      drain ();
      ignore
        (ok
           (Advisor_store.append (Advisor_store.create db) ~run:a.ids.run_id ~expected_sequence:1L
              (List.init 4 (fun _ -> A.Tool_finished { label = String.make 70000 'x'; outcome = A.Succeeded }))));
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      check "initial durable backlog requires resync"
        (match ok (R.next sub) with R.Resync_required _ -> true | _ -> false);
      ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
      drain ();
      check "backlog resync does not regenerate" (state db owner a = A.Cancelled))

let terminal_cancel_ordering () =
  let release = ref None in
  fixture
    (fun db ~uri:_ ~headers:_ ~body:_ ~on_chunk ->
      check "terminal provider outside guard" (not ((Sql.unguarded db).held ()));
      let gate, u = Eio.Promise.create () in
      release := Some u;
      on_chunk (text_chunk "partial");
      Eio.Promise.await gate;
      on_chunk finish_chunk;
      Ok ())
    (fun _ db service runtime _ ->
      let owner, _, p = login service "terminal-order" in
      let a = ok (R.submit runtime ~principal:p (submit "cancel-first")) in
      drain ();
      Eio.Promise.resolve (Option.get !release) ();
      ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
      drain ();
      check "accepted cancel wins queued completion" (state db owner a = A.Cancelled);
      check "cancel race one terminal"
        (List.length
           (List.filter
              (fun e ->
                match e.A.payload with A.Run_completed | A.Run_cancelled | A.Run_failed _ -> true | _ -> false)
              (journal db a))
        = 1);
      let b = ok (R.submit runtime ~principal:p (submit "complete-first")) in
      drain ();
      Eio.Promise.resolve (Option.get !release) ();
      drain ();
      check "completion first returns authoritative Completed"
        (ok (R.cancel runtime ~principal:p b.ids.run_id) = A.Completed);
      check "completion race one terminal"
        (List.length
           (List.filter
              (fun e ->
                match e.A.payload with A.Run_completed | A.Run_cancelled | A.Run_failed _ -> true | _ -> false)
              (journal db b))
        = 1))

let runtime_retry_and_origin_renewal () =
  let bodies = ref [] in
  fixture
    (fun _db ~uri:_ ~headers:_ ~body ~on_chunk ->
      bodies := !bodies @ [ body ];
      if List.length !bodies = 1 then (
        on_chunk (text_chunk "failed partial");
        Error (Https.Transport "synthetic"))
      else (
        on_chunk (text_chunk "fresh retry" ^ finish_chunk);
        Ok ()))
    (fun _ db service runtime _ ->
      let owner, _, a = login service "retry-runtime" in
      let _, _, b = login service "retry-runtime" in
      let first = ok (R.submit runtime ~principal:a (submit "retry-runtime")) in
      drain ();
      let retried =
        ok
          (R.retry runtime ~principal:b
             {
               A.turn_id = first.ids.turn_id;
               expected_version = first.accepted_version;
               idempotency_key = Test_service_support.key "retry-runtime-b";
             })
      in
      drain ();
      let repeated =
        ok
          (R.retry runtime ~principal:a
             {
               A.turn_id = first.ids.turn_id;
               expected_version = first.accepted_version;
               idempotency_key = Test_service_support.key "retry-runtime-b";
             })
      in
      check "retry idempotent one-shot" (repeated.ids = retried.ids && List.length !bodies = 2);
      let run = ok (Advisor_store.find_run (Advisor_store.create db) ~owner retried.ids.run_id) in
      check "retry B origin fresh identities"
        (run.originating_session = ok (Advisor_access.session service b)
        && retried.ids.attempt_id <> first.ids.attempt_id
        && run.state = A.Completed);
      let snapshot = ok (Advisor_store.snapshot (Advisor_store.create db) ~owner first.ids.conversation_id) in
      check "retry retains attempts one user message"
        (List.length snapshot.turns = 1 && List.length (List.hd snapshot.turns).attempts = 2);
      let open Yojson.Safe.Util in
      let messages = List.nth !bodies 1 |> Yojson.Safe.from_string |> member "messages" |> to_list in
      check "failed partial excluded from retry request"
        (List.map (member "role") messages = [ `String "system"; `String "user" ]));
  fixture silent (fun _ db service runtime f ->
      let owner, token, p = login service "renewal" in
      let a = ok (R.submit runtime ~principal:p (submit "renewal")) in
      let sub = ok (R.subscribe runtime ~principal:p ~run:a.ids.run_id ~after:0L) in
      drain ();
      Sql.(run db (stmt "UPDATE sessions SET expires_at=? WHERE token_hash=?" (text @-> text @-> nil) exec))
        (Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 (Option.get (Ptime.add_span now (Ptime.Span.of_int_s 1))))
        (Web_auth.hash token);
      ignore (Web_auth.session service ~now:f.wall token);
      let extended = ok (Advisor_access.expiry service p) in
      advance f 2.;
      check "watchdog rereads ordinary activity extension"
        (match state db owner a with A.Running _ -> true | _ -> false);
      ignore (ok (R.next sub));
      check "stream does not renew expiry" (Ptime.equal extended (ok (Advisor_access.expiry service p)));
      ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
      drain ();
      R.close sub)

let bounded_bridge_cancelled_writer () =
  let threshold, signal = Eio.Promise.create () in
  let producer_completed = ref false in
  fixture
    (fun db ~uri:_ ~headers:_ ~body:_ ~on_chunk ->
      check "bridge provider outside runtime DB guard" (not ((Sql.unguarded db).held ()));
      for i = 1 to 4000 do
        on_chunk (text_chunk "x");
        if i = 2000 then Eio.Promise.resolve signal ()
      done;
      producer_completed := true;
      on_chunk finish_chunk;
      Ok ())
    (fun _ db service runtime _ ->
      let owner, _, p = login service "bridge-backpressure" in
      let a = ok (R.submit runtime ~principal:p (submit "bridge-backpressure")) in
      Eio.Promise.await threshold;
      check "bounded envelope budget backpressures producer" (not !producer_completed);
      ignore (ok (R.cancel runtime ~principal:p a.ids.run_id));
      drain ();
      check "cancel releases blocked producer and consumer" (state db owner a = A.Cancelled))

let stop_between_commit_and_registration () =
  let calls = ref 0 in
  fixture
    (fun db ~uri ~headers ~body ~on_chunk ->
      incr calls;
      silent db ~uri ~headers ~body ~on_chunk)
    (fun sw db service runtime _ ->
      let owner, _, p = login service "stop-handoff" in
      let _, _, b = login service "stop-handoff" in
      let original = Sql.unguarded db in
      let gate, release = Eio.Promise.create () and committed, commit = Eio.Promise.create () in
      let armed = ref true in
      let store = Advisor_store.create db in
      Sql.set_guard db
        {
          original with
          exclusive =
            (fun fn ->
              let value = original.exclusive fn in
              if !armed then (
                armed := false;
                let runs = ok (Advisor_store.active_runs store) in
                if List.exists (fun (r : A.run) -> r.state = A.Queued) runs then (
                  Eio.Promise.resolve commit ();
                  Eio.Promise.await gate)
                else armed := true);
              value);
        };
      let accepted, resolve = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve resolve (ok (R.submit runtime ~principal:p (submit "stop-handoff"))));
      Eio.Promise.await committed;
      let run = List.hd (ok (Advisor_store.active_runs store)) in
      let repeat = ok (R.submit runtime ~principal:b (submit "stop-handoff")) in
      let valid_repeat =
        repeat.idempotent_repeat && repeat.ids = run.ids
        && (ok (Advisor_store.find_run store ~owner run.ids.run_id)).state = A.Queued
        && !calls = 0
      in
      if not valid_repeat then Eio.Promise.resolve release ();
      check "B repeat cannot kill or rebind A in-flight handoff" valid_repeat;
      check "Stop accepted before registration" (ok (R.cancel runtime ~principal:p run.ids.run_id) = A.Cancel_requested);
      Eio.Promise.resolve release ();
      let a = Eio.Promise.await accepted in
      drain ();
      check "never-started accepted Stop becomes terminal" (state db owner a = A.Cancelled);
      check "cancelled handoff starts no provider or run event"
        (!calls = 0 && match journal db a with [ { payload = A.Run_cancelled; _ } ] -> true | _ -> false))

let () =
  Alcotest.run "advisor runtime"
    [
      ( "native",
        [
          Alcotest.test_case "durable_delivery_and_runtime_deadlines" `Quick durable_delivery_and_runtime_deadlines;
          Alcotest.test_case "silent deadlines" `Quick silent_deadlines;
          Alcotest.test_case "sessions and reconnect" `Quick sessions_and_reconnect;
          Alcotest.test_case "capacity and duplicate starts" `Quick capacity_and_duplicate;
          Alcotest.test_case "expiry and total despite activity" `Quick expiry_and_keepalives;
          Alcotest.test_case "buffer flush cancellation drain deletion and replay" `Quick buffering_drain_delete;
          Alcotest.test_case "persistence failure" `Quick persistence_failure;
          Alcotest.test_case "slow reader and wakeup" `Quick slow_reader_and_wakeup;
          Alcotest.test_case "recovery and unknown start" `Quick recovery_and_unknown_start;
          Alcotest.test_case "tool rounds and fresh segments" `Quick tools_and_fresh_segments;
          Alcotest.test_case "runtime journal reserve" `Quick runtime_journal_reserve;
          Alcotest.test_case "runtime reserve admission" `Quick runtime_reserve_admission;
          Alcotest.test_case "bounded orphan sweep" `Quick orphan_sweep;
          Alcotest.test_case "cancelled in-flight handoff versus sweep" `Quick cancelled_handoff_and_sweep;
          Alcotest.test_case "revocation timer drain and typed subscriber denial" `Quick
            revoked_buffer_and_subscriber_denial;
          Alcotest.test_case "initial durable replay backlog limit" `Quick replay_backlog_limit;
          Alcotest.test_case "atomic terminal cancel ordering" `Quick terminal_cancel_ordering;
          Alcotest.test_case "runtime retry and origin renewal" `Quick runtime_retry_and_origin_renewal;
          Alcotest.test_case "bounded bridge cancelled writer" `Quick bounded_bridge_cancelled_writer;
          Alcotest.test_case "Stop between commit and registration" `Quick stop_between_commit_and_registration;
        ] );
    ]
