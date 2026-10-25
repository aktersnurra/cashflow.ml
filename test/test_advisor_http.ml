open Cashflow_core
open Cashflow_io
open Advisor_test_support
module H = Advisor_http
module W = Cashflow_advisor_protocol.Wire

let ok = function Ok x -> x | Error _ -> Alcotest.fail "unexpected error"
let status = function Some (H.Buffered (s, _, _)) -> s | _ -> 0
let body = function Some (H.Buffered (_, _, b)) -> b | _ -> failwith "buffered expected"

let fixture ?(available = false) ?(configured = false) test =
  Eio_main.run (fun env ->
      let db = Store_sqlite.open_memory () in
      Fun.protect
        ~finally:(fun () -> Sql.close db)
        (fun () ->
          Eio.Switch.run (fun sw ->
              Sql.set_guard db (Db_guard.make ());
              let service = Service.make ~now:(fun () -> now) db in
              let access = Test_service_support.sign_in service "http" in
              let token = Web_auth.create_session db ~now (Access.user access) in
              let csrf = (Option.get (Web_auth.inspect_session service ~now (Web_auth.session_ref token))).csrf_token in
              let calls = ref 0 in
              let limits =
                if configured then
                  Some
                    {
                      Advisor_provider.context_tokens = 262144;
                      completion_tokens = 1024;
                    }
                else None
              in
              let provider =
                Advisor_provider.create ?limits
                  ~post:(fun ~uri:_ ~headers:_ ~body:_ ~on_chunk:_ ->
                    incr calls;
                    if configured then Eio.Time.sleep env#clock 3600.;
                    Error Https.Tls_configuration)
                  ~api_key:"synthetic" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
              in
              let origin = Eio.Time.Mono.now env#mono_clock in
              let clock : Advisor_runtime.clock =
                {
                  now = service.now;
                  monotonic_s =
                    (fun () ->
                      Mtime.span origin (Eio.Time.Mono.now env#mono_clock) |> Mtime.Span.to_float_ns |> fun n ->
                      n /. 1e9);
                  sleep_s = Eio.Time.sleep env#clock;
                }
              in
              let runtime = Advisor_runtime.create ~sw ~service ~provider ~clock in
              let h =
                H.create ~service ~runtime ~public_origin:"https://cashflow.example/base" ~available
                  ~sleep_s:(Eio.Time.sleep env#clock)
              in
              let headers =
                [
                  ("cookie", Web_auth.session_cookie ^ "=" ^ token);
                  ("x-csrf-token", csrf);
                  ("origin", "https://cashflow.example");
                  ("content-type", "application/json");
                ]
              in
              let request ?readiness ?delivery_sleep ?(meth = H.GET) ?(headers = headers) ?(body = "") target =
                let h =
                  match (readiness, delivery_sleep) with
                  | None, None -> h
                  | _ ->
                      H.create ~service ~runtime ~public_origin:"https://cashflow.example/base"
                        ~available:(Option.value readiness ~default:available)
                        ~sleep_s:(Option.value delivery_sleep ~default:(Eio.Time.sleep env#clock))
                in
                let request = { H.meth; target; headers; body } in
                if target = "/api/commands/leave_household" then Some (H.leave_household h request)
                else H.handle h request
              in
              test env service token calls request)))

let admission () =
  fixture (fun _ service token calls req ->
      Alcotest.(check int) "missing cookie" 401 (status (req ~headers:[] "/api/advisor/status"));
      Alcotest.(check int) "status" 200 (status (req "/api/advisor/status"));
      Alcotest.(check bool) "no membership readiness" true
        (Yojson.Safe.Util.member "household" (Yojson.Safe.from_string (body (req "/api/advisor/status"))) = `Null);
      let expires () = (Option.get (Web_auth.inspect_session service ~now (Web_auth.session_ref token))).expires_at in
      Sql.run service.db Web_auth.touch_session now
        (Option.get (Ptime.add_span now (Ptime.Span.of_int_s 60)))
        (Web_auth.hash token);
      let before = expires () in
      List.iter (fun path -> ignore (req path)) [ "/api/advisor/status"; "/api/advisor/conversations" ];
      Alcotest.(check bool) "no touch" true (Ptime.equal before (expires ()));
      let i = Option.get (Web_auth.inspect_session service ~now (Web_auth.session_ref token)) in
      let auth =
        [
          ("cookie", Web_auth.session_cookie ^ "=" ^ token);
          ("x-csrf-token", i.csrf_token);
          ("origin", "https://cashflow.example");
        ]
      in
      Alcotest.(check int) "JSON required" 415 (status (req ~meth:POST ~headers:auth ~body:"{}" "/api/advisor/consent"));
      Alcotest.(check int)
        "cross site rejected" 403
        (status
           (req ~meth:POST
              ~headers:(("sec-fetch-site", "cross-site") :: ("content-type", "application/json") :: auth)
              ~body:"{}" "/api/advisor/consent"));
      Alcotest.(check int)
        "wrong csrf" 403
        (status
           (req ~meth:POST
              ~headers:[ ("cookie", Web_auth.session_cookie ^ "=" ^ token) ]
              ~body:"{}" "/api/advisor/consent"));
      Alcotest.(check int) "consent" 200 (status (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent"));
      Alcotest.(check int) "malformed" 400 (status (req ~meth:POST ~body:"{" "/api/advisor/turns"));
      Alcotest.(check int) "fields" 422 (status (req ~meth:POST ~body:"{}" "/api/advisor/turns"));
      List.iter
        (fun path -> Alcotest.(check int) path 422 (status (req ~meth:POST ~body:"{}" path)))
        [ "/api/advisor/turns/no/retry"; "/api/advisor/runs/no/cancel"; "/api/advisor/conversations/no/delete" ];
      Alcotest.(check int) "bad snapshot id" 422 (status (req "/api/advisor/conversations/no"));
      Alcotest.(check int) "bad run id" 422 (status (req "/api/advisor/runs/no/events"));
      Alcotest.(check int) "method" 405 (status (req ~meth:POST ~body:"{}" "/api/advisor/status"));
      Alcotest.(check int) "GET starts no model" 0 !calls)

let origin_and_fields () =
  fixture (fun _ service token _ req ->
      let csrf = (Option.get (Web_auth.inspect_session service ~now (Web_auth.session_ref token))).csrf_token in
      let base =
        [
          ("cookie", Web_auth.session_cookie ^ "=" ^ token); ("x-csrf-token", csrf); ("content-type", "application/json");
        ]
      in
      List.iter
        (fun origins ->
          Alcotest.(check int)
            "invalid origin" 403
            (status (req ~meth:POST ~headers:(origins @ base) ~body:"{}" "/api/advisor/consent")))
        [
          [];
          [ ("origin", "null") ];
          [ ("origin", "https://cashflow.example:") ];
          [ ("origin", "https://cashflow.example:garbage") ];
          [ ("origin", "https://cashflow.example#") ];
          [ ("origin", "https://evil.example") ];
          [ ("origin", "https://cashflow.example/path") ];
          [ ("origin", "https://cashflow.example"); ("Origin", "https://cashflow.example") ];
          [ ("origin", "https://user@cashflow.example") ];
        ];
      Alcotest.(check int)
        "default port and base path" 200
        (status
           (req
              ~headers:[ ("cookie", Web_auth.session_cookie ^ "=" ^ token); ("origin", "https://CASHFLOW.example:443") ]
              "/api/advisor/status"));
      Alcotest.(check int)
        "valid mutation default port" 200
        (status
           (req ~meth:POST
              ~headers:(("origin", "https://CASHFLOW.example:443") :: base)
              ~body:"{\"enabled\":true}" "/api/advisor/consent"));
      let command =
        W.encode_submit
          {
            W.conversation_id = None;
            scope = Personal;
            currency = { code = "SEK"; scale = 2 };
            locale = "en";
            user_text = "synthetic";
            idempotency_key = "unavailable";
            expected_version = "0";
            source_finding_key = None;
          }
      in
      Alcotest.(check int)
        "unavailable refuses before acceptance" 503
        (status (req ~meth:POST ~body:command "/api/advisor/turns")))

let commands () =
  fixture ~available:true (fun _ service token _ req ->
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let wire : W.submit =
        {
          conversation_id = None;
          scope = Personal;
          currency = { code = "SEK"; scale = 2 };
          locale = "en";
          user_text = "synthetic";
          idempotency_key = "first";
          expected_version = "0";
          source_finding_key = None;
        }
      in
      let send wire = req ~meth:POST ~body:(W.encode_submit wire) "/api/advisor/turns" in
      let extended =
        match Yojson.Safe.from_string (W.encode_submit wire) with
        | `Assoc fields -> Yojson.Safe.to_string (`Assoc (("unexpected", `Bool true) :: fields))
        | _ -> assert false
      in
      Alcotest.(check int) "unknown envelope fields" 422 (status (req ~meth:POST ~body:extended "/api/advisor/turns"));
      let response = send wire in
      Alcotest.(check int) "accept" 202 (status response);
      let a = ok (W.decode_accepted (body response)) in
      let repeat = ok (W.decode_accepted (body (send wire))) in
      Alcotest.(check bool) "same IDs" true (a.ids = repeat.ids && repeat.idempotent_repeat);
      Alcotest.(check int) "exact text conflict" 409 (status (send { wire with user_text = "synthetic " }));
      let snapshot_path = "/api/advisor/conversations/" ^ a.ids.conversation_id in
      Alcotest.(check int) "snapshot" 200 (status (req snapshot_path));
      Alcotest.(check int) "private list" 200 (status (req "/api/advisor/conversations?limit=1&offset=0"));
      let events = "/api/advisor/runs/" ^ a.ids.run_id ^ "/events" in
      Alcotest.(check int) "unauthenticated SSE" 401 (status (req ~headers:[] events));
      Alcotest.(check int) "nondecimal cursor" 422 (status (req (events ^ "?after=01")));
      (match req events with Some (H.Stream _) -> () | _ -> Alcotest.fail "subscription");
      Alcotest.(check int)
        "cancel" 200
        (status (req ~meth:POST ~body:"{}" ("/api/advisor/runs/" ^ a.ids.run_id ^ "/cancel")));
      for _ = 1 to 30 do
        Eio.Fiber.yield ()
      done;
      let retry : W.retry =
        { turn_id = a.ids.turn_id; expected_version = a.accepted_version; idempotency_key = "retry" }
      in
      let retry_path = "/api/advisor/turns/" ^ a.ids.turn_id ^ "/retry" in
      let retry_response = req ~meth:POST ~body:(W.encode_retry retry) retry_path in
      Alcotest.(check int) "retry" 202 (status retry_response);
      Alcotest.(check int)
        "concurrent loser" 409
        (status (req ~meth:POST ~body:(W.encode_retry { retry with idempotency_key = "loser" }) retry_path));
      let accepted_retry = ok (W.decode_accepted (body retry_response)) in
      Alcotest.(check bool)
        "retry repeat" true
        ((ok (W.decode_accepted (body (req ~meth:POST ~body:(W.encode_retry retry) retry_path)))).ids
       = accepted_retry.ids);
      let other = Test_service_support.sign_in service "other" in
      let other_token = Web_auth.create_session service.db ~now (Access.user other) in
      Alcotest.(check int)
        "other owner hidden" 404
        (status (req ~headers:[ ("cookie", Web_auth.session_cookie ^ "=" ^ other_token) ] snapshot_path));
      ignore token;
      ignore (req ~meth:POST ~body:"{\"enabled\":false}" "/api/advisor/consent");
      Alcotest.(check int) "optout blocks history" 403 (status (req snapshot_path));
      Alcotest.(check int)
        "owner deletes after optout" 200
        (status (req ~meth:POST ~body:"{}" (snapshot_path ^ "/delete")));
      Alcotest.(check int) "deleted hidden" 404 (status (req snapshot_path)))

let household_wrappers () =
  fixture ~available:true ~configured:true (fun _ service token _ req ->
      let make_headers token =
        let i = Option.get (Web_auth.inspect_session service ~now (Web_auth.session_ref token)) in
        [
          ("cookie", Web_auth.session_cookie ^ "=" ^ token);
          ("x-csrf-token", i.csrf_token);
          ("origin", "https://cashflow.example");
          ("content-type", "application/json");
        ]
      in
      let admin = Test_service_support.sign_in service ~admin:true "http-admin" in
      let household = ok (Service.create_household service admin ~name:"synthetic" ~default_currency:"SEK") in
      let admin_token = Web_auth.create_session service.db ~now (Access.user admin) in
      let member =
        Option.get
          (Directory.find_user service.db
             (Option.get (Web_auth.inspect_session service ~now (Web_auth.session_ref token))).user_id)
      in
      ignore
        (ok (Service.invite_member service (Service.access_for service (Access.user admin)) ~email:"http@example.com"));
      ignore
        (ok
           (Service.respond_to_invitation service (Service.access_for service member) ~household_id:household.id
              ~accept:true));
      let consent =
        Yojson.Safe.to_string
          (`Assoc [ ("enabled", `Bool true); ("household_id", `String (Household_id.to_string household.id)) ])
      in
      Alcotest.(check int) "member cannot enable" 403 (status (req ~meth:POST ~body:consent "/api/advisor/consent"));
      Alcotest.(check int)
        "admin enable" 200
        (status (req ~meth:POST ~headers:(make_headers admin_token) ~body:consent "/api/advisor/consent"));
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let wire : W.submit =
        {
          conversation_id = None;
          scope = Household (Household_id.to_string household.id);
          currency = { code = "SEK"; scale = 2 };
          locale = "en";
          user_text = "synthetic";
          idempotency_key = "household";
          expected_version = "0";
          source_finding_key = None;
        }
      in
      let a = ok (W.decode_accepted (body (req ~meth:POST ~body:(W.encode_submit wire) "/api/advisor/turns"))) in
      let path = "/api/advisor/conversations/" ^ a.ids.conversation_id in
      let disabled =
        Yojson.Safe.to_string
          (`Assoc [ ("enabled", `Bool false); ("household_id", `String (Household_id.to_string household.id)) ])
      in
      Alcotest.(check int)
        "disable" 200
        (status (req ~meth:POST ~headers:(make_headers admin_token) ~body:disabled "/api/advisor/consent"));
      Alcotest.(check int) "pinned household now denied" 403 (status (req path));
      ignore (req ~meth:POST ~headers:(make_headers admin_token) ~body:consent "/api/advisor/consent");
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let next =
        ok
          (W.decode_accepted
             (body
                (req ~meth:POST ~body:(W.encode_submit { wire with idempotency_key = "leave" }) "/api/advisor/turns")))
      in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      ignore (req ~meth:POST ~headers:(make_headers admin_token) ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let personal_wire = { wire with scope = Personal; idempotency_key = "other-owner-personal" } in
      let personal =
        ok
          (W.decode_accepted
             (body
                (req ~meth:POST ~headers:(make_headers admin_token) ~body:(W.encode_submit personal_wire)
                   "/api/advisor/turns")))
      in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      Sql.exec_script service.db
        "CREATE TRIGGER deny_cancel BEFORE UPDATE OF state ON advisor_runs BEGIN SELECT RAISE(ABORT, 'synthetic'); END";
      Alcotest.(check int)
        "failed leave safe storage error" 503
        (status (req ~meth:POST ~body:"{}" "/api/commands/leave_household"));
      Alcotest.(check int) "failed leave preserves scope" 200 (status (req path));
      let run =
        ok
          (Advisor_store.find_run (Advisor_store.create service.db) ~owner:member.id
             (Option.get (Advisor.Run_id.of_string next.ids.run_id)))
      in
      Alcotest.(check bool)
        "no premature cancellation signal" true
        (match run.state with Running _ -> true | _ -> false);
      Sql.exec_script service.db "DROP TRIGGER deny_cancel";
      Alcotest.(check int) "leave wrapper" 200 (status (req ~meth:POST ~body:"{}" "/api/commands/leave_household"));
      Alcotest.(check int) "leave pins inaccessible scope" 403 (status (req path));
      let personal_run =
        ok
          (Advisor_store.find_run (Advisor_store.create service.db) ~owner:(Access.user admin).id
             (Option.get (Advisor.Run_id.of_string personal.ids.run_id)))
      in
      Alcotest.(check bool)
        "leave preserves other owner Personal" true
        (match personal_run.state with Running _ -> true | _ -> false);
      ignore
        (req ~meth:POST ~headers:(make_headers admin_token) ~body:"{}"
           ("/api/advisor/runs/" ^ personal.ids.run_id ^ "/cancel"));
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let own_personal =
        ok
          (W.decode_accepted
             (body
                (req ~meth:POST
                   ~body:(W.encode_submit { personal_wire with idempotency_key = "own-personal" })
                   "/api/advisor/turns")))
      in
      Directory.set_membership service.db ~now ~household_id:household.id ~user_id:member.id Active;
      Alcotest.(check int)
        "leave with Personal run" 200
        (status (req ~meth:POST ~body:"{}" "/api/commands/leave_household"));
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let own_run =
        ok
          (Advisor_store.find_run (Advisor_store.create service.db) ~owner:member.id
             (Option.get (Advisor.Run_id.of_string own_personal.ids.run_id)))
      in
      Alcotest.(check bool)
        "leave preserves same owner Personal" true
        (match own_run.state with Running _ -> true | _ -> false);
      ignore (req ~meth:POST ~body:"{}" ("/api/advisor/runs/" ^ own_personal.ids.run_id ^ "/cancel"));
      Alcotest.(check int) "owner delete" 200 (status (req ~meth:POST ~body:"{}" (path ^ "/delete"))))

let readiness_repeats () =
  fixture ~available:true ~configured:true (fun _ service _ calls req ->
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let wire : W.submit =
        {
          conversation_id = None;
          scope = Personal;
          currency = { code = "SEK"; scale = 2 };
          locale = "en";
          user_text = "synthetic";
          idempotency_key = "readiness";
          expected_version = "0";
          source_finding_key = None;
        }
      in
      let send ?readiness w = req ?readiness ~meth:POST ~body:(W.encode_submit w) "/api/advisor/turns" in
      let a = ok (W.decode_accepted (body (send wire))) in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      Alcotest.(check int) "one worker" 1 !calls;
      let repeat = ok (W.decode_accepted (body (send ~readiness:false wire))) in
      Alcotest.(check bool) "unavailable repeat original" true (repeat.ids = a.ids && repeat.idempotent_repeat);
      Alcotest.(check int) "conflict wins" 409 (status (send ~readiness:false { wire with user_text = "changed" }));
      Alcotest.(check int)
        "fresh unavailable" 503
        (status (send ~readiness:false { wire with idempotency_key = "fresh" }));
      Alcotest.(check int) "still one worker" 1 !calls;
      let cancel = "/api/advisor/runs/" ^ a.ids.run_id ^ "/cancel" in
      ignore (req ~meth:POST ~body:"{}" cancel);
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      Alcotest.(check int) "terminal repeat" 202 (status (send ~readiness:false wire));
      let retry : W.retry =
        { turn_id = a.ids.turn_id; expected_version = a.accepted_version; idempotency_key = "readiness-retry" }
      in
      let path = "/api/advisor/turns/" ^ a.ids.turn_id ^ "/retry" in
      let r = ok (W.decode_accepted (body (req ~meth:POST ~body:(W.encode_retry retry) path))) in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let repeated =
        ok (W.decode_accepted (body (req ~readiness:false ~meth:POST ~body:(W.encode_retry retry) path)))
      in
      Alcotest.(check bool) "retry canonical unavailable" true (r.ids = repeated.ids);
      Alcotest.(check int) "no extra worker" 2 !calls;
      ignore (req ~meth:POST ~body:"{\"enabled\":false}" "/api/advisor/consent");
      ignore service)

let fake_connection () =
  let reqd = ref None in
  let connection =
    Httpun.Server_connection.create (fun r ->
        Httpun.Body.Reader.close (Httpun.Reqd.request_body r);
        reqd := Some r)
  in
  let text = "GET / HTTP/1.1\r\nHost: synthetic.invalid\r\n\r\n" in
  let b = Bigstringaf.of_string ~off:0 ~len:(String.length text) text in
  ignore (Httpun.Server_connection.read connection b ~off:0 ~len:(String.length text));
  (connection, Option.get !reqd)

let compacted_resync () =
  fixture ~available:true (fun _ service _ _ req ->
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let wire : W.submit =
        {
          conversation_id = None;
          scope = Personal;
          currency = { code = "SEK"; scale = 2 };
          locale = "en";
          user_text = "synthetic";
          idempotency_key = "pruned";
          expected_version = "0";
          source_finding_key = None;
        }
      in
      let a = ok (W.decode_accepted (body (req ~meth:POST ~body:(W.encode_submit wire) "/api/advisor/turns"))) in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let later = Option.get (Ptime.add_span now (Ptime.Span.of_int_s (2 * 86400))) in
      Advisor_store.prune (Advisor_store.create service.db) ~now:later;
      let subscription =
        match req ("/api/advisor/runs/" ^ a.ids.run_id ^ "/events?after=0") with
        | Some (H.Stream s) -> s
        | _ -> Alcotest.fail "stream"
      in
      let connection, reqd = fake_connection () in
      Eio.Switch.run (fun sw ->
          Eio.Fiber.fork ~sw (fun () -> ignore (H.stream subscription reqd));
          Eio.Fiber.yield ();
          match Httpun.Server_connection.next_write_operation connection with
          | `Write buffers ->
              let text =
                String.concat ""
                  (List.map (fun b -> Bigstringaf.substring b.Httpun.IOVec.buffer ~off:b.off ~len:b.len) buffers)
              in
              Alcotest.(check bool)
                "typed resync" true
                (List.mem "event: resync_required" (String.split_on_char '\n' text));
              Httpun.Server_connection.report_write_result connection (`Ok (String.length text))
          | _ -> Alcotest.fail "resync frame"))

let delivery_write_deadline ~flush () =
  fixture ~available:true (fun _ service _ _ req ->
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let wire : W.submit =
        {
          conversation_id = None;
          scope = Personal;
          currency = { code = "SEK"; scale = 2 };
          locale = "en";
          user_text = "synthetic";
          idempotency_key = "timeout";
          expected_version = "0";
          source_finding_key = None;
        }
      in
      let a = ok (W.decode_accepted (body (req ~meth:POST ~body:(W.encode_submit wire) "/api/advisor/turns"))) in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let deadline, expire = Eio.Promise.create () in
      let quiet, _ = Eio.Promise.create () in
      let write_timer = ref false and timer_closed = ref false and watcher_closed = ref false in
      let delivery_sleep seconds =
        Alcotest.(check bool) "actual DB guard free during waits" false ((Sql.unguarded service.db).held ());
        if seconds = 30. then (
          write_timer := true;
          Fun.protect ~finally:(fun () -> timer_closed := true) (fun () -> Eio.Promise.await deadline))
        else (
          Alcotest.(check (float 0.001)) "independent watcher" 0.05 seconds;
          Fun.protect ~finally:(fun () -> watcher_closed := true) (fun () -> Eio.Promise.await quiet))
      in
      let subscription =
        match req ~delivery_sleep ("/api/advisor/runs/" ^ a.ids.run_id ^ "/events") with
        | Some (H.Stream s) -> s
        | _ -> Alcotest.fail "stream"
      in
      let connection, reqd = fake_connection () in
      Eio.Switch.run (fun sw ->
          let result = ref None in
          Eio.Fiber.fork ~sw (fun () -> result := Some (H.stream subscription reqd));
          for _ = 1 to 20 do
            Eio.Fiber.yield ()
          done;
          Alcotest.(check bool) "per-write timer started" true !write_timer;
          Alcotest.(check bool) "flush pending before clock advance" true (!result = None);
          if flush then
            match Httpun.Server_connection.next_write_operation connection with
            | `Write buffers ->
                Httpun.Server_connection.report_write_result connection
                  (`Ok (List.fold_left (fun n b -> n + b.Httpun.IOVec.len) 0 buffers))
            | _ -> Alcotest.fail "queued response"
          else Eio.Promise.resolve expire ();
          for _ = 1 to 20 do
            Eio.Fiber.yield ()
          done;
          Alcotest.(check bool) "typed delivery outcome" true (!result = Some (if flush then H.Drained else H.Aborted));
          Alcotest.(check bool) "write timer cleaned up" true !timer_closed;
          Alcotest.(check bool) "watcher cleaned up" true !watcher_closed))

let protocol_drain_deadline () =
  Eio_main.run (fun _ ->
      let aborts = ref 0 in
      let deadline, expire = Eio.Promise.create () in
      Eio.Switch.run (fun sw ->
          Http_server.finish_stream ~sw
            ~sleep_s:(fun seconds ->
              Alcotest.(check (float 0.001)) "protocol-only one-second bound" 1. seconds;
              Eio.Promise.await deadline)
            ~abort:(fun () -> incr aborts)
            H.Drained;
          Eio.Fiber.yield ();
          Alcotest.(check int) "normal drain not hard-cancelled" 0 !aborts;
          Eio.Promise.resolve expire ();
          for _ = 1 to 10 do
            Eio.Fiber.yield ()
          done;
          Alcotest.(check int) "stalled drain bounded" 1 !aborts);
      let cancelled = ref false in
      Eio.Switch.run (fun sw ->
          let never, _ = Eio.Promise.create () in
          Http_server.finish_stream ~sw
            ~sleep_s:(fun _ -> Fun.protect ~finally:(fun () -> cancelled := true) (fun () -> Eio.Promise.await never))
            ~abort:(fun () -> incr aborts)
            H.Drained;
          Eio.Fiber.yield ());
      Alcotest.(check bool) "normal connection completion removes timer" true !cancelled;
      Alcotest.(check int) "no timeout after normal completion" 1 !aborts)

let writer_gate () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let _, denied = fake_connection () in
          Alcotest.(check bool)
            "denied before headers" true
            (Result.is_error (Http_sse.open_response_checked ~authorize:(fun () -> false) denied));
          Alcotest.(check bool) "no HTTP 200" true (Httpun.Reqd.response denied = None);
          let connection, reqd = fake_connection () in
          let authorized = ref true in
          let writer = ok (Http_sse.open_response_checked ~authorize:(fun () -> !authorized) reqd) in
          let finished = ref false in
          Eio.Fiber.fork ~sw (fun () ->
              Alcotest.(check bool)
                "loss during flush" true
                (Result.is_error
                   (Http_sse.write_frame writer { id = Some "1"; event = Some "advisor"; data = "synthetic" }));
              finished := true);
          Eio.Fiber.yield ();
          Alcotest.(check bool) "real flush pending" false !finished;
          authorized := false;
          (match Httpun.Server_connection.next_write_operation connection with
          | `Write buffers ->
              Httpun.Server_connection.report_write_result connection
                (`Ok (List.fold_left (fun sum b -> sum + b.Httpun.IOVec.len) 0 buffers))
          | _ -> Alcotest.fail "queued frame");
          for _ = 1 to 10 do
            Eio.Fiber.yield ()
          done;
          Alcotest.(check bool) "flush completed denied" true !finished;
          Alcotest.(check bool) "no heartbeat after loss" true (Result.is_error (Http_sse.heartbeat writer));
          Http_sse.close writer))

let stalled_delivery ~expiry () =
  fixture ~available:true (fun env service token _ req ->
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let wire : W.submit =
        {
          conversation_id = None;
          scope = Personal;
          currency = { code = "SEK"; scale = 2 };
          locale = "en";
          user_text = "synthetic";
          idempotency_key = "stalled";
          expected_version = "0";
          source_finding_key = None;
        }
      in
      let a = ok (W.decode_accepted (body (req ~meth:POST ~body:(W.encode_submit wire) "/api/advisor/turns"))) in
      for _ = 1 to 40 do
        Eio.Fiber.yield ()
      done;
      let subscription =
        match req ("/api/advisor/runs/" ^ a.ids.run_id ^ "/events") with
        | Some (H.Stream s) -> s
        | _ -> Alcotest.fail "stream"
      in
      let _, reqd = fake_connection () in
      Eio.Switch.run (fun sw ->
          let finished = ref false in
          Eio.Fiber.fork ~sw (fun () ->
              ignore (H.stream subscription reqd);
              finished := true);
          for _ = 1 to 20 do
            Eio.Fiber.yield ()
          done;
          Alcotest.(check bool) "writer is stalled" false !finished;
          if expiry then Sql.run service.db Web_auth.touch_session now Ptime.epoch (Web_auth.hash token)
          else ignore (req ~meth:POST ~body:"{\"enabled\":false}" "/api/advisor/consent");
          Eio.Time.sleep env#clock 0.075;
          Alcotest.(check bool) "expiry cancels stalled flush within polling bound" true !finished))

let household_readiness () =
  fixture (fun _ service _ calls req ->
    let open Yojson.Safe.Util in
    let readiness () = member "household" (Yojson.Safe.from_string (body (req "/api/advisor/status"))) in
    Alcotest.(check bool) "no household" true (readiness () = `Null);
    let admin = Test_service_support.sign_in service ~admin:true "http" in
    let h = Test_service_support.ok (Service.create_household service admin ~name:"Synthetic" ~default_currency:"SEK") in
    let state = readiness () in
    Alcotest.(check string) "current membership only" (Household_id.to_string h.id) (state |> member "household_id" |> to_string);
    Alcotest.(check bool) "default household disabled" false (state |> member "enabled" |> to_bool);
    Alcotest.(check bool) "admin can manage" true (state |> member "can_manage" |> to_bool);
    ignore (req ~meth:POST ~body:(Printf.sprintf "{\"enabled\":true,\"household_id\":%S}" (Household_id.to_string h.id)) "/api/advisor/consent");
    Alcotest.(check bool) "explicit enable reflected" true (readiness () |> member "enabled" |> to_bool);
    let member_access = Test_service_support.sign_in service ~admin:false "http" in
    Alcotest.(check bool) "ordinary member cannot manage" false (readiness () |> member "can_manage" |> to_bool);
    Alcotest.(check int) "member cannot mutate" 403
      (status (req ~meth:POST ~body:(Printf.sprintf "{\"enabled\":false,\"household_id\":%S}" (Household_id.to_string h.id)) "/api/advisor/consent"));
    ignore (Test_service_support.ok (Service.leave_household service member_access));
    Alcotest.(check bool) "removed membership not exposed" true (readiness () = `Null);
    let admin = Test_service_support.sign_in service ~admin:true "http" in
    let next = Test_service_support.ok (Service.create_household service admin ~name:"Other synthetic" ~default_currency:"SEK") in
    let state = readiness () in
    Alcotest.(check string) "changed membership revalidated" (Household_id.to_string next.id) (state |> member "household_id" |> to_string);
    Alcotest.(check bool) "new household not enabled by old switch" false (state |> member "enabled" |> to_bool);
    Alcotest.(check int) "status never calls provider" 0 !calls)

let session_currency_metadata () =
  fixture (fun _ service _ _ _ ->
    let admin = Test_service_support.sign_in service ~admin:true "http" in
    Sql.exec_script service.db "UPDATE currencies SET scale=0 WHERE code='SEK'; UPDATE currencies SET scale=4 WHERE code='EUR'";
    let session () = Commands.session_json service (Service.access_for service (Access.user admin)) in
    let open Yojson.Safe.Util in
    let metadata () = session () |> member "currency_metadata" |> to_list in
    let scale code = List.find (fun json -> json |> member "code" |> to_string = code) (metadata ()) |> member "scale" |> to_int in
    Alcotest.(check int) "registry scale zero" 0 (scale "SEK");
    Alcotest.(check string) "existing default string unchanged" "SEK" (session () |> member "user" |> member "default_currency" |> to_string);
    ignore (Test_service_support.ok (Service.create_household service admin ~name:"Same currency" ~default_currency:"SEK"));
    Alcotest.(check int) "same-code defaults deduplicated" 1 (List.length (metadata ()));
    ignore (Test_service_support.ok (Service.leave_household service (Service.access_for service (Access.user admin))));
    ignore (Test_service_support.ok (Service.create_household service (Service.access_for service (Access.user admin)) ~name:"Different currency" ~default_currency:"EUR"));
    Alcotest.(check int) "different defaults" 2 (List.length (metadata ()));
    Alcotest.(check int) "household registry scale four" 4 (scale "EUR");
    ignore (Test_service_support.ok (Service.leave_household service (Service.access_for service (Access.user admin))));
    Alcotest.(check int) "lost membership metadata removed" 1 (List.length (metadata ())))

let history_titles () =
  fixture (fun _ service token calls req ->
      let open Yojson.Safe.Util in
      ignore (req ~meth:POST ~body:"{\"enabled\":true}" "/api/advisor/consent");
      let admin = Test_service_support.sign_in service ~admin:true "http" in
      let owner = (Access.user admin).id in
      let household = ok (Service.create_household service admin ~name:"Synthetic history" ~default_currency:"SEK") in
      let consent enabled = Printf.sprintf "{\"enabled\":%b,\"household_id\":%S}" enabled (Household_id.to_string household.id) in
      ignore (req ~meth:POST ~body:(consent true) "/api/advisor/consent");
      let store = Advisor_store.create ~clock:service.now service.db in
      let add owner session scope key text =
        let accepted = ok (Advisor_store.accept store ~owner ~session ~now ~model:"synthetic" ~source_finding:None
          {(submit key) with scope; user_text=text}) in
        ignore (ok (Advisor_store.transition store ~run:accepted.ids.run_id
          (Advisor.Fail {code=Provider_unavailable; context=No_context})));
        accepted in
      let reference = Web_auth.session_ref token in
      let rocket n = String.concat "" (List.init n (fun _ -> "🚀")) in
      let cases = ["  Plan\tmy\n holiday  ", "Plan my holiday";
        rocket 81, rocket 80 ^ "…";
        String.make 80 'a' ^ " \n\t", String.make 80 'a';
        "<img src=x onerror=evil()>", "<img src=x onerror=evil()>";
        "Budget för februari", "Budget för februari"] in
      let expected = List.mapi (fun index (text, title) ->
        let accepted = add owner reference Advisor.Personal ("history-" ^ string_of_int index) text in
        (Advisor.Conversation_id.to_string accepted.ids.conversation_id, title)) cases in
      let household_chat = add owner reference (Advisor.Household household.id) "household-history" "HOUSEHOLD_PRIVATE_TITLE" in
      let other = Test_service_support.sign_in service "history-other" |> Access.user in
      let other_token = Web_auth.create_session service.db ~now other in
      ignore (add other.id (Web_auth.session_ref other_token) Advisor.Personal "other-history" "OTHER_OWNER_PRIVATE_TITLE");
      let read path = Yojson.Safe.from_string (body (req path)) in
      let rows = read "/api/advisor/conversations?limit=100" |> member "conversations" |> to_list in
      Alcotest.(check int) "only owned authorized rows" (List.length cases + 1) (List.length rows);
      List.iter (fun (id, title) ->
        let row = List.find (fun row -> row |> member "conversation_id" |> to_string = id) rows in
        Alcotest.(check string) "deterministic first-message title" (Yojson.Safe.to_string (`String title))
          (Yojson.Safe.to_string (row |> member "title"));
        Alcotest.(check string) "personal scope" "{\"type\":\"Personal\"}"
          (Yojson.Safe.to_string (row |> member "scope"))) expected;
      let household_id = Advisor.Conversation_id.to_string household_chat.ids.conversation_id in
      let household_row = List.find (fun row -> row |> member "conversation_id" |> to_string = household_id) rows in
      Alcotest.(check string) "authorized household title" "HOUSEHOLD_PRIVATE_TITLE" (household_row |> member "title" |> to_string);
      Sql.(run service.db (stmt "UPDATE advisor_conversations SET updated_at=? WHERE id=?" (text @-> text @-> nil) exec))
        (Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 (Option.get (Ptime.add_span now (Ptime.Span.of_int_s 60)))) household_id;
      ignore (req ~meth:POST ~body:(consent false) "/api/advisor/consent");
      let page = read "/api/advisor/conversations?limit=1" in
      Alcotest.(check int) "revoked title filtered" 0 (List.length (page |> member "conversations" |> to_list));
      Alcotest.(check int) "cursor passes filtered owner row" 1 (page |> member "next_offset" |> to_int);
      let page = read "/api/advisor/conversations?limit=100&offset=1" in
      Alcotest.(check int) "remaining personal titles still available" (List.length cases)
        (List.length (page |> member "conversations" |> to_list));
      Alcotest.(check bool) "end cursor" true (page |> member "next_offset" = `Null);
      Alcotest.(check int) "history titles do not call model" 0 !calls)

let () =
  Alcotest.run "advisor_http"
    [
      ( "adapter",
        [
          Alcotest.test_case "private first-question history titles" `Quick history_titles;
          Alcotest.test_case "authoritative session currency metadata" `Quick session_currency_metadata;
          Alcotest.test_case "current household readiness" `Quick household_readiness;
          Alcotest.test_case "unavailable canonical repeats" `Quick readiness_repeats;
          Alcotest.test_case "compacted cursor resync" `Quick compacted_resync;
          Alcotest.test_case "household admin consent" `Quick household_wrappers;
          Alcotest.test_case "write deadline" `Quick (delivery_write_deadline ~flush:false);
          Alcotest.test_case "successful write cancels deadline" `Quick (delivery_write_deadline ~flush:true);
          Alcotest.test_case "protocol drain deadline" `Quick protocol_drain_deadline;
          Alcotest.test_case "writer bound gate" `Quick writer_gate;
          Alcotest.test_case "stalled delivery expiry" `Quick (stalled_delivery ~expiry:true);
          Alcotest.test_case "stalled delivery revocation" `Quick (stalled_delivery ~expiry:false);
          Alcotest.test_case "authenticated admission" `Quick admission;
          Alcotest.test_case "origin and unavailable" `Quick origin_and_fields;
          Alcotest.test_case "commands idempotency retry deletion" `Quick commands;
        ] );
    ]
