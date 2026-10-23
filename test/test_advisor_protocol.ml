module A = Cashflow_core.Advisor
module W = Cashflow_advisor_protocol.Wire
module C = Cashflow_io.Advisor_codec

let check = Alcotest.(check bool)
let failure : A.error = { code = Server_restarted; context = No_context }

let transitions =
  [
    A.Start;
    Set_phase Streaming_text;
    Set_phase Running_tools;
    Request_cancel;
    Finish;
    Fail failure;
    Confirm_cancel;
    Recover_interrupted;
  ]

let terminal_transitions () =
  List.iter
    (fun state ->
      List.iter
        (fun transition -> check "terminal immutable" true (Result.is_error (A.step state transition)))
        transitions)
    [ A.Completed; Cancelled; Failed failure ];
  check "cancel before start" true (A.step Queued Request_cancel = Ok Cancel_requested);
  check "cannot start cancelled request" true (Result.is_error (A.step Cancel_requested Start));
  List.iter
    (fun state -> check "recover unfinished" true (A.step state Recover_interrupted = Ok (A.Failed failure)))
    [ A.Queued; Running Awaiting_model; Running Streaming_text; Running Running_tools; Cancel_requested ];
  check "complete" true (A.step (Running Streaming_text) Finish = Ok Completed);
  check "confirm cancel" true (A.step Cancel_requested Confirm_cancel = Ok Cancelled);
  check "fail" true (A.step Cancel_requested (Fail failure) = Ok (Failed failure));
  Alcotest.(check int) "global runs" 8 A.limits.active_runs;
  Alcotest.(check int) "run text bytes" 65536 A.limits.output_bytes;
  List.iter
    (fun (name, expected, actual) -> Alcotest.(check int) name expected actual)
    [
      ("per user", 1, A.limits.active_runs_per_user);
      ("per conversation", 1, A.limits.active_runs_per_conversation);
      ("user bytes", 8192, A.limits.user_bytes);
      ("rounds", 4, A.limits.model_rounds);
      ("tools", 8, A.limits.tool_calls);
      ("tool result", 16384, A.limits.tool_result_bytes);
      ("evidence", 65536, A.limits.evidence_bytes);
      ("context turns", 12, A.limits.context_turns);
      ("context bytes", 65536, A.limits.context_bytes);
      ("deadline", 120, A.limits.run_deadline_seconds);
      ("inactivity", 30, A.limits.upstream_inactivity_seconds);
      ("frame", 262144, A.limits.frame_bytes);
      ("backlog", 262144, A.limits.subscriber_bytes);
      ("journal", 1048576, A.limits.journal_bytes);
      ("replay retention", 86400, A.limits.replay_retention_seconds);
      ("subscriber grace", 30, A.limits.no_subscriber_seconds);
      ("flush ms", 100, A.limits.flush_milliseconds);
      ("flush bytes", 4096, A.limits.flush_bytes);
      ("render ms", 40, A.limits.render_milliseconds);
    ]

let%hegel_test terminal_sequences tc =
  let state = ref A.Queued in
  for _ = 1 to Hegel.draw tc (Hegel.integers ~min_value:1 ~max_value:200 ()) do
    let transition = List.nth transitions (Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:7 ())) in
    let result = A.step !state transition in
    if A.is_terminal !state then assert (Result.is_error result);
    match result with Ok next -> state := next | Error _ -> ()
  done
[@@settings Hegel.Settings.create ~test_cases:300 ()]

let uuid = "00000000-0000-4000-8000-000000000001"

let ids : W.identities =
  {
    conversation_id = uuid;
    turn_id = uuid;
    attempt_id = uuid;
    user_message_id = uuid;
    assistant_message_id = uuid;
    run_id = uuid;
  }

let currency : W.currency = { code = "SEK"; scale = 2 }
let money : W.money = { decimal = "9007199254740993.01"; currency }

let evidence : W.evidence =
  {
    id = uuid;
    scope = Personal;
    currency;
    sampled_at = "2026-10-22T12:00:00Z";
    title = "Synthetic balance";
    facts = [ { label = "Balance"; value = Money money } ];
    source_references = [ "account_1" ];
    import_status = Incomplete;
    reconciled = false;
    assumptions = [ "Synthetic estimate" ];
  }

let error : W.error = { code = Run_limit_reached; context = Limit Output_bytes }
let event payload : W.event = { ids; sequence = "9007199254740993"; payload }

let submit : W.submit =
  {
    conversation_id = None;
    scope = Household uuid;
    currency;
    locale = "sv";
    user_text = "Synthetic question";
    idempotency_key = "test-key";
    expected_version = "0";
    source_finding_key = Some "synthetic-finding";
  }

let snapshot : W.snapshot =
  {
    conversation_id = uuid;
    scope = Personal;
    currency;
    locale = "en";
    source_finding =
      Some
        {
          kind = "unusual_spending";
          key = "synthetic";
          scope = Personal;
          title = "Synthetic finding";
          summary = "Synthetic summary";
          sampled_at = "2026-10-22T12:00:00Z";
        };
    turns =
      [
        {
          turn_id = uuid;
          user_message_id = uuid;
          user_text = "Synthetic question";
          attempts =
            [
              {
                ids;
                ordinal = "1";
                state = Failed error;
                segments = [ Text_segment { segment_id = uuid; text = "Partial" }; Evidence_segment evidence ];
                last_sequence = "9007199254740993";
              };
            ];
        };
      ];
    active_run = None;
    version = "2";
  }

let ok = function Ok v -> v | Error _ -> Alcotest.fail "unexpected codec error"

let roundtrips () =
  List.iter
    (fun payload ->
      let e = event payload in
      check "event roundtrip" true (W.decode_event (W.encode_event e) = Ok e))
    [
      W.Run_started;
      Text_appended { segment_id = uuid; text = "Hej 🐈\n世界" };
      Tool_started "Checking bills";
      Tool_finished { label = "Checking bills"; outcome = Succeeded };
      Evidence_added evidence;
      Run_completed;
      Run_cancelled;
      Run_failed error;
    ];
  check "request roundtrip" true (W.decode_submit (W.encode_submit submit) = Ok submit);
  let retry : W.retry = { turn_id = uuid; expected_version = "2"; idempotency_key = "retry" } in
  check "retry roundtrip" true (W.decode_retry (W.encode_retry retry) = Ok retry);
  let accepted : W.accepted = { ids; accepted_version = "1"; idempotent_repeat = true } in
  check "accepted roundtrip" true (W.decode_accepted (W.encode_accepted accepted) = Ok accepted);
  check "snapshot roundtrip" true (W.decode_snapshot (W.encode_snapshot snapshot) = Ok snapshot);
  let decoded = ok (W.decode_event (W.encode_event (event (Evidence_added evidence)))) in
  (match decoded.payload with
  | Evidence_added e -> (
      match (List.hd e.facts).value with
      | Money m -> Alcotest.(check string) "large money preserved" "9007199254740993.01" m.decimal
      | _ -> Alcotest.fail "money missing")
  | _ -> Alcotest.fail "evidence missing");
  List.iter
    (fun state ->
      let turn : W.turn = List.hd snapshot.turns in
      let attempt : W.attempt = List.hd turn.attempts in
      let value = { snapshot with turns = [ { turn with attempts = [ { attempt with state } ] } ] } in
      check "snapshot states" true (W.decode_snapshot (W.encode_snapshot value) = Ok value))
    [
      W.Queued;
      Running Awaiting_model;
      Running Streaming_text;
      Running Running_tools;
      Cancel_requested;
      Completed;
      Cancelled;
      Failed error;
    ];
  check "native submit" true (Result.is_ok (C.submit submit));
  check "native retry" true (Result.is_ok (C.retry retry));
  let native_ids : A.identities =
    {
      conversation_id = Option.get (A.Conversation_id.of_string uuid);
      turn_id = Option.get (A.Turn_id.of_string uuid);
      attempt_id = Option.get (A.Attempt_id.of_string uuid);
      user_message_id = Option.get (A.Message_id.of_string uuid);
      assistant_message_id = Option.get (A.Message_id.of_string uuid);
      run_id = Option.get (A.Run_id.of_string uuid);
    }
  in
  let native_evidence : A.evidence =
    {
      id = Option.get (A.Evidence_id.of_string uuid);
      scope = Personal;
      currency = Result.get_ok (Cashflow_core.Currency.make ~code:"SEK" ~scale:2);
      sampled_at = Result.get_ok (Cashflow_core.Time.parse_offset_datetime "2026-10-22T12:00:00Z");
      title = evidence.title;
      facts =
        [
          {
            label = "Balance";
            value =
              Money
                {
                  minor_units = 900719925474099301L;
                  currency = Result.get_ok (Cashflow_core.Currency.make ~code:"SEK" ~scale:2);
                };
          };
        ];
      source_references = evidence.source_references;
      import_status = Incomplete;
      reconciled = false;
      assumptions = evidence.assumptions;
    }
  in
  List.iter
    (fun (native_payload, wire_payload) ->
      check "native public event" true
        (C.event { ids = native_ids; sequence = 9007199254740993L; payload = native_payload } = event wire_payload))
    [
      (A.Run_started, W.Run_started);
      ( A.Text_appended { segment_id = Option.get (A.Segment_id.of_string uuid); text = "Synthetic" },
        W.Text_appended { segment_id = uuid; text = "Synthetic" } );
      (A.Tool_started "Checking", W.Tool_started "Checking");
      ( A.Tool_finished { label = "Checking"; outcome = Succeeded },
        W.Tool_finished { label = "Checking"; outcome = Succeeded } );
      (A.Run_completed, W.Run_completed);
      (A.Run_cancelled, W.Run_cancelled);
      (A.Run_failed failure, W.Run_failed { code = Server_restarted; context = No_context });
    ];
  check "native evidence projection" true
    (C.event { ids = native_ids; sequence = 9007199254740993L; payload = Evidence_added native_evidence }
    = event (Evidence_added evidence));
  List.iter
    (fun (minor_units, decimal) ->
      let e =
        {
          native_evidence with
          facts = [ { label = "Balance"; value = Money { minor_units; currency = native_evidence.currency } } ];
        }
      in
      let projected = C.event { ids = native_ids; sequence = 1L; payload = Evidence_added e } in
      check "signed int64 money decodes" true (Result.is_ok (W.decode_event (W.encode_event projected)));
      match projected.payload with
      | Evidence_added e -> (
          match (List.hd e.facts).value with
          | Money m -> Alcotest.(check string) "signed money exact" decimal m.decimal
          | _ -> assert false)
      | _ -> assert false)
    [ (Int64.min_int, "-92233720368547758.08"); (Int64.max_int, "92233720368547758.07"); (0L, "0.00"); (-1L, "-0.01") ];
  check "native accepted projection" true
    (C.accepted { ids = native_ids; accepted_version = 1L; idempotent_repeat = true } = accepted);
  let native_snapshot : A.snapshot =
    {
      conversation_id = native_ids.conversation_id;
      scope = Personal;
      currency = native_evidence.currency;
      locale = "en";
      source_finding = None;
      turns = [];
      active_run = Some native_ids.run_id;
      version = 2L;
    }
  in
  check "native snapshot projection" true
    (Result.is_ok (W.decode_snapshot (W.encode_snapshot (C.snapshot native_snapshot))))

let unsafe_values () =
  let reject name s = check name true (Result.is_error (W.decode_event s)) in
  List.iter
    (fun sequence -> reject "unsafe counter" (W.encode_event { (event Run_started) with sequence }))
    [ "-1"; "01"; "1.0"; "1e3"; "9223372036854775808"; "0" ];
  List.iter
    (fun id -> reject "malformed UUID" (W.encode_event { (event Run_started) with ids = { ids with run_id = id } }))
    [ "not-a-uuid"; ""; "00000000-0000-4000-8000-00000000000z" ];
  List.iter (reject "invalid json/envelope")
    [
      "{";
      "null";
      "[]";
      "{\"version\":2,\"data\":{}}";
      "{\"version\":\"1\",\"data\":{}}";
      "{\"version\":1,\"version\":1,\"data\":{}}";
      "{\"version\":1,\"data\":{}}";
    ];
  check "unknown version typed" true (W.decode_event "{\"version\":2,\"data\":{}}" = Error W.Unknown_version);
  let json = Yojson.Safe.from_string (W.encode_event (event Run_started)) in
  let mutate_payload payload =
    match json with
    | `Assoc envelope ->
        let data = List.assoc "data" envelope in
        let data =
          match data with
          | `Assoc fs -> `Assoc (("payload", payload) :: List.remove_assoc "payload" fs)
          | _ -> assert false
        in
        Yojson.Safe.to_string (`Assoc (("data", data) :: List.remove_assoc "data" envelope))
    | _ -> assert false
  in
  reject "unknown payload" (mutate_payload (`Assoc [ ("type", `String "Reasoning") ]));
  reject "missing delta text"
    (mutate_payload (`Assoc [ ("type", `String "Text_appended"); ("value", `Assoc [ ("segment_id", `String uuid) ]) ]));
  List.iter
    (fun decimal ->
      reject "unsafe money"
        (W.encode_event
           (event
              (Evidence_added { evidence with facts = [ { label = "Bad"; value = Money { money with decimal } } ] }))))
    [ "NaN"; "1e12"; "1.001"; "01.00"; "-0.00"; "92233720368547758.08" ];
  List.iter
    (fun sampled_at ->
      reject "invalid sampled time" (W.encode_event (event (Evidence_added { evidence with sampled_at }))))
    [ "not-a-time"; "2026-02-30T12:00:00Z"; "2026-10-22T24:00:00Z" ];
  reject "invalid display date"
    (W.encode_event
       (event (Evidence_added { evidence with facts = [ { label = "Date"; value = Date "2026-02-30" } ] })));
  let eur : W.currency = { code = "EUR"; scale = 2 } in
  reject "mixed currencies" (W.encode_event (event (Evidence_added { evidence with currency = eur })));
  check "invalid inbound UUID" true (Result.is_error (C.submit { submit with scope = Household "secret" }));
  List.iter
    (fun s -> check "unsafe submit" true (Result.is_error (W.decode_submit (W.encode_submit s))))
    [
      { submit with expected_version = "-1" };
      { submit with expected_version = "1" };
      { submit with locale = "xx" };
      { submit with user_text = String.make 8193 'x' };
      { submit with user_text = "" };
      { submit with idempotency_key = "bad\nkey" };
      { submit with currency = { code = "sek"; scale = 2 } };
    ];
  check "snapshot scope mismatch" true
    (Result.is_error (W.decode_snapshot (W.encode_snapshot { snapshot with scope = Household uuid })));
  check "snapshot currency mismatch" true
    (Result.is_error (W.decode_snapshot (W.encode_snapshot { snapshot with currency = eur })));
  check "invalid UTF-8" true
    (Result.is_error (W.decode_event (W.encode_event (event (Text_appended { segment_id = uuid; text = "\xff" })))));
  check "empty display label" true (Result.is_error (W.decode_event (W.encode_event (event (Tool_started "")))))

let timestamp_seconds () =
  List.iter
    (fun suffix ->
      List.iter
        (fun (seconds, valid) ->
          let sampled_at = "2026-10-22T12:34:" ^ seconds ^ suffix in
          let check_timestamp name decode encode value =
            let expected = if valid then Ok value else Error (W.Invalid_field "timestamp") in
            check (name ^ " " ^ sampled_at) true (decode (encode value) = expected)
          in
          let evidence = { evidence with sampled_at } in
          check_timestamp "event evidence" W.decode_event W.encode_event (event (Evidence_added evidence));
          let finding = { (Option.get snapshot.source_finding) with sampled_at } in
          check_timestamp "snapshot finding" W.decode_snapshot W.encode_snapshot
            { snapshot with source_finding = Some finding };
          let turns =
            List.map
              (fun (turn : W.turn) ->
                let attempts =
                  List.map
                    (fun (attempt : W.attempt) -> { attempt with segments = [ Evidence_segment evidence ] })
                    turn.attempts
                in
                { turn with attempts })
              snapshot.turns
          in
          check_timestamp "snapshot evidence" W.decode_snapshot W.encode_snapshot { snapshot with turns })
        [ ("59", true); ("60", false) ])
    [ "Z"; "+02:00"; "-05:30"; ".123Z"; ".123+02:00"; ".123-05:30" ]

let%hegel_test total_decoders tc =
  let length = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:1000 ()) in
  let text = String.init length (fun _ -> Char.chr (Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:255 ()))) in
  ignore (W.decode_event text);
  ignore (W.decode_submit text);
  ignore (W.decode_retry text);
  ignore (W.decode_accepted text);
  ignore (W.decode_snapshot text)
[@@settings Hegel.Settings.create ~test_cases:300 ()]

let () =
  Alcotest.run "advisor protocol"
    [
      ( "terminal_transitions_and_wire_roundtrip",
        [
          Alcotest.test_case "transitions" `Quick terminal_transitions;
          Alcotest.test_case "Hegel terminal sequences (300 cases)" `Quick terminal_sequences;
          Alcotest.test_case "wire roundtrips and native projection" `Quick roundtrips;
          Alcotest.test_case "unsafe values" `Quick unsafe_values;
          Alcotest.test_case "timestamp seconds subset" `Quick timestamp_seconds;
          Alcotest.test_case "Hegel total decoders (300 cases)" `Quick total_decoders;
        ] );
    ]
