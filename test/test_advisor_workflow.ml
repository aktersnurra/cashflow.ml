open Cashflow_core
module A = Advisor
module E = Cashflow_workflow.Advisor_effects
module W = Cashflow_workflow.Advisor_workflow
module T = Cashflow_workflow.Advisor_tools

let uuid = "00000000-0000-4000-8000-000000000001"
let get = Option.get

let ids : A.identities =
  {
    conversation_id = get (A.Conversation_id.of_string uuid);
    turn_id = get (A.Turn_id.of_string uuid);
    attempt_id = get (A.Attempt_id.of_string uuid);
    user_message_id = get (A.Message_id.of_string uuid);
    assistant_message_id = get (A.Message_id.of_string uuid);
    run_id = get (A.Run_id.of_string uuid);
  }

let currency = Result.get_ok (Currency.make ~code:"SEK" ~scale:2)

let context : E.context =
  {
    system_instructions = "fixture";
    history = [];
    selection = { selected_turns = 0; omitted_turns = 0 };
    user_text = "synthetic";
    messages = [];
    source_finding = None;
    evidence = [];
    locale = "sv";
    currency;
    model_aliases = [ "account_1" ];
    context_token_budget = Some 1000000;
    segment_id = get (A.Segment_id.of_string uuid);
  }

let run : A.run =
  {
    ids;
    owner_id = get (User_id.of_string uuid);
    originating_session = get (A.Session_ref.of_hash "synthetic-session");
    model = "synthetic";
    protocol_version = 1;
    prompt_version = 1;
    started_at = Ptime.epoch;
    updated_at = Ptime.epoch;
    state = A.Queued;
    counters =
      { output_bytes = 0; model_rounds = 0; tool_calls = 0; evidence_bytes = 0; journal_bytes = 0; last_sequence = 0L };
  }

let error code = A.{ code; context = No_context }

let evidence : T.evidence =
  {
    id = get (A.Evidence_id.of_string uuid);
    scope = A.Personal;
    sampled_at = Ptime.epoch;
    currency;
    result = T.Goals_result [];
    total_count = 0;
    returned_count = 0;
    has_more = false;
    import_status = A.Current;
    imported_through = None;
    reconciled = true;
    assumptions = [];
    source_references = [];
  }

let display : A.evidence =
  {
    id = evidence.id;
    scope = evidence.scope;
    sampled_at = evidence.sampled_at;
    currency;
    title = "Goals";
    facts = [];
    source_references = [];
    import_status = A.Current;
    reconciled = true;
    assumptions = [];
  }

type fixture = {
  mutable trace : string list;
  mutable events : E.model_event list list;
  mutable current : E.model_event list;
  mutable state : A.run_state;
  mutable cancelled : bool;
  mutable clock : int;
  mutable ctx : E.context;
  mutable size : E.context_size;
  mutable selected : E.selected_evidence;
  mutable fail_at : string option;
  mutable cancel_at : string option;
  mutable revoke_at : string option;
  mutable denied : bool;
  mutable contexts : E.context list;
  mutable payloads : A.event_payload list;
  mutable reads : int;
  mutable tool_error : A.error option;
  mutable parses : int;
  mutable closes : int;
  mutable terminals : int;
  mutable started : int;
  mutable attempts : int;
  mutable tick : int;
  mutable raise_at : string option;
  mutable last_evidence : T.evidence option;
  mutable reuse_evidence : bool;
  mutable cancel_on_finish : bool;
  mutable disclosure_enabled : bool;
  mutable fail_second_measure : bool;
  mutable fail_after_text : bool;
  mutable transitions : A.transition list;
  mutable keepalives : int;
  mutable allocations : int;
  mutable reuse_segment : bool;
}

let fixture rounds =
  {
    trace = [];
    events = rounds;
    current = [];
    state = A.Queued;
    cancelled = false;
    clock = 0;
    ctx = context;
    size = { serialized_bytes = 100; conservative_tokens = 100; output_reserve_tokens = 65536 };
    selected = { model_content = "{}"; display; serialized_bytes = 2 };
    fail_at = None;
    cancel_at = None;
    revoke_at = None;
    denied = false;
    contexts = [];
    payloads = [];
    reads = 0;
    tool_error = None;
    parses = 0;
    closes = 0;
    terminals = 0;
    started = 0;
    attempts = 0;
    tick = 0;
    raise_at = None;
    last_evidence = None;
    reuse_evidence = false;
    cancel_on_finish = false;
    disclosure_enabled = true;
    fail_second_measure = false;
    fail_after_text = false;
    transitions = [];
    keepalives = 0;
    allocations = 0;
    reuse_segment = false;
  }

let record f name =
  f.trace <- f.trace @ [ name ];
  if f.cancel_at = Some name then f.cancelled <- true;
  if f.revoke_at = Some name then f.denied <- true;
  if f.raise_at = Some name then failwith "synthetic interpreter exception"

let result f name value =
  record f name;
  if f.fail_at = Some name then Error (error A.Provider_unavailable) else Ok value

let accepted = A.{ ids; accepted_version = 1L; idempotent_repeat = false }

let handler f : E.handler =
  {
    handle =
      (fun (type a) (op : a E.operation) : a ->
        match op with
        | E.Check_authority ->
            record f "auth";
            if f.denied then Error (error A.Access_denied) else Ok ()
        | E.Check_cancelled ->
            record f "cancel";
            f.cancelled
        | E.Now ->
            record f "now";
            f.clock <- f.clock + f.tick;
            get (Ptime.of_float_s (float f.clock))
        | E.Load_context -> result f "load" f.ctx
        | E.Allocate_segment ->
            f.allocations <- f.allocations + 1;
            let id =
              if f.reuse_segment then f.ctx.segment_id
              else get (A.Segment_id.of_string (Printf.sprintf "00000000-0000-4000-8000-%012d" (f.allocations + 1)))
            in
            result f "allocate" id
        | E.Measure_context c ->
            f.contexts <- f.contexts @ [ c ];
            if f.fail_second_measure && List.length f.contexts > 1 then Error (error A.Provider_unavailable)
            else result f "measure" f.size
        | E.Begin_model_round _ ->
            record f "begin";
            (match f.events with
            | h :: t ->
                f.current <- h;
                f.events <- t
            | [] -> f.current <- []);
            if f.fail_at = Some "begin" then Error (error A.Provider_unavailable) else Ok ()
        | E.Read_model_event ->
            for _ = 1 to f.keepalives do
              record f "keepalive";
              f.clock <- f.clock + 5
            done;
            f.keepalives <- 0;
            let value =
              match f.current with
              | [] -> None
              | h :: t ->
                  f.current <- t;
                  Some h
            in
            if f.fail_after_text && f.payloads <> [] then Error (error A.Provider_unavailable)
            else result f "read" value
        | E.Close_model_round ->
            f.closes <- f.closes + 1;
            record f "close"
        | E.Parse_tool call ->
            f.parses <- f.parses + 1;
            record f ("parse:" ^ call.id);
            if call.name = "bad" then Error (error A.Tool_not_allowed)
            else if call.name = "forecast" then Ok (T.Forecast ({ currency = None }, 6,
              { leave_out = []; input = Cashflow_workflow.Forecast_request.no_scenario }))
            else Ok (T.Goals { currency = None })
        | E.Read_tool _ ->
            f.reads <- f.reads + 1;
            let ev =
              match f.last_evidence with
              | Some ev when f.reuse_evidence -> ev
              | _ -> { evidence with sampled_at = get (Ptime.of_float_s (float f.reads)) }
            in
            f.last_evidence <- Some ev;
            let result = result f "tool" ev in
            (match f.tool_error with Some e -> Error e | None -> result)
        | E.Select_evidence ev ->
            Alcotest.(check bool)
              "fresh exact read object" true
              (match f.last_evidence with Some exact -> exact == ev | None -> false);
            result f "select" { f.selected with display = { f.selected.display with sampled_at = ev.sampled_at } }
        | E.Append_events payloads ->
            f.payloads <- f.payloads @ payloads;
            result f
              (match payloads with
              | [ A.Text_appended _ ] -> "text"
              | [ A.Evidence_added _ ] -> "evidence"
              | [ A.Tool_started _ ] -> "tool_started"
              | [ A.Tool_finished _ ] -> "tool_finished"
              | _ -> "append")
              ()
        | E.Commit_terminal transition ->
            f.terminals <- f.terminals + 1;
            f.transitions <- f.transitions @ [ transition ];
            record f "terminal";
            if f.cancel_on_finish && transition = A.Finish then f.cancelled <- true;
            if f.cancelled && transition = A.Finish then f.state <- A.Cancel_requested;
            let next = A.step f.state transition in
            (match next with Ok s -> f.state <- s | _ -> ());
            next
        | E.Persist_turn _ ->
            f.attempts <- f.attempts + 1;
            if not f.disclosure_enabled then Error (error A.Consent_required) else result f "persist" accepted
        | E.Persist_retry _ ->
            f.attempts <- f.attempts + 1;
            if not f.disclosure_enabled then Error (error A.Consent_required) else result f "retry" accepted
        | E.Start_run _ ->
            record f "start";
            if f.started = 0 then f.started <- 1
        | E.Request_cancel _ ->
            record f "request_cancel";
            if not (A.is_terminal f.state) then (
              f.cancelled <- true;
              f.state <- A.Cancel_requested);
            Ok f.state
        | E.Delete_owned_conversation _ -> result f "delete" ());
  }

let execute ?(run = run) f = E.run (handler f) (fun () -> W.execute ~run)
let completed = function Ok A.Completed -> () | _ -> Alcotest.fail "expected completion"
let failed = function Ok (A.Failed _) | Error _ -> () | _ -> Alcotest.fail "expected failure"
let stop = [ E.Text "hello"; E.Round_finished E.Stop ]

let bounded_workflow_trace () =
  let f = fixture [ stop ] in
  completed (execute f);
  Alcotest.(check int) "cleanup" 1 f.closes;
  Alcotest.(check int) "Start, streaming phase, terminal" 3 f.terminals

let check_int = Alcotest.check Alcotest.int
let check_bool = Alcotest.check Alcotest.bool
let call ?(name = "goals") i : E.raw_call = { id = string_of_int i; name; arguments = "{}"; index = i }
let tool_round calls = [ E.Complete_calls calls; E.Round_finished E.Tool_calls ]
let count f name = List.length (List.filter (( = ) name) f.trace)

let position f name =
  let rec find i = function [] -> 9999 | h :: t -> if h = name then i else find (i + 1) t in
  find 0 f.trace

let cancelled = function Ok A.Cancelled -> () | _ -> Alcotest.fail "expected cancelled"

let sequential_selection () =
  let f = fixture [ tool_round [ call 0; call 1 ]; stop ] in
  completed (execute f);
  check_int "both parsed" 2 f.parses;
  check_int "both read" 2 f.reads;
  check_bool "parse all before read" true (position f "parse:1" < position f "tool");
  check_bool "select before display" true (position f "select" < position f "evidence");
  let before =
    [
      "tool_started";
      "tool";
      "select";
      "evidence";
      "tool_finished";
      "tool_started";
      "tool";
      "select";
      "evidence";
      "tool_finished";
    ]
  in
  check_bool "sequential tools" true (List.filter (fun n -> List.mem n before) f.trace = before);
  let last = List.hd (List.rev f.contexts) in
  check_bool "call ids and exact selected content" true
    (List.filter_map (function E.Tool x -> Some (x.call_id, x.content) | _ -> None) last.messages
    = [ ("0", "{}"); ("1", "{}") ]);
  check_int "one current user" 1
    (List.length (List.filter (function E.User "synthetic" -> true | _ -> false) last.messages));
  check_bool "renewed auth before external steps" true
    (let rec scan prev = function
       | [] -> true
       | h :: t ->
           (if List.mem h [ "begin"; "read"; "tool"; "select"; "evidence"; "text" ] then
              List.mem "auth" prev && List.mem "cancel" prev
            else true)
           && scan (if List.mem h [ "begin"; "read"; "tool"; "select"; "evidence"; "text" ] then [] else h :: prev) t
     in
     scan [] f.trace)

let malformed_batches () =
  List.iter
    (fun calls ->
      let f = fixture [ tool_round calls ] in
      failed (execute f);
      check_int "zero reads after parse failure" 0 f.reads;
      check_int "close before failure" 1 f.closes)
    [
      [ call 0; call ~name:"bad" 1 ];
      [ call 0; call 0 ];
      [ { (call 0) with id = "" } ];
      [ { (call 0) with index = -1 } ];
    ]

let cancellation_boundaries () =
  List.iter
    (fun point ->
      let f = fixture [ tool_round [ call 0 ]; stop ] in
      f.cancel_at <- Some point;
      cancelled (execute f);
      check_bool "never complete" true (f.state <> A.Completed);
      if List.mem point [ "load"; "measure"; "begin"; "read" ] then check_int "no tools after cancellation" 0 f.reads;
      if point = "select" then check_int "no display after cancellation" 0 (count f "evidence"))
    [ "auth"; "now"; "load"; "measure"; "begin"; "read"; "tool"; "select"; "evidence"; "tool_finished" ];
  let f = fixture [ stop ] in
  f.cancelled <- true;
  f.state <- A.Cancel_requested;
  cancelled (execute f);
  check_int "never start" 0 (count f "begin")

let authority_boundaries () =
  List.iter
    (fun point ->
      let f = fixture [ tool_round [ call 0 ]; stop ] in
      f.revoke_at <- Some point;
      failed (execute f);
      if point = "select" then check_int "no display on authority loss" 0 (count f "evidence");
      check_bool "no completed run" true (f.state <> A.Completed))
    [ "load"; "measure"; "begin"; "read"; "tool"; "select"; "evidence" ];
  let f = fixture [ stop ] in
  f.denied <- true;
  failed (execute f);
  check_int "no start on queued authority loss" 0 (count f "begin");
  check_int "one failure transition" 1 f.terminals

let errors_cleanup () =
  List.iter
    (fun point ->
      let f = fixture [ tool_round [ call 0 ]; stop ] in
      f.fail_at <- Some point;
      failed (execute f);
      if List.mem point [ "begin"; "read"; "tool"; "select" ] then check_int "closed" 1 f.closes;
      if point = "select" then (
        check_int "no evidence" 0 (count f "evidence");
        check_int "no followup" 1 (count f "begin")))
    [ "load"; "measure"; "begin"; "read"; "tool"; "select"; "text"; "evidence" ];
  let f = fixture [ stop ] in
  f.raise_at <- Some "read";
  failed (execute f);
  check_int "exception closes" 1 f.closes;
  List.iter
    (fun round ->
      let f = fixture [ round ] in
      failed (execute f);
      check_int "malformed closed" 1 f.closes)
    [
      [];
      [ E.Text "partial" ];
      [ E.Complete_calls [ call 0 ]; E.Text "bad" ];
      [ E.Round_finished E.Length ];
      [ E.Round_finished E.Content_filter ];
      [ E.Round_finished E.Tool_calls ];
      [ E.Complete_calls [ call 0 ]; E.Round_finished E.Stop ];
    ]

let output_budget () =
  let f = fixture [ [ E.Text (String.make 65536 'x'); E.Round_finished E.Stop ] ] in
  completed (execute f);
  let f = fixture [ [ E.Text (String.make 65536 'x'); E.Text "x"; E.Round_finished E.Stop ] ] in
  failed (execute f);
  check_int "only bounded public text" 1 (count f "text");
  let f =
    fixture
      [
        [ E.Text (String.make 10000 'x'); E.Complete_calls [ call 0 ]; E.Round_finished E.Tool_calls ];
        [ E.Text (String.make 55537 'x'); E.Round_finished E.Stop ];
      ]
  in
  (match execute f with
  | Ok (A.Failed { code = A.Run_limit_reached; context = A.Limit A.Output_bytes }) -> ()
  | _ -> Alcotest.fail "expected whole-attempt Output_bytes failure");
  check_int "whole attempt output" 1 (count f "text");
  let f =
    fixture
      [
        [ E.Text (String.make 10000 'x'); E.Complete_calls [ call 0 ]; E.Round_finished E.Tool_calls ];
        [ E.Text (String.make 55536 'x'); E.Round_finished E.Stop ];
      ]
  in
  completed (execute f);
  check_int "exact whole-attempt output budget" 2 (count f "text")

let rounds_tools_budget () =
  let f = fixture (List.init 3 (fun i -> tool_round [ call i ]) @ [ stop ]) in
  completed (execute f);
  check_int "exact round budget succeeds" 4 (count f "begin");
  let f = fixture (List.init 4 (fun i -> tool_round [ call i ]) @ [ stop ]) in
  failed (execute f);
  check_int "max rounds" 4 (count f "begin");
  let f = fixture [ tool_round (List.init 8 call); stop ] in
  completed (execute f);
  check_int "max executions" 8 f.reads;
  let f = fixture [ tool_round (List.init 9 call); stop ] in
  failed (execute f);
  check_int "oversized batch no reads" 0 f.reads;
  let f = fixture [ tool_round (List.init 5 call); tool_round (List.init 4 (fun i -> call (i + 5))); stop ] in
  failed (execute f);
  check_int "whole attempt tools" 5 f.reads

let selection_budgets () =
  List.iter
    (fun bytes ->
      let f = fixture [ tool_round [ call 0 ]; stop ] in
      f.selected <- { f.selected with model_content = String.make bytes 'x'; serialized_bytes = bytes };
      let r = execute f in
      if bytes = 16384 then completed r
      else (
        failed r;
        check_int "no oversized display" 0 (count f "evidence")))
    [ 16384; 16385 ];
  let f = fixture [ tool_round [ call 0 ]; stop ] in
  f.selected <- { f.selected with serialized_bytes = 3 };
  failed (execute f);
  check_int "bad measurement never displayed" 0 (count f "evidence");
  let f = fixture [ tool_round (List.init 5 call); stop ] in
  f.selected <- { f.selected with model_content = String.make 16384 'x'; serialized_bytes = 16384 };
  failed (execute f);
  check_int "aggregate only four evidence displays" 4 (count f "evidence")

let followup_conversation_budget () =
  let f =
    fixture [ [ E.Text (String.make 11000 'x'); E.Complete_calls [ call 0 ]; E.Round_finished E.Tool_calls ]; stop ]
  in
  failed (execute f);
  check_int "oversized active conversation never disclosed in followup" 1 (count f "begin")

let context_budgets () =
  List.iter
    (fun budget ->
      let f = fixture [ stop ] in
      f.ctx <- { context with context_token_budget = budget };
      failed (execute f);
      check_int "missing reliable limit no generation" 0 (count f "begin"))
    [ None; Some 0; Some (-1) ];
  List.iter
    (fun size ->
      let f = fixture [ stop ] in
      f.size <- size;
      f.ctx <- { context with context_token_budget = Some 65636 };
      failed (execute f);
      check_int "no generation on bad size" 0 (count f "begin"))
    [
      E.{ serialized_bytes = 101; conservative_tokens = 100; output_reserve_tokens = 65536 };
      E.{ serialized_bytes = 100; conservative_tokens = 101; output_reserve_tokens = 65536 };
      E.{ serialized_bytes = 100; conservative_tokens = 100; output_reserve_tokens = 0 };
    ];
  let f = fixture [ stop ] in
  f.ctx <- { context with context_token_budget = Some 65636 };
  completed (execute f);
  let f = fixture [ stop ] in
  f.ctx <- { context with messages = [ E.Tool { call_id = "stale"; content = "old" } ] };
  failed (execute f);
  check_int "reject stale context" 0 (count f "begin");
  let f = fixture [ stop ] in
  f.ctx <- { context with user_text = String.make 8193 'x' };
  failed (execute f)

let deadline () =
  let f = fixture [ [ E.Text "one"; E.Text "two"; E.Text "three"; E.Round_finished E.Stop ] ] in
  f.tick <- 10;
  failed (execute f);
  check_bool "original deadline not renewed by activity" true (f.clock >= 120);
  check_int "deadline cleanup" 1 f.closes;
  let f = fixture [ stop ] in
  f.clock <- 120;
  failed (execute f);
  check_int "expired queued no start" 0 (count f "begin")

let keepalive_total_deadline () =
  let f = fixture [ stop ] in
  f.keepalives <- 24;
  failed (execute f);
  check_int "all raw comments accounted" 24 (count f "keepalive");
  check_int "keepalives cannot extend original deadline" 120 f.clock;
  check_int "no late text publication" 0 (count f "text");
  check_int "deadline closes model" 1 f.closes

let history () =
  let attempt state text : A.attempt =
    {
      ids;
      ordinal = 1L;
      state;
      segments = [ A.Text_segment { segment_id = context.segment_id; text }; A.Evidence_segment display ];
      last_sequence = 1L;
    }
  in
  let turn attempts : A.turn =
    { turn_id = ids.turn_id; user_message_id = ids.user_message_id; user_text = "one-user"; attempts }
  in
  let snapshot turns : A.snapshot =
    {
      conversation_id = ids.conversation_id;
      scope = A.Personal;
      currency;
      locale = "sv";
      source_finding = None;
      turns;
      active_run = None;
      version = 1L;
    }
  in
  let turns =
    [
      turn
        [
          attempt (A.Failed (error A.Provider_unavailable)) "failed";
          attempt A.Cancelled "cancelled";
          attempt A.Completed "success";
        ];
      turn [ attempt A.Cancelled "no answer" ];
    ]
  in
  let chosen = Result.get_ok (W.select_history (snapshot turns)) in
  check_int "one successful turn" 1 (List.length chosen);
  check_bool "success only no old evidence" true ((List.hd chosen).assistant_text = "success");
  let chosen = Result.get_ok (W.select_history (snapshot (List.init 14 (fun _ -> List.hd turns)))) in
  check_int "last 12 completed turns" 12 (List.length chosen);
  let f = fixture [ stop ] in
  f.ctx <-
    { context with history = chosen @ [ List.hd chosen ]; selection = { selected_turns = 13; omitted_turns = 2 } };
  completed (execute f);
  let ctx = List.hd f.contexts in
  check_int "context bound" 12 (List.length ctx.history);
  check_int "visible omitted metadata" 3 ctx.selection.omitted_turns;
  let large = { (List.hd chosen) with E.assistant_text = String.make 11000 'x' } in
  let f = fixture [ stop ] in
  f.ctx <- { context with history = [ large ] };
  completed (execute f);
  check_int "whole too-large turn excluded" 0 (List.length (List.hd f.contexts).history)

let commands () =
  let submit : A.submit =
    {
      conversation_id = None;
      scope = A.Personal;
      currency;
      locale = "sv";
      user_text = "synthetic";
      idempotency_key = Result.get_ok (Idempotency_key.of_string "key");
      expected_version = 0L;
      source_finding_key = None;
    }
  in
  let retry : A.retry = { turn_id = ids.turn_id; expected_version = 1L; idempotency_key = submit.idempotency_key } in
  let f = fixture [] in
  let run fn = E.run (handler f) fn in
  ignore (Result.get_ok (run (fun () -> W.accept_turn submit)));
  ignore (Result.get_ok (run (fun () -> W.retry_turn retry)));
  check_bool "persist before start" true (position f "persist" < position f "start");
  check_int "idempotent supervisor start" 1 f.started;
  let f = fixture [] in
  f.fail_at <- Some "persist";
  check_bool "persist failed" true (Result.is_error (E.run (handler f) (fun () -> W.accept_turn submit)));
  check_int "no start before commit" 0 f.started;
  let f = fixture [] in
  check_bool "shape rejected" true
    (Result.is_error (E.run (handler f) (fun () -> W.accept_turn { submit with user_text = "" })));
  check_int "invalid no persistence" 0 f.attempts;
  let f = fixture [] in
  f.state <- A.Completed;
  check_bool "cancel after complete returns complete" true
    (E.run (handler f) (fun () -> W.cancel_run ids.run_id) = Ok A.Completed);
  let f = fixture [] in
  f.disclosure_enabled <- false;
  check_bool "ownership deletion independent of disclosure configuration/consent" true
    (E.run (handler f) (fun () -> W.delete_conversation ids.conversation_id) = Ok ());
  check_bool "identity then owner delete only" true (f.trace = [ "auth"; "delete" ])

let command_authority_and_validation () =
  let submit : A.submit =
    {
      conversation_id = None;
      scope = A.Personal;
      currency;
      locale = "sv";
      user_text = "synthetic";
      idempotency_key = Result.get_ok (Idempotency_key.of_string "command-key");
      expected_version = 0L;
      source_finding_key = None;
    }
  in
  let f = fixture [] in
  f.denied <- true;
  check_bool "invalid identity cannot delete" true
    (Result.is_error (E.run (handler f) (fun () -> W.delete_conversation ids.conversation_id)));
  check_int "no delete after identity failure" 0 (count f "delete");
  let f = fixture [] in
  f.disclosure_enabled <- false;
  check_bool "optout blocks submit" true (Result.is_error (E.run (handler f) (fun () -> W.accept_turn submit)));
  check_int "optout never starts generation" 0 f.started;
  check_bool "same owner may still delete after optout" true
    (E.run (handler f) (fun () -> W.delete_conversation ids.conversation_id) = Ok ());
  List.iter
    (fun request ->
      let f = fixture [] in
      check_bool "invalid command rejected" true (Result.is_error (E.run (handler f) (fun () -> W.accept_turn request)));
      check_int "invalid command no storage mutation" 0 f.attempts)
    [
      { submit with user_text = String.make 8193 'x' };
      { submit with expected_version = -1L };
      { submit with locale = "" };
    ]

let terminal_order () =
  let f = fixture [ stop ] in
  completed (execute f);
  check_bool "close before terminal" true (position f "close" < List.length f.trace - 1);
  check_bool "text while stream open" true (position f "text" < position f "close");
  check_bool "no duplicate appended terminal" true
    (not (List.exists (function A.Run_completed | A.Run_failed _ | A.Run_cancelled -> true | _ -> false) f.payloads));
  let f = fixture [ stop ] in
  f.cancel_on_finish <- true;
  cancelled (execute f);
  check_bool "finish race cannot complete" true (f.state = A.Cancelled)

let fresh_and_unique_evidence () =
  let f = fixture [ tool_round [ call 0 ]; tool_round [ call 1 ]; stop ] in
  completed (execute f);
  let samples = List.filter_map (function A.Evidence_added e -> Some e.sampled_at | _ -> None) f.payloads in
  check_bool "imports yield independent sample times" true
    (samples = [ get (Ptime.of_float_s 1.); get (Ptime.of_float_s 2.) ]);
  let f = fixture [ tool_round (List.init 8 call); stop ] in
  f.reuse_evidence <- true;
  f.selected <- { f.selected with model_content = String.make 16384 'x'; serialized_bytes = 16384 };
  completed (execute f);
  check_int "unique object not double charged" 8 f.reads;
  check_int "repeated object publishes one evidence card" 1 (count f "evidence");
  check_int "repeated object keeps every tool completion" 8 (count f "tool_finished");
  let f = fixture [ tool_round [ call 0 ]; stop ] in
  f.fail_second_measure <- true;
  failed (execute f);
  check_int "failed followup measurement no network" 1 (count f "begin");
  check_int "all opened streams closed" 1 f.closes;
  let f = fixture [ stop ] in
  f.fail_after_text <- true;
  failed (execute f);
  check_int "generic provider failure preserves committed partial text" 1 (count f "text");
  check_int "generic failure closes" 1 f.closes

let every_terminal () =
  List.iter
    (fun setup ->
      let f = fixture [ stop ] in
      setup f;
      ignore (execute f);
      let terminals =
        List.filter (function A.Finish | A.Fail _ | A.Confirm_cancel -> true | _ -> false) f.transitions
      in
      check_int "one terminal transition" 1 (List.length terminals);
      check_bool "last operation terminal commit" true (List.hd (List.rev f.trace) = "terminal"))
    [ (fun _ -> ()); (fun f -> f.fail_at <- Some "read"); (fun f -> f.cancel_at <- Some "read") ]

let streaming_phase () =
  let f = fixture [ stop ] in
  completed (execute f);
  check_bool "streaming phase persisted" true (List.mem (A.Set_phase A.Streaming_text) f.transitions)

let recoverable_forecast_validation () =
  let forecast i = { (call i) with name = "forecast" } in
  let round calls = [ E.Complete_calls calls; E.Round_finished E.Tool_calls ] in
  let f = fixture [round [forecast 0]; stop] in
  f.tool_error <- Some (error A.Invalid_tool_arguments);
  completed (execute f);
  check_int "validation is not evidence" 0 (count f "evidence");
  check_int "no selection of rejected read" 0 (count f "select");
  check_bool "rejected completion" true
    (List.mem (A.Tool_finished { label = "forecast"; outcome = A.Rejected }) f.payloads);
  let feedback = List.filter_map (function E.Tool t -> Some t.content | _ -> None)
    (List.hd (List.rev f.contexts)).messages in
  (match feedback with
   | [content] -> check_bool "fixed bounded validation feedback" true
       (String.length content <= 512 && String.starts_with ~prefix:"{\"error\":" content)
   | _ -> Alcotest.fail "missing single validation feedback");
  check_int "no new attempt" 0 f.attempts;
  List.iter (fun e ->
    let f = fixture [round [forecast 0]; stop] in
    f.tool_error <- Some e;
    failed (execute f);
    check_int "non-validation errors cannot resume" 1 (count f "begin"))
    [error A.Access_denied; error A.Scope_mismatch; error A.Consent_required;
     error A.Not_found; error A.Storage_unavailable; error A.Tool_not_allowed;
     error A.Invalid_stream; error A.Provider_unavailable;
     { A.code = A.Run_limit_reached; context = A.Limit A.Tool_calls };
     { A.code = A.Invalid_tool_arguments; context = A.Limit A.Context_bytes }];
  let f = fixture [round [call 0]; stop] in
  f.tool_error <- Some (error A.Invalid_tool_arguments);
  failed (execute f);
  check_int "other tools remain fail-closed" 1 (count f "begin");
  List.iter (fun (calls, maximum) ->
    let f = fixture (List.init 5 (fun _ -> round calls)) in
    f.tool_error <- Some (error A.Invalid_tool_arguments);
    failed (execute f);
    check_bool "normal tool/round caps bound corrections" true (f.reads <= 8);
    check_int "bounded provider rounds" maximum (count f "begin");
    check_bool "ordinary limit, not implicit regeneration" true
      (match f.state with
       | A.Failed { code = A.Run_limit_reached; context = A.Limit kind } ->
           kind = (if maximum = 4 then A.Model_rounds else A.Tool_calls)
       | _ -> false);
    check_int "correction never creates another attempt" 0 f.attempts)
    [[forecast 0], 4; [forecast 0; forecast 1; forecast 2], 3];
  List.iter (fun cancel ->
    let f = fixture [round [forecast 0]; stop] in
    f.tool_error <- Some (error A.Invalid_tool_arguments);
    if cancel then f.cancel_at <- Some "tool" else f.revoke_at <- Some "tool";
    ignore (execute f);
    check_int "authority checked before feedback" 0 (count f "tool_finished");
    check_int "no provider after cancellation/revocation" 1 (count f "begin")) [false; true]

let completion_reserve () =
  let f = fixture [ stop ] in
  f.size <- { f.size with output_reserve_tokens = 2048 };
  completed (execute f);
  check_int "bounded completion smaller than byte cap admits generation" 1 (count f "begin");
  let f = fixture [ stop ] in
  f.size <- { f.size with output_reserve_tokens = 65536 };
  completed (execute f);
  List.iter
    (fun size ->
      let f = fixture [ stop ] in
      f.size <- size;
      failed (execute f);
      check_int "malformed or overflowing measurement no generation" 0 (count f "begin"))
    [
      E.{ serialized_bytes = -1; conservative_tokens = 1; output_reserve_tokens = 65536 };
      E.{ serialized_bytes = 1; conservative_tokens = -1; output_reserve_tokens = 65536 };
      E.{ serialized_bytes = 1; conservative_tokens = 1; output_reserve_tokens = -1 };
      E.{ serialized_bytes = max_int; conservative_tokens = max_int; output_reserve_tokens = max_int };
    ];
  let f = fixture [ stop ] in
  f.size <- { f.size with output_reserve_tokens = 65536 };
  failed (execute ~run:{ run with counters = { run.counters with output_bytes = 65536 } } f);
  check_int "exhausted output budget no model work" 0 (count f "begin")

let ordered_text_spans () =
  let rounds =
    [
      [ E.Text "A"; E.Text "a"; E.Complete_calls [ call 0 ]; E.Round_finished E.Tool_calls ];
      [ E.Text "B"; E.Text "b"; E.Round_finished E.Stop ];
    ]
  in
  let f = fixture rounds in
  completed (execute f);
  let spans = List.filter_map (function A.Text_appended d -> Some d.segment_id | _ -> None) f.payloads in
  (match spans with
  | [ a; a2; b; b2 ] -> check_bool "stable within-span, fresh after tools" true (a = a2 && b = b2 && a <> b)
  | _ -> Alcotest.fail "expected four deltas");
  check_int "one lazy followup allocation" 1 f.allocations;
  let f = fixture rounds in
  f.reuse_segment <- true;
  failed (execute f);
  check_int "reused id cannot append followup text" 2 (count f "text");
  let f = fixture rounds in
  f.fail_at <- Some "allocate";
  failed (execute f);
  check_int "allocation failure closes all streams" 2 f.closes;
  List.iter
    (fun revoke ->
      let f = fixture rounds in
      if revoke then f.revoke_at <- Some "allocate" else f.cancel_at <- Some "allocate";
      let outcome = execute f in
      if revoke then failed outcome else cancelled outcome;
      check_int "no text after allocation revocation/cancellation" 2 (count f "text");
      check_int "allocation boundary cleanup" 2 f.closes)
    [ false; true ]

let tests =
  [
    ("fresh unique evidence", fresh_and_unique_evidence);
    ("single terminal", every_terminal);
    ("ordered text spans", ordered_text_spans);
    ("recoverable forecast validation", recoverable_forecast_validation);
    ("completion reserve", completion_reserve);
    ("streaming phase", streaming_phase);
    ("streams and completes", bounded_workflow_trace);
    ("sequential selected tools", sequential_selection);
    ("parse all", malformed_batches);
    ("cancellation boundaries", cancellation_boundaries);
    ("authority boundaries", authority_boundaries);
    ("errors and cleanup", errors_cleanup);
    ("output budget", output_budget);
    ("round and tool budgets", rounds_tools_budget);
    ("selection budgets", selection_budgets);
    ("context budgets", context_budgets);
    ("followup conversation budget", followup_conversation_budget);
    ("original deadline", deadline);
    ("keepalive total deadline", keepalive_total_deadline);
    ("successful history", history);
    ("command ordering", commands);
    ("command authority and validation", command_authority_and_validation);
    ("terminal ordering", terminal_order);
  ]

let () =
  Alcotest.run "advisor workflow"
    [ ("bounded_workflow_trace", List.map (fun (n, f) -> Alcotest.test_case n `Quick f) tests) ]
