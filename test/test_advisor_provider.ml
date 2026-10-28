module P = Cashflow_io.Advisor_provider
module A = Cashflow_core.Advisor
module Sql = Cashflow_io.Sql

let limits : P.generation_limits = { context_tokens = 262144; completion_tokens = 32768 }
let create ~post ~api_key ~model ~provider_only = P.create ~limits ~post ~api_key ~model ~provider_only ()
let request : P.request = { system = "synthetic instructions"; messages = [ P.User "synthetic user" ] }
let frame s = "data: " ^ s ^ "\r\n\r\n"
let chunk delta finish = frame (Yojson.Safe.to_string (`Assoc ["choices", `List [`Assoc ["index", `Int 0; "delta", delta; "finish_reason", finish]]]))
let text s = chunk (`Assoc ["content", `String s]) `Null
let finish reason = chunk (`Assoc []) (`String reason)
let done_ = frame "[DONE]"
let usage = frame {|{"choices":[{"index":0,"delta":{},"finish_reason":"stop"}],"usage":{"prompt_tokens":3,"completion_tokens":2,"total_tokens":5}}|}
let call index fields = `Assoc (("index", `Int index) :: fields)
let tools calls = chunk (`Assoc ["tool_calls", `List calls]) `Null
let function_ fields = "function", `Assoc fields
let full_call index id name args = call index ["id", `String id; "type", `String "function"; function_ ["name", `String name; "arguments", `String args]]
let run ?(providers=["amazon-bedrock"]) chunks =
  let events = ref [] and requests = ref [] in
  let db = Cashflow_io.Store_sqlite.open_memory () in
  let post ~uri:_ ~headers:_ ~body ~on_chunk =
    Alcotest.(check bool) "outside transaction" false (Sql.in_transaction db);
    requests := Yojson.Safe.from_string body :: !requests;
    List.iter on_chunk chunks; Ok ()
  in
  let p = create ~post ~api_key:"synthetic-key" ~model:"synthetic-model" ~provider_only:providers in
  let result = P.stream p request ~on_event:(fun event -> events := event :: !events) in
  result, List.rev !events, !requests
let ok = function Ok _ -> () | Error _ -> Alcotest.fail "expected success"
let failed = function Error _ -> () | Ok _ -> Alcotest.fail "expected failure"
let policy requests =
  let open Yojson.Safe.Util in
  List.iter (fun r ->
    let p = member "provider" r in
    Alcotest.(check int) "whole completion cap every round" limits.completion_tokens (member "max_tokens" r |> to_int);
    Alcotest.(check bool) "require cap support every round" true (member "require_parameters" p = `Bool true);
    Alcotest.(check bool) "ZDR" true (member "zdr" p = `Bool true);
    Alcotest.(check string) "deny collection" "deny" (member "data_collection" p |> to_string);
    Alcotest.(check (list string)) "allowlist unchanged" ["amazon-bedrock"] (member "only" p |> to_list |> List.map to_string);
    Alcotest.(check bool) "stream" true (member "stream" r = `Bool true);
    List.iter (fun key -> Alcotest.(check bool) ("no " ^ key) true (member key r = `Null)) ["plugins"; "reasoning"; "server_tools"];
    Alcotest.(check int) "fifteen schemas" 15 (member "tools" r |> to_list |> List.length)) requests
let fragmented_provider_rounds () =
  let wire = ": keepalive\r\n\r\n" ^ text "Hej €雪" ^ finish "stop" ^ usage ^ usage ^ done_ in
  for i = 0 to String.length wire do
    let result, events, requests = run [String.sub wire 0 i; String.sub wire i (String.length wire-i)] in
    ok result; policy requests;
    Alcotest.(check bool) "normalized text and one finish" true (events = [P.Text "Hej €雪"; P.Round_finished P.Stop]);
    match result with Ok u -> Alcotest.(check int) "usage not summed" 5 u.total_tokens | _ -> ()
  done;
  let result, _, _ = run (List.init (String.length wire) (fun i -> String.make 1 wire.[i])) in ok result
let interleaved_tools () =
  let start = tools [full_call 1 "b" "goals" "{"; full_call 0 "a" "cycle_summary" "{\"cycles\":"] in
  let end_ = tools [call 0 [function_ ["arguments", `String "2}"]]; call 1 [function_ ["arguments", `String "}"]]] in
  let accounting = frame {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":3,"completion_tokens":2,"total_tokens":5}}|} in
  let wire = start ^ end_ ^ finish "tool_calls" ^ accounting ^ accounting ^ done_ in
  for i = 0 to String.length wire do
    let result, events, requests = run [String.sub wire 0 i; String.sub wire i (String.length wire-i)] in
    ok result; policy requests;
    match events with
    | [P.Calls_ready [a;b]; P.Round_finished P.Tool_calls] ->
        Alcotest.(check string) "index order" "a" a.id;
        Alcotest.(check string) "complete args" "{\"cycles\":2}" a.arguments;
        Alcotest.(check string) "second id" "b" b.id
    | _ -> Alcotest.fail "partial or missing calls"
  done
let trailing_noop_deltas () =
  let accounting delta finish_reason = frame (Yojson.Safe.to_string (`Assoc [
    "choices", `List [`Assoc ["index", `Int 0; "delta", `Assoc delta; "finish_reason", finish_reason]];
    "usage", `Assoc ["prompt_tokens", `Int 3; "completion_tokens", `Int 2; "total_tokens", `Int 5]])) in
  let noops = List.concat_map (fun content ->
    List.map (fun calls -> content @ calls)
      [[]; ["tool_calls", `Null]; ["tool_calls", `List []]])
    [[]; ["content", `Null]; ["content", `String ""]] in
  List.iter (fun (prefix, reason, expected) ->
    let trailing = List.concat_map (fun delta ->
      [accounting delta `Null; accounting delta (`String reason)]) noops in
    let result, events, requests = run (prefix :: finish reason :: trailing @ [done_]) in
    ok result; policy requests;
    Alcotest.(check bool) "no no-op events or duplicate calls" true (events = expected);
    (match result with
     | Ok u -> Alcotest.(check (triple int int int)) "usage retained, not summed" (3,2,5)
         (u.prompt_tokens,u.completion_tokens,u.total_tokens)
     | Error _ -> Alcotest.fail "trailing no-op rejected");
    List.iter (fun delta ->
      let result, events, _ = run [prefix; finish reason; accounting delta `Null; done_] in
      (match result with Error { A.code = A.Invalid_stream; _ } -> ()
       | _ -> Alcotest.fail "semantic or malformed trailing delta accepted");
      Alcotest.(check bool) "no failed round completion/calls" true
        (List.for_all (function P.Text _ -> true | _ -> false) events))
      [["content", `String "late"]; ["content", `Int 0]; ["content", `List []];
       ["tool_calls", `List [full_call 0 "late" "goals" "{}"]];
       ["tool_calls", `List [`Assoc []]]; ["tool_calls", `String ""]; ["tool_calls", `Assoc []]];
    List.iter (fun suffix ->
      let result, _, _ = run [prefix; finish reason; suffix] in
      failed result)
      [accounting ["content", `String ""; "tool_calls", `List []]
         (`String (if reason = "stop" then "tool_calls" else "stop")) ^ done_;
       done_ ^ accounting [] `Null])
    [text "answer", "stop", [P.Text "answer"; P.Round_finished P.Stop];
     tools [full_call 0 "a" "goals" "{}"], "tool_calls",
       [P.Calls_ready [{ id = "a"; name = "goals"; arguments = "{}"; index = 0 }]; P.Round_finished P.Tool_calls]]

let failures () =
  List.iter (fun wire -> let result, events, requests = run [wire] in failed result; policy requests;
    Alcotest.(check bool) "no calls/finish on failure" true (List.for_all (function P.Text _ -> true | _ -> false) events))
    [text "partial" ^ frame {|{"error":{"message":"PRIVATE PROVIDER BODY"}}|} ^ done_;
     "event: error\n" ^ frame {|{"message":"PRIVATE"}|};
     text "partial" ^ finish "stop";
     tools [full_call 0 "a" "goals" "{"] ^ finish "tool_calls" ^ done_;
     tools [full_call 0 "a" "goals" "{\"unknown\":1}"] ^ finish "tool_calls" ^ done_;
     tools [full_call 0 "a" "arbitrary_query" "{}"] ^ finish "tool_calls" ^ done_;
     done_; finish "stop" ^ done_ ^ text "late";
     tools [full_call 0 "same" "goals" "{}"; full_call 1 "same" "goals" "{}"] ^ finish "tool_calls" ^ done_;
     tools [full_call 0 "a" "goals" "{}"; full_call 0 "b" "goals" "{}"] ^ finish "tool_calls" ^ done_;
     tools [full_call 0 "a" "goals" "{}"] ^ finish "stop" ^ done_;
     tools [full_call 0 "a" "goals" "{\"currency\":\"SEK\",\"currency\":\"USD\"}"] ^ finish "tool_calls" ^ done_;
     tools [full_call 0 "a" "goals" (String.make 8193 ' ')] ^ finish "tool_calls" ^ done_;
     "data: " ^ String.make (256*1024) 'x';
     tools (List.init 9 (fun i -> full_call i (string_of_int i) "goals" "{}")) ^ finish "tool_calls" ^ done_];
  let result, _, requests = run ~providers:[] [] in failed result;
  Alcotest.(check int) "empty allowlist sends nothing" 0 (List.length requests)
let cancellation () =
  Eio_main.run (fun _ ->
    let events = ref [] and returned = ref false and requests = ref [] in
    let cancelled =
      try Eio.Switch.run (fun sw ->
        let post ~uri:_ ~headers:_ ~body ~on_chunk =
          requests := Yojson.Safe.from_string body :: !requests;
          on_chunk (tools [full_call 0 "a" "goals" "{"]);
          Eio.Switch.fail sw Exit; Eio.Fiber.yield (); Ok () in
        let p = create ~post ~api_key:"fake" ~model:"fake" ~provider_only:["amazon-bedrock"] in
        ignore (P.stream p request ~on_event:(fun e -> events := e :: !events));
        returned := true);
        false
      with Exit -> true in
    Alcotest.(check bool) "cancellation propagated" true cancelled;
    Alcotest.(check bool) "not converted into a result" false !returned;
    policy !requests;
    Alcotest.(check int) "no partial calls" 0 (List.length !events))
let follow_up_policy () =
  let requests = ref [] in
  let db = Cashflow_io.Store_sqlite.open_memory () in
  let post ~uri:_ ~headers:_ ~body ~on_chunk =
    Alcotest.(check bool) "follow-up outside transaction" false (Sql.in_transaction db);
    requests := !requests @ [Yojson.Safe.from_string body];
    if List.length !requests = 1 then on_chunk (tools [full_call 0 "call_1" "goals" "{}"] ^ finish "tool_calls" ^ done_)
    else on_chunk (text "synthetic answer" ^ finish "stop" ^ done_);
    Ok () in
  let p = create ~post ~api_key:"fake" ~model:"fake" ~provider_only:["amazon-bedrock"] in
  let calls = ref [] in
  ok (P.stream p request ~on_event:(function P.Calls_ready c -> calls := c | _ -> ()));
  let follow_up : P.request = { request with messages = request.messages @ [P.Assistant ("", !calls); P.Tool { call_id = "call_1"; content = "synthetic tool result" }] } in
  ok (P.stream p follow_up ~on_event:(fun _ -> ()));
  Alcotest.(check int) "two rounds only" 2 (List.length !requests); policy !requests;
  let open Yojson.Safe.Util in
  let messages = List.nth !requests 1 |> member "messages" |> to_list in
  Alcotest.(check (list string)) "message order" ["system";"user";"assistant";"tool"] (List.map (fun m -> member "role" m |> to_string) messages);
  Alcotest.(check string) "tool ID preserved" "call_1" (List.nth messages 3 |> member "tool_call_id" |> to_string)
let multiline_and_failures () =
  let result, events, requests = run [": heartbeat\n\ndata: {\"choices\":\ndata: [{\"index\":0,\"delta\":{\"content\":\"live\"},\"finish_reason\":null}]}\n\n"; finish "stop"; done_] in
  ok result; policy requests;
  Alcotest.(check bool) "multiline data" true (events = [P.Text "live"; P.Round_finished P.Stop]);
  List.iter (fun error ->
    let captured = ref [] in
    let post ~uri:_ ~headers:_ ~body ~on_chunk:_ = captured := [Yojson.Safe.from_string body]; Error error in
    let p = create ~post ~api_key:"fake" ~model:"fake" ~provider_only:["amazon-bedrock"] in
    (match P.stream p request ~on_event:(fun _ -> Alcotest.fail "no error-body events") with
    | Error { A.code = A.Provider_unavailable; context = A.No_context } -> ()
    | _ -> Alcotest.fail "safe provider error");
    Alcotest.(check int) "no retry" 1 (List.length !captured); policy !captured)
    [Cashflow_io.Https.Http_status 400; Cashflow_io.Https.Http_status 403; Cashflow_io.Https.Transport "PRIVATE BODY"];
  let exposed = ref false and captured = ref [] in
  let post ~uri:_ ~headers:_ ~body ~on_chunk =
    captured := [Yojson.Safe.from_string body];
    on_chunk (text "incremental");
    Alcotest.(check bool) "text before upstream completes" true !exposed;
    on_chunk (finish "stop" ^ done_); Ok () in
  let p = create ~post ~api_key:"fake" ~model:"fake" ~provider_only:["amazon-bedrock"] in
  ok (P.stream p request ~on_event:(function P.Text _ -> exposed := true | _ -> ()));
  policy !captured
let schemas () =
  let examples = ["cycle_summary", "{\"cycles\":6}"; "category_spending", "{\"cycle_offset\":-5}";
    "category_trend", "{\"category\":\"account_1\",\"cycles\":1}";
    "relevant_transactions", "{\"category\":\"account_1\",\"limit\":20}";
    "recurring", "{}"; "budget_status", "{}"; "goals", "{\"currency\":\"SEK\"}"; "inbox_rows", "{}"; "categories", "{\"currency\":\"SEK\"}";
    "forecast", "{\"cycles\":6,\"scenario\":{\"leave_out\":[\"series_1\"],\"one_off\":[{\"date\":\"2026-10-22\",\"amount\":\"-100\",\"description\":\"synthetic\"}],\"follow_budgets\":true}}"] in
  List.iter (fun (name,args) ->
    ok (Cashflow_io.Advisor_projection.parse_call ~name ~arguments:args);
    let result, _, requests = run [tools [full_call 0 "id" name args] ^ finish "tool_calls" ^ done_] in ok result; policy requests) examples;
  let open Yojson.Safe.Util in
  let name_of schema = member "function" schema |> member "name" |> to_string in
  let reads = List.filter (fun schema -> not (List.mem (name_of schema) [ "propose_set_budget"; "propose_categorize"; "propose_add_rule"; "propose_save_goal"; "propose_new_category" ])) P.tool_schemas in
  Alcotest.(check (list string)) "schema names" (List.map fst examples) (List.map name_of reads);
  (* The one proposing tool takes no currency: it acts in the conversation's own. *)
  let propose = List.find (fun schema -> name_of schema = "propose_set_budget") P.tool_schemas in
  let propose_params = member "function" propose |> member "parameters" in
  Alcotest.(check (list string)) "propose fields" ["category"; "monthly_amount"; "rollover"]
    (member "properties" propose_params |> to_assoc |> List.map fst |> List.sort String.compare);
  Alcotest.(check (list string)) "propose requires" ["category"; "monthly_amount"] (member "required" propose_params |> to_list |> List.map to_string);
  ok (Cashflow_io.Advisor_projection.parse_call ~name:"propose_set_budget" ~arguments:"{\"category\":\"account_1\",\"monthly_amount\":\"3500.50\",\"rollover\":true}");
  failed (Cashflow_io.Advisor_projection.parse_call ~name:"propose_set_budget" ~arguments:"{\"category\":\"account_1\",\"monthly_amount\":\"1\",\"currency\":\"SEK\"}");
  failed (Cashflow_io.Advisor_projection.parse_call ~name:"propose_set_budget" ~arguments:"{\"category\":\"account_1\"}");
  let simple name fields required =
    let params = List.find (fun schema -> name_of schema = name) P.tool_schemas |> member "function" |> member "parameters" in
    Alcotest.(check (list string)) (name ^ " fields") fields (member "properties" params |> to_assoc |> List.map fst |> List.sort String.compare);
    Alcotest.(check (list string)) (name ^ " requires") required (member "required" params |> to_list |> List.map to_string)
  in
  simple "propose_add_rule" [ "category"; "direction"; "pattern" ] [ "pattern"; "category" ];
  simple "propose_save_goal" [ "monthly_amount"; "name"; "target_amount"; "target_date" ] [ "name"; "target_amount" ];
  simple "propose_new_category" [ "kind"; "name" ] [ "name"; "kind" ];
  ok (Cashflow_io.Advisor_projection.parse_call ~name:"propose_add_rule" ~arguments:"{\"pattern\":\"spotify\",\"category\":\"account_1\",\"direction\":\"out\"}");
  ok (Cashflow_io.Advisor_projection.parse_call ~name:"propose_save_goal" ~arguments:"{\"name\":\"Sofa\",\"target_amount\":\"9000\",\"target_date\":\"2027-06-01\"}");
  ok (Cashflow_io.Advisor_projection.parse_call ~name:"propose_new_category" ~arguments:"{\"name\":\"Coffee\",\"kind\":\"expense\"}");
  List.iter
    (fun (name, arguments) -> failed (Cashflow_io.Advisor_projection.parse_call ~name ~arguments))
    [ ("propose_add_rule", "{\"pattern\":\"x\",\"category\":\"account_1\",\"currency\":\"SEK\"}");
      ("propose_add_rule", "{\"pattern\":\"x\",\"category\":\"account_1\",\"direction\":\"sideways\"}");
      ("propose_save_goal", "{\"name\":\"Sofa\"}");
      ("propose_save_goal", "{\"name\":\"Sofa\",\"target_amount\":\"1\",\"currency\":\"SEK\"}");
      ("propose_new_category", "{\"name\":\"Coffee\",\"kind\":\"asset\"}");
      ("propose_new_category", "{\"name\":\"Coffee\"}") ];
  let categorize = List.find (fun schema -> name_of schema = "propose_categorize") P.tool_schemas |> member "function" |> member "parameters" in
  Alcotest.(check (list string)) "categorize fields" ["rows"] (member "properties" categorize |> to_assoc |> List.map fst);
  ok (Cashflow_io.Advisor_projection.parse_call ~name:"propose_categorize" ~arguments:"{\"rows\":[{\"row\":\"row_ab_1\",\"category\":\"account_1\"}]}");
  failed (Cashflow_io.Advisor_projection.parse_call ~name:"propose_categorize" ~arguments:"{\"rows\":[{\"row\":\"account_1\",\"category\":\"account_1\"}]}");
  failed (Cashflow_io.Advisor_projection.parse_call ~name:"propose_categorize" ~arguments:"{\"rows\":[{\"row\":\"row_ab_1\",\"category\":\"account_1\",\"currency\":\"USD\"}]}");
  failed (Cashflow_io.Advisor_projection.parse_call ~name:"propose_categorize" ~arguments:"{\"rows\":[],\"currency\":\"SEK\"}");
  failed (Cashflow_io.Advisor_projection.parse_call ~name:"propose_categorize" ~arguments:(Printf.sprintf "{\"rows\":[%s]}" (String.concat "," (List.init 21 (fun i -> Printf.sprintf "{\"row\":\"row_ab_%d\",\"category\":\"account_1\"}" i)))));
  let properties s = member "properties" s |> to_assoc |> List.map fst |> List.sort String.compare in
  let expected = ["cycle_summary", ["cycles"]; "category_spending", ["cycle_offset"];
    "category_trend", ["category"; "cycles"]; "relevant_transactions", ["cycle_offset"; "category"; "limit"];
    "recurring", []; "budget_status", ["cycle_offset"]; "goals", []; "inbox_rows", []; "categories", []; "forecast", ["cycles"; "scenario"]] in
  List.iter2 (fun schema (name, keys) ->
    let params = member "function" schema |> member "parameters" in
    Alcotest.(check (list string)) (name ^ " exact fields") (List.sort String.compare ("currency" :: keys)) (properties params);
    Alcotest.(check bool) "no unknown fields" true (member "additionalProperties" params = `Bool false);
    Alcotest.(check (list string)) "only mandatory field" (if name = "category_trend" then ["category"] else []) (member "required" params |> to_list |> List.map to_string);
    failed (Cashflow_io.Advisor_projection.parse_call ~name ~arguments:"{\"unknown\":1}")) reads expected;
  let parameters name = List.find (fun s -> member "function" s |> member "name" = `String name) P.tool_schemas |> member "function" |> member "parameters" |> member "properties" in
  let bounds name key lo hi =
    let s = parameters name |> member key in
    Alcotest.(check int) "minimum" lo (member "minimum" s |> to_int);
    Alcotest.(check int) "maximum" hi (member "maximum" s |> to_int) in
  bounds "cycle_summary" "cycles" 1 6; bounds "category_trend" "cycles" 1 6;
  List.iter (fun name -> bounds name "cycle_offset" (-5) 0) ["category_spending"; "relevant_transactions"; "budget_status"];
  bounds "relevant_transactions" "limit" 1 20;
  let forecast = parameters "forecast" in
  Alcotest.(check (list int)) "forecast horizons" [1;3;6] (member "cycles" forecast |> member "enum" |> to_list |> List.map to_int);
  let scenario = member "scenario" forecast in
  Alcotest.(check (list string)) "scenario fields" ["follow_budgets"; "leave_out"; "one_off"] (properties scenario);
  let forecast_description = member "cycles" forecast |> member "description" |> to_string in
  Alcotest.(check bool) "documented actual default horizon" true
    (Cashflow_core.Str_compat.contains ~sub:"default 3" forecast_description);
  let sp = member "properties" scenario in
  Alcotest.(check int) "series cap" 20 (member "leave_out" sp |> member "maxItems" |> to_int);
  Alcotest.(check int) "one-off cap" 10 (member "one_off" sp |> member "maxItems" |> to_int);
  let item = member "one_off" sp |> member "items" in
  Alcotest.(check (list string)) "one-off exact fields" ["amount"; "date"; "description"] (properties item);
  let item_properties = member "properties" item in
  List.iter (fun (field, fragment) ->
    let description = member field item_properties |> member "description" |> to_string in
    Alcotest.(check bool) (field ^ " semantic guidance") true
      (Cashflow_core.Str_compat.contains ~sub:fragment description))
    ["date", "current local date"; "date", "inclusive";
     "amount", "Negative for expenses"; "amount", "pinned currency"; "amount", "Nonzero"];
  List.iter (fun (name,args) ->
    failed (Cashflow_io.Advisor_projection.parse_call ~name ~arguments:args);
    let result, events, _ = run [tools [full_call 0 "id" name args] ^ finish "tool_calls" ^ done_] in
    failed result; Alcotest.(check int) "typed failure never publishes calls" 0 (List.length events))
    ["cycle_summary", "{\"cycles\":7}"; "category_spending", "{\"cycle_offset\":-6}";
     "category_trend", "{}"; "category_trend", "{\"category\":\"raw-id\"}";
     "relevant_transactions", "{\"limit\":21}"; "budget_status", "{\"cycle_offset\":1}";
     "goals", "{\"currency\":\"sek\"}"; "forecast", "{\"cycles\":2}";
     "forecast", "{\"scenario\":{\"arbitrary\":true}}"]
let configured_bounds () =
  let sent = ref [] and activity = ref 0 in
  let post ~uri:_ ~headers:_ ~body ~on_chunk =
    sent := body :: !sent;
    List.iter on_chunk [": ignored\n\n"; text "live"; finish "stop"; done_]; Ok () in
  let make ?limits () = P.create ?limits ~post ~api_key:"fake" ~model:"synthetic" ~provider_only:["amazon-bedrock"] () in
  List.iter (fun p -> failed (P.prepare p request); failed (P.stream p request ~on_event:(fun _ -> Alcotest.fail "unsupported event")))
    [make ();
     make ~limits:{ limits with completion_tokens = 0 } ()];
  Alcotest.(check int) "unsupported sends nothing" 0 (List.length !sent);
  List.iter (fun l -> failed (P.prepare (make ~limits:l ()) request))
    [{ limits with context_tokens = 0 }; { limits with context_tokens = -1 };
     { limits with completion_tokens = max_int };
     { context_tokens = max_int; completion_tokens = max_int }];
  let supported = P.create ~post ~api_key:"fake" ~model:"anthropic/claude-sonnet-4.6"
    ~provider_only:["amazon-bedrock"] () in
  (match P.prepare supported request with Ok _ -> () | Error _ -> Alcotest.fail "supported profile default admission");
  let p = make ~limits () in
  let prepared = match P.prepare p request with Ok p -> p | Error _ -> Alcotest.fail "prepare" in
  ok (P.stream_prepared p prepared ~on_activity:(fun () -> incr activity) ~on_event:(fun _ -> ()));
  Alcotest.(check int) "exact measured request" (String.length (List.hd !sent)) (P.serialized_bytes prepared);
  Alcotest.(check int) "raw ignored activity" 4 !activity;
  let bytes = P.serialized_bytes prepared in
  Alcotest.(check int) "byte estimate plus framing allowance" (bytes+4096) (P.conservative_tokens prepared);
  let tight = {limits with context_tokens=bytes+4096+limits.completion_tokens} in
  let tight_prepared = match P.prepare (make ~limits:tight ()) request with Ok p -> p | Error _ -> Alcotest.fail "exact estimate" in
  Alcotest.(check int) "estimate" (bytes+4096) (P.conservative_tokens tight_prepared);
  failed (P.prepare (make ~limits:{tight with context_tokens=tight.context_tokens-1} ()) request);
  failed (P.stream_prepared (make ~limits ()) prepared ~on_activity:(fun () -> ()) ~on_event:(fun _ -> ()));
  let other = make ~limits:{ limits with completion_tokens = 12 } () in
  failed (P.stream_prepared other prepared ~on_activity:(fun () -> ()) ~on_event:(fun _ -> ()));
  Alcotest.(check int) "cross-configuration sends nothing" 1 (List.length !sent);
  let open Yojson.Safe.Util in
  let body = Yojson.Safe.from_string (List.hd !sent) in
  Alcotest.(check int) "whole completion cap" limits.completion_tokens (body |> member "max_tokens" |> to_int);
  Alcotest.(check bool) "require cap support" true (body |> member "provider" |> member "require_parameters" = `Bool true)
let safe_transport_diagnostics () =
  let previous = Logs.reporter () and global_level = Logs.level () in
  let levels = List.map (fun src -> src, Logs.Src.level src) (Logs.Src.list ()) in
  let captured = ref [] in
  let report src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ format ->
      Format.kasprintf (fun message ->
        captured := (Logs.Src.name src, level, message) :: !captured;
        over (); k ()) format) in
  Fun.protect ~finally:(fun () ->
    Logs.set_reporter previous;
    Logs.set_level ~all:false global_level;
    List.iter (fun (src, level) -> Logs.Src.set_level src level) levels)
    (fun () ->
      Logs.set_level (Some Logs.Debug);
      Logs.set_reporter { Logs.report };
      List.iter (fun (error, category) ->
        captured := [];
        let calls = ref 0 in
        let post ~uri:_ ~headers:_ ~body:_ ~on_chunk:_ = incr calls; Error error in
        let p = create ~post ~api_key:"SECRET-dummy-key" ~model:"PRIVATE-model"
          ~provider_only:["PRIVATE-provider"] in
        let request : P.request = { system = "PRIVATE-system"; messages = [P.User "PRIVATE-finances"] } in
        (match P.stream p request ~on_event:(fun _ -> Alcotest.fail "no error events") with
         | Error { A.code = A.Provider_unavailable; context = A.No_context } -> ()
         | _ -> Alcotest.fail "unchanged safe error");
        Alcotest.(check int) "no diagnostic retry" 1 !calls;
        (* Exact allowlisted log equality excludes all synthetic secret/request/error data. *)
        Alcotest.(check (list (triple string bool string))) "only safe boundary classification"
          [("cashflow.advisor_provider", true, "Advisor provider request failed: " ^ category)]
          (List.map (fun (source, level, message) -> source, level = Logs.Warning, message) !captured))
        [Cashflow_io.Https.Tls_configuration, "TLS_configuration";
         Cashflow_io.Https.Invalid_scheme, "invalid_scheme";
         Cashflow_io.Https.Http_status 400, "http_status400";
         Cashflow_io.Https.Http_status 403, "http_status403";
         Cashflow_io.Https.Http_status 503, "http_status503";
         Cashflow_io.Https.Transport "SECRET exception https://private/?key=SECRET header PRIVATE response body finances", "transport"])

let safe_decode_locations () =
  let previous = Logs.reporter () and global_level = Logs.level () in
  let levels = List.map (fun src -> src, Logs.Src.level src) (Logs.Src.list ()) in
  let captured = ref [] in
  let report src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ format ->
      Format.kasprintf (fun message ->
        captured := (Logs.Src.name src, level = Logs.Warning, message) :: !captured;
        over (); k ()) format) in
  Fun.protect ~finally:(fun () ->
    Logs.set_reporter previous;
    Logs.set_level ~all:false global_level;
    List.iter (fun (src, level) -> Logs.Src.set_level src level) levels)
    (fun () ->
      Logs.set_level (Some Logs.Debug);
      Logs.set_reporter { Logs.report };
      let choice fields = frame ("{\"choices\":[{" ^ fields ^ "}]}") in
      List.iter (fun (wire, location) ->
        captured := [];
        let calls = ref 0 in
        let post ~uri:_ ~headers:_ ~body:_ ~on_chunk = incr calls; on_chunk wire; Ok () in
        let p = create ~post ~api_key:"PRIVATE-key" ~model:"PRIVATE-model" ~provider_only:["PRIVATE-route"] in
        (match P.stream p { system = "PRIVATE-system"; messages = [P.User "PRIVATE-finances"] }
          ~on_event:(fun _ -> ()) with
         | Error { A.code = A.Invalid_stream; context = A.No_context } -> ()
         | _ -> Alcotest.fail "unchanged invalid stream classification");
        Alcotest.(check int) "no decode retry" 1 !calls;
        Alcotest.(check (list (triple string bool string))) "only fixed location, no payload"
          [("cashflow.advisor_provider", true, "Advisor provider invalid stream: location=" ^ location)]
          !captured)
        [frame "PRIVATE-invalid-json", "json";
         frame "[]", "root";
         frame "{\"usage\":{\"prompt_tokens\":\"PRIVATE-usage\"}}", "usage";
         frame "{\"choices\":\"PRIVATE-choices\"}", "choices";
         frame "{\"choices\":[\"PRIVATE-choice\"]}", "choice";
         choice "\"index\":1", "index";
         choice "\"index\":0,\"delta\":\"PRIVATE-delta\"", "delta";
         finish "stop" ^ text "PRIVATE-late-content", "content-after-finish";
         choice "\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":8,\"id\":\"PRIVATE-call\"}]}", "tool-fragment";
         finish "PRIVATE-finish-reason", "finish-reason";
         choice "\"index\":0,\"delta\":{\"content\":42}", "content";
         done_, "done-without-finish";
         finish "stop" ^ done_ ^ frame "PRIVATE-after-DONE", "after-DONE"];
      captured := [];
      let post ~uri:_ ~headers:_ ~body:_ ~on_chunk = on_chunk (text "PRIVATE-success" ^ finish "stop" ^ done_); Ok () in
      let p = create ~post ~api_key:"PRIVATE-key" ~model:"synthetic" ~provider_only:["synthetic"] in
      ok (P.stream p request ~on_event:(fun _ -> ()));
      Alcotest.(check int) "no successful-frame logs" 0 (List.length !captured))

let () = Alcotest.run "advisor provider" ["synthetic", [
  Alcotest.test_case "fragmented_provider_rounds" `Quick fragmented_provider_rounds;
  Alcotest.test_case "interleaved_tools" `Quick interleaved_tools;
  Alcotest.test_case "trailing no-op deltas" `Quick trailing_noop_deltas;
  Alcotest.test_case "failures" `Quick failures;
  Alcotest.test_case "cancellation" `Quick cancellation;
  Alcotest.test_case "follow-up policy and ordering" `Quick follow_up_policy;
  Alcotest.test_case "multiline, transport and live text" `Quick multiline_and_failures;
  Alcotest.test_case "configured completion bound and exact request" `Quick configured_bounds;
  Alcotest.test_case "safe transport diagnostics" `Quick safe_transport_diagnostics;
  Alcotest.test_case "safe decode locations" `Quick safe_decode_locations;
  Alcotest.test_case "schemas" `Quick schemas]]
