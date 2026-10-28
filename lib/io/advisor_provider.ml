module A = Cashflow_core.Advisor
module Sse = Cashflow_advisor_protocol.Sse

let log_src = Logs.Src.create "cashflow.advisor_provider" ~doc:"Advisor provider boundary"
module Log = (val Logs.src_log log_src : Logs.LOG)

(* Never log transport strings: they can contain credentials or request/response data. *)
let log_stream_error = function
  | Https.Tls_configuration -> Log.warn (fun m -> m "Advisor provider request failed: TLS_configuration")
  | Https.Invalid_scheme -> Log.warn (fun m -> m "Advisor provider request failed: invalid_scheme")
  | Https.Http_status status -> Log.warn (fun m -> m "Advisor provider request failed: http_status%d" status)
  | Https.Transport _ -> Log.warn (fun m -> m "Advisor provider request failed: transport")

type raw_call = { id : string; name : string; arguments : string; index : int }
type message = User of string | Assistant of string * raw_call list | Tool of { call_id : string; content : string }
type request = { system : string; messages : message list }
type finish_reason = Stop | Tool_calls | Length | Content_filter
type usage = { prompt_tokens : int; completion_tokens : int; total_tokens : int }
type provider_event = Text of string | Calls_ready of raw_call list | Round_finished of finish_reason
type generation_limits = { context_tokens : int; completion_tokens : int }
type t = { limits : generation_limits option; identity : unit ref; post : Https.stream_post; api_key : string; app_url : string option; model : string; provider_only : string list }
(* Policy below the documented Sonnet Bedrock capabilities, not a
   tokenizer/framing guarantee. See design/advisor-deployment.md for sources. *)
let supported_models = [ "anthropic/claude-sonnet-5.5"; "anthropic/claude-sonnet-4.6" ]

let supported_limits ~model ~provider_only =
  if List.mem model supported_models && provider_only = ["amazon-bedrock"] then
    Some { context_tokens = 200000; completion_tokens = 8192 }
  else None

let create ?limits ?app_url ~post ~api_key ~model ~provider_only () =
  let limits = match limits with Some _ -> limits | None -> supported_limits ~model ~provider_only in
  { limits; identity = ref (); post; api_key; app_url; model; provider_only }
let model t = t.model
let limits t = t.limits
type prepared_request = { identity : unit ref; body : string; conservative_tokens : int }
let serialized_bytes p = String.length p.body
let conservative_tokens p = p.conservative_tokens

let str s = `String s
let integer lo hi = `Assoc ["type", str "integer"; "minimum", `Int lo; "maximum", `Int hi]
let string max = `Assoc ["type", str "string"; "maxLength", `Int max]
let object_ required properties = `Assoc ["type", str "object"; "properties", `Assoc properties;
  "required", `List (List.map str required); "additionalProperties", `Bool false]
let array max items = `Assoc ["type", str "array"; "maxItems", `Int max; "items", items]
let reference prefix = `Assoc ["type", str "string"; "maxLength", `Int 100;
  "pattern", str ("^" ^ prefix ^ "[a-z0-9_]+$")]
let tool_schemas =
  let currency = `Assoc ["type", str "string"; "pattern", str "^[A-Z]{3}$"] in
  let cycles = "cycles", integer 1 6 and offset = "cycle_offset", integer (-5) 0 in
  let category = "category", reference "account_" in
  let scenario = object_ [] ["leave_out", array 20 (reference "series_");
    "one_off", array 10 (object_ ["date"; "amount"] [
      "date", `Assoc ["type", str "string"; "format", str "date"; "maxLength", `Int 512;
        "description", str "ISO YYYY-MM-DD, from the current local date through that date plus the selected cycles in months, inclusive. Choose a horizon covering the date; ask if the intended year is unclear."];
      "amount", `Assoc ["type", str "string"; "maxLength", `Int 30;
        "description", str "Nonzero signed decimal in the pinned currency (not minor units), within its decimal precision. Negative for expenses, positive for income; no currency symbols or thousands separators."];
      "description", string 512]); "follow_budgets", `Assoc ["type", str "boolean"]] in
  let propose_categorize = `Assoc ["type", str "function"; "function", `Assoc [
    "name", str "propose_categorize";
    "description", str "Propose filing uncategorized transactions under categories, up to 20 rows. Use row references from inbox_rows and category references from categories (expense category for money out, income category for money in). Nothing changes until the user confirms it with Apply; you only see that it is awaiting confirmation.";
    "parameters", object_ ["rows"] ["rows", array 20 (object_ ["row"; "category"] ["row", reference "row_"; "category", reference "account_"])]]] in
  let propose name description required properties =
    `Assoc ["type", str "function"; "function", `Assoc ["name", str name; "description", str (description ^ " Nothing changes until the user confirms it with Apply; you only see that it is awaiting confirmation."); "parameters", object_ required properties]] in
  let propose_rule = propose "propose_add_rule" "Propose a rule that suggests a category on import when a merchant's text contains a pattern. It only suggests; it never files anything by itself."
    ["pattern"; "category"] ["pattern", string 60; category; "direction", `Assoc ["type", str "string"; "enum", `List [str "in"; str "out"]]] in
  let propose_goal = propose "propose_save_goal" "Propose a new savings goal."
    ["name"; "target_amount"] ["name", string 60;
      "target_amount", `Assoc ["type", str "string"; "maxLength", `Int 30; "description", str "Positive decimal in the pinned currency (not minor units), no symbols or separators."];
      "target_date", `Assoc ["type", str "string"; "format", str "date"; "maxLength", `Int 30; "description", str "ISO YYYY-MM-DD, in the future. Optional."];
      "monthly_amount", `Assoc ["type", str "string"; "maxLength", `Int 30; "description", str "Planned monthly saving, non-negative decimal in the pinned currency. Optional."]] in
  let propose_update_goal = propose "propose_update_goal" "Propose changing an existing savings goal. Use a goal reference from goals, and give only the fields that change; fields left out stay as they are (a date or monthly saving cannot be removed this way)."
    ["goal"] ["goal", reference "goal_"; "name", string 60;
      "target_amount", `Assoc ["type", str "string"; "maxLength", `Int 30; "description", str "New target, positive decimal in the pinned currency (not minor units), no symbols or separators."];
      "target_date", `Assoc ["type", str "string"; "format", str "date"; "maxLength", `Int 30; "description", str "New ISO YYYY-MM-DD target date, in the future."];
      "monthly_amount", `Assoc ["type", str "string"; "maxLength", `Int 30; "description", str "New planned monthly saving, non-negative decimal in the pinned currency."]] in
  let propose_entry = propose "propose_entry" "Propose a manual entry: a purchase (bank account to expense category), income (income category to bank account) or a move between two of the user's own accounts. Use account references from bank_accounts and categories."
    ["from_account"; "to_account"; "amount"; "description"] ["from_account", reference "account_"; "to_account", reference "account_";
      "amount", `Assoc ["type", str "string"; "maxLength", `Int 30; "description", str "Positive decimal in the pinned currency (not minor units), no symbols or separators."];
      "date", `Assoc ["type", str "string"; "format", str "date"; "maxLength", `Int 30; "description", str "ISO YYYY-MM-DD, within the last year or next month. Defaults to today."];
      "description", string 80] in
  let propose_category = propose "propose_new_category" "Propose a new expense or income category, for the conversation's own scope."
    ["name"; "kind"] ["name", string 40; "kind", `Assoc ["type", str "string"; "enum", `List [str "expense"; str "income"]]] in
  (* A proposal acts in the conversation's own scope and currency, so it takes no currency argument. *)
  let file_inbox = `Assoc ["type", str "function"; "function", `Assoc [
    "name", str "file_inbox";
    "description", str "Only when the user asks you to categorise their transactions for them: files, without asking, every uncategorized row cashflow is at least 80 percent sure of (the user's own rules and past choices, or a calibrated classifier), and returns what was filed and the rows that need the user's review with cashflow's best guess. This changes their books at once; each filed row can be recategorised. Report what was filed, then list the rows needing review and offer propose_categorize for them.";
    "parameters", object_ [] ["keep_on", `Assoc ["type", str "boolean";
      "description", str "Also file future imports by themselves at the same confidence, when the user asked for this to continue."]]]] in
  let propose_budget = `Assoc ["type", str "function"; "function", `Assoc [
    "name", str "propose_set_budget";
    "description", str "Propose a monthly budget for one expense category. Nothing changes until the user confirms it with Apply; you only see that it is awaiting confirmation.";
    "parameters", object_ ["category"; "monthly_amount"] [category;
      "monthly_amount", `Assoc ["type", str "string"; "maxLength", `Int 30;
        "description", str "Non-negative decimal in the pinned currency (not minor units). No currency symbols or thousands separators. 0 removes the budget."];
      "rollover", `Assoc ["type", str "boolean"; "description", str "Carry what is left or overspent into the next cycle."]]]] in
  List.map (fun (name, required, fields) -> `Assoc ["type", str "function";
    "function", `Assoc ["name", str name; "parameters", object_ required (("currency", currency) :: fields)]])
    ["cycle_summary", [], [cycles]; "category_spending", [], [offset];
     "category_trend", ["category"], [category; cycles];
     "relevant_transactions", [], [offset; category; "limit", integer 1 20];
     "recurring", [], []; "budget_status", [], [offset]; "goals", [], [];
     "inbox_rows", [], []; "categories", [], []; "bank_accounts", [], [];
     "forecast", [], ["cycles", `Assoc ["type", str "integer"; "enum", `List [`Int 1; `Int 3; `Int 6];
       "description", str "Forecast horizon in months from the current local date: 1, 3 or 6 (default 3). All one-off dates must fit this horizon."]; "scenario", scenario]] @ [propose_budget; propose_categorize; propose_rule; propose_goal; propose_update_goal; propose_category; propose_entry; file_inbox]

let call_json (c : raw_call) = `Assoc ["id", str c.id; "type", str "function";
  "function", `Assoc ["name", str c.name; "arguments", str c.arguments]]
let message_json = function
  | User content -> `Assoc ["role", str "user"; "content", str content]
  | Assistant (content, calls) -> `Assoc (["role", str "assistant"; "content", str content] @
      if calls = [] then [] else ["tool_calls", `List (List.map call_json calls)])
  | Tool { call_id; content } -> `Assoc ["role", str "tool"; "tool_call_id", str call_id; "content", str content]
let body t completion_tokens request = Yojson.Safe.to_string (`Assoc [
  "max_tokens", `Int completion_tokens; "model", str t.model; "stream", `Bool true; "stream_options", `Assoc ["include_usage", `Bool true];
  "messages", `List (`Assoc ["role", str "system"; "content", str request.system] :: List.map message_json request.messages);
  "tools", `List tool_schemas;
  "provider", `Assoc ["require_parameters", `Bool true; "only", `List (List.map str t.provider_only); "zdr", `Bool true; "data_collection", str "deny"]])

exception Decode of A.error
let reject code = raise (Decode { A.code; context = A.No_context })
let invalid () = reject A.Invalid_stream
let fields = function `Assoc fields -> fields | _ -> invalid ()
let member key fields = Option.value (List.assoc_opt key fields) ~default:`Null
let int = function `Int n when n >= 0 -> n | _ -> invalid ()
let bounded max = function `String s when String.length s <= max -> s | _ -> invalid ()
(* Per round: eight calls, 8 KiB arguments each, 128-byte names and 256-byte
   IDs; raw traffic is also capped so ignored provider fields cannot grow a round. *)
type fragment = { mutable call_id : string; name_buffer : Buffer.t; args : Buffer.t }
let append max buffer json = match json with
  | `Null -> ()
  | `String s when Buffer.length buffer + String.length s <= max -> Buffer.add_string buffer s
  | _ -> invalid ()
let prepare (t : t) request =
  match t.limits with
  | Some l when l.context_tokens > 0 && l.completion_tokens > 0 &&
      t.provider_only <> [] ->
      let body = body t l.completion_tokens request in
      let bytes = String.length body in
      (* One token per serialized UTF-8 byte plus an estimated framing allowance.
         Upstream context errors remain terminal; this is not a tokenizer proof. *)
      if bytes > max_int - 4096 then
        Error { A.code = A.Invalid_request; context = A.No_context }
      else let conservative_tokens = bytes + 4096 in
      if l.completion_tokens > l.context_tokens || conservative_tokens > l.context_tokens - l.completion_tokens then
        Error { A.code = A.Run_limit_reached; context = A.Limit A.Context_bytes }
      else Ok { identity = t.identity; body; conservative_tokens }
  | _ -> Error { A.code = A.Invalid_request; context = A.No_context }

let stream_prepared (t : t) prepared ~on_activity ~on_event =
  if t.identity != prepared.identity then Error { A.code = A.Invalid_request; context = A.No_context }
  else
  let parser = Sse.create ~max_frame_bytes:(256 * 1024) in
  let fragments = Hashtbl.create 8 in
  let reason = ref None and done_seen = ref false and bytes = ref 0 and output = ref 0 in
  let usage = ref { prompt_tokens = 0; completion_tokens = 0; total_tokens = 0 } in
  (* Only fixed local tags are assigned here, never provider-supplied values.
     Record the active invariant without logging successful frames or payloads. *)
  let location = ref "sse-framing" in
  let decode_frame (frame : Sse.frame) =
    location := "after-DONE";
    if !done_seen then invalid ();
    if frame.event = Some "error" then reject A.Provider_unavailable;
    if frame.data = "[DONE]" then (location := "done-without-finish"; if !reason = None then invalid (); done_seen := true; [])
    else (
      location := "json";
      let json = try Yojson.Safe.from_string frame.data with Yojson.Json_error _ | Stack_overflow -> invalid () in
      location := "root";
      let f = fields json in
      if List.mem_assoc "error" f then reject A.Provider_unavailable;
      location := "usage";
      (match member "usage" f with
      | `Null -> ()
      | u -> let u = fields u in
          let count key old = match member key u with `Null -> old | x -> max old (int x) in
          usage := { prompt_tokens = count "prompt_tokens" !usage.prompt_tokens;
            completion_tokens = count "completion_tokens" !usage.completion_tokens;
            total_tokens = count "total_tokens" !usage.total_tokens });
      location := "choices";
      match member "choices" f with
      | `Null | `List [] -> []
      | `List [choice] ->
          location := "choice";
          let c = fields choice in
          location := "index";
          if member "index" c <> `Int 0 then invalid ();
          location := "delta";
          let delta = match member "delta" c with `Null -> [] | d -> fields d in
          let content = member "content" delta and tools = member "tool_calls" delta in
          location := "content-after-finish";
          (* Trailing usage envelopes may carry empty deltas, but never new
             semantic output or malformed content/tool fields after finish. *)
          if !reason <> None &&
             (not (content = `Null || content = `String "") ||
              not (tools = `Null || tools = `List [])) then invalid ();
          location := "tool-fragment";
          (match tools with
          | `Null -> ()
          | `List calls -> List.iter (fun call ->
              let c = fields call in
              let index = int (member "index" c) in
              if index >= 8 then invalid ();
              let fragment = match Hashtbl.find_opt fragments index with
                | Some f -> f
                | None -> let f = { call_id = ""; name_buffer = Buffer.create 32; args = Buffer.create 128 } in
                    Hashtbl.add fragments index f; f in
              (match member "type" c with `Null | `String "function" -> () | _ -> invalid ());
              (match member "id" c with
              | `Null -> ()
              | id -> let id = bounded 256 id in
                  if id = "" || (fragment.call_id <> "" && fragment.call_id <> id) then invalid ();
                  if Hashtbl.fold (fun i f duplicate -> duplicate || (i <> index && f.call_id = id)) fragments false then invalid ();
                  fragment.call_id <- id);
              (match member "function" c with
              | `Null -> ()
              | fn -> let fn = fields fn in
                  append 128 fragment.name_buffer (member "name" fn);
                  append 8192 fragment.args (member "arguments" fn))) calls
          | _ -> invalid ());
          location := "finish-reason";
          (match member "finish_reason" c with
          | `Null -> ()
          | r -> let r = match r with
              | `String "stop" -> Stop | `String "tool_calls" -> Tool_calls
              | `String "length" -> Length | `String "content_filter" -> Content_filter | _ -> invalid () in
              (match !reason with None -> reason := Some r | Some old when old = r -> () | _ -> invalid ()));
          location := "content";
          (match content with
          | `Null | `String "" -> []
          | `String text -> output := !output + String.length text;
              if !output > 65536 then raise (Decode { A.code = A.Run_limit_reached; context = A.Limit A.Output_bytes });
              [Text text]
          | _ -> invalid ())
      | _ -> invalid ())
  in
  (* Only our private decoder exception is caught. Cancellation and callback
     exceptions must cross this boundary unchanged. No provider body is logged. *)
  try
    let on_chunk chunk =
      on_activity ();
      location := "raw-byte-limit";
      bytes := !bytes + String.length chunk;
      if !bytes > 2 * 1024 * 1024 then invalid ();
      location := "sse-framing";
      let frames = match Sse.feed parser chunk with Ok f -> f | Error _ -> invalid () in
      List.iter (fun frame -> List.iter on_event (decode_frame frame)) frames
    in
    match t.post ~uri:"https://openrouter.ai/api/v1/chat/completions"
      ~headers:(["authorization", "Bearer " ^ t.api_key; "content-type", "application/json"] @ Llm.attribution_headers ?app_url:t.app_url ())
      ~body:prepared.body ~on_chunk with
    | Error error ->
        log_stream_error error;
        Error { A.code = A.Provider_unavailable; context = A.No_context }
    | Ok () ->
        location := "sse-finish";
        (match Sse.finish parser with Error _ -> invalid () | Ok () -> ());
        location := "missing-DONE";
        if not !done_seen then invalid ();
        location := "tool-finalization";
        let calls = Hashtbl.fold (fun index f calls ->
          let name = Buffer.contents f.name_buffer and arguments = Buffer.contents f.args in
          if f.call_id = "" then invalid ();
          (match Advisor_projection.parse_call ~name ~arguments with Ok _ -> () | Error e -> raise (Decode e));
          { id = f.call_id; name; arguments; index } :: calls) fragments []
          |> List.sort (fun a b -> Int.compare a.index b.index) in
        location := "finish-consistency";
        let reason = match !reason with Some r -> r | None -> invalid () in
        if (calls <> []) <> (reason = Tool_calls) then invalid ();
        if calls <> [] then on_event (Calls_ready calls);
        on_event (Round_finished reason);
        Ok !usage
  with Decode e ->
    if e.A.code = A.Invalid_stream then
      Log.warn (fun m -> m "Advisor provider invalid stream: location=%s" !location);
    Error e

let stream t request ~on_event =
  match prepare t request with
  | Error e -> Error e
  | Ok prepared -> stream_prepared t prepared ~on_activity:(fun () -> ()) ~on_event
