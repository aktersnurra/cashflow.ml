module A = Cashflow_core.Advisor
module Sse = Cashflow_advisor_protocol.Sse

type raw_call = { id : string; name : string; arguments : string; index : int }
type message = User of string | Assistant of string * raw_call list | Tool of { call_id : string; content : string }
type request = { system : string; messages : message list }
type finish_reason = Stop | Tool_calls | Length | Content_filter
type usage = { prompt_tokens : int; completion_tokens : int; total_tokens : int }
type provider_event = Text of string | Calls_ready of raw_call list | Round_finished of finish_reason
type t = { post : Https.stream_post; api_key : string; model : string; provider_only : string list }
let create ~post ~api_key ~model ~provider_only = { post; api_key; model; provider_only }

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
      "date", `Assoc ["type", str "string"; "format", str "date"; "maxLength", `Int 512];
      "amount", string 30; "description", string 512]); "follow_budgets", `Assoc ["type", str "boolean"]] in
  List.map (fun (name, required, fields) -> `Assoc ["type", str "function";
    "function", `Assoc ["name", str name; "parameters", object_ required (("currency", currency) :: fields)]])
    ["cycle_summary", [], [cycles]; "category_spending", [], [offset];
     "category_trend", ["category"], [category; cycles];
     "relevant_transactions", [], [offset; category; "limit", integer 1 20];
     "recurring", [], []; "budget_status", [], [offset]; "goals", [], [];
     "forecast", [], ["cycles", `Assoc ["type", str "integer"; "enum", `List [`Int 1; `Int 3; `Int 6]]; "scenario", scenario]]

let call_json (c : raw_call) = `Assoc ["id", str c.id; "type", str "function";
  "function", `Assoc ["name", str c.name; "arguments", str c.arguments]]
let message_json = function
  | User content -> `Assoc ["role", str "user"; "content", str content]
  | Assistant (content, calls) -> `Assoc (["role", str "assistant"; "content", str content] @
      if calls = [] then [] else ["tool_calls", `List (List.map call_json calls)])
  | Tool { call_id; content } -> `Assoc ["role", str "tool"; "tool_call_id", str call_id; "content", str content]
let body t request = Yojson.Safe.to_string (`Assoc [
  "model", str t.model; "stream", `Bool true; "stream_options", `Assoc ["include_usage", `Bool true];
  "messages", `List (`Assoc ["role", str "system"; "content", str request.system] :: List.map message_json request.messages);
  "tools", `List tool_schemas;
  "provider", `Assoc ["only", `List (List.map str t.provider_only); "zdr", `Bool true; "data_collection", str "deny"]])

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
let stream t request ~on_event =
  if t.provider_only = [] then Error { A.code = A.Provider_unavailable; context = A.No_context }
  else
  let parser = Sse.create ~max_frame_bytes:(256 * 1024) in
  let fragments = Hashtbl.create 8 in
  let reason = ref None and done_seen = ref false and bytes = ref 0 and output = ref 0 in
  let usage = ref { prompt_tokens = 0; completion_tokens = 0; total_tokens = 0 } in
  let decode_frame (frame : Sse.frame) =
    if !done_seen then invalid ();
    if frame.event = Some "error" then reject A.Provider_unavailable;
    if frame.data = "[DONE]" then (if !reason = None then invalid (); done_seen := true; [])
    else
      let json = try Yojson.Safe.from_string frame.data with Yojson.Json_error _ | Stack_overflow -> invalid () in
      let f = fields json in
      if List.mem_assoc "error" f then reject A.Provider_unavailable;
      (match member "usage" f with
      | `Null -> ()
      | u -> let u = fields u in
          let count key old = match member key u with `Null -> old | x -> max old (int x) in
          usage := { prompt_tokens = count "prompt_tokens" !usage.prompt_tokens;
            completion_tokens = count "completion_tokens" !usage.completion_tokens;
            total_tokens = count "total_tokens" !usage.total_tokens });
      match member "choices" f with
      | `Null | `List [] -> []
      | `List [choice] ->
          let c = fields choice in
          if member "index" c <> `Int 0 then invalid ();
          let delta = match member "delta" c with `Null -> [] | d -> fields d in
          let content = member "content" delta and tools = member "tool_calls" delta in
          if !reason <> None && (content <> `Null || tools <> `Null) then invalid ();
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
          (match member "finish_reason" c with
          | `Null -> ()
          | r -> let r = match r with
              | `String "stop" -> Stop | `String "tool_calls" -> Tool_calls
              | `String "length" -> Length | `String "content_filter" -> Content_filter | _ -> invalid () in
              (match !reason with None -> reason := Some r | Some old when old = r -> () | _ -> invalid ()));
          (match content with
          | `Null | `String "" -> []
          | `String text -> output := !output + String.length text;
              if !output > 65536 then raise (Decode { A.code = A.Run_limit_reached; context = A.Limit A.Output_bytes });
              [Text text]
          | _ -> invalid ())
      | _ -> invalid ()
  in
  (* Only our private decoder exception is caught. Cancellation and callback
     exceptions must cross this boundary unchanged. No provider body is logged. *)
  try
    let on_chunk chunk =
      bytes := !bytes + String.length chunk;
      if !bytes > 2 * 1024 * 1024 then invalid ();
      let frames = match Sse.feed parser chunk with Ok f -> f | Error _ -> invalid () in
      List.iter (fun frame -> List.iter on_event (decode_frame frame)) frames
    in
    match t.post ~uri:"https://openrouter.ai/api/v1/chat/completions"
      ~headers:["authorization", "Bearer " ^ t.api_key; "content-type", "application/json"; "x-title", "cashflow"]
      ~body:(body t request) ~on_chunk with
    | Error _ -> Error { A.code = A.Provider_unavailable; context = A.No_context }
    | Ok () ->
        (match Sse.finish parser with Error _ -> invalid () | Ok () -> ());
        if not !done_seen then invalid ();
        let calls = Hashtbl.fold (fun index f calls ->
          let name = Buffer.contents f.name_buffer and arguments = Buffer.contents f.args in
          if f.call_id = "" then invalid ();
          (match Advisor_projection.parse_call ~name ~arguments with Ok _ -> () | Error e -> raise (Decode e));
          { id = f.call_id; name; arguments; index } :: calls) fragments []
          |> List.sort (fun a b -> Int.compare a.index b.index) in
        let reason = match !reason with Some r -> r | None -> invalid () in
        if (calls <> []) <> (reason = Tool_calls) then invalid ();
        if calls <> [] then on_event (Calls_ready calls);
        on_event (Round_finished reason);
        Ok !usage
  with Decode e -> Error e
