type currency = { code : string; scale : int }
type scope = Personal | Household of string
type phase = Awaiting_model | Streaming_text | Running_tools

type limit_kind =
  | Active_runs
  | User_bytes
  | Output_bytes
  | Model_rounds
  | Tool_calls
  | Tool_result_bytes
  | Evidence_bytes
  | Context_turns
  | Context_bytes
  | Run_deadline
  | Upstream_inactivity
  | Frame_bytes
  | Subscriber_bytes
  | Journal_bytes

type resource_kind = Conversation | Turn | Attempt | Run | Message | Evidence

type error_code =
  | Access_denied
  | Advisor_disabled
  | Consent_required
  | Scope_mismatch
  | Run_busy
  | Idempotency_conflict
  | Invalid_tool_arguments
  | Tool_not_allowed
  | Provider_unavailable
  | Invalid_stream
  | Run_limit_reached
  | Storage_unavailable
  | Server_restarted
  | Invalid_transition
  | Invalid_request
  | Unsupported_version
  | Version_conflict
  | Capacity_exceeded
  | Cursor_expired
  | Cursor_gap
  | Not_found

type error_context =
  | No_context
  | Limit of limit_kind
  | Version of string
  | Cursor of string
  | Resource of resource_kind

type error = { code : error_code; context : error_context }
type run_state = Queued | Running of phase | Cancel_requested | Completed | Cancelled | Failed of error
type money = { decimal : string; currency : currency }
type finding = { kind : string; key : string; scope : scope; title : string; summary : string; sampled_at : string }
type display_value = Text of string | Money of money | Date of string
type fact = { label : string; value : display_value }
type import_status = Current | Stale | Incomplete | Unknown

type evidence = {
  id : string;
  scope : scope;
  currency : currency;
  sampled_at : string;
  title : string;
  facts : fact list;
  source_references : string list;
  import_status : import_status;
  reconciled : bool;
  assumptions : string list;
}

type identities = {
  conversation_id : string;
  turn_id : string;
  attempt_id : string;
  user_message_id : string;
  assistant_message_id : string;
  run_id : string;
}

type submit = {
  conversation_id : string option;
  scope : scope;
  currency : currency;
  locale : string;
  user_text : string;
  idempotency_key : string;
  expected_version : string;
  source_finding_key : string option;
}

type accepted = { ids : identities; accepted_version : string; idempotent_repeat : bool }
type retry = { turn_id : string; expected_version : string; idempotency_key : string }
type tool_outcome = Succeeded | Unavailable | Rejected
type tool_progress = { label : string; outcome : tool_outcome }
type text_delta = { segment_id : string; text : string }

type event_payload =
  | Run_started
  | Text_appended of text_delta
  | Tool_started of string
  | Tool_finished of tool_progress
  | Evidence_added of evidence
  | Run_completed
  | Run_cancelled
  | Run_failed of error

type event = { ids : identities; sequence : string; payload : event_payload }
type segment = Text_segment of text_delta | Evidence_segment of evidence

type attempt = {
  ids : identities;
  ordinal : string;
  state : run_state;
  segments : segment list;
  last_sequence : string;
}

type turn = { turn_id : string; user_message_id : string; user_text : string; attempts : attempt list }

type snapshot = {
  conversation_id : string;
  scope : scope;
  currency : currency;
  locale : string;
  source_finding : finding option;
  turns : turn list;
  active_run : string option;
  version : string;
}

type decode_error = Invalid_json | Invalid_field of string | Unknown_variant of string | Unknown_version

let version = 1

exception Decode of decode_error

let invalid field = raise (Decode (Invalid_field field))

let valid_utf8 s =
  let length = String.length s in
  let byte i = Char.code s.[i] in
  let continuation i = i < length && byte i >= 0x80 && byte i <= 0xbf in
  let rec loop i =
    if i = length then true
    else
      let b = byte i in
      if b < 0x80 then loop (i + 1)
      else if b >= 0xc2 && b <= 0xdf then continuation (i + 1) && loop (i + 2)
      else if b >= 0xe0 && b <= 0xef then
        continuation (i + 1)
        && continuation (i + 2)
        && (b <> 0xe0 || byte (i + 1) >= 0xa0)
        && (b <> 0xed || byte (i + 1) < 0xa0)
        && loop (i + 3)
      else if b >= 0xf0 && b <= 0xf4 then
        continuation (i + 1)
        && continuation (i + 2)
        && continuation (i + 3)
        && (b <> 0xf0 || byte (i + 1) >= 0x90)
        && (b <> 0xf4 || byte (i + 1) < 0x90)
        && loop (i + 4)
      else false
  in
  loop 0

let str = function `String s when valid_utf8 s -> s | _ -> invalid "string"
let boolean = function `Bool b -> b | _ -> invalid "bool"
let integer = function `Int n -> n | _ -> invalid "integer"

let fields = function
  | `Assoc fs when List.length fs = List.length (List.sort_uniq String.compare (List.map fst fs)) -> fs
  | _ -> invalid "object"

let field name j = match List.assoc_opt name (fields j) with Some v -> v | None -> invalid name
let list decode = function `List xs -> List.map decode xs | _ -> invalid "list"
let option decode = function `Null -> None | j -> Some (decode j)
let jlist encode xs = `List (List.map encode xs)
let joption encode = function None -> `Null | Some x -> encode x

let uuid s =
  let valid = ref (String.length s = 36) in
  String.iteri
    (fun i c ->
      let ok =
        if List.mem i [ 8; 13; 18; 23 ] then c = '-' else match c with '0' .. '9' | 'a' .. 'f' -> true | _ -> false
      in
      if not ok then valid := false)
    s;
  if not !valid then invalid "uuid";
  s

let counter s =
  (try
     let n = Int64.of_string s in
     if n < 0L || Int64.to_string n <> s then invalid "counter"
   with Failure _ -> invalid "counter");
  s

let digits_at s start length =
  if start < 0 || length < 1 || start + length > String.length s then invalid "date/time";
  let part = String.sub s start length in
  if not (String.for_all (function '0' .. '9' -> true | _ -> false) part) then invalid "date/time";
  int_of_string part

let valid_date s =
  if String.length s <> 10 || s.[4] <> '-' || s.[7] <> '-' then invalid "date";
  let year = digits_at s 0 4 and month = digits_at s 5 2 and day = digits_at s 8 2 in
  let leap = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) in
  let days =
    match month with
    | 2 -> if leap then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | _ -> invalid "date"
  in
  if day < 1 || day > days then invalid "date";
  s

(* Version 1 supports RFC3339 without leap seconds (00..59); no extra browser dependency. *)
let timestamp s =
  let length = String.length s in
  if length < 20 || s.[10] <> 'T' || s.[13] <> ':' || s.[16] <> ':' then invalid "timestamp";
  ignore (valid_date (String.sub s 0 10));
  if digits_at s 11 2 > 23 || digits_at s 14 2 > 59 || digits_at s 17 2 > 59 then invalid "timestamp";
  let zone =
    if s.[19] = '.' then (
      let pos = ref 20 in
      while !pos < length && s.[!pos] >= '0' && s.[!pos] <= '9' do
        incr pos
      done;
      if !pos = 20 then invalid "timestamp";
      !pos)
    else 19
  in
  if zone = length - 1 && s.[zone] = 'Z' then s
  else if
    zone + 6 = length
    && (s.[zone] = '+' || s.[zone] = '-')
    && s.[zone + 3] = ':'
    && digits_at s (zone + 1) 2 <= 23
    && digits_at s (zone + 4) 2 <= 59
  then s
  else invalid "timestamp"

let key s =
  if String.length s = 0 || String.length s > 256 || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) s
  then invalid "idempotency_key";
  s

let locale s =
  if s <> "en" && s <> "sv" then invalid "locale";
  s

let nonempty name s =
  if s = "" then invalid name;
  s

let valid_currency (c : currency) =
  if
    String.length c.code <> 3
    || (not (String.for_all (function 'A' .. 'Z' -> true | _ -> false) c.code))
    || c.scale < 0 || c.scale > 4
  then invalid "currency";
  c

let currency_to_json (c : currency) = `Assoc [ ("code", `String c.code); ("scale", `Int c.scale) ]
let currency_of_json j = valid_currency { code = str (field "code" j); scale = integer (field "scale" j) }

let valid_money (m : money) =
  let s = m.decimal in
  let unsigned = if String.length s > 0 && s.[0] = '-' then String.sub s 1 (String.length s - 1) else s in
  let parts = String.split_on_char '.' unsigned in
  let whole, fraction =
    match parts with
    | [ w ] when m.currency.scale = 0 -> (w, "")
    | [ w; f ] when String.length f = m.currency.scale && m.currency.scale > 0 -> (w, f)
    | _ -> invalid "money"
  in
  let digits s = String.for_all (function '0' .. '9' -> true | _ -> false) s in
  if whole = "" || (String.length whole > 1 && whole.[0] = '0') || not (digits whole && digits fraction) then
    invalid "money";
  let units = (if String.starts_with ~prefix:"-" s then "-" else "") ^ whole ^ fraction in
  (try ignore (Int64.of_string units) with Failure _ -> invalid "money");
  if String.starts_with ~prefix:"-" s && String.for_all (( = ) '0') (whole ^ fraction) then invalid "money";
  m

let money_to_json (m : money) = `Assoc [ ("decimal", `String m.decimal); ("currency", currency_to_json m.currency) ]

let money_of_json j =
  valid_money { decimal = str (field "decimal" j); currency = currency_of_json (field "currency" j) }

let scope_to_json = function
  | Personal -> `Assoc [ ("type", `String "Personal") ]
  | Household x -> `Assoc [ ("type", `String "Household"); ("value", `String x) ]

let scope_of_json j =
  match str (field "type" j) with
  | "Personal" -> Personal
  | "Household" -> Household (uuid (str (field "value" j)))
  | s -> raise (Decode (Unknown_variant s))

let phase_to_json = function
  | Awaiting_model -> `Assoc [ ("type", `String "Awaiting_model") ]
  | Streaming_text -> `Assoc [ ("type", `String "Streaming_text") ]
  | Running_tools -> `Assoc [ ("type", `String "Running_tools") ]

let phase_of_json j =
  match str (field "type" j) with
  | "Awaiting_model" -> Awaiting_model
  | "Streaming_text" -> Streaming_text
  | "Running_tools" -> Running_tools
  | s -> raise (Decode (Unknown_variant s))

let limit_kind_to_json = function
  | Active_runs -> `Assoc [ ("type", `String "Active_runs") ]
  | User_bytes -> `Assoc [ ("type", `String "User_bytes") ]
  | Output_bytes -> `Assoc [ ("type", `String "Output_bytes") ]
  | Model_rounds -> `Assoc [ ("type", `String "Model_rounds") ]
  | Tool_calls -> `Assoc [ ("type", `String "Tool_calls") ]
  | Tool_result_bytes -> `Assoc [ ("type", `String "Tool_result_bytes") ]
  | Evidence_bytes -> `Assoc [ ("type", `String "Evidence_bytes") ]
  | Context_turns -> `Assoc [ ("type", `String "Context_turns") ]
  | Context_bytes -> `Assoc [ ("type", `String "Context_bytes") ]
  | Run_deadline -> `Assoc [ ("type", `String "Run_deadline") ]
  | Upstream_inactivity -> `Assoc [ ("type", `String "Upstream_inactivity") ]
  | Frame_bytes -> `Assoc [ ("type", `String "Frame_bytes") ]
  | Subscriber_bytes -> `Assoc [ ("type", `String "Subscriber_bytes") ]
  | Journal_bytes -> `Assoc [ ("type", `String "Journal_bytes") ]

let limit_kind_of_json j =
  match str (field "type" j) with
  | "Active_runs" -> Active_runs
  | "User_bytes" -> User_bytes
  | "Output_bytes" -> Output_bytes
  | "Model_rounds" -> Model_rounds
  | "Tool_calls" -> Tool_calls
  | "Tool_result_bytes" -> Tool_result_bytes
  | "Evidence_bytes" -> Evidence_bytes
  | "Context_turns" -> Context_turns
  | "Context_bytes" -> Context_bytes
  | "Run_deadline" -> Run_deadline
  | "Upstream_inactivity" -> Upstream_inactivity
  | "Frame_bytes" -> Frame_bytes
  | "Subscriber_bytes" -> Subscriber_bytes
  | "Journal_bytes" -> Journal_bytes
  | s -> raise (Decode (Unknown_variant s))

let resource_kind_to_json = function
  | Conversation -> `Assoc [ ("type", `String "Conversation") ]
  | Turn -> `Assoc [ ("type", `String "Turn") ]
  | Attempt -> `Assoc [ ("type", `String "Attempt") ]
  | Run -> `Assoc [ ("type", `String "Run") ]
  | Message -> `Assoc [ ("type", `String "Message") ]
  | Evidence -> `Assoc [ ("type", `String "Evidence") ]

let resource_kind_of_json j =
  match str (field "type" j) with
  | "Conversation" -> Conversation
  | "Turn" -> Turn
  | "Attempt" -> Attempt
  | "Run" -> Run
  | "Message" -> Message
  | "Evidence" -> Evidence
  | s -> raise (Decode (Unknown_variant s))

let error_code_to_json = function
  | Access_denied -> `Assoc [ ("type", `String "Access_denied") ]
  | Advisor_disabled -> `Assoc [ ("type", `String "Advisor_disabled") ]
  | Consent_required -> `Assoc [ ("type", `String "Consent_required") ]
  | Scope_mismatch -> `Assoc [ ("type", `String "Scope_mismatch") ]
  | Run_busy -> `Assoc [ ("type", `String "Run_busy") ]
  | Idempotency_conflict -> `Assoc [ ("type", `String "Idempotency_conflict") ]
  | Invalid_tool_arguments -> `Assoc [ ("type", `String "Invalid_tool_arguments") ]
  | Tool_not_allowed -> `Assoc [ ("type", `String "Tool_not_allowed") ]
  | Provider_unavailable -> `Assoc [ ("type", `String "Provider_unavailable") ]
  | Invalid_stream -> `Assoc [ ("type", `String "Invalid_stream") ]
  | Run_limit_reached -> `Assoc [ ("type", `String "Run_limit_reached") ]
  | Storage_unavailable -> `Assoc [ ("type", `String "Storage_unavailable") ]
  | Server_restarted -> `Assoc [ ("type", `String "Server_restarted") ]
  | Invalid_transition -> `Assoc [ ("type", `String "Invalid_transition") ]
  | Invalid_request -> `Assoc [ ("type", `String "Invalid_request") ]
  | Unsupported_version -> `Assoc [ ("type", `String "Unsupported_version") ]
  | Version_conflict -> `Assoc [ ("type", `String "Version_conflict") ]
  | Capacity_exceeded -> `Assoc [ ("type", `String "Capacity_exceeded") ]
  | Cursor_expired -> `Assoc [ ("type", `String "Cursor_expired") ]
  | Cursor_gap -> `Assoc [ ("type", `String "Cursor_gap") ]
  | Not_found -> `Assoc [ ("type", `String "Not_found") ]

let error_code_of_json j =
  match str (field "type" j) with
  | "Access_denied" -> Access_denied
  | "Advisor_disabled" -> Advisor_disabled
  | "Consent_required" -> Consent_required
  | "Scope_mismatch" -> Scope_mismatch
  | "Run_busy" -> Run_busy
  | "Idempotency_conflict" -> Idempotency_conflict
  | "Invalid_tool_arguments" -> Invalid_tool_arguments
  | "Tool_not_allowed" -> Tool_not_allowed
  | "Provider_unavailable" -> Provider_unavailable
  | "Invalid_stream" -> Invalid_stream
  | "Run_limit_reached" -> Run_limit_reached
  | "Storage_unavailable" -> Storage_unavailable
  | "Server_restarted" -> Server_restarted
  | "Invalid_transition" -> Invalid_transition
  | "Invalid_request" -> Invalid_request
  | "Unsupported_version" -> Unsupported_version
  | "Version_conflict" -> Version_conflict
  | "Capacity_exceeded" -> Capacity_exceeded
  | "Cursor_expired" -> Cursor_expired
  | "Cursor_gap" -> Cursor_gap
  | "Not_found" -> Not_found
  | s -> raise (Decode (Unknown_variant s))

let error_context_to_json = function
  | No_context -> `Assoc [ ("type", `String "No_context") ]
  | Limit x -> `Assoc [ ("type", `String "Limit"); ("value", limit_kind_to_json x) ]
  | Version x -> `Assoc [ ("type", `String "Version"); ("value", `String x) ]
  | Cursor x -> `Assoc [ ("type", `String "Cursor"); ("value", `String x) ]
  | Resource x -> `Assoc [ ("type", `String "Resource"); ("value", resource_kind_to_json x) ]

let error_context_of_json j =
  match str (field "type" j) with
  | "No_context" -> No_context
  | "Limit" -> Limit (limit_kind_of_json (field "value" j))
  | "Version" -> Version (counter (str (field "value" j)))
  | "Cursor" -> Cursor (counter (str (field "value" j)))
  | "Resource" -> Resource (resource_kind_of_json (field "value" j))
  | s -> raise (Decode (Unknown_variant s))

let error_to_json (v : error) =
  `Assoc [ ("code", error_code_to_json v.code); ("context", error_context_to_json v.context) ]

let error_of_json j : error =
  { code = error_code_of_json (field "code" j); context = error_context_of_json (field "context" j) }

let run_state_to_json = function
  | Queued -> `Assoc [ ("type", `String "Queued") ]
  | Running x -> `Assoc [ ("type", `String "Running"); ("value", phase_to_json x) ]
  | Cancel_requested -> `Assoc [ ("type", `String "Cancel_requested") ]
  | Completed -> `Assoc [ ("type", `String "Completed") ]
  | Cancelled -> `Assoc [ ("type", `String "Cancelled") ]
  | Failed x -> `Assoc [ ("type", `String "Failed"); ("value", error_to_json x) ]

let run_state_of_json j =
  match str (field "type" j) with
  | "Queued" -> Queued
  | "Running" -> Running (phase_of_json (field "value" j))
  | "Cancel_requested" -> Cancel_requested
  | "Completed" -> Completed
  | "Cancelled" -> Cancelled
  | "Failed" -> Failed (error_of_json (field "value" j))
  | s -> raise (Decode (Unknown_variant s))

let finding_to_json (v : finding) =
  `Assoc
    [
      ("kind", `String v.kind);
      ("key", `String v.key);
      ("scope", scope_to_json v.scope);
      ("title", `String v.title);
      ("summary", `String v.summary);
      ("sampled_at", `String v.sampled_at);
    ]

let finding_of_json j : finding =
  {
    kind = str (field "kind" j);
    key = str (field "key" j);
    scope = scope_of_json (field "scope" j);
    title = str (field "title" j);
    summary = str (field "summary" j);
    sampled_at = timestamp (str (field "sampled_at" j));
  }

let display_value_to_json = function
  | Text x -> `Assoc [ ("type", `String "Text"); ("value", `String x) ]
  | Money x -> `Assoc [ ("type", `String "Money"); ("value", money_to_json x) ]
  | Date x -> `Assoc [ ("type", `String "Date"); ("value", `String x) ]

let display_value_of_json j =
  match str (field "type" j) with
  | "Text" -> Text (str (field "value" j))
  | "Money" -> Money (money_of_json (field "value" j))
  | "Date" -> Date (valid_date (str (field "value" j)))
  | s -> raise (Decode (Unknown_variant s))

let fact_to_json (v : fact) = `Assoc [ ("label", `String v.label); ("value", display_value_to_json v.value) ]
let fact_of_json j : fact = { label = str (field "label" j); value = display_value_of_json (field "value" j) }

let import_status_to_json = function
  | Current -> `Assoc [ ("type", `String "Current") ]
  | Stale -> `Assoc [ ("type", `String "Stale") ]
  | Incomplete -> `Assoc [ ("type", `String "Incomplete") ]
  | Unknown -> `Assoc [ ("type", `String "Unknown") ]

let import_status_of_json j =
  match str (field "type" j) with
  | "Current" -> Current
  | "Stale" -> Stale
  | "Incomplete" -> Incomplete
  | "Unknown" -> Unknown
  | s -> raise (Decode (Unknown_variant s))

let evidence_to_json (v : evidence) =
  `Assoc
    [
      ("id", `String v.id);
      ("scope", scope_to_json v.scope);
      ("currency", currency_to_json v.currency);
      ("sampled_at", `String v.sampled_at);
      ("title", `String v.title);
      ("facts", jlist (fun x -> fact_to_json x) v.facts);
      ("source_references", jlist (fun x -> `String x) v.source_references);
      ("import_status", import_status_to_json v.import_status);
      ("reconciled", `Bool v.reconciled);
      ("assumptions", jlist (fun x -> `String x) v.assumptions);
    ]

let evidence_of_json j : evidence =
  {
    id = uuid (str (field "id" j));
    scope = scope_of_json (field "scope" j);
    currency = currency_of_json (field "currency" j);
    sampled_at = timestamp (str (field "sampled_at" j));
    title = str (field "title" j);
    facts = list (fun x -> fact_of_json x) (field "facts" j);
    source_references = list (fun x -> str x) (field "source_references" j);
    import_status = import_status_of_json (field "import_status" j);
    reconciled = boolean (field "reconciled" j);
    assumptions = list (fun x -> str x) (field "assumptions" j);
  }

let identities_to_json (v : identities) =
  `Assoc
    [
      ("conversation_id", `String v.conversation_id);
      ("turn_id", `String v.turn_id);
      ("attempt_id", `String v.attempt_id);
      ("user_message_id", `String v.user_message_id);
      ("assistant_message_id", `String v.assistant_message_id);
      ("run_id", `String v.run_id);
    ]

let identities_of_json j : identities =
  {
    conversation_id = uuid (str (field "conversation_id" j));
    turn_id = uuid (str (field "turn_id" j));
    attempt_id = uuid (str (field "attempt_id" j));
    user_message_id = uuid (str (field "user_message_id" j));
    assistant_message_id = uuid (str (field "assistant_message_id" j));
    run_id = uuid (str (field "run_id" j));
  }

let submit_to_json (v : submit) =
  `Assoc
    [
      ("conversation_id", joption (fun x -> `String x) v.conversation_id);
      ("scope", scope_to_json v.scope);
      ("currency", currency_to_json v.currency);
      ("locale", `String v.locale);
      ("user_text", `String v.user_text);
      ("idempotency_key", `String v.idempotency_key);
      ("expected_version", `String v.expected_version);
      ("source_finding_key", joption (fun x -> `String x) v.source_finding_key);
    ]

let submit_of_json j : submit =
  {
    conversation_id = option (fun x -> uuid (str x)) (field "conversation_id" j);
    scope = scope_of_json (field "scope" j);
    currency = currency_of_json (field "currency" j);
    locale = str (field "locale" j);
    user_text = str (field "user_text" j);
    idempotency_key = key (str (field "idempotency_key" j));
    expected_version = counter (str (field "expected_version" j));
    source_finding_key = option (fun x -> str x) (field "source_finding_key" j);
  }

let accepted_to_json (v : accepted) =
  `Assoc
    [
      ("ids", identities_to_json v.ids);
      ("accepted_version", `String v.accepted_version);
      ("idempotent_repeat", `Bool v.idempotent_repeat);
    ]

let accepted_of_json j : accepted =
  {
    ids = identities_of_json (field "ids" j);
    accepted_version = counter (str (field "accepted_version" j));
    idempotent_repeat = boolean (field "idempotent_repeat" j);
  }

let retry_to_json (v : retry) =
  `Assoc
    [
      ("turn_id", `String v.turn_id);
      ("expected_version", `String v.expected_version);
      ("idempotency_key", `String v.idempotency_key);
    ]

let retry_of_json j : retry =
  {
    turn_id = uuid (str (field "turn_id" j));
    expected_version = counter (str (field "expected_version" j));
    idempotency_key = key (str (field "idempotency_key" j));
  }

let tool_outcome_to_json = function
  | Succeeded -> `Assoc [ ("type", `String "Succeeded") ]
  | Unavailable -> `Assoc [ ("type", `String "Unavailable") ]
  | Rejected -> `Assoc [ ("type", `String "Rejected") ]

let tool_outcome_of_json j =
  match str (field "type" j) with
  | "Succeeded" -> Succeeded
  | "Unavailable" -> Unavailable
  | "Rejected" -> Rejected
  | s -> raise (Decode (Unknown_variant s))

let tool_progress_to_json (v : tool_progress) =
  `Assoc [ ("label", `String v.label); ("outcome", tool_outcome_to_json v.outcome) ]

let tool_progress_of_json j : tool_progress =
  { label = str (field "label" j); outcome = tool_outcome_of_json (field "outcome" j) }

let text_delta_to_json (v : text_delta) = `Assoc [ ("segment_id", `String v.segment_id); ("text", `String v.text) ]
let text_delta_of_json j : text_delta = { segment_id = uuid (str (field "segment_id" j)); text = str (field "text" j) }

let event_payload_to_json = function
  | Run_started -> `Assoc [ ("type", `String "Run_started") ]
  | Text_appended x -> `Assoc [ ("type", `String "Text_appended"); ("value", text_delta_to_json x) ]
  | Tool_started x -> `Assoc [ ("type", `String "Tool_started"); ("value", `String x) ]
  | Tool_finished x -> `Assoc [ ("type", `String "Tool_finished"); ("value", tool_progress_to_json x) ]
  | Evidence_added x -> `Assoc [ ("type", `String "Evidence_added"); ("value", evidence_to_json x) ]
  | Run_completed -> `Assoc [ ("type", `String "Run_completed") ]
  | Run_cancelled -> `Assoc [ ("type", `String "Run_cancelled") ]
  | Run_failed x -> `Assoc [ ("type", `String "Run_failed"); ("value", error_to_json x) ]

let event_payload_of_json j =
  match str (field "type" j) with
  | "Run_started" -> Run_started
  | "Text_appended" -> Text_appended (text_delta_of_json (field "value" j))
  | "Tool_started" -> Tool_started (str (field "value" j))
  | "Tool_finished" -> Tool_finished (tool_progress_of_json (field "value" j))
  | "Evidence_added" -> Evidence_added (evidence_of_json (field "value" j))
  | "Run_completed" -> Run_completed
  | "Run_cancelled" -> Run_cancelled
  | "Run_failed" -> Run_failed (error_of_json (field "value" j))
  | s -> raise (Decode (Unknown_variant s))

let event_to_json (v : event) =
  `Assoc
    [
      ("ids", identities_to_json v.ids); ("sequence", `String v.sequence); ("payload", event_payload_to_json v.payload);
    ]

let event_of_json j : event =
  {
    ids = identities_of_json (field "ids" j);
    sequence = counter (str (field "sequence" j));
    payload = event_payload_of_json (field "payload" j);
  }

let segment_to_json = function
  | Text_segment x -> `Assoc [ ("type", `String "Text_segment"); ("value", text_delta_to_json x) ]
  | Evidence_segment x -> `Assoc [ ("type", `String "Evidence_segment"); ("value", evidence_to_json x) ]

let segment_of_json j =
  match str (field "type" j) with
  | "Text_segment" -> Text_segment (text_delta_of_json (field "value" j))
  | "Evidence_segment" -> Evidence_segment (evidence_of_json (field "value" j))
  | s -> raise (Decode (Unknown_variant s))

let attempt_to_json (v : attempt) =
  `Assoc
    [
      ("ids", identities_to_json v.ids);
      ("ordinal", `String v.ordinal);
      ("state", run_state_to_json v.state);
      ("segments", jlist (fun x -> segment_to_json x) v.segments);
      ("last_sequence", `String v.last_sequence);
    ]

let attempt_of_json j : attempt =
  {
    ids = identities_of_json (field "ids" j);
    ordinal = counter (str (field "ordinal" j));
    state = run_state_of_json (field "state" j);
    segments = list (fun x -> segment_of_json x) (field "segments" j);
    last_sequence = counter (str (field "last_sequence" j));
  }

let turn_to_json (v : turn) =
  `Assoc
    [
      ("turn_id", `String v.turn_id);
      ("user_message_id", `String v.user_message_id);
      ("user_text", `String v.user_text);
      ("attempts", jlist (fun x -> attempt_to_json x) v.attempts);
    ]

let turn_of_json j : turn =
  {
    turn_id = uuid (str (field "turn_id" j));
    user_message_id = uuid (str (field "user_message_id" j));
    user_text = str (field "user_text" j);
    attempts = list (fun x -> attempt_of_json x) (field "attempts" j);
  }

let snapshot_to_json (v : snapshot) =
  `Assoc
    [
      ("conversation_id", `String v.conversation_id);
      ("scope", scope_to_json v.scope);
      ("currency", currency_to_json v.currency);
      ("locale", `String v.locale);
      ("source_finding", joption (fun x -> finding_to_json x) v.source_finding);
      ("turns", jlist (fun x -> turn_to_json x) v.turns);
      ("active_run", joption (fun x -> `String x) v.active_run);
      ("version", `String v.version);
    ]

let snapshot_of_json j : snapshot =
  {
    conversation_id = uuid (str (field "conversation_id" j));
    scope = scope_of_json (field "scope" j);
    currency = currency_of_json (field "currency" j);
    locale = str (field "locale" j);
    source_finding = option (fun x -> finding_of_json x) (field "source_finding" j);
    turns = list (fun x -> turn_of_json x) (field "turns" j);
    active_run = option (fun x -> uuid (str x)) (field "active_run" j);
    version = counter (str (field "version" j));
  }

let same_currency (a : currency) (b : currency) = a.code = b.code && a.scale = b.scale

let validate_evidence (e : evidence) =
  List.iter
    (fun (f : fact) ->
      match f.value with
      | Money m when not (same_currency m.currency e.currency) -> invalid "mixed currencies"
      | _ -> ())
    e.facts

let validate_submit (s : submit) =
  ignore (locale s.locale);
  if s.user_text = "" || String.length s.user_text > 8192 then invalid "user_text";
  if s.conversation_id = None && s.expected_version <> "0" then invalid "expected_version";
  Option.iter (fun k -> ignore (nonempty "source_finding_key" k)) s.source_finding_key

let validate_event (e : event) =
  if e.sequence = "0" then invalid "sequence";
  match e.payload with
  | Evidence_added e -> validate_evidence e
  | Tool_started label -> ignore (nonempty "label" label)
  | Tool_finished p -> ignore (nonempty "label" p.label)
  | _ -> ()

let validate_snapshot (s : snapshot) =
  ignore (locale s.locale);
  Option.iter (fun (f : finding) -> if f.scope <> s.scope then invalid "finding scope") s.source_finding;
  List.iter
    (fun (t : turn) ->
      List.iter
        (fun (a : attempt) ->
          if
            a.ids.conversation_id <> s.conversation_id
            || a.ids.turn_id <> t.turn_id
            || a.ids.user_message_id <> t.user_message_id
          then invalid "attempt identities";
          List.iter
            (function
              | Text_segment _ -> ()
              | Evidence_segment e ->
                  validate_evidence e;
                  if e.scope <> s.scope || not (same_currency e.currency s.currency) then
                    invalid "evidence scope/currency")
            a.segments)
        t.attempts)
    s.turns

let envelope encode v = Yojson.Safe.to_string (`Assoc [ ("version", `Int version); ("data", encode v) ])

let decode parse validate text =
  try
    let j = Yojson.Safe.from_string text in
    if integer (field "version" j) <> version then raise (Decode Unknown_version);
    let value = parse (field "data" j) in
    validate value;
    Ok value
  with
  | Decode e -> Error e
  | Yojson.Json_error _ -> Error Invalid_json

let encode_submit = envelope submit_to_json
let decode_submit = decode submit_of_json validate_submit
let encode_retry = envelope retry_to_json
let decode_retry = decode retry_of_json (fun _ -> ())
let encode_accepted = envelope accepted_to_json
let decode_accepted = decode accepted_of_json (fun _ -> ())
let encode_event = envelope event_to_json
let decode_event = decode event_of_json validate_event
let encode_snapshot = envelope snapshot_to_json
let decode_snapshot = decode snapshot_of_json validate_snapshot
