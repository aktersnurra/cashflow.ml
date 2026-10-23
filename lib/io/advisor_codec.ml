module A = Cashflow_core.Advisor
module W = Cashflow_advisor_protocol.Wire
open Cashflow_core

exception Invalid

let require = function Some x -> x | None -> raise Invalid
let get = function Ok x -> x | Error _ -> raise Invalid
let currency_out c : W.currency = { code = Currency.code c; scale = Currency.scale c }
let currency_in (c : W.currency) = get (Currency.make ~code:c.code ~scale:c.scale)

let money_out (m : A.money) : W.money =
  (* Strip the sign as text: abs min_int overflows, and no JS float may touch money. *)
  let units = Int64.to_string m.minor_units in
  let sign, digits = if units.[0] = '-' then ("-", String.sub units 1 (String.length units - 1)) else ("", units) in
  let scale = Currency.scale m.currency in
  let decimal =
    if scale = 0 then units
    else
      let digits = String.make (max 0 (scale + 1 - String.length digits)) '0' ^ digits in
      let split = String.length digits - scale in
      sign ^ String.sub digits 0 split ^ "." ^ String.sub digits split scale
  in
  { decimal; currency = currency_out m.currency }

let scope_out (v : A.scope) : W.scope =
  match v with A.Personal -> W.Personal | A.Household x -> W.Household (Household_id.to_string x)

let phase_out (v : A.phase) : W.phase =
  match v with
  | A.Awaiting_model -> W.Awaiting_model
  | A.Streaming_text -> W.Streaming_text
  | A.Running_tools -> W.Running_tools

let limit_kind_out (v : A.limit_kind) : W.limit_kind =
  match v with
  | A.Active_runs -> W.Active_runs
  | A.User_bytes -> W.User_bytes
  | A.Output_bytes -> W.Output_bytes
  | A.Model_rounds -> W.Model_rounds
  | A.Tool_calls -> W.Tool_calls
  | A.Tool_result_bytes -> W.Tool_result_bytes
  | A.Evidence_bytes -> W.Evidence_bytes
  | A.Context_turns -> W.Context_turns
  | A.Context_bytes -> W.Context_bytes
  | A.Run_deadline -> W.Run_deadline
  | A.Upstream_inactivity -> W.Upstream_inactivity
  | A.Frame_bytes -> W.Frame_bytes
  | A.Subscriber_bytes -> W.Subscriber_bytes
  | A.Journal_bytes -> W.Journal_bytes

let resource_kind_out (v : A.resource_kind) : W.resource_kind =
  match v with
  | A.Conversation -> W.Conversation
  | A.Turn -> W.Turn
  | A.Attempt -> W.Attempt
  | A.Run -> W.Run
  | A.Message -> W.Message
  | A.Evidence -> W.Evidence

let error_code_out (v : A.error_code) : W.error_code =
  match v with
  | A.Access_denied -> W.Access_denied
  | A.Advisor_disabled -> W.Advisor_disabled
  | A.Consent_required -> W.Consent_required
  | A.Scope_mismatch -> W.Scope_mismatch
  | A.Run_busy -> W.Run_busy
  | A.Idempotency_conflict -> W.Idempotency_conflict
  | A.Invalid_tool_arguments -> W.Invalid_tool_arguments
  | A.Tool_not_allowed -> W.Tool_not_allowed
  | A.Provider_unavailable -> W.Provider_unavailable
  | A.Invalid_stream -> W.Invalid_stream
  | A.Run_limit_reached -> W.Run_limit_reached
  | A.Storage_unavailable -> W.Storage_unavailable
  | A.Server_restarted -> W.Server_restarted
  | A.Invalid_transition -> W.Invalid_transition
  | A.Invalid_request -> W.Invalid_request
  | A.Unsupported_version -> W.Unsupported_version
  | A.Version_conflict -> W.Version_conflict
  | A.Capacity_exceeded -> W.Capacity_exceeded
  | A.Cursor_expired -> W.Cursor_expired
  | A.Cursor_gap -> W.Cursor_gap
  | A.Not_found -> W.Not_found

let error_context_out (v : A.error_context) : W.error_context =
  match v with
  | A.No_context -> W.No_context
  | A.Limit x -> W.Limit (limit_kind_out x)
  | A.Version x -> W.Version (Int64.to_string x)
  | A.Cursor x -> W.Cursor (Int64.to_string x)
  | A.Resource x -> W.Resource (resource_kind_out x)

let error_out (v : A.error) : W.error = { code = error_code_out v.code; context = error_context_out v.context }

let run_state_out (v : A.run_state) : W.run_state =
  match v with
  | A.Queued -> W.Queued
  | A.Running x -> W.Running (phase_out x)
  | A.Cancel_requested -> W.Cancel_requested
  | A.Completed -> W.Completed
  | A.Cancelled -> W.Cancelled
  | A.Failed x -> W.Failed (error_out x)

let finding_out (v : A.finding) : W.finding =
  {
    kind = v.kind;
    key = v.key;
    scope = scope_out v.scope;
    title = v.title;
    summary = v.summary;
    sampled_at = Time.to_utc_string v.sampled_at;
  }

let display_value_out (v : A.display_value) : W.display_value =
  match v with A.Text x -> W.Text x | A.Money x -> W.Money (money_out x) | A.Date x -> W.Date (Date.to_string x)

let fact_out (v : A.fact) : W.fact = { label = v.label; value = display_value_out v.value }

let import_status_out (v : A.import_status) : W.import_status =
  match v with A.Current -> W.Current | A.Stale -> W.Stale | A.Incomplete -> W.Incomplete | A.Unknown -> W.Unknown

let evidence_out (v : A.evidence) : W.evidence =
  {
    id = A.Evidence_id.to_string v.id;
    scope = scope_out v.scope;
    currency = currency_out v.currency;
    sampled_at = Time.to_utc_string v.sampled_at;
    title = v.title;
    facts = List.map (fun x -> fact_out x) v.facts;
    source_references = v.source_references;
    import_status = import_status_out v.import_status;
    reconciled = v.reconciled;
    assumptions = v.assumptions;
  }

let identities_out (v : A.identities) : W.identities =
  {
    conversation_id = A.Conversation_id.to_string v.conversation_id;
    turn_id = A.Turn_id.to_string v.turn_id;
    attempt_id = A.Attempt_id.to_string v.attempt_id;
    user_message_id = A.Message_id.to_string v.user_message_id;
    assistant_message_id = A.Message_id.to_string v.assistant_message_id;
    run_id = A.Run_id.to_string v.run_id;
  }

let accepted_out (v : A.accepted) : W.accepted =
  {
    ids = identities_out v.ids;
    accepted_version = Int64.to_string v.accepted_version;
    idempotent_repeat = v.idempotent_repeat;
  }

let tool_outcome_out (v : A.tool_outcome) : W.tool_outcome =
  match v with A.Succeeded -> W.Succeeded | A.Unavailable -> W.Unavailable | A.Rejected -> W.Rejected

let tool_progress_out (v : A.tool_progress) : W.tool_progress =
  { label = v.label; outcome = tool_outcome_out v.outcome }

let text_delta_out (v : A.text_delta) : W.text_delta =
  { segment_id = A.Segment_id.to_string v.segment_id; text = v.text }

let event_payload_out (v : A.event_payload) : W.event_payload =
  match v with
  | A.Run_started -> W.Run_started
  | A.Text_appended x -> W.Text_appended (text_delta_out x)
  | A.Tool_started x -> W.Tool_started x
  | A.Tool_finished x -> W.Tool_finished (tool_progress_out x)
  | A.Evidence_added x -> W.Evidence_added (evidence_out x)
  | A.Run_completed -> W.Run_completed
  | A.Run_cancelled -> W.Run_cancelled
  | A.Run_failed x -> W.Run_failed (error_out x)

let event_out (v : A.event) : W.event =
  { ids = identities_out v.ids; sequence = Int64.to_string v.sequence; payload = event_payload_out v.payload }

let segment_out (v : A.segment) : W.segment =
  match v with
  | A.Text_segment x -> W.Text_segment (text_delta_out x)
  | A.Evidence_segment x -> W.Evidence_segment (evidence_out x)

let attempt_out (v : A.attempt) : W.attempt =
  {
    ids = identities_out v.ids;
    ordinal = Int64.to_string v.ordinal;
    state = run_state_out v.state;
    segments = List.map (fun x -> segment_out x) v.segments;
    last_sequence = Int64.to_string v.last_sequence;
  }

let turn_out (v : A.turn) : W.turn =
  {
    turn_id = A.Turn_id.to_string v.turn_id;
    user_message_id = A.Message_id.to_string v.user_message_id;
    user_text = v.user_text;
    attempts = List.map (fun x -> attempt_out x) v.attempts;
  }

let snapshot_out (v : A.snapshot) : W.snapshot =
  {
    conversation_id = A.Conversation_id.to_string v.conversation_id;
    scope = scope_out v.scope;
    currency = currency_out v.currency;
    locale = v.locale;
    source_finding = Option.map (fun x -> finding_out x) v.source_finding;
    turns = List.map (fun x -> turn_out x) v.turns;
    active_run = Option.map (fun x -> A.Run_id.to_string x) v.active_run;
    version = Int64.to_string v.version;
  }

let scope_in (v : W.scope) : A.scope =
  match v with W.Personal -> A.Personal | W.Household x -> A.Household (require (Household_id.of_string x))

let submit_in (v : W.submit) : A.submit =
  {
    conversation_id = Option.map (fun x -> require (A.Conversation_id.of_string x)) v.conversation_id;
    scope = scope_in v.scope;
    currency = currency_in v.currency;
    locale = v.locale;
    user_text = v.user_text;
    idempotency_key = get (Idempotency_key.of_string v.idempotency_key);
    expected_version = Int64.of_string v.expected_version;
    source_finding_key = v.source_finding_key;
  }

let retry_in (v : W.retry) : A.retry =
  {
    turn_id = require (A.Turn_id.of_string v.turn_id);
    expected_version = Int64.of_string v.expected_version;
    idempotency_key = get (Idempotency_key.of_string v.idempotency_key);
  }

let event = event_out
let snapshot = snapshot_out
let accepted = accepted_out

let inbound decode encode convert value =
  match decode (encode value) with
  | Error _ -> Error { A.code = Invalid_request; context = No_context }
  | Ok valid -> ( try Ok (convert valid) with Invalid -> Error { A.code = Invalid_request; context = No_context })

let submit = inbound W.decode_submit W.encode_submit submit_in
let retry = inbound W.decode_retry W.encode_retry retry_in
