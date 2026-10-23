type currency = { code : string; scale : int }
(** Version-1 DTOs. Counters and money are decimal strings, never JS floats. *)

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

(** Encoders are total structural projections. Decoders validate the version-1 envelope, required fields, canonical
    decimal counters, UUIDs, UTF-8 and money. Optional fields are present with null when absent. Currency scale is 0..4;
    money decimal precision exactly matches scale and fits signed int64 minor units. Timestamps use RFC3339 with seconds
    00..59 (leap seconds unsupported), including offsets and fractions; dates use YYYY-MM-DD. *)
type decode_error = Invalid_json | Invalid_field of string | Unknown_variant of string | Unknown_version

val version : int
val encode_submit : submit -> string
val decode_submit : string -> (submit, decode_error) result
val encode_retry : retry -> string
val decode_retry : string -> (retry, decode_error) result
val encode_accepted : accepted -> string
val decode_accepted : string -> (accepted, decode_error) result
val encode_event : event -> string
val decode_event : string -> (event, decode_error) result
val encode_snapshot : snapshot -> string
val decode_snapshot : string -> (snapshot, decode_error) result
