module Conversation_id = Id.Make ()
module Turn_id = Id.Make ()
module Attempt_id = Id.Make ()
module Run_id = Id.Make ()
module Message_id = Id.Make ()
module Evidence_id = Id.Make ()
module Segment_id = Id.Make ()

module Session_ref = struct
  type t = string

  let of_hash s = if s = "" then None else Some s
  let to_hash s = s
end

type scope = Personal | Household of Household_id.t
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

type error_context = No_context | Limit of limit_kind | Version of int64 | Cursor of int64 | Resource of resource_kind
type error = { code : error_code; context : error_context }
type run_state = Queued | Running of phase | Cancel_requested | Completed | Cancelled | Failed of error
type money = { minor_units : int64; currency : Currency.t }
type finding = { kind : string; key : string; scope : scope; title : string; summary : string; sampled_at : Ptime.t }
type display_value = Text of string | Money of money | Date of Date.t
type fact = { label : string; value : display_value }
type import_status = Current | Stale | Incomplete | Unknown

type evidence = {
  id : Evidence_id.t;
  scope : scope;
  currency : Currency.t;
  sampled_at : Ptime.t;
  title : string;
  facts : fact list;
  source_references : string list;
  import_status : import_status;
  reconciled : bool;
  assumptions : string list;
}

type identities = {
  conversation_id : Conversation_id.t;
  turn_id : Turn_id.t;
  attempt_id : Attempt_id.t;
  user_message_id : Message_id.t;
  assistant_message_id : Message_id.t;
  run_id : Run_id.t;
}

type submit = {
  conversation_id : Conversation_id.t option;
  scope : scope;
  currency : Currency.t;
  locale : string;
  user_text : string;
  idempotency_key : Idempotency_key.t;
  expected_version : int64;
  source_finding_key : string option;
}

type accepted = { ids : identities; accepted_version : int64; idempotent_repeat : bool }
type retry = { turn_id : Turn_id.t; expected_version : int64; idempotency_key : Idempotency_key.t }
type tool_outcome = Succeeded | Unavailable | Rejected
type tool_progress = { label : string; outcome : tool_outcome }
type text_delta = { segment_id : Segment_id.t; text : string }

type event_payload =
  | Run_started
  | Text_appended of text_delta
  | Tool_started of string
  | Tool_finished of tool_progress
  | Evidence_added of evidence
  | Run_completed
  | Run_cancelled
  | Run_failed of error

type event = { ids : identities; sequence : int64; payload : event_payload }
type segment = Text_segment of text_delta | Evidence_segment of evidence
type attempt = { ids : identities; ordinal : int64; state : run_state; segments : segment list; last_sequence : int64 }
type turn = { turn_id : Turn_id.t; user_message_id : Message_id.t; user_text : string; attempts : attempt list }

type snapshot = {
  conversation_id : Conversation_id.t;
  scope : scope;
  currency : Currency.t;
  locale : string;
  source_finding : finding option;
  turns : turn list;
  active_run : Run_id.t option;
  version : int64;
}

type transition =
  | Start
  | Set_phase of phase
  | Request_cancel
  | Finish
  | Fail of error
  | Confirm_cancel
  | Recover_interrupted

type limits = {
  active_runs : int;
  active_runs_per_user : int;
  active_runs_per_conversation : int;
  user_bytes : int;
  output_bytes : int;
  model_rounds : int;
  tool_calls : int;
  tool_result_bytes : int;
  evidence_bytes : int;
  context_turns : int;
  context_bytes : int;
  run_deadline_seconds : int;
  upstream_inactivity_seconds : int;
  frame_bytes : int;
  subscriber_bytes : int;
  journal_bytes : int;
  replay_retention_seconds : int;
  no_subscriber_seconds : int;
  flush_milliseconds : int;
  flush_bytes : int;
  render_milliseconds : int;
}

type counters = {
  output_bytes : int;
  model_rounds : int;
  tool_calls : int;
  evidence_bytes : int;
  journal_bytes : int;
  last_sequence : int64;
}

type run = {
  ids : identities;
  owner_id : User_id.t;
  originating_session : Session_ref.t;
  model : string;
  protocol_version : int;
  prompt_version : int;
  started_at : Ptime.t;
  updated_at : Ptime.t;
  state : run_state;
  counters : counters;
}

let limits : limits =
  {
    active_runs = 8;
    active_runs_per_user = 1;
    active_runs_per_conversation = 1;
    user_bytes = 8192;
    output_bytes = 65536;
    model_rounds = 4;
    tool_calls = 8;
    tool_result_bytes = 16384;
    evidence_bytes = 65536;
    context_turns = 12;
    context_bytes = 65536;
    run_deadline_seconds = 120;
    upstream_inactivity_seconds = 30;
    frame_bytes = 262144;
    subscriber_bytes = 262144;
    journal_bytes = 1048576;
    replay_retention_seconds = 86400;
    no_subscriber_seconds = 30;
    flush_milliseconds = 100;
    flush_bytes = 4096;
    render_milliseconds = 40;
  }

let is_terminal = function Completed | Cancelled | Failed _ -> true | _ -> false

let step state transition =
  if is_terminal state then Error { code = Invalid_transition; context = No_context }
  else
    match (state, transition) with
    | _, Recover_interrupted -> Ok (Failed { code = Server_restarted; context = No_context })
    | Queued, Start -> Ok (Running Awaiting_model)
    | Running _, Set_phase phase -> Ok (Running phase)
    | (Queued | Running _), Request_cancel -> Ok Cancel_requested
    | Running _, Finish -> Ok Completed
    | (Running _ | Cancel_requested), Fail error -> Ok (Failed error)
    | Cancel_requested, Confirm_cancel -> Ok Cancelled
    | _ -> Error { code = Invalid_transition; context = No_context }
