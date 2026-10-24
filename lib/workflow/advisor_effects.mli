module A = Cashflow_core.Advisor
module T = Advisor_tools

type raw_call = { id : string; name : string; arguments : string; index : int }
type finish_reason = Stop | Tool_calls | Length | Content_filter
type model_event = Text of string | Complete_calls of raw_call list | Round_finished of finish_reason
type message = User of string | Assistant of string * raw_call list | Tool of { call_id : string; content : string }
type context_turn = { turn_id : A.Turn_id.t; attempt_id : A.Attempt_id.t; user_text : string; assistant_text : string }
type selection = { selected_turns : int; omitted_turns : int }
type selected_evidence = { model_content : string; display : A.evidence; serialized_bytes : int }

type context = {
  system_instructions : string;
  history : context_turn list;
  selection : selection;
  user_text : string;
  messages : message list;
  source_finding : A.finding option;
  evidence : selected_evidence list;
  locale : string;
  currency : Cashflow_core.Currency.t;
  model_aliases : string list;
  context_token_budget : int option;
  segment_id : A.Segment_id.t;
}
(** Load_context supplies detached, currently permitted data, no old tool results or messages. The workflow constructs
    ordered messages, adds its safety instructions, and bounds history. No session/owner identifiers belong in model
    context. The reliable configured model limit must be present, never guessed. segment_id is reserved for the first
    round's text span. After each tool round, Allocate_segment supplies a fresh identity lazily when nonempty text
    arrives. IDs must never be reused within an attempt, including the initial segment_id; allocation belongs to IO and
    returns only a detached ID, not a native handle. *)

type context_size = { serialized_bytes : int; conservative_tokens : int; output_reserve_tokens : int }

(** Run handlers capture one run; Check_authority revalidates full generation and origin-session authority. Command
    handlers capture a principal: Check_authority validates identity only; persist effects enforce scope/consent and
    deletion enforces ownership without disclosure consent. Mutations commit before signaling. Start_run is idempotent,
    never restarts terminal runs, and runs outside storage. Persist effects validate the complete request shape
    (including UTF-8) atomically. Request_cancel is idempotent for pending/terminal states: persist first, then signal
    outside storage. Internal run cleanup may request/confirm cancellation after authority loss without disclosing
    anything; command cancellation requires current authorization.

    Select_evidence must pass the exact Read_tool object to model_json with the same attempt-local alias handle,
    serialize once outside guards, then display that same object. serialized_bytes = String.length model_content. No
    display on error. Workflow publishes Evidence_added only on the first selection of an exact evidence object;
    repeated selections retain every Tool result and Tool_finished event. Append_events must persist all supplied
    semantic events without silently dropping duplicates. Load_context must set omitted_turns to the number already
    omitted from successful history; execute adds further whole-turn omissions. Map only model-facing messages/data,
    never internal turn/attempt/segment IDs, to the provider. Measure_context measures the complete serialized request
    including tools/system, estimates input tokens and reserves the configured upstream
    max_tokens completion policy. This estimate is not a proof of provider framing/tokenization.
    The local UTF-8 output cap is independent; a smaller completion policy is allowed.
    IO rejects unsupported models/configuration and upstream context failures without relaxed retry. Begin/Read preserve
    the original total deadline (measured from run.started_at, never reset); IO additionally supervises raw activity,
    cancellation and inactivity. Append_events persists before delivery, rechecks authority, buffers at most 100ms/4KiB,
    and enforces journal/frame/backlog bounds. Despite its name, Commit_terminal also applies Start/Set_phase through
    the same atomic store transition seam. Terminal cleanup remains available after access loss; delivery is separately
    authorized. Commit_terminal flushes pending text and atomically commits/publishes exactly one terminal event,
    resolving cancellation races (Finish can never complete Cancel_requested). Close is safe after partial/failed Begin
    and on all exit paths. No raw content/error logging. *)

type _ operation =
  | Check_authority : (unit, A.error) result operation
  | Now : Ptime.t operation
  | Check_cancelled : bool operation
  | Load_context : (context, A.error) result operation
  | Allocate_segment : (A.Segment_id.t, A.error) result operation
  | Measure_context : context -> (context_size, A.error) result operation
  | Begin_model_round : context -> (unit, A.error) result operation
  | Read_model_event : (model_event option, A.error) result operation
  | Close_model_round : unit operation
  | Parse_tool : raw_call -> (T.request, A.error) result operation
  | Read_tool : T.request -> (T.evidence, A.error) result operation
  | Select_evidence : T.evidence -> (selected_evidence, A.error) result operation
  | Append_events : A.event_payload list -> (unit, A.error) result operation
  | Commit_terminal : A.transition -> (A.run_state, A.error) result operation
  | Persist_turn : A.submit -> (A.accepted, A.error) result operation
  | Persist_retry : A.retry -> (A.accepted, A.error) result operation
  | Start_run : A.accepted -> unit operation
  | Request_cancel : A.Run_id.t -> (A.run_state, A.error) result operation
  | Delete_owned_conversation : A.Conversation_id.t -> (unit, A.error) result operation

type handler = { handle : 'a. 'a operation -> 'a }

val perform : 'a operation -> 'a
val run : handler -> (unit -> 'a) -> 'a
