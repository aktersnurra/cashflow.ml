module W = Wire

type notice =
  | Quiet
  | Interrupted
  | Request_failed
  | Choose_context
  | Busy
  | Invalid_state
  | Answer_completed
  | Answer_stopped
  | Answer_failed
  | Answer_running
  | Stopping

type token = { epoch : int; generation : int }

(* Rejected is an uncertain/protocol response; only Not_accepted proves a write was refused. *)
type failure = Session_lost | Access_lost | Transport | Rejected | Not_accepted of W.error_code
type household_status = { household_id : string; enabled : bool; can_manage : bool }
type status = { available : bool; consent : bool; household : household_status option }
type metadata = { conversation_id : string; version : string; updated_at : string }
type connection = Disconnected | Connecting | Connected | Reconnecting
type pending = Submit of W.submit | Retry of W.retry

type command =
  | Post_turn of token * W.submit
  | Retry_turn of token * W.retry
  | Cancel_run of token * W.identities
  | Fetch_snapshot of token * string
  | Subscribe of token * W.identities * string
  | Unsubscribe
  | Delete_conversation of token * string
  | Announce_status of notice
  | Get_status of token
  | Set_consent of token * bool * string option
  | List_conversations of token * int * int
  | Schedule_flush of token
  | Cancel_flush

type action =
  | Activate
  | Deactivate
  | Auth_lost
  | Leave_surface
  | Open_conversation of string
  | Open_latest
  | Open_empty of W.scope * W.currency * string
  | Open_finding of string * W.scope * W.currency * string
  | Draft_changed of string
  | Submit_draft of string
  | Retry_latest of string
  | Repeat_pending
  | Stop
  | Reconnect
  | Delete
  | Delete_owned of string
  | Consent of bool * string option
  | Refresh_status
  | Load_history of int
  | Accepted of token * (W.accepted, failure) result
  | Snapshot of token * (W.snapshot, failure) result
  | Status of token * (status, failure) result
  | History of token * (metadata list, failure) result
  | Consent_result of token * (unit, failure) result
  | Deleted of token * (unit, failure) result
  | Stopped of token * (string, failure) result
  | Stream_invalid of token
  | Event of token * W.event
  | Connection of token * connection
  | Resync of token
  | Flush of token
  | Background of token
  | Foreground of token
  | Follow of bool

type view = {
  selected_conversation : string option;
  first_acceptance : string option; (** New-conversation acceptance only, never history selection. *)
  context_scope : W.scope option;
  authenticated : bool;
  access_denied : bool;
  denied_conversation : string option;
  snapshot : W.snapshot option;
  draft : string;
  connection : connection;
  status : status option;
  history : metadata list;
  pending : pending option;
  notice : notice;
  following : bool;
  new_output : bool;
  stopping : bool;
  can_reconnect : bool;
}

type t

val empty : t
val reduce : t -> action -> t * command list
val view : t -> view
val command_is_current : t -> command -> bool
