module A = Cashflow_core.Advisor

type t
type replay = Events of A.event list | Resync_required of int64
type consent_scope = User of Cashflow_core.User_id.t | Household of Cashflow_core.Household_id.t

type metadata = {
  conversation_id : A.Conversation_id.t;
  scope : A.scope;
  currency : Cashflow_core.Currency.t;
  locale : string;
  version : int64;
  updated_at : Ptime.t;
  active_run : A.Run_id.t option;
}

type history_entry = { conversation : metadata; title : string option }

val create : ?clock:(unit -> Ptime.t) -> Sql.db -> t
(** Inject the runtime's clock. Explicit [now] arguments override it; append and transition sample it once. Terminal
    timestamps are immutable. *)

val accept :
  ?available:bool ->
  ?resolve_source_finding:(unit -> (A.finding option, A.error) result) ->
  t ->
  owner:Cashflow_core.User_id.t ->
  session:A.Session_ref.t ->
  now:Ptime.t ->
  model:string ->
  source_finding:A.finding option ->
  A.submit ->
  (A.accepted, A.error) result

(** Readiness is checked only after canonical repeat/conflict resolution. The optional trusted
    resolver captures a finding only for new available work, inside the short transaction;
    it must never wait on network/provider/client IO. Combining it with a non-None explicit
    finding is invalid. Repeats retain the original captured finding. *)

val retry :
  ?available:bool ->
  t ->
  owner:Cashflow_core.User_id.t ->
  session:A.Session_ref.t ->
  now:Ptime.t ->
  model:string ->
  A.retry ->
  (A.accepted, A.error) result

val append : t -> run:A.Run_id.t -> expected_sequence:int64 -> A.event_payload list -> (A.event list, A.error) result
val flush_pending : t -> run:A.Run_id.t -> expected_sequence:int64 -> A.event_payload list -> (A.event list, A.error) result
(** Runtime-internal drain of previously authorized buffered text only. Accepts Running
    or Cancel_requested, never new callbacks after cancellation; no publication authority. *)
val prune_batch : t -> now:Ptime.t -> limit:int -> (bool, A.error) result
(** At most [limit] event rows and request tombstones, and at most [limit] run updates.
    Ordered prefixes advance replay floors; true means more eligible work. Limit: 1..256. *)
val snapshot : t -> owner:Cashflow_core.User_id.t -> A.Conversation_id.t -> (A.snapshot, A.error) result
val replay : t -> run:A.Run_id.t -> after:int64 -> limit:int -> (replay, A.error) result
val transition : t -> run:A.Run_id.t -> A.transition -> (A.run_state, A.error) result

val delete :
  t -> owner:Cashflow_core.User_id.t -> now:Ptime.t -> A.Conversation_id.t -> (A.Run_id.t list, A.error) result

val recover : t -> now:Ptime.t -> unit
val prune : t -> now:Ptime.t -> unit
val consent : t -> consent_scope -> (bool, A.error) result

val set_consent : t -> consent_scope -> enabled:bool -> (unit, A.error) result
(** Low-level scoped storage, disabled when absent. Authorization is the caller's responsibility. *)

type memory = { id : string; text : string; updated_at : Ptime.t }

val memory_limit : int
(** Notes per user. *)

val memories : t -> owner:Cashflow_core.User_id.t -> (memory list, A.error) result
(** The owner's notes, oldest first. *)

val add_memory : t -> owner:Cashflow_core.User_id.t -> now:Ptime.t -> string -> (memory, A.error) result
(** Text is trimmed and its whitespace runs (line breaks included) become single spaces, so a note can never start a
    new prompt line. Empty, invalid UTF-8 or over 280 characters is [Invalid_request]; past {!memory_limit} is
    [Capacity_exceeded]. *)

val update_memory : t -> owner:Cashflow_core.User_id.t -> now:Ptime.t -> id:string -> string -> (memory, A.error) result
(** Same text rules. Another owner's or an unknown id is [Not_found]. *)

val forget_memory : t -> owner:Cashflow_core.User_id.t -> id:string -> (unit, A.error) result
(** Deletes the row. Another owner's or an unknown id is [Not_found]. *)

val list : t -> owner:Cashflow_core.User_id.t -> ?limit:int -> ?offset:int -> unit -> (history_entry list, A.error) result
(** Owner-bound preview projection. The caller must still authorize history before disclosure. *)
val metadata : t -> owner:Cashflow_core.User_id.t -> A.Conversation_id.t -> (metadata, A.error) result
val find_run : t -> owner:Cashflow_core.User_id.t -> A.Run_id.t -> (A.run, A.error) result
(** Unknown and other-owner IDs both return [Not_found], including terminal runs. *)
val active_runs : t -> (A.run list, A.error) result
val active_runs_for_owner : t -> owner:Cashflow_core.User_id.t -> (A.run list, A.error) result

val account_alias : t -> conversation:A.Conversation_id.t -> Cashflow_core.Account_id.t -> (string, A.error) result
(** The account must already be authorized by the caller. Aliases never enter metadata. *)

val resolve_account_alias :
  t -> owner:Cashflow_core.User_id.t -> conversation:A.Conversation_id.t -> alias:string -> (Cashflow_core.Account_id.t, A.error) result
(** Owner-checked lookup only, not financial authorization. Unknown aliases and wrong owners
    or conversations are indistinguishable. Callers must recheck current pinned scope. *)
