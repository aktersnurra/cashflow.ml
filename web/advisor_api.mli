module W = Cashflow_advisor_protocol.Wire
module C = Cashflow_advisor_protocol.Client_state

val submit : W.submit -> (W.accepted, C.failure) result Async_kernel.Deferred.t
val retry : W.retry -> (W.accepted, C.failure) result Async_kernel.Deferred.t
val snapshot : string -> (W.snapshot, C.failure) result Async_kernel.Deferred.t
val status : unit -> (C.status, C.failure) result Async_kernel.Deferred.t
val history : limit:int -> offset:int -> (C.history_page, C.failure) result Async_kernel.Deferred.t
val cancel : string -> (string, C.failure) result Async_kernel.Deferred.t
val delete : string -> (unit, C.failure) result Async_kernel.Deferred.t
val consent : enabled:bool -> household_id:string option -> (unit, C.failure) result Async_kernel.Deferred.t

type memory = { id : string; text : string }

val memories : unit -> (memory list, C.failure) result Async_kernel.Deferred.t
val add_memory : string -> (unit, C.failure) result Async_kernel.Deferred.t
val update_memory : id:string -> string -> (unit, C.failure) result Async_kernel.Deferred.t
val forget_memory : string -> (unit, C.failure) result Async_kernel.Deferred.t

val decide_proposal : id:string -> string -> (W.proposal_state, Cashflow_advisor_protocol.Client_state.failure) result Async_kernel.Deferred.t
(** [action] is "apply" or "dismiss". The result is the state the server recorded. *)
