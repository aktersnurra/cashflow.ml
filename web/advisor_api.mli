module W = Cashflow_advisor_protocol.Wire
module C = Cashflow_advisor_protocol.Client_state

val submit : W.submit -> (W.accepted, C.failure) result Async_kernel.Deferred.t
val retry : W.retry -> (W.accepted, C.failure) result Async_kernel.Deferred.t
val snapshot : string -> (W.snapshot, C.failure) result Async_kernel.Deferred.t
val status : unit -> (C.status, C.failure) result Async_kernel.Deferred.t
val history : limit:int -> offset:int -> (C.metadata list, C.failure) result Async_kernel.Deferred.t
val cancel : string -> (string, C.failure) result Async_kernel.Deferred.t
val delete : string -> (unit, C.failure) result Async_kernel.Deferred.t
val consent : enabled:bool -> household_id:string option -> (unit, C.failure) result Async_kernel.Deferred.t
