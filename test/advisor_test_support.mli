module A = Cashflow_core.Advisor

val now : Ptime.t
val session : A.Session_ref.t
val fixture : unit -> Cashflow_io.Sql.db * Cashflow_io.Advisor_store.t * Cashflow_core.User_id.t * Ptime.t ref
val submit : ?conversation:A.Conversation_id.t -> ?version:int64 -> string -> A.submit
val ok : ('a, A.error) result -> 'a
val segment : int -> A.Segment_id.t
