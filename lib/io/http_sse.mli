type writer
(** Subscriber work runs in a connection-owned cancellation scope. Pending flush waits are cancelled and the writer
    closed on termination; generation has a separate lifetime. *)

type write_error = Closed

val open_response_checked : authorize:(unit -> bool) -> Httpun.Reqd.t -> (writer, write_error) result
(** Gate bound once; checks before headers/enqueue and after flush. Already enqueued bytes cannot be recalled. *)

val open_response : Httpun.Reqd.t -> writer
(** Explicit ungated compatibility path for the transport spike only. *)

val write_frame : writer -> Cashflow_advisor_protocol.Sse.frame -> (unit, write_error) result
(** Waits for Httpun's flush acknowledgement; never holds a database guard. *)

val heartbeat : writer -> (unit, write_error) result
val is_closed : writer -> bool
val close : writer -> unit
