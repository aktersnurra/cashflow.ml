type writer
type write_error = Closed
(** Subscriber work must run in a connection-owned cancellation scope. On
    connection-handler termination, cancel pending flush waits and close in a
    finally block. The generation run must have a separate lifetime. *)
val open_response : Httpun.Reqd.t -> writer
(** Waits for Httpun's flush acknowledgement; never holds a database guard. *)
val write_frame : writer -> Cashflow_advisor_protocol.Sse.frame -> (unit, write_error) result
val heartbeat : writer -> (unit, write_error) result
val close : writer -> unit
