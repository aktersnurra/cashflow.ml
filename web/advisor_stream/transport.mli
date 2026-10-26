type connection = Connecting | Connected | Reconnecting | Closed
type subscription
val connect : url:string -> after:string option -> on_frame:(Cashflow_advisor_protocol.Sse.frame -> unit) -> on_connection:(connection -> unit) -> subscription
val close : subscription -> unit
