type meth = GET | POST | HEAD | OTHER
type request = { meth : meth; target : string; headers : (string * string) list; body : string }
type t
type subscription
type response = Buffered of int * (string * string) list * string | Stream of subscription

val create :
  service:Service.t ->
  runtime:Advisor_runtime.t ->
  public_origin:string ->
  available:bool ->
  sleep_s:(float -> unit) ->
  t

val handle : t -> request -> response option
val close : subscription -> unit

type stream_end = Drained | Aborted

val stream : subscription -> Httpun.Reqd.t -> stream_end
(** Always closes the connection-owned subscription/writer. Drained means frames were flush-acknowledged and writer
    closed, not client receipt or completed protocol drain. Each frame/heartbeat write has an independent 30-second
    deadline; expiry aborts only this delivery, without changing run state or retrying generation. *)

val logout : t -> request -> response
val leave_household : t -> request -> response
