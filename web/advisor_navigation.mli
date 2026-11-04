open Bonsai_web.Cont
module W = Cashflow_advisor_protocol.Wire
module C = Cashflow_advisor_protocol.Client_state

val can_read_scope : C.status option -> W.scope -> bool
val now_ms : unit -> float
val age_caption : string -> float -> string -> string
(** [age_caption locale now updated_at]: how long ago, for a row in a list of matters. *)


val component :
  locale:string Bonsai.t ->
  advisor:Advisor.controller Bonsai.t ->
  route:Route.t Bonsai.t ->
  on_select:(string -> unit Effect.t) Bonsai.t ->
  on_new:(W.scope -> unit Effect.t) Bonsai.t ->
  menu_button:Vdom.Node.t Bonsai.t ->
  Bonsai.graph -> Vdom.Node.t Bonsai.t
