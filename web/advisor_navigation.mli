open Bonsai_web.Cont
module W = Cashflow_advisor_protocol.Wire

val component :
  locale:string Bonsai.t ->
  advisor:Advisor.controller Bonsai.t ->
  route:Route.t Bonsai.t ->
  on_select:(string -> unit Effect.t) Bonsai.t ->
  on_new:(W.scope -> unit Effect.t) Bonsai.t ->
  Bonsai.graph -> Vdom.Node.t Bonsai.t
