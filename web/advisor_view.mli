val render :
  locale:string ->
  view:Cashflow_advisor_protocol.Client_state.view ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) ->
  Bonsai_web.Vdom.Node.t

val notice : string -> Cashflow_advisor_protocol.Client_state.notice -> string
