val render_turns :
  locale:string ->
  snapshot:Cashflow_advisor_protocol.Wire.snapshot option ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) ->
  decided:(string -> Cashflow_advisor_protocol.Wire.proposal_state option) ->
  decide:(string -> string -> unit Bonsai_web.Effect.t) ->
  markdown:Bonsai_web.Vdom.Node.t Core.String.Map.t ->
  can_retry:bool ->
  chosen_attempts:(string * int) Core.String.Map.t ->
  (** by turn id: the attempt picked, and how many the turn had then *)
  choose_attempt:(turn_id:string -> attempt_id:string -> count:int -> unit Bonsai_web.Effect.t) ->
  Bonsai_web.Vdom.Node.t list
(** Derived nodes belong to the current authorized snapshot's component lifetime. *)

val can_retry : Cashflow_advisor_protocol.Client_state.view -> bool
(** Whether a new attempt could start now, were the latest answer one that can be retried. *)

val render_body :
  attach:Bonsai_web.Vdom.Node.t option ->
  (** the desk's "+", on the left of the composer *)
  turns:Bonsai_web.Vdom.Node.t list ->
  show_source:bool ->
  intro:bool ->
  (** say what the advisor is for, before the first message *)
  starters:bool ->
  (** open as a landing, offering questions to send with a click *)
  locale:string ->
  view:Cashflow_advisor_protocol.Client_state.view ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) ->
  Bonsai_web.Vdom.Node.t

val render :
  locale:string ->
  view:Cashflow_advisor_protocol.Client_state.view ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) ->
  Bonsai_web.Vdom.Node.t

val render_inline : locale:string -> view:Cashflow_advisor_protocol.Client_state.view ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) -> Bonsai_web.Vdom.Node.t

val notice : string -> Cashflow_advisor_protocol.Client_state.notice -> string

val consent : locale:string -> view:Cashflow_advisor_protocol.Client_state.view ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) ->
  decline:unit Bonsai_web.Effect.t -> Bonsai_web.Vdom.Node.t
val household_setting : locale:string -> view:Cashflow_advisor_protocol.Client_state.view ->
  dispatch:(Cashflow_advisor_protocol.Client_state.action -> unit Bonsai_web.Effect.t) -> Bonsai_web.Vdom.Node.t
