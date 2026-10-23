open Bonsai_web.Cont
module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

type controller = {
  view : C.view;
  dispatch : C.action -> unit Effect.t;
  open_finding : key:string -> scope:W.scope -> currency:W.currency -> locale:string -> unit Effect.t;
  open_conversation : string -> unit Effect.t;
  open_empty : scope:W.scope -> currency:W.currency -> locale:string -> unit Effect.t;
  open_latest : unit Effect.t;
  leave_surface : unit Effect.t;
  stop : unit Effect.t;
  invalidate : unit Effect.t; (** Synchronously closes resources before queued private-view clearing. *)
}

val component : ctx:Ctx.t -> Bonsai.graph -> controller Bonsai.t
(** Mount once in the signed-in shell, above the route switch. Surface leave is not cleanup.
    Deactivation invalidates callbacks; the shell must run [invalidate] before replacing a session identity. *)
