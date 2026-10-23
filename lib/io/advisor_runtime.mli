module A = Cashflow_core.Advisor

type clock = { now : unit -> Ptime.t; monotonic_s : unit -> float; sleep_s : float -> unit }
type t
type subscription
type delivery = Events of A.event list | Heartbeat | Resync_required of int64 | End

val create : sw:Eio.Switch.t -> service:Service.t -> provider:Advisor_provider.t -> clock:clock -> t
(** Exactly one authoritative supervisor per service database, on one Eio domain; multi-process/multi-supervisor
    ownership is unsupported. Recovery never generates and relies on the store's eight-active-run invariant. The service
    database must outlive [sw]. Cancelling [sw] seals/drains workers; normal scope exit joins them. Provider
    configuration must independently attest context/input/whole-completion bounds for every route; no production route
    is attested here.

    Maintenance fails unregistered Queued records after 30s, never registered or in-flight handoffs. It rechecks state
    and handoff bookkeeping atomically, samples at most eight active runs, and yields outside guards. Pruning uses
    bounded batches on a 60s cadence. Known revocations must be signalled immediately after commit; expiry/deadline
    supervision also rechecks fresh authority at most every 50ms. *)

val submit : t -> principal:Advisor_access.principal -> A.submit -> (A.accepted, A.error) result
val retry : t -> principal:Advisor_access.principal -> A.retry -> (A.accepted, A.error) result

val start : t -> A.accepted -> unit
(** Only starts canonical acceptances registered by this runtime's submit/retry. Unknown, foreign-runtime and duplicate
    acceptances cannot create work or rebind the origin. *)

val cancel : t -> principal:Advisor_access.principal -> A.Run_id.t -> (A.run_state, A.error) result
val signal_cancel : t -> A.Run_id.t list -> unit
val delete : t -> principal:Advisor_access.principal -> A.Conversation_id.t -> (unit, A.error) result

val subscribe :
  t -> principal:Advisor_access.principal -> run:A.Run_id.t -> after:int64 -> (subscription, A.error) result

val next : subscription -> (delivery, A.error) result
(** One caller at a time. Coalesced wakeups precede durable catch-up, one bounded event per delivery; unread encoded
    journal bytes above 256 KiB require resync. A disconnected HTTP scope must [close] its subscription, not cancel the
    run. The HTTP adapter must reauthorize immediately before actual writes, outside guards, and clear its client
    transcript on typed access loss. *)

val close : subscription -> unit
