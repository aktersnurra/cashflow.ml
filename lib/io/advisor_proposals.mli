(** Applying and dismissing advisor proposals. Only an explicit click reaches these. *)

val apply :
  Service.t -> principal:Advisor_access.principal -> id:string -> (Cashflow_core.Advisor.proposal_state, Cashflow_core.Advisor.error) result
(** Applies an undecided proposal with the caller's own access. A proposal whose basis changed becomes [Out_of_date]
    and writes nothing; one that the service rejects becomes [Not_applied]. Anything already decided (or lapsed)
    returns its recorded state and writes nothing, so a repeated click is harmless. Another user's id is [Not_found]. *)

val undo :
  Service.t -> principal:Advisor_access.principal -> id:string -> (Cashflow_core.Advisor.proposal_state, Cashflow_core.Advisor.error) result
(** Takes an applied proposal back: the budget it set, or the rows it filed, only if they are still as it left them.
    Otherwise [Version_conflict] and nothing changes. Undoing twice returns [Reverted]; an unapplied proposal is
    [Invalid_request]. *)

val dismiss :
  Service.t -> principal:Advisor_access.principal -> id:string -> (Cashflow_core.Advisor.proposal_state, Cashflow_core.Advisor.error) result
