val event : Cashflow_core.Advisor.event -> Cashflow_advisor_protocol.Wire.event
(** Explicit public projection; internal run/session data has no wire conversion. *)

val snapshot : Cashflow_core.Advisor.snapshot -> Cashflow_advisor_protocol.Wire.snapshot
val accepted : Cashflow_core.Advisor.accepted -> Cashflow_advisor_protocol.Wire.accepted
val submit : Cashflow_advisor_protocol.Wire.submit -> (Cashflow_core.Advisor.submit, Cashflow_core.Advisor.error) result
val retry : Cashflow_advisor_protocol.Wire.retry -> (Cashflow_core.Advisor.retry, Cashflow_core.Advisor.error) result

val money_out : Cashflow_core.Advisor.money -> Cashflow_advisor_protocol.Wire.money
val evidence_out : Cashflow_core.Advisor.evidence -> Cashflow_advisor_protocol.Wire.evidence
val proposal_state_out : Cashflow_core.Advisor.proposal_state -> Cashflow_advisor_protocol.Wire.proposal_state
