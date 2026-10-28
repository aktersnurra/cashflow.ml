module A = Cashflow_core.Advisor
module T = Cashflow_workflow.Advisor_tools

type aliases

val create_aliases :
  Service.t ->
  principal:Advisor_access.principal ->
  scope:A.scope ->
  conversation:A.Conversation_id.t ->
  (aliases, A.error) result
(** One opaque handle per answer attempt. Account aliases persist per conversation; series aliases are unique to this
    handle and only resolve series supplied in its evidence. Neither a handle nor an existing alias grants authority. *)

val parse_call : name:string -> arguments:string -> (T.request, A.error) result

val read :
  ?filed:Autofile.report ->
  (** For [File_inbox]: what was filed, in the caller's transaction; the read only reports it. *)
  Service.t ->
  principal:Advisor_access.principal ->
  scope:A.scope ->
  aliases:aliases ->
  currency:Cashflow_core.Currency.t ->
  T.request ->
  (T.evidence, A.error) result

val model_json : aliases:aliases -> T.evidence -> (Yojson.Safe.t, A.error) result
(** Only evidence returned with this handle is accepted. Serializes outside the database guard; enforces 16 KiB/result
    and 64 KiB combined unique selected evidence. *)

val display : T.evidence -> A.evidence

val resolve_finding :
  Service.t -> principal:Advisor_access.principal -> scope:A.scope -> key:string -> (A.finding, A.error) result
(** Deterministic current feed only; supported kinds: overview, budget_alert, unusual_spend, shortfall, goal_off_track.
    Unsupported or ambiguous scopes fail closed. *)
