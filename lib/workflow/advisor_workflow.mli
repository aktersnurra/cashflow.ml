module A = Cashflow_core.Advisor

val language_instruction : string -> string
(** The reply-language instruction for a conversation's locale; unknown locales are named by their code. *)

val select_history : A.snapshot -> (Advisor_effects.context_turn list, A.error) result
(** Last whole successful turns only, bounded by twelve turns and conservative serialized conversation bytes. No failed
    output, old evidence, or summarization. *)

val execute : run:A.run -> (A.run_state, A.error) result
val accept_turn : A.submit -> (A.accepted, A.error) result
val retry_turn : A.retry -> (A.accepted, A.error) result
val cancel_run : A.Run_id.t -> (A.run_state, A.error) result
val delete_conversation : A.Conversation_id.t -> (unit, A.error) result
