module A = Cashflow_core.Advisor
(** IO-only, current session and pinned-scope authority. No session reference is public. *)

type principal

val authenticate : Service.t -> now:Ptime.t -> token:string -> (principal, A.error) result
val access : Service.t -> principal -> (Cashflow_core.Access.t, A.error) result
val user : Service.t -> principal -> (Cashflow_core.User.t, A.error) result
val expiry : Service.t -> principal -> (Ptime.t, A.error) result

val session : Service.t -> principal -> (A.Session_ref.t, A.error) result
(** Accessors revalidate without touching expiry, using the service clock. A principal is an identity handle, never a cached grant of
    authority. *)

val authorize_scope : Service.t -> principal -> A.scope -> (unit, A.error) result
val authorize_history : Service.t -> principal -> A.Conversation_id.t -> (unit, A.error) result

val authorize_run : Service.t -> subscriber:principal -> run:A.Run_id.t -> (A.run, A.error) result
(** Unfinished runs additionally require the initiating session to remain valid. Terminal replay is history and does not depend on the
    initiating session. Pending cancellation does not prevent separately authorized history reads of committed partials. *)

val revoke_session : Service.t -> now:Ptime.t -> A.Session_ref.t -> A.Run_id.t list
(** Deletes the session and requests cancellation atomically. Raises [Sql.Sql_error] on storage failure, returning no IDs. Expiry callers
    must first inspect the actual current expiry, since ordinary authenticated activity may have extended it. *)

val set_user_consent : Service.t -> principal -> enabled:bool -> (A.Run_id.t list, A.error) result
val set_household_consent : Service.t -> principal -> Cashflow_core.Household_id.t -> enabled:bool -> (A.Run_id.t list, A.error) result

val leave_household : Service.t -> principal -> (A.Run_id.t list, A.error) result
(** Coordinates the existing [Service.leave_household] operation, not a model tool. Production leave routing must use this wrapper to make
    revocation atomic. *)

val delete_conversation : Service.t -> principal -> A.Conversation_id.t -> (A.Run_id.t list, A.error) result
(** Ownership-only: scope and consent loss do not prevent deletion. All mutation wrappers must own the outer transaction (nested calls fail
    without changes). They return affected unfinished run IDs only after commit, for signaling outside the guard. *)
