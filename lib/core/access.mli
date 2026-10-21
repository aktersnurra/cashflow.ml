(** What a signed-in user may use: their own accounts, and their active household's.

    A value of [t] is the proof that the user was authenticated. Service functions take one,
    and the ledger only ever sees accounts through {!restrict}, so a request can't touch an
    account its user may not use: such accounts are simply not found. *)
type t

val make : user:User.t -> household:Household.t option -> t
(** Only the authentication layer calls this, after verifying a token or a session. *)

val user : t -> User.t
val household : t -> Household.t option
val personal : t -> Owner.t
val owners : t -> Owner.t list
val can_use : t -> Owner.t -> bool
val is_admin : t -> bool
val restrict : t -> Ledger_core.view -> Ledger_core.view
