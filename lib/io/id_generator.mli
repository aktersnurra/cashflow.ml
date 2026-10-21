(** Live generators; clock and randomness stay outside [Cashflow_core]. *)

val account : unit -> Cashflow_core.Account_id.t
val transfer : unit -> Cashflow_core.Transfer_id.t
val user : unit -> Cashflow_core.User_id.t
val household : unit -> Cashflow_core.Household_id.t
