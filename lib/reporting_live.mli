(** Interpret reporting operations using request-local authority, clock and SQLite storage.
    Snapshot facts are materialized inside one guarded transaction. *)
val run : db:Sql.db -> access:Cashflow_core.Access.t -> now:(unit -> Ptime.t) -> (unit -> 'a) -> 'a
