(** Interpret reporting operations using request-local authority, and the service's clock and
    SQLite storage.
    Snapshot facts are materialized inside one guarded transaction. *)
val run : service:Service.t -> access:Cashflow_core.Access.t -> (unit -> 'a) -> 'a
