type handler = { handle : 'a. 'a Effect_ops.t -> 'a }
val perform : 'a Effect_ops.t -> 'a
val run : handler -> (unit -> 'a) -> 'a
