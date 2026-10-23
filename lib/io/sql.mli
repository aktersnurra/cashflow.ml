(** Typed SQLite statements.

    A statement's parameters and result columns are described by a {!spec}, a typed list of
    {!field}s in the style of [Printf]'s format GADT. Running a statement gives a curried function
    over its parameters, and decoding a row applies a constructor to its columns, so a mismatch
    between SQL and OCaml types is a compile error at the call site:

    {[
      let account_name =
        Sql.(stmt "SELECT name FROM accounts WHERE id = ?" (text @-> nil) (opt (row (text @-> nil) Fun.id)))

      let name : string option = Sql.run db account_name "0190…"
    ]}

    Failures (a constraint violation, a column that doesn't decode) raise {!Sql_error}. They only
    ever mean a bug or a broken database, so they are handled once, at the transaction boundary. *)

exception Sql_error of string

type _ field =
  | Int : int field
  | Int64 : int64 field
  | Text : string field
  | Blob : string field
  | Bool : bool field  (** stored as 0 or 1 *)
  | Nullable : 'a field -> 'a option field
  | Map : { name : string; field : 'b field; to_db : 'a -> 'b; of_db : 'b -> 'a option } -> 'a field
      (** A domain type stored as a column type; [of_db] returning [None] is a decode error. *)

type ('f, 'r) spec = Nil : ('r, 'r) spec | Cons : 'a field * ('f, 'r) spec -> ('a -> 'f, 'r) spec

val int : int field
val int64 : int64 field
val text : string field
val blob : string field
val bool : bool field
val nullable : 'a field -> 'a option field
val map : name:string -> 'b field -> to_db:('a -> 'b) -> of_db:('b -> 'a option) -> 'a field
val nil : ('r, 'r) spec
val ( @-> ) : 'a field -> ('f, 'r) spec -> ('a -> 'f, 'r) spec

type 'row row

val row : ('make, 'row) spec -> 'make -> 'row row
(** Decodes the columns in order and applies the constructor to them. *)

type _ mode
(** What running a statement returns. *)

val exec : unit mode
val one : 'row row -> 'row mode  (** exactly one row, or {!Sql_error} *)
val opt : 'row row -> 'row option mode
val many : 'row row -> 'row list mode

type 'f t

val stmt : string -> ('f, 'a) spec -> 'a mode -> 'f t

type db

val open_db : string -> db
(** Opens a database with foreign keys on and, for files, WAL journaling. ":memory:" works. *)

type guard = { exclusive : 'a. (unit -> 'a) -> 'a; held : unit -> bool }
(** How concurrent users share the connection: [exclusive f] runs [f] with the connection to
    itself, and [held ()] says whether the caller is already inside such a call. Every statement
    and transaction runs under it. The default does nothing, for single-threaded use; a server
    with concurrent request fibers sets one that serialises them ({!Db_guard}). Code holding the
    connection should not wait on the network, since everyone else waits on it. *)

val set_guard : db -> guard -> unit
val unguarded : db -> guard

val close : db -> unit
val run : db -> 'f t -> 'f
val exec_script : db -> string -> unit

val transaction : db -> (unit -> 'a) -> 'a
(** [BEGIN IMMEDIATE] ... [COMMIT], rolled back if the function raises. Nested calls become
    savepoints, so a service can group several operations that each use a transaction. *)

val read_snapshot : db -> (unit -> 'value) -> 'value
(** [BEGIN DEFERRED] at the outer boundary, with the same guard, savepoints and exception
    cleanup as {!transaction}. The first read pins the snapshot. Intended for read callbacks;
    this signature does not enforce read-only SQL. *)

val in_transaction : db -> bool

val changes : db -> int
(** Rows changed by the last statement. *)

val transaction_result : db -> (unit -> ('a, 'e) result) -> ('a, 'e) result
(** Like {!transaction}, but rolls back when the function returns [Error] too. *)
