exception Sql_error of string

type _ field =
  | Int : int field
  | Int64 : int64 field
  | Text : string field
  | Bool : bool field
  | Nullable : 'a field -> 'a option field
  | Map : { name : string; field : 'b field; to_db : 'a -> 'b; of_db : 'b -> 'a option } -> 'a field

type ('f, 'r) spec = Nil : ('r, 'r) spec | Cons : 'a field * ('f, 'r) spec -> ('a -> 'f, 'r) spec

let int = Int
let int64 = Int64
let text = Text
let bool = Bool
let nullable field = Nullable field
let map ~name field ~to_db ~of_db = Map { name; field; to_db; of_db }
let nil = Nil
let ( @-> ) field rest = Cons (field, rest)

type 'row row = Row : ('make, 'row) spec * 'make -> 'row row

let row spec make = Row (spec, make)

type _ mode =
  | Exec : unit mode
  | One : 'row row -> 'row mode
  | Opt : 'row row -> 'row option mode
  | Many : 'row row -> 'row list mode

let exec = Exec
let one row = One row
let opt row = Opt row
let many row = Many row

type 'f t = Stmt : { sql : string; params : ('f, 'a) spec; mode : 'a mode } -> 'f t

let stmt sql params mode = Stmt { sql; params; mode }

type db = { handle : Sqlite3.db; statements : (string, Sqlite3.stmt) Hashtbl.t; mutable depth : int }

let fail fmt = Printf.ksprintf (fun message -> raise (Sql_error message)) fmt

let check db rc what =
  if not (Sqlite3.Rc.is_success rc) then fail "%s: %s (%s)" what (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db.handle)

let exec_script db sql = check db (Sqlite3.exec db.handle sql) "exec"

let open_db path =
  let db = { handle = Sqlite3.db_open path; statements = Hashtbl.create 32; depth = 0 } in
  exec_script db "PRAGMA foreign_keys = ON";
  if path <> ":memory:" then exec_script db "PRAGMA journal_mode = WAL";
  db

let close db =
  Hashtbl.iter (fun _ statement -> ignore (Sqlite3.finalize statement)) db.statements;
  ignore (Sqlite3.db_close db.handle)

let changes db = Sqlite3.changes db.handle

let rec encode : type a. a field -> a -> Sqlite3.Data.t =
 fun field value ->
  match field with
  | Int -> INT (Int64.of_int value)
  | Int64 -> INT value
  | Text -> TEXT value
  | Bool -> INT (if value then 1L else 0L)
  | Nullable field -> (match value with None -> NULL | Some value -> encode field value)
  | Map { field; to_db; _ } -> encode field (to_db value)

let rec field_name : type a. a field -> string = function
  | Int -> "int"
  | Int64 -> "int64"
  | Text -> "text"
  | Bool -> "bool"
  | Nullable field -> field_name field ^ " option"
  | Map { name; _ } -> name

let rec decode : type a. a field -> Sqlite3.Data.t -> a option =
 fun field data ->
  match field, data with
  | Int, INT value -> Some (Int64.to_int value)
  | Int64, INT value -> Some value
  | Text, TEXT value -> Some value
  | Bool, INT value -> Some (not (Int64.equal value 0L))
  | Nullable _, NULL -> Some None
  | Nullable field, data -> Option.map Option.some (decode field data)
  | Map { field; of_db; _ }, data -> Option.bind (decode field data) of_db
  | (Int | Int64 | Text | Bool), _ -> None

let decode_row statement (Row (spec, make)) =
  let rec go : type make row. (make, row) spec -> make -> int -> row =
   fun spec make index ->
    match spec with
    | Nil -> make
    | Cons (field, rest) -> (
        match decode field (Sqlite3.column statement index) with
        | Some value -> go rest (make value) (index + 1)
        | None -> fail "column %d does not decode as %s" index (field_name field))
  in
  go spec make 0

let prepared db sql =
  match Hashtbl.find_opt db.statements sql with
  | Some statement -> statement
  | None ->
      let statement = try Sqlite3.prepare db.handle sql with Sqlite3.Error message -> fail "prepare %S: %s" sql message in
      Hashtbl.replace db.statements sql statement;
      statement

let execute : type a. db -> string -> Sqlite3.Data.t list -> a mode -> a =
 fun db sql values mode ->
  let statement = prepared db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.reset statement); ignore (Sqlite3.clear_bindings statement))
    (fun () : a ->
      check db (Sqlite3.bind_values statement values) ("bind " ^ sql);
      let rec rows : type r. r list -> r row -> r list =
       fun acc row_decoder ->
        match Sqlite3.step statement with
        | ROW -> rows (decode_row statement row_decoder :: acc) row_decoder
        | DONE -> List.rev acc
        | rc -> fail "%s: %s (%s)" sql (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db.handle)
      in
      match mode with
      | Exec -> (
          match Sqlite3.step statement with
          | DONE | ROW -> ()
          | rc -> fail "%s: %s (%s)" sql (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db.handle))
      | One row -> (match rows [] row with [ value ] -> value | values -> fail "%s: expected one row, got %d" sql (List.length values))
      | Opt row -> (match rows [] row with [] -> None | [ value ] -> Some value | _ -> fail "%s: expected at most one row" sql)
      | Many row -> rows [] row)

let run db (Stmt { sql; params; mode }) =
  let rec go : type f a. (f, a) spec -> Sqlite3.Data.t list -> (Sqlite3.Data.t list -> a) -> f =
   fun spec values finish ->
    match spec with
    | Nil -> finish (List.rev values)
    | Cons (field, rest) -> fun value -> go rest (encode field value :: values) finish
  in
  go params [] (fun values -> execute db sql values mode)

let transaction db f =
  let savepoint = Printf.sprintf "cashflow_%d" db.depth in
  let begin_, commit, rollback =
    if db.depth = 0 then ("BEGIN IMMEDIATE", "COMMIT", "ROLLBACK")
    else ("SAVEPOINT " ^ savepoint, "RELEASE " ^ savepoint, "ROLLBACK TO " ^ savepoint ^ "; RELEASE " ^ savepoint)
  in
  exec_script db begin_;
  db.depth <- db.depth + 1;
  match f () with
  | value ->
      db.depth <- db.depth - 1;
      exec_script db commit;
      value
  | exception exn ->
      db.depth <- db.depth - 1;
      (try exec_script db rollback with Sql_error _ -> ());
      raise exn

exception Rollback_result

(* Like [transaction], but an [Error] result also rolls back, so a use case that fails halfway
   leaves nothing behind. *)
let transaction_result db f =
  let result = ref None in
  match transaction db (fun () -> match f () with Ok _ as ok -> ok | Error _ as error -> result := Some error; raise Rollback_result) with
  | ok -> ok
  | exception Rollback_result -> Option.get !result
