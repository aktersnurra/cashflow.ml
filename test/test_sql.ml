module Db_guard = Cashflow_io.Db_guard
module Sql = Cashflow_io.Sql
(* The connection guard: request fibers share one connection, so a fiber that yields inside a
   transaction must not let another fiber's statements into it. *)
let insert = Sql.(stmt "INSERT INTO t (v) VALUES (?)" (text @-> nil) exec)
let values = Sql.(stmt "SELECT v FROM t ORDER BY v" nil (many (row (text @-> nil) Fun.id)))

let test_fibers_take_turns () =
  Eio_main.run @@ fun _ ->
  let db = Sql.open_db ":memory:" in
  Sql.exec_script db "CREATE TABLE t (v TEXT)";
  Sql.set_guard db (Db_guard.make ());
  Eio.Fiber.both
    (fun () ->
      (* Yields halfway through a transaction that then fails. *)
      try
        Sql.transaction db (fun () ->
            Sql.run db insert "rolled back";
            Eio.Fiber.yield ();
            Sql.transaction db (fun () -> Sql.run db insert "nested, rolled back too");
            failwith "abort")
      with Failure _ -> ())
    (fun () -> Sql.run db insert "kept");
  Alcotest.(check (list string)) "only the other fiber's write" [ "kept" ] (Sql.run db values)

let () = Alcotest.run "sql" [ ("guard", [ Alcotest.test_case "fibers take turns with the connection" `Quick test_fibers_take_turns ]) ]
