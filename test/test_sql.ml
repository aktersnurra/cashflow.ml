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

let with_wal_pair f =
  let path = Filename.temp_file "sql-snapshot-" ".sqlite3" in
  Fun.protect ~finally:(fun () -> List.iter (fun suffix ->
    let file = path ^ suffix in if Sys.file_exists file then Sys.remove file) [""; "-wal"; "-shm"])
    (fun () ->
      let reader = Sql.open_db path in
      Fun.protect ~finally:(fun () -> Sql.close reader) (fun () ->
        Sql.exec_script reader "CREATE TABLE t (v TEXT); PRAGMA busy_timeout = 0";
        let writer = Sql.open_db path in
        Fun.protect ~finally:(fun () -> Sql.close writer) (fun () ->
          Sql.exec_script writer "PRAGMA busy_timeout = 0";
          f reader writer)))

let snapshot_survives_writer_commit () =
  with_wal_pair (fun reader writer ->
    let count db = List.length (Sql.run db values) in
    Sql.read_snapshot reader (fun () ->
      Alcotest.(check int) "first read" 0 (count reader);
      Sql.transaction writer (fun () -> Sql.run writer insert "committed");
      Alcotest.(check int) "same snapshot" 0 (count reader));
    Sql.read_snapshot reader (fun () -> Alcotest.(check int) "fresh snapshot" 1 (count reader)))

let nested_read_cleanup () =
  with_wal_pair (fun db writer ->
    let check_values expected = Alcotest.(check (list string)) "rows" expected (Sql.run db values) in
    List.iter (fun outer ->
      outer db (fun () ->
        Sql.read_snapshot db (fun () ->
          Alcotest.(check bool) "nested transaction active" true (Sql.in_transaction db));
        (try Sql.read_snapshot db (fun () ->
          (* The callback is not a read-only SQL capability; savepoint rollback still applies. *)
          Sql.run db insert "discarded";
          raise Exit) with Exit -> ());
        Alcotest.(check bool) "outer survives nested exception" true (Sql.in_transaction db);
        check_values []);
      Alcotest.(check bool) "outer completed" false (Sql.in_transaction db))
      [Sql.transaction; Sql.read_snapshot];
    (try Sql.read_snapshot db (fun () -> raise Exit) with Exit -> ());
    Alcotest.(check bool) "root exception cleanup" false (Sql.in_transaction db);
    Sql.transaction db (fun () ->
      let conflict = try Sql.transaction writer (fun () -> ()); false with Sql.Sql_error _ -> true in
      Alcotest.(check bool) "writes still exclude writers" true conflict;
      Sql.run db insert "kept";
      let result = Sql.transaction_result db (fun () -> Sql.run db insert "result rollback"; Error "abort") in
      Alcotest.(check (result unit string)) "result error retained" (Error "abort") result;
      check_values ["kept"]);
    check_values ["kept"])

let commit_failure_cleanup () =
  Eio_main.run @@ fun _ ->
  let db = Sql.open_db ":memory:" in
  Fun.protect ~finally:(fun () -> Sql.close db) @@ fun () ->
  Sql.exec_script db "CREATE TABLE t(v TEXT); CREATE TABLE parent(id INTEGER PRIMARY KEY); CREATE TABLE child(id INTEGER REFERENCES parent(id) DEFERRABLE INITIALLY DEFERRED)";
  Sql.set_guard db (Db_guard.make ());
  let failed = ref false in
  Eio.Fiber.both
    (fun () ->
      try Sql.transaction db (fun () ->
        Sql.run db insert "failed commit";
        Sql.exec_script db "INSERT INTO child VALUES (42)";
        Eio.Fiber.yield ())
      with Sql.Sql_error _ -> failed := true)
    (fun () ->
      Alcotest.(check (list string)) "competing fiber sees rollback" [] (Sql.run db values);
      Sql.transaction db (fun () -> Sql.run db insert "fresh transaction"));
  Alcotest.(check bool) "deferred constraint failed at commit" true !failed;
  Alcotest.(check bool) "depth restored" false (Sql.in_transaction db);
  Alcotest.(check (list string)) "fresh transaction committed" ["fresh transaction"] (Sql.run db values);
  Sql.transaction db (fun () ->
    Sql.transaction db (fun () -> Sql.run db insert "nested success");
    (try Sql.transaction db (fun () -> Sql.run db insert "nested failure"; raise Exit) with Exit -> ()));
  (try Sql.transaction db (fun () -> Sql.run db insert "callback failure"; raise Exit) with Exit -> ());
  Alcotest.(check (list string)) "nested and callback cleanup" ["fresh transaction";"nested success"] (Sql.run db values)

let () = Alcotest.run "sql" [ ("guard", [ Alcotest.test_case "commit_failure_cleanup" `Quick commit_failure_cleanup; Alcotest.test_case "snapshot_survives_writer_commit" `Quick snapshot_survives_writer_commit; Alcotest.test_case "nested_read_cleanup" `Quick nested_read_cleanup; Alcotest.test_case "fibers take turns with the connection" `Quick test_fibers_take_turns ]) ]
