(* The connection guard for the server (see Sql.guard): request fibers take turns with the
   connection through a mutex, and a fiber-local mark lets the fiber holding it nest
   transactions without waiting on itself. *)
let holder : unit Eio.Fiber.key = Eio.Fiber.create_key ()

let make () : Sql.guard =
  let mutex = Eio.Mutex.create () in
  { exclusive = (fun f -> Eio.Mutex.use_ro mutex (fun () -> Eio.Fiber.with_binding holder () f));
    held = (fun () -> Option.is_some (Eio.Fiber.get holder)) }
