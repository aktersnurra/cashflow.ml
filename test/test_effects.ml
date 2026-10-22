open Cashflow_workflow

let ok = function Ok x -> x | Error _ -> failwith "fixture"
let sek = ok (Cashflow_core.Currency.make ~code:"SEK" ~scale:2)
let clock = Ptime.epoch
let handler = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a ->
  match op with
  | Now -> clock
  | Resolve_currency "SEK" -> Ok sek
  | Resolve_currency code -> Error (Reporting_error.Unknown_currency code)
  | Load_cashflow _ -> Error (Reporting_error.Storage_unavailable Load_cashflow)
  | Load_forecast _ -> Error (Reporting_error.Storage_unavailable Load_forecast)) }
let check = Alcotest.(check bool)
let typed () =
  check "normal return" true (Effects.run handler (fun () -> 42) = 42);
  Effects.run handler (fun () ->
    check "clock" true (Ptime.equal clock (Effects.perform Now));
    check "currency" true (Cashflow_core.Currency.equal sek (ok (Effects.perform (Resolve_currency "SEK"))));
    check "expected error" true (Effects.perform (Resolve_currency "BAD") = Error (Reporting_error.Unknown_currency "BAD")))

type _ Effect.t += Outer : int Effect.t
let forwarding () =
  let value = Effect.Deep.try_with (fun () -> Effects.run handler (fun () -> Effect.perform Outer + 1)) ()
    { effc = (fun (type a) (eff : a Effect.t) -> match eff with
      | Outer -> Some (fun (k : (a, _) Effect.Deep.continuation) -> Effect.Deep.continue k 7)
      | _ -> None) } in
  Alcotest.(check int) "forwarded" 8 value
let nested () =
  let inner = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a -> match op with Now -> Ptime.max | _ -> handler.handle op) } in
  Effects.run handler (fun () ->
    check "inner" true (Effects.run inner (fun () -> Ptime.equal Ptime.max (Effects.perform Now)));
    check "outer restored" true (Ptime.equal clock (Effects.perform Now)))
exception Original of int ref
let exception_case operation_raises () =
  let cleanup = ref 0 and token = ref 0 in
  let original = Original token in
  let raising = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a ->
    match op with Now when operation_raises -> raise original | _ -> handler.handle op) } in
  let caught = try
    Effects.run raising (fun () -> Fun.protect ~finally:(fun () -> incr cleanup) (fun () ->
      ignore (Effects.perform Now); if !cleanup = 0 then raise original)); false
    with exn -> exn == original in
  check "original exception identity" true caught;
  Alcotest.(check int) "cleanup exactly once" 1 !cleanup
let unhandled () =
  let raised = try ignore (Effects.perform Now); false with Effect.Unhandled _ -> true in
  check "unhandled invocation" true raised
let fibers () =
  Eio_main.run (fun _ ->
    let seen = Array.make 2 [] in
    let fiber i (time : Ptime.t) =
      let local = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a ->
        Eio.Fiber.yield ();
        match op with Now -> time | _ -> handler.handle op) } in
      Effects.run local (fun () -> for _ = 1 to 3 do seen.(i) <- Effects.perform Now :: seen.(i) done)
    in
    Eio.Fiber.both (fun () -> fiber 0 clock) (fun () -> fiber 1 Ptime.max);
    List.iteri (fun i time -> check "fiber-local clocks" true (List.length seen.(i) = 3 && List.for_all (Ptime.equal time) seen.(i))) [clock; Ptime.max])
let access_fibers () =
  let module C = Cashflow_core in
  let user n : C.User.t = {
    id=Option.get (C.User_id.of_string (Printf.sprintf "00000000-0000-4000-8000-%012d" n));
    oidc_issuer="test"; oidc_subject=string_of_int n; display_name="Fiber";
    email=None; locale="sv"; timezone="UTC"; default_currency="SEK";
    pay_cycle=C.Pay_cycle_setting.calendar_month; is_admin=false; created_at=clock
  } in
  let accesses = Array.init 2 (fun i -> C.Access.make ~user:(user (i+1)) ~household:None) in
  let times = [|clock; ok (C.Time.parse_offset_datetime "2026-10-22T00:00:00Z")|] in
  Eio_main.run (fun _ ->
    let fiber i =
      let loads = ref 0 in
      let local = { Effects.handle = (fun (type a) (op : a Effect_ops.t) : a ->
        Eio.Fiber.yield ();
        match op with
        | Now -> times.(i)
        | Resolve_currency _ -> Ok sek
        | Load_cashflow request ->
            incr loads;
            check "fiber request access" true (Reporting_context.access (Cashflow_request.context request) == accesses.(i));
            Ok {Cashflow_facts.accounts=[]; transfers=[]; reconciled_through=None}
        | Load_forecast _ -> failwith "no forecast here") } in
      let report = ok (Effects.run local (fun () ->
        Reporting.cashflow_summary ~access:accesses.(i) ~scope:Me ~cycles:1 ())) in
      check "fiber report clock" true (C.Date.compare (Reporting_context.today report.context) (C.Time.local_date C.Time.utc times.(i)) = 0);
      Alcotest.(check int) "one load" 1 !loads
    in
    Eio.Fiber.both (fun () -> fiber 0) (fun () -> fiber 1))
let () = Alcotest.run "Effects" ["bridge", List.map (fun (n,f) -> Alcotest.test_case n `Quick f)
  ["typed answers",typed; "forwarding",forwarding; "nested",nested; "operation exception",exception_case true; "resumed exception",exception_case false; "unhandled",unhandled; "interleaved clock fibers",fibers; "interleaved access workflows",access_fibers]]
