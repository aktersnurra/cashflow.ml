(* Synthetic loopback-only fixture. No production route/provider bypass. *)
open Cashflow_io
module A = Cashflow_core.Advisor
module R = Advisor_runtime
module X = Advisor_access

let ok = Result.get_ok

let financial_digest db =
  let tables = ["accounts"; "account_details"; "transfers"; "budget_versions"; "savings_goals"; "import_batches"; "import_rows"] in
  let rows = List.concat_map (fun table ->
    let columns = Sql.(run db (stmt ("SELECT name FROM pragma_table_info('" ^ table ^ "') ORDER BY cid") nil (many (row (text @-> nil) Fun.id)))) in
    let sql = "SELECT " ^ String.concat " || '|' || " (List.map (fun c -> "quote(\"" ^ c ^ "\")") columns)
      ^ " FROM " ^ table ^ " ORDER BY " ^ String.concat "," (List.map (fun c -> "\"" ^ c ^ "\"") columns) in
    table :: Sql.(run db (stmt sql nil (many (row (text @-> nil) Fun.id))))) tables in
  Digest.to_hex (Digest.string (String.concat "\n" rows))

let () =
  Eio_main.run @@ fun env ->
  let db = Store_sqlite.open_memory () in
  Fun.protect
    ~finally:(fun () -> Sql.close db)
    (fun () ->
      Eio.Switch.run (fun sw ->
          let product = Array.exists (( = ) "--product") Sys.argv in
          let nonstandard_currency = product && Array.exists (( = ) "--currency-scales") Sys.argv in
          if nonstandard_currency then Sql.exec_script db "UPDATE currencies SET scale=0 WHERE code='SEK'; UPDATE currencies SET scale=4 WHERE code='EUR'";
          let provider_starts = ref 0 in
          let browser_text = Array.exists (( = ) "--browser-text") Sys.argv in
          let small_buffers = Array.exists (( = ) "--small-socket-buffers") Sys.argv in
          let pending_writes = Hashtbl.create 4 and next_connection = ref 0 in
          let drain_timeouts = ref 0 in
          let guard = Db_guard.make () in
          Sql.set_guard db guard;
          let released = ref false and producer_closed = ref false and connections = ref 0 in
          let gate, release = Eio.Promise.create () in
          let service = Service.make ~now:(if product then (fun () -> Test_service_support.now) else Ptime_clock.now) db in
          let access =
            Service.sign_in service
              {
                issuer = "https://synthetic.invalid";
                subject = "advisor-fixture";
                display_name = None;
                email = None;
                is_admin = Some product;
                locale = None;
              }
          in
          let user = Cashflow_core.Access.user access in
          let token = Web_auth.create_session db ~now:(service.now ()) user in
          let token_b = Web_auth.create_session db ~now:(service.now ()) user in
          let token_c = if product then
            let other = Test_service_support.sign_in service "Other synthetic user" in
            Web_auth.create_session db ~now:(service.now ()) (Cashflow_core.Access.user other)
            else token_b in
          let principal = ok (X.authenticate service ~now:(service.now ()) ~token) in
          if not product then ignore (ok (X.set_user_consent service principal ~enabled:true));
          let product_refresh, product_import =
            if not product then ((fun () -> ()), (fun () -> ())) else (
              let module S = Test_service_support in
              ignore (S.ok (Service.create_household service access ~name:"Synthetic household" ~default_currency:(if nonstandard_currency then "EUR" else "SEK")));
              let access = Service.access_for service user in
              let bank = S.open_checking service access "Everyday" in
              let bills = S.open_checking service access "Bills" in
              let salary = S.account_named service access "Salary" in
              let groceries = S.account_named service access "Groceries" in
              let rent = S.account_named service access "Subscriptions" in
              let post key from_account to_account value on =
                ignore (S.ok (Service.add_entry service access
                  { (S.entry key ~from_account ~to_account value) with value_date = S.date on })) in
              post "income" salary bank 5000000 "2026-06-25";
              List.iteri (fun n on -> post ("groceries" ^ string_of_int n) bank groceries 100000 on)
                ["2026-07-01"; "2026-08-01"; "2026-09-01"; "2026-10-01"];
              post "extra groceries" bank groceries 150000 "2026-10-10";
              post "bills income" salary bills 3000000 "2026-07-01";
              List.iteri (fun n on -> post ("RENT " ^ string_of_int n) bills rent 800000 on)
                ["2026-07-20"; "2026-08-20"; "2026-09-20"];
              let statement = S.open_checking service access ~statement_backed:true "Import account" in
              ignore (S.ok_import (Import.confirm (Import.context service access)
                { account_id = statement; file_name = None;
                  content = "Datum;Text;Belopp;Saldo\n2026-10-20;Synthetic purchase;-250,00;-250,00\n"; decisions = [] }));
              let item = List.hd (Inbox.list service access ~limit:10) in
              ignore (S.ok (Inbox.categorize service access ~transfer_id:item.item.transfer_id ~category_id:groceries ()));
              ((fun () ->
                let ctx = S.ok (Insights.context service access ~scope:Me ()) in
                ignore (S.ok (Insights.set_budget ctx ~category_id:groceries ~monthly_amount:500000L ~rollover:false ~effective_from:None))),
               (fun () ->
                ignore (S.ok_import (Import.confirm (Import.context service access)
                  { account_id = statement; file_name = None;
                    content = "Datum;Text;Belopp;Saldo\n2026-10-21;Synthetic followup;-400,00;-650,00\n"; decisions = [] }));
                List.iter (fun item -> ignore (S.ok (Inbox.categorize service access ~transfer_id:item.Inbox.item.transfer_id ~category_id:groceries ())))
                  (Inbox.list service access ~limit:10))))
          in
          let credentials token =
            let i = Option.get (Web_auth.inspect_session service ~now:(service.now ()) (Web_auth.session_ref token)) in
            `Assoc [ ("token", `String token); ("csrf", `String i.csrf_token) ]
          in
          let post ~uri:_ ~headers:_ ~body ~on_chunk =
            incr provider_starts;
            Fun.protect
              ~finally:(fun () -> producer_closed := true)
              (fun () ->
                assert (not (guard.held ()));
                let text value =
                  "data: "
                  ^ Yojson.Safe.to_string
                      (`Assoc
                         [
                           ( "choices",
                             `List
                               [
                                 `Assoc
                                   [
                                     ("index", `Int 0);
                                     ("delta", `Assoc [ ("content", `String value) ]);
                                     ("finish_reason", `Null);
                                   ];
                               ] );
                         ])
                  ^ "\n\n"
                in
                on_chunk
                  (text
                     (if small_buffers then "first" ^ String.make ((60 * 1024) - 5) 'x'
                      else if browser_text then
                        "first héj <img src=x onerror=window.fixtureXss=true> [evidence:not-authorized]\n"
                        ^ String.concat "\n" (List.init 50 (fun n -> "synthetic line " ^ string_of_int n))
                      else if product && Cashflow_core.Str_compat.contains ~sub:"change my budget" body then
                        "I cannot change budgets or move money. Open Budgets to make changes."
                      else "first"));
                Eio.Promise.await gate;
                assert (not (guard.held ()));
                on_chunk
                  (text (if browser_text then "\nsecond <script>window.fixtureXss=true</script>" else " second")
                  ^ "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n");
                Ok ())
          in
          let provider =
            Advisor_provider.create
              ~limits:
                {
                  context_tokens = 262144;
                  completion_tokens = 16384;
                  whole_completion_cap_supported = true;
                  request_tokens_per_byte = 1;
                  request_token_overhead = 0;
                }
              ~post ~api_key:"synthetic" ~model:"synthetic" ~provider_only:[ "synthetic" ] ()
          in
          let origin = Eio.Time.Mono.now env#mono_clock in
          let clock : R.clock =
            {
              now = service.now;
              monotonic_s =
                (fun () ->
                  Mtime.span origin (Eio.Time.Mono.now env#mono_clock) |> Mtime.Span.to_float_ns |> fun ns -> ns /. 1e9);
              sleep_s =
                (fun seconds ->
                  assert (not (guard.held ()));
                  Eio.Time.sleep env#clock seconds);
            }
          in
          let runtime = R.create ~sw ~service ~provider ~clock in
          let advisor =
            Some
              (Advisor_http.create ~service ~runtime ~public_origin:"http://127.0.0.1:18197" ~available:(not (Array.exists (( = ) "--unavailable") Sys.argv))
                 ~sleep_s:clock.sleep_s)
          in
          let config =
            ok
              (Config.parse
                 {|((database :memory:) (listen 127.0.0.1:18197) (public_url http://127.0.0.1:18197) (oidc ((issuer https://synthetic.invalid) (audience synthetic))))|})
          in
          let config = if product then { config with static_dir = Sys.argv.(1) } else config in
          let deps : Http_server.deps =
            { service; advisor; config; verify_bearer = (fun _ -> Error ()); web_auth = None; now = service.now }
          in
          let send reqd value = Http_server.respond_with reqd (Http_server.json 200 value) in
          let handler connection_sw on_stream_end =
            Httpun_eio.Server.create_connection_handler ~sw:connection_sw
              ~error_handler:(fun _ ?request:_ _ _ -> ())
              ~request_handler:(fun _ wrapped ->
                let reqd = wrapped.Gluten.Reqd.reqd in
                match (Httpun.Reqd.request reqd).target with
                | "/fixture/import" when product ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    product_import (); send reqd (`String "ok")
                | "/fixture/resolve-source" when product ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    product_refresh (); send reqd (`String "ok")
                | "/fixture/session" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    send reqd (`Assoc ([ ("a", credentials token); ("b", credentials token_b) ] @ if product then [("c", credentials token_c)] else []))
                | "/release" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    if not !released then (
                      released := true;
                      Eio.Promise.resolve release ());
                    send reqd (`String "ok")
                | ("/fixture/expire-a" | "/fixture/expire-b") as path ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    Sql.run db Web_auth.touch_session (service.now ()) Ptime.epoch
                      (Web_auth.hash (if path = "/fixture/expire-a" then token else token_b));
                    send reqd (`String "ok")
                | "/fixture/prune" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    Advisor_store.prune (Advisor_store.create db)
                      ~now:(Option.get (Ptime.add_span (service.now ()) (Ptime.Span.of_int_s (2 * 86400))));
                    send reqd (`String "ok")
                | "/state" ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    send reqd
                      (`Assoc
                         [
                           ("provider_starts", `Int !provider_starts);
                           ("financial_digest", if product then `String (financial_digest db) else `Null);
                           ("drain_timeouts", `Int !drain_timeouts);
                           ("released", `Bool !released);
                           ("producer_closed", `Bool !producer_closed);
                           ("connections", `Int !connections);
                           ( "blocked_writes",
                             `Int
                               (Hashtbl.fold
                                  (fun _ since count ->
                                    if clock.monotonic_s () -. since >= 0.05 then count + 1 else count)
                                  pending_writes 0) );
                         ])
                | ("/" | "/main.bc.js") as path when not product && Array.length Sys.argv > 1 ->
                    Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd);
                    let file = if path = "/" then "index.html" else "main.bc.js" in
                    let body = In_channel.with_open_bin (Filename.concat Sys.argv.(1) file) In_channel.input_all in
                    Http_server.respond_with reqd
                      (Http_server.respond 200 (if path = "/" then "text/html" else "application/javascript") body)
                | target when product && not (String.starts_with ~prefix:"/api/" target || String.starts_with ~prefix:"/auth/" target) ->
                    let path = List.hd (String.split_on_char '?' target) in
                    let valid = List.mem path ["/"; "/advisor"; "/cashflow"; "/transactions"; "/inbox"; "/accounts"; "/budgets"; "/forecast"; "/household"; "/insights"; "/settings"; "/add";
                      "/assets/main.bc.js"; "/assets/style.css"; "/assets/manifest.webmanifest"; "/assets/icon.svg"]
                      || (match String.split_on_char '/' path with [""; ("advisor" | "accounts"); id] ->
                        Option.is_some (Cashflow_core.Account_id.of_string id)
                        | [""; "transactions"; offset] -> Option.is_some (int_of_string_opt offset)
                        | _ -> false) in
                    if valid then Http_server.serve_request ~sw:connection_sw ~on_stream_end deps reqd
                    else (Httpun.Body.Reader.close (Httpun.Reqd.request_body reqd); Http_server.respond_with reqd (Http_server.plain 404 "Not Found"))
                | _ -> Http_server.serve_request ~sw:connection_sw ~on_stream_end deps reqd)
          in
          let listener =
            Eio.Net.listen ~sw ~reuse_addr:true ~backlog:32 env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 18197))
          in
          Eio.Net.run_server
            ~on_error:(fun _ -> ())
            listener
            (fun socket addr ->
              incr connections;
              incr next_connection;
              let id = !next_connection in
              if small_buffers then
                Eio_unix.Fd.use_exn "fixture SO_SNDBUF"
                  (Option.get (Eio_unix.Resource.fd_opt socket))
                  (fun fd -> Unix.setsockopt_int fd Unix.SO_SNDBUF 2048);
              let socket =
                let module Monitor = struct
                  type t = unit
                  type tag = [ `Generic ]

                  let read_methods = []
                  let single_read () buffer = Eio.Flow.single_read socket buffer

                  let single_write () buffers =
                    assert (not (guard.held ()));
                    Hashtbl.replace pending_writes id (clock.monotonic_s ());
                    Fun.protect
                      ~finally:(fun () -> Hashtbl.remove pending_writes id)
                      (fun () -> Eio.Flow.single_write socket buffers)

                  let copy () ~src = Eio.Flow.Pi.simple_copy ~single_write () ~src
                  let shutdown () direction = Eio.Flow.shutdown socket direction
                  let close () = Eio.Flow.close socket
                  let setsockopt () option value = Eio.Net.setsockopt socket option value
                  let getsockopt () option = Eio.Net.getsockopt socket option
                end in
                Eio.Resource.T ((), Eio.Net.Pi.stream_socket (module Monitor))
              in
              Fun.protect
                ~finally:(fun () -> decr connections)
                (fun () ->
                  try
                    Eio.Cancel.sub (fun context ->
                        Eio.Switch.run (fun connection_sw ->
                            Fun.protect
                              ~finally:(fun () -> Eio.Cancel.cancel context Exit)
                              (fun () ->
                                handler connection_sw
                                  (fun result ->
                                    Http_server.finish_stream ~sw:connection_sw ~sleep_s:clock.sleep_s
                                      ~abort:(fun () ->
                                        if result = Advisor_http.Drained then incr drain_timeouts;
                                        Eio.Cancel.cancel context Exit)
                                      result)
                                  addr socket)))
                  with Eio.Cancel.Cancelled Exit -> ()))))
