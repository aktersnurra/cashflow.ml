let with_config f =
  match Config.load_from_environment () with
  | Error reason -> Logs.err (fun m -> m "invalid configuration: %s" reason); 1
  | Ok config -> f config

let with_database config f =
  let db = Sql.open_db config.Config.database_path in
  Fun.protect ~finally:(fun () -> Sql.close db) (fun () ->
      match Schema.migrate db with
      | () -> f db
      | exception Sql.Sql_error message -> Logs.err (fun m -> m "migration failed: %s" message); 1)

let migrate () = with_config (fun config -> with_database config (fun _ -> Logs.app (fun m -> m "schema at version %d" Schema.latest_version); 0))

module Ledger_sqlite = Ledger.Make (Store_sqlite)

(* Recomputes every balance from the transfers; a mismatch means a bug or a damaged database. *)
let verify () =
  with_config (fun config ->
      with_database config (fun db ->
          match Ledger_sqlite.verify db with
          | [] -> Logs.app (fun m -> m "all balances match their transfers"); 0
          | mismatches ->
              List.iter (fun (m : Ledger_core.mismatch) -> Logs.err (fun l -> l "account %s: stored balances differ from its transfers" (Account_id.to_string m.account_id))) mismatches;
              1))

let env () : Ledger_core.env = { now = Ptime_clock.now; fresh_account_id = Account_id.fresh; fresh_transfer_id = Transfer_id.fresh }

(* Run daily from cron: voids pending transfers past their expiry date. *)
let void_expired () =
  with_config (fun config ->
      with_database config (fun db ->
          let today = Time.local_date (Result.get_ok (Time.parse_zone "Europe/Stockholm")) (Ptime_clock.now ()) in
          let voided = Ledger_sqlite.void_expired db (env ()) ~today in
          Logs.app (fun m -> m "voided %d expired pending transfers" (List.length voided));
          0))

let serve () =
  with_config (fun config ->
      Eio_main.run @@ fun env ->
      with_database config (fun db ->
          let assistant =
            Option.map (fun api_key -> Llm.openrouter ~post:(Https.post env) ~api_key ~model:config.openrouter_model) config.openrouter_api_key
          in
          let service = Service.make ?assistant db in
          let client = Oidc.https_client env in
          let verify_bearer = Oidc.make ~issuer:config.oidc_issuer ~audience:config.oidc_audience ~client () in
          let web_auth =
            match config.web_client_id, config.web_client_secret with
            | Some client_id, Some client_secret ->
                let verify_id_token = Oidc.make ~issuer:config.oidc_issuer ~audience:client_id ~client () in
                Some
                  (Web_auth.make ~client ~verify_id_token
                     ~config:
                       { issuer = config.oidc_issuer; client_id; client_secret; redirect_uri = config.public_base_url ^ "/auth/callback";
                         admin_group = config.admin_group })
            | _ -> None
          in
          (* Runs until the process is stopped. *)
          Http_server.run env { service; verify_bearer; web_auth; config; now = Ptime_clock.now }))

(* For local development and browser tests: signs a user in without the identity provider and
   prints a session token to use as the cashflow_session cookie. It needs the database file and
   CASHFLOW_ALLOW_DEV_SESSIONS=1, so it grants nothing that write access to the file doesn't. *)
let create_session subject admin =
  if Sys.getenv_opt "CASHFLOW_ALLOW_DEV_SESSIONS" <> Some "1" then (
    Logs.err (fun m -> m "set CASHFLOW_ALLOW_DEV_SESSIONS=1 to create development sessions");
    1)
  else
    with_config (fun config ->
        with_database config (fun db ->
            let service = Service.make db in
            let access =
              Service.sign_in service
                { issuer = config.oidc_issuer; subject; display_name = Some subject; email = Some (subject ^ "@example.com"); is_admin = Some admin; locale = Sys.getenv_opt "CASHFLOW_DEV_LOCALE" }
            in
            print_endline (Web_auth.create_session db ~now:(Ptime_clock.now ()) (Access.user access));
            0))

let command =
  let open Cmdliner in
  let cmd name doc f = Cmd.v (Cmd.info name ~doc) Term.(const (fun () -> exit (f ())) $ const ()) in
  Cmd.group (Cmd.info "cashflow" ~doc:"A double-entry household cashflow ledger")
    [ cmd "serve" "Run the HTTP service (MCP, web API and UI)" serve; cmd "migrate" "Apply SQLite migrations" migrate;
      cmd "verify" "Recompute every balance from its transfers" verify; cmd "void-expired" "Void pending transfers past their expiry date" void_expired;
      Cmd.v
        (Cmd.info "create-session" ~doc:"Development only: sign a user in and print a session token")
        Term.(
          const (fun subject admin -> exit (create_session subject admin))
          $ Arg.(required & pos 0 (some string) None & info [] ~docv:"SUBJECT")
          $ Arg.(value & flag & info [ "admin" ] ~doc:"Make the user an admin")) ]

let () =
  Logging.setup ();
  exit (Cmdliner.Cmd.eval command)
