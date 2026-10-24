module Config = Cashflow_io.Config
module Db_guard = Cashflow_io.Db_guard
module Http_server = Cashflow_io.Http_server
module Https = Cashflow_io.Https
module Id_generator = Cashflow_io.Id_generator
module Json = Cashflow_io.Json
module Ledger = Cashflow_io.Ledger
module Llm = Cashflow_io.Llm
module Logging = Cashflow_io.Logging
module Logo_fetch = Cashflow_io.Logo_fetch
module Oidc = Cashflow_io.Oidc
module Schema = Cashflow_io.Schema
module Service = Cashflow_io.Service
module Sql = Cashflow_io.Sql
module Store_sqlite = Cashflow_io.Store_sqlite
module Web_auth = Cashflow_io.Web_auth
module Access = Cashflow_core.Access
module Account_id = Cashflow_core.Account_id
module Csv = Cashflow_core.Csv
module Date = Cashflow_core.Date
module Ledger_core = Cashflow_core.Ledger_core
module Statement_format = Cashflow_core.Statement_format
module Time = Cashflow_core.Time
module Xlsx = Cashflow_core.Xlsx

let with_config path f =
  match Config.load path with
  | Error reason -> Logs.err (fun m -> m "invalid configuration: %s" reason); 1
  | Ok config ->
      Logging.configure config.Config.log_level;
      f config

(* Validates the configuration file, secrets included, without touching the database. *)
let check_config path = with_config path (fun config -> Logs.app (fun m -> m "%s is valid; serving %s" path config.public_base_url); 0)

let with_database config f =
  let db = Sql.open_db config.Config.database_path in
  Fun.protect ~finally:(fun () -> Sql.close db) (fun () ->
      match Schema.migrate db with
      | () -> f db
      | exception Sql.Sql_error message -> Logs.err (fun m -> m "migration failed: %s" message); 1)

let migrate path = with_config path (fun config -> with_database config (fun _ -> Logs.app (fun m -> m "schema at version %d" Schema.latest_version); 0))

module Ledger_sqlite = Ledger.Make (Store_sqlite)

(* Recomputes every balance from the transfers; a mismatch means a bug or a damaged database. *)
let verify path =
  with_config path (fun config ->
      with_database config (fun db ->
          match Ledger_sqlite.verify db with
          | [] -> Logs.app (fun m -> m "all balances match their transfers"); 0
          | mismatches ->
              List.iter (fun (m : Ledger_core.mismatch) -> Logs.err (fun l -> l "account %s: stored balances differ from its transfers" (Account_id.to_string m.account_id))) mismatches;
              1))

let env () : Ledger_core.env = { now = Ptime_clock.now; fresh_account_id = Id_generator.account; fresh_transfer_id = Id_generator.transfer }

(* Run daily from cron: voids pending transfers past their expiry date. *)
let void_expired path =
  with_config path (fun config ->
      with_database config (fun db ->
          let today = Time.local_date (Result.get_ok (Time.parse_zone "Europe/Stockholm")) (Ptime_clock.now ()) in
          let voided = Ledger_sqlite.void_expired db (env ()) ~today in
          Logs.app (fun m -> m "voided %d expired pending transfers" (List.length voided));
          0))

let serve path =
  with_config path (fun config ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      with_database config (fun db ->
          (* Request fibers share the connection; they take turns with it. *)
          Sql.set_guard db (Db_guard.make ());
          let assistant =
            Option.map
              (fun api_key ->
                Llm.openrouter ~post:(Https.post env) ~api_key ~model:config.openrouter_model
                  ~provider_only:config.openrouter_provider_only)
              config.openrouter_api_key
          in
          let service = Service.make ?assistant ~logo_network:(Https.logo_network env)
            ~schedule:(fun work -> Eio.Fiber.fork ~sw work) db in
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
let create_session path subject admin =
  if Sys.getenv_opt "CASHFLOW_ALLOW_DEV_SESSIONS" <> Some "1" then (
    Logs.err (fun m -> m "set CASHFLOW_ALLOW_DEV_SESSIONS=1 to create development sessions");
    1)
  else
    with_config path (fun config ->
        with_database config (fun db ->
            let service = Service.make db in
            let access =
              Service.sign_in service
                { issuer = config.oidc_issuer; subject; display_name = Some subject; email = Some (subject ^ "@example.com"); is_admin = Some admin; locale = Sys.getenv_opt "CASHFLOW_DEV_LOCALE" }
            in
            print_endline (Web_auth.create_session db ~now:(Ptime_clock.now ()) (Access.user access));
            0))

(* Reads a statement file the way an import would, without a database: which format it is,
   which account it says it is for, and every row. For checking a bank's export. *)
let read_statement path =
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error reason -> Logs.err (fun m -> m "%s" reason); 1
  | content -> (
      let table = if Xlsx.is_xlsx content then Xlsx.table content else Ok (Csv.parse content) in
      match Result.map (fun t -> (t, Statement_format.detect t)) table with
      | Error reason -> Logs.err (fun m -> m "can't read %s: %s" path reason); 1
      | Ok (_, None) -> Logs.err (fun m -> m "%s: no header row with a date and an amount column" path); 1
      | Ok (table, Some (module F)) -> (
          match F.parse ~scale:2 table with
          | Error reason -> Logs.err (fun m -> m "%s: %s" path reason); 1
          | Ok rows ->
              let money v = Json.decimal ~scale:2 v in
              Printf.printf "format: %s\naccount: %s\nrows: %d\n" F.name
                (Option.fold ~none:"(not stated)" ~some:(fun d -> "****" ^ d) (F.account_hint table)) (List.length rows);
              List.iter
                (fun (r : Statement_format.row) ->
                  Printf.printf "%s  %-30s %12s %12s\n" (Date.to_string r.booking_date) r.description (money r.amount)
                    (Option.fold ~none:"" ~some:money r.balance))
                rows;
              0))

let command =
  let open Cmdliner in
  let config =
    let env = Cmd.Env.info "CASHFLOW_CONFIG" in
    Arg.(value & opt string Config.default_path & info [ "config"; "c" ] ~env ~docv:"FILE" ~doc:"The configuration file, an S-expression; see cashflow.example.sexp.")
  in
  let cmd name doc f = Cmd.v (Cmd.info name ~doc) Term.(const (fun path -> exit (f path)) $ config) in
  Cmd.group (Cmd.info "cashflow" ~doc:"A double-entry household cashflow ledger")
    [ cmd "serve" "Run the HTTP service (MCP, web API and UI)" serve; cmd "migrate" "Apply SQLite migrations" migrate;
      cmd "verify" "Recompute every balance from its transfers" verify; cmd "void-expired" "Void pending transfers past their expiry date" void_expired;
      cmd "check-config" "Validate the configuration file" check_config;
      Cmd.v
        (Cmd.info "read-statement" ~doc:"Read a bank statement file (.csv or .xlsx) as an import would, and print its rows")
        Term.(const (fun path -> exit (read_statement path)) $ Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"));
      Cmd.v
        (Cmd.info "create-session" ~doc:"Development only: sign a user in and print a session token")
        Term.(
          const (fun path subject admin -> exit (create_session path subject admin))
          $ config
          $ Arg.(required & pos 0 (some string) None & info [] ~docv:"SUBJECT")
          $ Arg.(value & flag & info [ "admin" ] ~doc:"Make the user an admin")) ]

let () =
  Logging.setup ();
  exit (Cmdliner.Cmd.eval command)
