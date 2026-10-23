module Config = Cashflow_io.Config
module Db_guard = Cashflow_io.Db_guard
module Http_server = Cashflow_io.Http_server
module Logo_fetch = Cashflow_io.Logo_fetch
module Oidc = Cashflow_io.Oidc
module Schema = Cashflow_io.Schema
module Service = Cashflow_io.Service
module Sql = Cashflow_io.Sql

(* Browser-only fixture server. The same HTTP/API/database stack runs as production; only the
   outbound merchant network is replaced, and it fails closed outside the fixture hostname. *)
let () =
  let config = match Config.load (Sys.getenv "CASHFLOW_CONFIG") with Ok c -> c | Error e -> failwith e in
  let db = Sql.open_db config.Config.database_path in
  Schema.migrate db;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Sql.set_guard db (Db_guard.make ());
  let fixture_png =
    let channel = open_in_bin "test/fixtures/logo_64x48.png" in
    Fun.protect ~finally:(fun () -> close_in channel) (fun () -> really_input_string channel (in_channel_length channel)) in
  let logo_network = Logo_fetch.{ with_request = (fun f -> f ());
    resolve = (fun ~host -> if host = "logo-fixture.com" then Ok ["8.8.8.8"] else Error Unreachable_site);
    get = (fun ~uri ~address ~max_bytes:_ ->
      if Uri.host uri = Some "logo-fixture.com" && address = "8.8.8.8" && Uri.path uri = "/icon.png"
      then Ok {status=200; headers=[]; body=fixture_png}
      else Error Unreachable_site) } in
  let service = Service.make ~logo_network ~schedule:(fun work -> Eio.Fiber.fork ~sw work) db in
  let client = Oidc.https_client env in
  let verify_bearer = Oidc.make ~issuer:config.oidc_issuer ~audience:config.oidc_audience ~client () in
  Http_server.run env {service; verify_bearer; web_auth=None; config; now=Ptime_clock.now}
