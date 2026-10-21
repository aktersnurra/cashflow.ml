(* Configuration from the environment; see .env.example. *)
type t = {
  database_path : string;
  listen_address : string;
  public_base_url : string;
  oidc_issuer : string;
  oidc_audience : string;  (** the MCP resource: the audience bearer tokens must carry *)
  web_client_id : string option;  (** the web UI's OIDC client; without it, only MCP is served *)
  web_client_secret : string option;
  admin_group : string;
  static_dir : string;
  openrouter_api_key : string option;
  openrouter_model : string;
}

let optional name = match Sys.getenv_opt name with Some value when String.trim value <> "" -> Some (String.trim value) | _ -> None
let required name = Option.to_result ~none:("missing required configuration " ^ name) (optional name)

let load_from_environment () =
  let ( let* ) = Result.bind in
  let* database_path = required "CASHFLOW_DATABASE_PATH" in
  let* listen_address = required "CASHFLOW_LISTEN_ADDRESS" in
  let* public_base_url = required "CASHFLOW_PUBLIC_BASE_URL" in
  let* oidc_issuer = required "CASHFLOW_OIDC_ISSUER" in
  let* oidc_audience = required "CASHFLOW_OIDC_AUDIENCE" in
  let web_client_id = optional "CASHFLOW_WEB_CLIENT_ID" and web_client_secret = optional "CASHFLOW_WEB_CLIENT_SECRET" in
  if Option.is_some web_client_id <> Option.is_some web_client_secret then
    Error "set both CASHFLOW_WEB_CLIENT_ID and CASHFLOW_WEB_CLIENT_SECRET, or neither"
  else
    Ok
      { database_path; listen_address; public_base_url = (if String.ends_with ~suffix:"/" public_base_url then String.sub public_base_url 0 (String.length public_base_url - 1) else public_base_url);
        oidc_issuer; oidc_audience; web_client_id; web_client_secret;
        admin_group = Option.value (optional "CASHFLOW_ADMIN_GROUP") ~default:"cashflow-admin";
        static_dir = Option.value (optional "CASHFLOW_STATIC_DIR") ~default:"/usr/local/share/cashflow/web";
        openrouter_api_key = optional "CASHFLOW_OPENROUTER_API_KEY";
        openrouter_model = Option.value (optional "CASHFLOW_OPENROUTER_MODEL") ~default:"anthropic/claude-haiku-5.5" }

(* copied from kcal: lib/config.ml *)
let protected_resource_metadata_url ~audience =
  let uri = Uri.of_string audience in
  let path = match Uri.path uri with "" | "/" -> "" | path -> path in
  Uri.to_string (Uri.with_path uri ("/.well-known/oauth-protected-resource" ^ path))
