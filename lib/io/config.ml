(* Configuration as code: a typed S-expression file (see cashflow.example.sexp), read into
   [File.t] with derived converters. Unknown or misspelt fields are errors, missing optional ones
   take their defaults, and secrets can live outside the file. [t] is the resolved form the server
   uses. *)
let default_openrouter_provider_only = [ "google-vertex"; "amazon-bedrock"; "azure" ]

type t = {
  database_path : string;
  listen_address : string;
  public_base_url : string;
  oidc_issuer : string;
  oidc_audience : string;  (** the MCP resource: the audience bearer tokens must carry *)
  web_client_id : string option;  (** the web UI's OIDC client; without it, only MCP is served *)
  web_client_secret : string option;
  static_dir : string;
  openrouter_api_key : string option;
  openrouter_model : string;  (** categorising, pairing and summaries *)
  advisor_model : string;  (** the advisor; the same as [openrouter_model] unless set apart *)
  openrouter_provider_only : string list;
  advisor_limits : Advisor_provider.generation_limits option;
  log_level : Logs.level option;  (** [None] silences logging *)
}

let advisor_available config =
  Option.fold ~none:false ~some:(fun key -> String.trim key <> "") config.openrouter_api_key
  && Option.is_some config.advisor_limits

module File = struct
  open Sexplib0.Sexp_conv

  (* A secret written inline, read from a file (trailing newline trimmed), or taken from an
     environment variable, so the config file itself can be kept in version control. *)
  type secret = Inline of string | File of string | Env of string [@@deriving sexp]

  type oidc = {
    issuer : string;
    audience : string;  (** what MCP bearer tokens must carry: usually the public URL *)
    admin_group : string; [@default ""]  (** no longer used; read so that older config files still load *)
  }
  [@@deriving sexp]

  type web = { client_id : string; client_secret : secret } [@@deriving sexp]
  type advisor_limits = {
    context_tokens : int;
    completion_tokens : int;
  }
  [@@deriving sexp]

  type openrouter = {
    api_key : secret;
    advisor_limits : advisor_limits option; [@sexp.option]
    model : string; [@default "anthropic/claude-haiku-5.5"]
    advisor_model : string option; [@sexp.option]
    provider_only : string list; [@default default_openrouter_provider_only]
  }
  [@@deriving sexp]

  type log_level = Quiet | Error | Warning | Info | Debug [@@deriving sexp]

  type t = {
    database : string;
    listen : string;  (** host:port of the private listener behind the reverse proxy *)
    public_url : string;
    oidc : oidc;
    web : web option; [@sexp.option]  (** the web UI's OIDC client; without it only MCP is served *)
    static_dir : string; [@default "/usr/local/share/cashflow/web"]
    openrouter : openrouter option; [@sexp.option]
    log_level : log_level; [@default Info]
  }
  [@@deriving sexp]
end

let ( let* ) = Result.bind

let read_secret = function
  | File.Inline value -> Ok value
  | Env name -> (
      match Sys.getenv_opt name with
      | Some value when String.trim value <> "" -> Ok (String.trim value)
      | _ -> Error (Printf.sprintf "environment variable %s is not set" name))
  | File path -> (
      match In_channel.with_open_bin path In_channel.input_all with
      | value when String.trim value <> "" -> Ok (String.trim value)
      | _ -> Error (Printf.sprintf "secret file %s is empty" path)
      | exception Sys_error reason -> Error (Printf.sprintf "can't read secret file: %s" reason))

let check condition message = if condition then Ok () else Error message

let resolve (file : File.t) =
  let* () = check (match String.rindex_opt file.listen ':' with Some i -> i > 0 && i < String.length file.listen - 1 | None -> false) "listen must be host:port" in
  let* () = check (String.starts_with ~prefix:"https://" file.oidc.issuer) "oidc.issuer must be an https URL" in
  let* () =
    check (String.starts_with ~prefix:"https://" file.public_url || String.starts_with ~prefix:"http://127.0.0.1" file.public_url || String.starts_with ~prefix:"http://localhost" file.public_url)
      "public_url must be https (or http on localhost, for development)"
  in
  let* () =
    match file.openrouter with
    | Some o -> check (o.provider_only <> []) "openrouter.provider_only must not be empty"
    | None -> Ok ()
  in
  let* advisor_limits =
    match file.openrouter with
    | None -> Ok None
    | Some o ->
        let supported = Advisor_provider.supported_limits ~model:(Option.value o.advisor_model ~default:o.model) ~provider_only:o.provider_only in
        match o.advisor_limits, supported with
        | None, limits -> Ok limits
        | Some _, None -> Error "openrouter.advisor_limits requires a supported Advisor model (openrouter.advisor_model, else openrouter.model): anthropic/claude-sonnet-5.5 (or claude-sonnet-4.6) with provider_only (amazon-bedrock)"
        | Some l, Some policy ->
            let* () = check
              (l.context_tokens > 0 && l.completion_tokens > 0 && l.completion_tokens <= l.context_tokens
               && l.context_tokens <= policy.context_tokens && l.completion_tokens <= policy.completion_tokens)
              "openrouter.advisor_limits requires positive context/completion, completion <= context, context <= 200000 and completion <= 8192" in
            Ok (Some { Advisor_provider.context_tokens = l.context_tokens; completion_tokens = l.completion_tokens })
  in
  let* web_client_secret = match file.web with Some web -> Result.map Option.some (read_secret web.client_secret) | None -> Ok None in
  let* openrouter_api_key = match file.openrouter with Some o -> Result.map Option.some (read_secret o.api_key) | None -> Ok None in
  let public_base_url = if String.ends_with ~suffix:"/" file.public_url then String.sub file.public_url 0 (String.length file.public_url - 1) else file.public_url in
  Ok
    { database_path = file.database; listen_address = file.listen; public_base_url; oidc_issuer = file.oidc.issuer; oidc_audience = file.oidc.audience;
      web_client_id = Option.map (fun (w : File.web) -> w.client_id) file.web; web_client_secret;
      static_dir = file.static_dir; openrouter_api_key;
      openrouter_model = Option.fold ~none:"anthropic/claude-haiku-5.5" ~some:(fun (o : File.openrouter) -> o.model) file.openrouter;
      advisor_model = Option.fold ~none:"anthropic/claude-haiku-5.5" ~some:(fun (o : File.openrouter) -> Option.value o.advisor_model ~default:o.model) file.openrouter;
      openrouter_provider_only = Option.fold ~none:default_openrouter_provider_only ~some:(fun (o : File.openrouter) -> o.provider_only) file.openrouter;
      advisor_limits;
      log_level = (match file.log_level with Quiet -> None | Error -> Some Logs.Error | Warning -> Some Warning | Info -> Some Info | Debug -> Some Debug) }

(* "lib/config.ml.File.t_of_sexp: extra fields: statc_dir" reads as "unknown field statc_dir". *)
let describe exn =
  let message = match exn with Sexplib0.Sexp.Of_sexp_error (Failure m, _) | Failure m -> m | e -> Printexc.to_string e in
  let message = match String.index_opt message ':' with Some i when String.contains (String.sub message 0 i) '.' -> String.trim (String.sub message (i + 1) (String.length message - i - 1)) | _ -> message in
  let prefix = "extra fields: " in
  if String.starts_with ~prefix message then
    let fields = String.split_on_char ' ' (String.sub message (String.length prefix) (String.length message - String.length prefix)) in
    (`Unknown fields, "unknown field " ^ String.concat ", " fields)
  else (`Other, message)

(* The position of the field named [name] inside a record, if it is there. *)
let rec find_field name (sexp : Sexplib.Sexp.Annotated.t) =
  match sexp with
  | List (range, Atom (_, Sexplib.Sexp.Atom atom) :: _, _) when atom = name -> Some range
  | List (_, children, _) -> List.find_map (find_field name) children
  | Atom _ -> None

let parse ?(file_name = "config") text =
  match Sexplib.Sexp.of_string_conv text File.t_of_sexp with
  | `Result file -> resolve file
  | `Error (exn, sexp) ->
      let kind, message = describe exn in
      let range =
        match kind with
        | `Unknown (field :: _) -> Option.value (find_field field sexp) ~default:(Sexplib.Sexp.Annotated.get_range sexp)
        | `Unknown [] | `Other -> Sexplib.Sexp.Annotated.get_range sexp
      in
      Error (Printf.sprintf "%s:%d:%d: %s" file_name range.start_pos.line (range.start_pos.col + 1) message)
  | exception Failure reason -> Error (Printf.sprintf "%s: not a valid S-expression: %s" file_name reason)
  | exception Sexplib.Sexp.Parse_error e -> Error (Printf.sprintf "%s: not a valid S-expression: %s" file_name e.err_msg)

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> parse ~file_name:path text
  | exception Sys_error reason -> Error reason

let default_path = "/usr/local/etc/cashflow.sexp"

(* copied from kcal: lib/config.ml *)
let protected_resource_metadata_url ~audience =
  let uri = Uri.of_string audience in
  let path = match Uri.path uri with "" | "/" -> "" | path -> path in
  Uri.to_string (Uri.with_path uri ("/.well-known/oauth-protected-resource" ^ path))
