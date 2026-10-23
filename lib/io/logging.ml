(* copied from kcal: lib/logging.ml *)
let pp_level ppf level =
  Format.pp_print_string ppf
    (match level with
    | Logs.App -> "APP"
    | Logs.Error -> "ERROR"
    | Logs.Warning -> "WARN"
    | Logs.Info -> "INFO"
    | Logs.Debug -> "DEBUG")

let timestamp () =
  match Ptime.of_float_s (Unix.gettimeofday ()) with
  | Some time -> Ptime.to_rfc3339 ~tz_offset_s:0 time
  | None -> "-"

(* A custom reporter (rather than Logs_fmt) so every line carries a UTC
   timestamp and the emitting source name, which the default reporters omit. *)
let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf (fun ?header ?tags fmt ->
        ignore tags;
        let header = match header with Some header -> header | None -> Format.asprintf "%a" pp_level level in
        Format.kfprintf k Format.err_formatter
          ("%s %s [%s] @[" ^^ fmt ^^ "@]@.")
          (timestamp ()) header (Logs.Src.name src))
  in
  { Logs.report }

let level_of_env ~default =
  match Sys.getenv_opt "CASHFLOW_LOG_LEVEL" with
  | None -> default
  | Some value -> ( match Logs.level_of_string value with Ok level -> level | Error (`Msg _) -> default)

(* tls.tracing dumps plaintext application data (including recoverable hex).
   Application-wide Debug must never enable it, including during concurrent
   reconfiguration. Set it separately, never transiently via set_level ~all:true;
   retain its non-payload warnings/errors and all other sources' diagnostics. *)
let set_level level =
  let tls_level = match level with Some Logs.Debug -> Some Logs.Info | level -> level in
  Logs.Src.set_level Tls.Core.src tls_level;
  Logs.set_level ~all:false level;
  List.iter (fun source ->
    if not (Logs.Src.equal source Tls.Core.src) then Logs.Src.set_level source level)
    (Logs.Src.list ())

let setup () =
  Logs.set_reporter reporter;
  set_level (level_of_env ~default:(Some Logs.Info))

(* The level from the configuration file; CASHFLOW_LOG_LEVEL still wins, for a one-off debug run. *)
let configure level = set_level (level_of_env ~default:level)
