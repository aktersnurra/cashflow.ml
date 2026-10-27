(* Talking to the server: the session (with its CSRF token) and the commands of lib/commands.ml,
   all through /api. Results are [(json, message) result]; a message is already readable. *)
open! Core
open! Async_kernel

type json = Yojson.Safe.t

let csrf_token = ref ""

(* Every completed response is [Ok (code, body)]; [Error] is only a request that never got one.
   Async_js.Http turns non-2xx into an error and drops the body, so it can't tell the two apart. *)
let http ~meth ~url ?body () =
  let open Js_of_ocaml in
  let req = XmlHttpRequest.create () in
  let result = Ivar.create () in
  let settle value = Ivar.fill_if_empty result value in
  let network message _ =
    settle (Error (`Network message));
    Js._true
  in
  req##_open (Js.string (match meth with `Get -> "GET" | `Post -> "POST")) (Js.string url) Js._true;
  req##setRequestHeader (Js.string "x-csrf-token") (Js.string !csrf_token);
  req##setRequestHeader (Js.string "content-type") (Js.string "application/json");
  req##.onerror := Dom.handler (network "Network error");
  req##.onabort := Dom.handler (network "Request aborted");
  req##.ontimeout := Dom.handler (network "Timeout");
  req##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        if Poly.equal req##.readyState XmlHttpRequest.DONE && req##.status <> 0 then
          settle (Ok (req##.status, Js.Opt.case req##.responseText (fun () -> "") Js.to_string)));
  req##send (Js.Opt.option (Option.map body ~f:Js.string));
  Ivar.read result

let parse body = try Some (Yojson.Safe.from_string body) with Yojson.Json_error _ -> None

let error_message body =
  match parse body with
  | Some (`Assoc fields) -> (match List.Assoc.find fields ~equal:String.equal "error" with Some (`String m) -> m | _ -> body)
  | _ -> body

type session = Signed_in of json | Signed_out | Unreachable of string

let accept_session json =
  match json with
  | `Assoc fields -> (match List.Assoc.find fields ~equal:String.equal "csrf_token" with Some (`String t) -> csrf_token := t | _ -> ())
  | _ -> ()

let session ?(publish_csrf = true) () =
  match%map http ~meth:`Get ~url:"/api/session" () with
  | Ok (200, body) -> (
      match parse body with
      | Some json ->
          if publish_csrf then accept_session json;
          Signed_in json
      | None -> Unreachable "the server sent something unexpected")
  | Ok (401, _) -> Signed_out
  | Ok (_, body) -> Unreachable (error_message body)
  | Error (`Network m) -> Unreachable m

let command name (args : json) =
  match%map http ~meth:`Post ~url:("/api/commands/" ^ name) ~body:(Yojson.Safe.to_string args) () with
  | Ok (200, body) -> (match parse body with Some json -> Ok json | None -> Error "the server sent something unexpected")
  | Ok (_, body) -> Error (error_message body)
  | Error (`Network m) -> Error m

(* Logo editing needs machine-readable failure codes; other commands retain readable errors. *)
let command_with_code name (args : json) =
  match%map http ~meth:`Post ~url:("/api/commands/" ^ name) ~body:(Yojson.Safe.to_string args) () with
  | Ok (200, body) -> (match parse body with Some json -> Ok json | None -> Error "unreachable_site")
  | Ok (_, body) ->
      (match parse body with
       | Some (`Assoc fields) -> (match List.Assoc.find fields ~equal:String.equal "code" with Some (`String code) -> Error code | _ -> Error "unreachable_site")
       | _ -> Error "unreachable_site")
  | Error (`Network _) -> Error "unreachable_site"

let logo_command_effect = Bonsai_web.Effect.of_deferred_fun (fun (name, args) -> command_with_code name args)
let run_with_code name args = logo_command_effect (name, args)

let command_effect = Bonsai_web.Effect.of_deferred_fun (fun (name, args) -> command name args)
let run name args = command_effect (name, args)

let sign_out () =
  match%map http ~meth:`Post ~url:"/auth/logout" ~body:"{}" () with
  | Ok (200, _) -> true
  | _ -> false

(* Idempotency keys for writes made from the UI: random, so every click is one change, and a
   retry of the same request (the same key) is safe. *)
let fresh_key prefix =
  let random = Random.State.make_self_init () in
  prefix ^ ":" ^ String.init 20 ~f:(fun _ -> "abcdefghijklmnopqrstuvwxyz0123456789".[Random.State.int random 36])

(* Advisor shares session CSRF, not the legacy generic command dispatcher. *)
let advisor_csrf_token () = !csrf_token
