open! Core
open! Async_kernel
module W = Cashflow_advisor_protocol.Wire
module C = Cashflow_advisor_protocol.Client_state
open Js_of_ocaml

(* Async_js.Http discards non-2xx status. Keep it at this browser edge so an
   authoritative denial is never mistaken for a lost connection. *)
let request ?body path decode =
  let req = XmlHttpRequest.create () in
  let result = Ivar.create () in
  let settle value =
    if not (Ivar.is_full result) then (
      req##.onreadystatechange := Js.wrap_callback (fun () -> ());
      req##.onerror := Dom.handler (fun _ -> Js._true);
      req##.onabort := Dom.handler (fun _ -> Js._true);
      req##.ontimeout := Dom.handler (fun _ -> Js._true);
      Ivar.fill_exn result value)
  in
  req##_open (Js.string (if Option.is_some body then "POST" else "GET")) (Js.string ("/api/advisor/" ^ path)) Js._true;
  Js.Unsafe.set req "timeout" 15000;
  req##setRequestHeader (Js.string "content-type") (Js.string "application/json");
  req##setRequestHeader (Js.string "x-csrf-token") (Js.string (Api.advisor_csrf_token ()));
  let transport _ =
    settle (Error C.Transport);
    Js._true
  in
  req##.onerror := Dom.handler transport;
  req##.onabort := Dom.handler transport;
  req##.ontimeout := Dom.handler transport;
  req##.onreadystatechange :=
    Js.wrap_callback (fun () ->
        if Poly.equal req##.readyState XmlHttpRequest.DONE then
          let code = req##.status in
          if code = 401 then settle (Error C.Session_lost)
          else if List.mem [ 403; 404 ] code ~equal:Int.equal then settle (Error C.Access_lost)
          else if code = 0 then settle (Error C.Transport)
          else if code >= 200 && code < 300 then
            let content = Js.Opt.case req##.responseText (fun () -> "") Js.to_string in
            if String.length content > 4 * 1024 * 1024 then settle (Error C.Rejected)
            else settle (match decode content with Some x -> Ok x | None -> Error C.Rejected)
          else settle (Error C.Rejected));
  req##send (Js.Opt.option (Option.map body ~f:Js.string));
  Ivar.read result

let wire f s = Result.ok (f s)
let json f s = try f (Yojson.Safe.from_string s) with _ -> None
let field name = function `Assoc xs -> List.Assoc.find xs name ~equal:String.equal | _ -> None
let str = function Some (`String s) -> Some s | _ -> None
let bool = function Some (`Bool b) -> Some b | _ -> None
let submit p = request ~body:(W.encode_submit p) "turns" (wire W.decode_accepted)

let retry (p : W.retry) =
  request ~body:(W.encode_retry p) ("turns/" ^ Js_of_ocaml.Url.urlencode p.turn_id ^ "/retry") (wire W.decode_accepted)

let snapshot id = request ("conversations/" ^ Js_of_ocaml.Url.urlencode id) (wire W.decode_snapshot)

let status () =
  request "status"
    (json (fun j ->
         match (bool (field "available" j), bool (field "consent" j)) with
         | Some available, Some consent -> Some { C.available; consent }
         | _ -> None))

let history ~limit ~offset =
  request
    (sprintf "conversations?limit=%d&offset=%d" limit offset)
    (json (fun j ->
         match field "conversations" j with
         | Some (`List xs) ->
             let xs =
               List.map xs ~f:(fun j ->
                   match (str (field "conversation_id" j), str (field "version" j), str (field "updated_at" j)) with
                   | Some conversation_id, Some version, Some updated_at ->
                       Some { C.conversation_id; version; updated_at }
                   | _ -> None)
             in
             Option.all xs
         | _ -> None))

let cancel id =
  request ~body:"{}"
    ("runs/" ^ Js_of_ocaml.Url.urlencode id ^ "/cancel")
    (json (fun j ->
         match str (field "state" j) with
         | Some (("queued" | "running" | "cancel_requested" | "completed" | "cancelled" | "failed") as s) -> Some s
         | _ -> None))

let delete id =
  request ~body:"{}"
    ("conversations/" ^ Js_of_ocaml.Url.urlencode id ^ "/delete")
    (json (fun j -> match str (field "status" j) with Some "deleted" -> Some () | _ -> None))

let consent ~enabled ~household_id =
  let fields =
    [ ("enabled", `Bool enabled) ] @ match household_id with None -> [] | Some id -> [ ("household_id", `String id) ]
  in
  request
    ~body:(Yojson.Safe.to_string (`Assoc fields))
    "consent"
    (json (fun j -> Option.map (bool (field "enabled" j)) ~f:(fun _ -> ())))
