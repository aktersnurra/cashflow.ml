module A = Cashflow_core.Advisor
module W = Cashflow_advisor_protocol.Wire
module Sse = Cashflow_advisor_protocol.Sse
module X = Advisor_access
module R = Advisor_runtime
module Household_id = Cashflow_core.Household_id

type meth = GET | POST | HEAD | OTHER
type request = { meth : meth; target : string; headers : (string * string) list; body : string }

type t = {
  service : Service.t;
  runtime : R.t;
  origin : string * string * int;
  available : bool;
  sleep_s : float -> unit;
}

type subscription = { owner : t; principal : X.principal; run : A.Run_id.t; subscription : R.subscription }
type response = Buffered of int * (string * string) list * string | Stream of subscription

let ( let* ) = Result.bind

let origin uri =
  let scheme = Option.map String.lowercase_ascii (Uri.scheme uri)
  and host = Option.map String.lowercase_ascii (Uri.host uri) in
  match (scheme, host) with
  | Some (("http" | "https") as scheme), Some host when host <> "" && Uri.userinfo uri = None ->
      let port = Option.value (Uri.port uri) ~default:(if scheme = "https" then 443 else 80) in
      if port > 0 && port <= 65535 then Some (scheme, host, port) else None
  | _ -> None

let create ~service ~runtime ~public_origin ~available ~sleep_s =
  let origin =
    match origin (Uri.of_string public_origin) with Some x -> x | None -> invalid_arg "Advisor public origin"
  in
  { service; runtime; origin; available; sleep_s }

let headers =
  [
    ("content-type", "application/json");
    ("cache-control", "private, no-store");
    ("x-content-type-options", "nosniff");
    ("referrer-policy", "same-origin");
    ("x-frame-options", "DENY");
    ("content-security-policy", "default-src 'self'; frame-ancestors 'none'; base-uri 'none'");
  ]

let json status value = Buffered (status, headers, Yojson.Safe.to_string value)
let code status name = json status (`Assoc [ ("error", `String name) ])

let error (e : A.error) =
  let status, name =
    match e.code with
    | Access_denied -> (403, "Access_denied")
    | Advisor_disabled -> (403, "Advisor_disabled")
    | Consent_required -> (403, "Consent_required")
    | Scope_mismatch -> (403, "Scope_mismatch")
    | Not_found -> (404, "Not_found")
    | Run_busy -> (409, "Run_busy")
    | Version_conflict -> (409, "Version_conflict")
    | Idempotency_conflict -> (409, "Idempotency_conflict")
    | Capacity_exceeded -> (503, "Capacity_exceeded")
    | Provider_unavailable -> (503, "Provider_unavailable")
    | Storage_unavailable -> (503, "Storage_unavailable")
    | _ -> (422, "Invalid_request")
  in
  code status name

let invalid = { A.code = A.Invalid_request; context = A.No_context }
let values req key = List.filter_map (fun (k, v) -> if String.lowercase_ascii k = key then Some v else None) req.headers
let header req key = match values req key with [ v ] -> Some v | _ -> None

let same_origin t value =
  try
    let u = Uri.of_string value in
    value <> "null"
    && String.lowercase_ascii value = String.lowercase_ascii (Uri.to_string u)
    && Uri.path u = ""
    && Uri.query u = []
    && Uri.fragment u = None
    && origin u = Some t.origin
  with _ -> false

let origin_allowed t req ~required =
  let sites = values req "sec-fetch-site" in
  List.length sites <= 1
  && (not (List.exists (fun site -> String.lowercase_ascii (String.trim site) = "cross-site") sites))
  && match values req "origin" with [] -> not required | [ v ] -> same_origin t v | _ -> false

let authenticate t req =
  match Web_auth.cookie_value req.headers Web_auth.session_cookie with
  | None -> Error (code 401 "Authentication_required")
  | Some token -> (
      match X.authenticate t.service ~now:(t.service.now ()) ~token with
      | Ok p -> Ok p
      | Error { A.code = A.Storage_unavailable; _ } -> Error (code 503 "Storage_unavailable")
      | Error _ -> Error (code 401 "Authentication_required"))

let mutation t req p =
  if not (origin_allowed t req ~required:true) then Error (code 403 "Access_denied")
  else
    let* reference = Result.map_error error (X.session t.service p) in
    let identity =
      Sql.read_snapshot t.service.db (fun () -> Web_auth.inspect_session t.service ~now:(t.service.now ()) reference)
    in
    match identity with
    | None -> Error (code 401 "Authentication_required")
    | Some i when header req "x-csrf-token" <> Some i.csrf_token -> Error (code 403 "Access_denied")
    | Some _ -> (
        let is_json =
          match header req "content-type" with
          | Some v -> String.trim (List.hd (String.split_on_char ';' (String.lowercase_ascii v))) = "application/json"
          | None -> false
        in
        if not is_json then Error (code 415 "Send_JSON")
        else try Ok (Yojson.Safe.from_string req.body) with Yojson.Json_error _ -> Error (code 400 "Malformed_JSON"))

let object_fields allowed = function
  | `Assoc fields
    when List.for_all (fun (k, _) -> List.mem k allowed) fields
         && List.length fields = List.length (List.sort_uniq String.compare (List.map fst fields)) ->
      Ok fields
  | _ -> Error invalid

let command_fields allowed value =
  let* envelope = object_fields [ "version"; "data" ] value in
  match List.assoc_opt "data" envelope with
  | None -> Error invalid
  | Some data ->
      let* fields = object_fields allowed data in
      let check name keys =
        match List.assoc_opt name fields with
        | None -> Ok ()
        | Some x -> Result.map (fun _ -> ()) (object_fields keys x)
      in
      let* () = check "scope" [ "type"; "value" ] in
      check "currency" [ "code"; "scale" ]

let unit_body json =
  let* _ = object_fields [] json in
  Ok ()

let state_json state =
  let name =
    match state with
    | A.Queued -> "queued"
    | Running _ -> "running"
    | Cancel_requested -> "cancel_requested"
    | Completed -> "completed"
    | Cancelled -> "cancelled"
    | Failed _ -> "failed"
  in
  json 200 (`Assoc [ ("state", `String name) ])

let cursor value =
  try
    let n = Int64.of_string value in
    if n >= 0L && Int64.to_string n = value then Ok n else Error invalid
  with _ -> Error invalid

let query_int query name default max =
  match List.assoc_opt name query with
  | None -> Ok default
  | Some [ v ] ->
      let* n = cursor v in
      if n <= Int64.of_int max then Ok (Int64.to_int n) else Error invalid
  | _ -> Error invalid

let run_id s = match A.Run_id.of_string s with Some x -> Ok x | None -> Error invalid
let conversation_id s = match A.Conversation_id.of_string s with Some x -> Ok x | None -> Error invalid

let read t p f =
  Sql.read_snapshot t.service.db (fun () ->
      let* user = X.user t.service p in
      f user (Advisor_store.create ~clock:t.service.now t.service.db))

let route t req p path query parsed =
  let result f = match f () with Ok r -> r | Error e -> error e in
  match (req.meth, path) with
  | GET, [ "status" ] ->
      result (fun () ->
          read t p (fun user store ->
              let* consent = Advisor_store.consent store (User user.id) in
              Ok (json 200 (`Assoc [ ("available", `Bool t.available); ("consent", `Bool consent) ]))))
  | POST, [ "consent" ] ->
      result (fun () ->
          let* fields = object_fields [ "enabled"; "household_id" ] (Option.get parsed) in
          match List.assoc_opt "enabled" fields with
          | Some (`Bool enabled) ->
              let* ids =
                match List.assoc_opt "household_id" fields with
                | None -> X.set_user_consent t.service p ~enabled
                | Some (`String id) -> (
                    match Household_id.of_string id with
                    | Some id -> X.set_household_consent t.service p id ~enabled
                    | None -> Error invalid)
                | _ -> Error invalid
              in
              R.signal_cancel t.runtime ids;
              Ok (json 200 (`Assoc [ ("enabled", `Bool enabled) ]))
          | _ -> Error invalid)
  | POST, [ "turns" ] ->
      result (fun () ->
          let* () =
            command_fields
              [
                "conversation_id";
                "scope";
                "currency";
                "locale";
                "user_text";
                "idempotency_key";
                "expected_version";
                "source_finding_key";
              ]
              (Option.get parsed)
          in
          let* wire = Result.map_error (fun _ -> invalid) (W.decode_submit req.body) in
          let* command = Advisor_codec.submit wire in
          let* accepted = R.submit ~available:t.available t.runtime ~principal:p command in
          Ok (Buffered (202, headers, W.encode_accepted (Advisor_codec.accepted accepted))))
  | POST, [ "turns"; id; "retry" ] ->
      result (fun () ->
          let* () = command_fields [ "turn_id"; "expected_version"; "idempotency_key" ] (Option.get parsed) in
          let* wire = Result.map_error (fun _ -> invalid) (W.decode_retry req.body) in
          if wire.turn_id <> id then Error invalid
          else
            let* command = Advisor_codec.retry wire in
            let* accepted = R.retry ~available:t.available t.runtime ~principal:p command in
            Ok (Buffered (202, headers, W.encode_accepted (Advisor_codec.accepted accepted))))
  | GET, [ "conversations" ] ->
      result (fun () ->
          let* limit = query_int query "limit" 20 100 in
          let* offset = query_int query "offset" 0 max_int in
          if limit = 0 then Error invalid
          else
            read t p (fun user store ->
                let* items = Advisor_store.list store ~owner:user.id ~limit ~offset () in
                let items =
                  List.filter
                    (fun (m : Advisor_store.metadata) ->
                      Result.is_ok (X.authorize_history t.service p m.conversation_id))
                    items
                in
                Ok
                  (json 200
                     (`Assoc
                        [
                          ( "conversations",
                            `List
                              (List.map
                                 (fun (m : Advisor_store.metadata) ->
                                   `Assoc
                                     [
                                       ("conversation_id", `String (A.Conversation_id.to_string m.conversation_id));
                                       ("version", `String (Int64.to_string m.version));
                                       ("updated_at", `String (Ptime.to_rfc3339 m.updated_at));
                                     ])
                                 items) );
                        ]))))
  | GET, [ "conversations"; id ] ->
      result (fun () ->
          let* id = conversation_id id in
          read t p (fun user store ->
              let* () = X.authorize_history t.service p id in
              let* snapshot = Advisor_store.snapshot store ~owner:user.id id in
              Ok (Buffered (200, headers, W.encode_snapshot (Advisor_codec.snapshot snapshot)))))
  | POST, [ "conversations"; id; "delete" ] ->
      result (fun () ->
          let* () = unit_body (Option.get parsed) in
          let* id = conversation_id id in
          let* () = R.delete t.runtime ~principal:p id in
          Ok (json 200 (`Assoc [ ("status", `String "deleted") ])))
  | POST, [ "runs"; id; "cancel" ] ->
      result (fun () ->
          let* () = unit_body (Option.get parsed) in
          let* id = run_id id in
          let* state = R.cancel t.runtime ~principal:p id in
          Ok (state_json state))
  | GET, [ "runs"; id; "events" ] ->
      result (fun () ->
          let* run = run_id id in
          let* after =
            match (values req "last-event-id", List.assoc_opt "after" query) with
            | [ v ], _ -> cursor v
            | [], None -> Ok 0L
            | [], Some [ v ] -> cursor v
            | _ -> Error invalid
          in
          let* subscription = R.subscribe t.runtime ~principal:p ~run ~after in
          Ok (Stream { owner = t; principal = p; run; subscription }))
  | ( (GET | POST | HEAD | OTHER),
      ( [ "status" ]
      | [ "consent" ]
      | [ "turns" ]
      | [ "conversations" ]
      | [ "conversations"; _ ]
      | [ "turns"; _; "retry" ]
      | [ "conversations"; _; "delete" ]
      | [ "runs"; _; "cancel" ]
      | [ "runs"; _; "events" ] ) ) ->
      code 405 "Method_not_allowed"
  | _ -> code 404 "Not_found"

let handle t req =
  let uri = Uri.of_string req.target in
  let path = Uri.path uri in
  if not (path = "/api/advisor" || String.starts_with ~prefix:"/api/advisor/" path) then None
  else
    Some
      (try
         match authenticate t req with
         | Error r -> r
         | Ok p -> (
             if not (origin_allowed t req ~required:false) then code 403 "Access_denied"
             else
               let parts = String.split_on_char '/' path |> List.filter (( <> ) "") |> List.tl |> List.tl in
               match req.meth with
               | POST -> (
                   match mutation t req p with
                   | Error r -> r
                   | Ok parsed -> route t req p parts (Uri.query uri) (Some parsed))
               | _ -> route t req p parts (Uri.query uri) None)
       with Sql.Sql_error _ -> code 503 "Storage_unavailable")

let logout_impl t req =
  match authenticate t req with
  | Error r -> r
  | Ok p -> (
      match mutation t req p with
      | Error r -> r
      | Ok parsed -> (
          match unit_body parsed with
          | Error e -> error e
          | Ok () -> (
              match X.session t.service p with
              | Error e -> error e
              | Ok reference ->
                  let ids = X.revoke_session t.service ~now:(t.service.now ()) reference in
                  R.signal_cancel t.runtime ids;
                  Buffered
                    ( 200,
                      ("set-cookie", Web_auth.clear_cookie Web_auth.session_cookie) :: headers,
                      "{\"status\":\"signed_out\"}" ))))

let leave_household_impl t req =
  match authenticate t req with
  | Error r -> r
  | Ok p -> (
      match mutation t req p with
      | Error r -> r
      | Ok parsed -> (
          match unit_body parsed with
          | Error e -> error e
          | Ok () -> (
              match X.leave_household t.service p with
              | Error e -> error e
              | Ok ids -> (
                  R.signal_cancel t.runtime ids;
                  match X.access t.service p with
                  | Error e -> error e
                  | Ok access -> json 200 (Commands.session_json t.service access)))))

let logout t req = try logout_impl t req with Sql.Sql_error _ -> code 503 "Storage_unavailable"
let leave_household t req = try leave_household_impl t req with Sql.Sql_error _ -> code 503 "Storage_unavailable"
let close sub = R.close sub.subscription

type stream_end = Drained | Aborted

let stream sub reqd =
  let allowed () = Result.is_ok (X.authorize_run sub.owner.service ~subscriber:sub.principal ~run:sub.run) in
  Fun.protect
    ~finally:(fun () -> R.close sub.subscription)
    (fun () ->
      match Http_sse.open_response_checked ~authorize:allowed reqd with
      | Error Http_sse.Closed ->
          let response =
            match X.authorize_run sub.owner.service ~subscriber:sub.principal ~run:sub.run with
            | Error e -> error e
            | Ok _ -> code 403 "Access_denied"
          in
          (match response with
          | Buffered (status, headers, body) ->
              Httpun.Reqd.respond_with_string reqd
                (Httpun.Response.create ~headers:(Httpun.Headers.of_list headers) (Httpun.Status.of_code status))
                body
          | Stream _ -> assert false);
          Drained
      | Ok writer ->
          Fun.protect
            ~finally:(fun () -> Http_sse.close writer)
            (fun () ->
              let write operation =
                Eio.Fiber.first operation (fun () ->
                    sub.owner.sleep_s 30.;
                    Error Http_sse.Closed)
              in
              let rec loop () =
                match R.next sub.subscription with
                | Error _ -> Aborted
                | Ok R.End -> Drained
                | Ok R.Heartbeat ->
                    if Result.is_ok (write (fun () -> Http_sse.heartbeat writer)) then loop () else Aborted
                | Ok (R.Resync_required floor) ->
                    if
                      Result.is_ok
                        (write (fun () ->
                             Http_sse.write_frame writer
                               {
                                 Sse.id = None;
                                 event = Some "resync_required";
                                 data =
                                   Yojson.Safe.to_string
                                     (`Assoc [ ("version", `Int 1); ("after", `String (Int64.to_string floor)) ]);
                               }))
                    then Drained
                    else Aborted
                | Ok (R.Events events) ->
                    if
                      List.for_all
                        (fun e ->
                          Result.is_ok
                            (write (fun () ->
                                 Http_sse.write_frame writer
                                   {
                                     Sse.id = Some (Int64.to_string e.A.sequence);
                                     event = Some "advisor";
                                     data = W.encode_event (Advisor_codec.event e);
                                   })))
                        events
                    then loop ()
                    else Aborted
              in
              Eio.Fiber.first loop (fun () ->
                  let rec watch () =
                    if (not (Http_sse.is_closed writer)) && allowed () then (
                      sub.owner.sleep_s 0.05;
                      watch ())
                    else Aborted
                  in
                  watch ())))
