open Cashflow_core
module A = Advisor
module W = Cashflow_advisor_protocol.Wire

type t = { db : Sql.db; clock : unit -> Ptime.t }
type replay = Events of A.event list | Resync_required of int64
type consent_scope = User of User_id.t | Household of Household_id.t

type metadata = {
  conversation_id : A.Conversation_id.t;
  scope : A.scope;
  currency : Currency.t;
  locale : string;
  version : int64;
  updated_at : Ptime.t;
  active_run : A.Run_id.t option;
}

let create ?(clock = Ptime_clock.now) db = { db; clock }
let error code = { A.code; context = A.No_context }

exception Reject of A.error

let reject code = raise (Reject (error code))
let require = function Some x -> x | None -> raise (Sql.Sql_error "invalid advisor stored identity")
let valid = function Ok x -> x | Error _ -> raise (Sql.Sql_error "invalid advisor stored projection")

let time s =
  match Ptime.of_rfc3339 s with Ok (t, _, _) -> t | Error _ -> raise (Sql.Sql_error "invalid advisor timestamp")

(* Fixed-width UTC preserves exact retention boundaries and lexicographic SQL ordering. *)
let stamp t = Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0 t

let tx t f =
  try Sql.transaction_result t.db (fun () -> try Ok (f ()) with Reject e -> Error e)
  with Sql.Sql_error _ -> Error (error A.Storage_unavailable)

let read t f =
  try Sql.read_snapshot t.db (fun () -> try Ok (f ()) with Reject e -> Error e)
  with Sql.Sql_error _ -> Error (error A.Storage_unavailable)

let cid = A.Conversation_id.to_string
let rid = A.Run_id.to_string
let uid = User_id.to_string
let generate_uuid = Uuidm.v4_gen (Random.State.make_self_init ())
let fresh () = Uuidm.to_string (generate_uuid ())

let scope_in : W.scope -> A.scope = function
  | Personal -> Personal
  | Household h -> Household (require (Household_id.of_string h))

let currency_in (c : W.currency) = valid (Currency.make ~code:c.code ~scale:c.scale)

let identities_in (i : W.identities) : A.identities =
  {
    conversation_id = require (A.Conversation_id.of_string i.conversation_id);
    turn_id = require (A.Turn_id.of_string i.turn_id);
    attempt_id = require (A.Attempt_id.of_string i.attempt_id);
    run_id = require (A.Run_id.of_string i.run_id);
    user_message_id = require (A.Message_id.of_string i.user_message_id);
    assistant_message_id = require (A.Message_id.of_string i.assistant_message_id);
  }

let limit_kind_in : W.limit_kind -> A.limit_kind = function
  | W.Active_runs -> A.Active_runs
  | W.User_bytes -> A.User_bytes
  | W.Output_bytes -> A.Output_bytes
  | W.Model_rounds -> A.Model_rounds
  | W.Tool_calls -> A.Tool_calls
  | W.Tool_result_bytes -> A.Tool_result_bytes
  | W.Evidence_bytes -> A.Evidence_bytes
  | W.Context_turns -> A.Context_turns
  | W.Context_bytes -> A.Context_bytes
  | W.Run_deadline -> A.Run_deadline
  | W.Upstream_inactivity -> A.Upstream_inactivity
  | W.Frame_bytes -> A.Frame_bytes
  | W.Subscriber_bytes -> A.Subscriber_bytes
  | W.Journal_bytes -> A.Journal_bytes

let resource_kind_in : W.resource_kind -> A.resource_kind = function
  | W.Conversation -> A.Conversation
  | W.Turn -> A.Turn
  | W.Attempt -> A.Attempt
  | W.Run -> A.Run
  | W.Message -> A.Message
  | W.Evidence -> A.Evidence

let error_code_in : W.error_code -> A.error_code = function
  | W.Access_denied -> A.Access_denied
  | W.Advisor_disabled -> A.Advisor_disabled
  | W.Consent_required -> A.Consent_required
  | W.Scope_mismatch -> A.Scope_mismatch
  | W.Run_busy -> A.Run_busy
  | W.Idempotency_conflict -> A.Idempotency_conflict
  | W.Invalid_tool_arguments -> A.Invalid_tool_arguments
  | W.Tool_not_allowed -> A.Tool_not_allowed
  | W.Provider_unavailable -> A.Provider_unavailable
  | W.Invalid_stream -> A.Invalid_stream
  | W.Run_limit_reached -> A.Run_limit_reached
  | W.Storage_unavailable -> A.Storage_unavailable
  | W.Server_restarted -> A.Server_restarted
  | W.Invalid_transition -> A.Invalid_transition
  | W.Invalid_request -> A.Invalid_request
  | W.Unsupported_version -> A.Unsupported_version
  | W.Version_conflict -> A.Version_conflict
  | W.Capacity_exceeded -> A.Capacity_exceeded
  | W.Cursor_expired -> A.Cursor_expired
  | W.Cursor_gap -> A.Cursor_gap
  | W.Not_found -> A.Not_found

let error_context_in : W.error_context -> A.error_context = function
  | No_context -> No_context
  | Limit x -> Limit (limit_kind_in x)
  | Version x -> Version (Int64.of_string x)
  | Cursor x -> Cursor (Int64.of_string x)
  | Resource x -> Resource (resource_kind_in x)

let error_in (e : W.error) : A.error = { code = error_code_in e.code; context = error_context_in e.context }

let finding_in (f : W.finding) : A.finding =
  {
    kind = f.kind;
    key = f.key;
    scope = scope_in f.scope;
    title = f.title;
    summary = f.summary;
    sampled_at = time f.sampled_at;
  }

let display_in : W.display_value -> A.display_value = function
  | Text s -> Text s
  | Date d -> Date (valid (Date.of_string d))
  | Money m ->
      Money
        {
          minor_units = Int64.of_string (String.concat "" (String.split_on_char '.' m.decimal));
          currency = currency_in m.currency;
        }

let evidence_in (e : W.evidence) : A.evidence =
  {
    id = require (A.Evidence_id.of_string e.id);
    scope = scope_in e.scope;
    currency = currency_in e.currency;
    sampled_at = time e.sampled_at;
    title = e.title;
    facts = List.map (fun (f : W.fact) -> { A.label = f.label; value = display_in f.value }) e.facts;
    source_references = e.source_references;
    import_status =
      (match e.import_status with Current -> Current | Stale -> Stale | Incomplete -> Incomplete | Unknown -> Unknown);
    reconciled = e.reconciled;
    assumptions = e.assumptions;
  }

let payload_in : W.event_payload -> A.event_payload = function
  | Run_started -> Run_started
  | Run_completed -> Run_completed
  | Run_cancelled -> Run_cancelled
  | Run_failed e -> Run_failed (error_in e)
  | Text_appended d -> Text_appended { segment_id = require (A.Segment_id.of_string d.segment_id); text = d.text }
  | Tool_started s -> Tool_started s
  | Tool_finished p ->
      Tool_finished
        {
          label = p.label;
          outcome = (match p.outcome with Succeeded -> Succeeded | Unavailable -> Unavailable | Rejected -> Rejected);
        }
  | Evidence_added e -> Evidence_added (evidence_in e)

let encode_event (e : A.event) =
  let projection = Advisor_codec.event e in
  let payload =
    match (e.payload, projection.payload) with
    | A.Evidence_added original, W.Evidence_added projected ->
        W.Evidence_added { projected with sampled_at = stamp original.sampled_at }
    | _, payload -> payload
  in
  W.encode_event { projection with payload }

let encode_descriptor (s : A.snapshot) =
  let projection = Advisor_codec.snapshot s in
  let source_finding =
    match (s.source_finding, projection.source_finding) with
    | Some original, Some projected -> Some { projected with sampled_at = stamp original.sampled_at }
    | _ -> None
  in
  W.encode_snapshot { projection with source_finding }

let decode_event s =
  let e = valid (W.decode_event s) in
  { A.ids = identities_in e.ids; sequence = Int64.of_string e.sequence; payload = payload_in e.payload }

let decode_accepted s =
  let a = valid (W.decode_accepted s) in
  {
    A.ids = identities_in a.ids;
    accepted_version = Int64.of_string a.accepted_version;
    idempotent_repeat = a.idempotent_repeat;
  }

let encode_accepted a = W.encode_accepted (Advisor_codec.accepted a)

let descriptor s : A.snapshot =
  let s = valid (W.decode_snapshot s) in
  {
    conversation_id = require (A.Conversation_id.of_string s.conversation_id);
    scope = scope_in s.scope;
    currency = currency_in s.currency;
    locale = s.locale;
    source_finding = Option.map finding_in s.source_finding;
    turns = [];
    active_run = None;
    version = Int64.of_string s.version;
  }

let conversation t owner id =
  match
    Sql.(
      run t.db
        (stmt "SELECT projection,version,updated_at FROM advisor_conversations WHERE id=? AND owner_id=?"
           (text @-> text @-> nil)
           (opt (row (text @-> int64 @-> text @-> nil) (fun p v u -> (p, v, u))))))
      (cid id) (uid owner)
  with
  | None -> reject A.Not_found
  | Some (p, v, u) -> ({ (descriptor p) with version = v }, time u)

let active_id t id =
  Sql.(
    run t.db
      (stmt "SELECT id FROM advisor_runs WHERE conversation_id=? AND terminal_at IS NULL" (text @-> nil)
         (opt (row (text @-> nil) (fun s -> require (A.Run_id.of_string s))))))
    (cid id)

let metadata t ~owner id =
  read t (fun () ->
      let s, u = conversation t owner id in
      {
        conversation_id = id;
        scope = s.scope;
        currency = s.currency;
        locale = s.locale;
        version = s.version;
        updated_at = u;
        active_run = active_id t id;
      })

let list t ~owner ?(limit = 20) ?(offset = 0) () =
  read t (fun () ->
      if limit < 1 || offset < 0 then reject A.Invalid_request;
      let ids =
        Sql.(
          run t.db
            (stmt
               "SELECT id FROM advisor_conversations WHERE owner_id=? ORDER BY updated_at DESC,id DESC LIMIT ? OFFSET ?"
               (text @-> int @-> int @-> nil)
               (many (row (text @-> nil) (fun s -> require (A.Conversation_id.of_string s))))))
          (uid owner) (min 100 limit) offset
      in
      List.map
        (fun id ->
          let s, u = conversation t owner id in
          {
            conversation_id = id;
            scope = s.scope;
            currency = s.currency;
            locale = s.locale;
            version = s.version;
            updated_at = u;
            active_run = active_id t id;
          })
        ids)

(* The joined row is the immutable attempt identity plus the mutable runtime checkpoint. *)
let run_query =
  "SELECT \
   c.id,t.id,a.id,t.user_message_id,a.assistant_message_id,r.id,r.owner_id,r.originating_session,r.model,r.protocol_version,r.prompt_version,r.started_at,r.updated_at,r.state,r.failure,r.output_bytes,r.model_rounds,r.tool_calls,r.evidence_bytes,r.journal_bytes,r.last_sequence \
   FROM advisor_runs r JOIN advisor_attempts a ON a.id=r.attempt_id JOIN advisor_turns t ON t.id=a.turn_id JOIN \
   advisor_conversations c ON c.id=t.conversation_id "

let run_row =
  Sql.(
    row
      (text @-> text @-> text @-> text @-> text @-> text @-> text @-> text @-> text @-> int @-> int @-> text @-> text
     @-> text @-> nullable text @-> int @-> int @-> int @-> int @-> int @-> int64 @-> nil)
      (fun c
           t
           a
           u
           m
           r
           o
           session
           model
           protocol_version
           prompt_version
           started
           updated
           state
           failure
           output_bytes
           model_rounds
           tool_calls
           evidence_bytes
           journal_bytes
           last_sequence
         ->
        let ids : A.identities =
          {
            conversation_id = require (A.Conversation_id.of_string c);
            turn_id = require (A.Turn_id.of_string t);
            attempt_id = require (A.Attempt_id.of_string a);
            user_message_id = require (A.Message_id.of_string u);
            assistant_message_id = require (A.Message_id.of_string m);
            run_id = require (A.Run_id.of_string r);
          }
        in
        let state : A.run_state =
          match state with
          | "queued" -> Queued
          | "awaiting" -> Running Awaiting_model
          | "streaming" -> Running Streaming_text
          | "tools" -> Running Running_tools
          | "cancelling" -> Cancel_requested
          | "completed" -> Completed
          | "cancelled" -> Cancelled
          | "failed" -> (
              match (decode_event (require failure)).payload with
              | Run_failed e -> Failed e
              | _ -> raise (Sql.Sql_error "invalid failure"))
          | _ -> raise (Sql.Sql_error "invalid state")
        in
        {
          A.ids;
          owner_id = require (User_id.of_string o);
          originating_session = require (A.Session_ref.of_hash session);
          model;
          protocol_version;
          prompt_version;
          started_at = time started;
          updated_at = time updated;
          state;
          counters = { output_bytes; model_rounds; tool_calls; evidence_bytes; journal_bytes; last_sequence };
        }))

let get_run t id =
  match Sql.(run t.db (stmt (run_query ^ "WHERE r.id=?") (text @-> nil) (opt run_row))) (rid id) with
  | Some r -> r
  | None -> reject A.Not_found

let find_run t ~owner id =
  read t (fun () ->
      let run = get_run t id in
      if not (User_id.equal run.owner_id owner) then reject A.Not_found;
      run)

let active_runs t =
  read t (fun () -> Sql.(run t.db (stmt (run_query ^ "WHERE r.terminal_at IS NULL ORDER BY r.id") nil (many run_row))))

let active_runs_for_owner t ~owner =
  read t (fun () ->
      Sql.(
        run t.db
          (stmt
             (run_query ^ "WHERE r.terminal_at IS NULL AND r.owner_id=? ORDER BY r.id")
             (text @-> nil) (many run_row)))
        (uid owner))

let capacity t owner conversation =
  let count =
    Sql.(
      run t.db (stmt "SELECT count(*) FROM advisor_runs WHERE terminal_at IS NULL" nil (one (row (int @-> nil) Fun.id))))
  in
  let busy =
    Sql.(
      run t.db
        (stmt "SELECT count(*) FROM advisor_runs WHERE terminal_at IS NULL AND (owner_id=? OR conversation_id=?)"
           (text @-> text @-> nil)
           (one (row (int @-> nil) Fun.id))))
      (uid owner) (cid conversation)
  in
  if busy > 0 then reject A.Run_busy;
  if count >= A.limits.active_runs then raise (Reject { code = A.Capacity_exceeded; context = A.Limit A.Active_runs })

let fingerprint s = Digestif.SHA256.(to_hex (digest_string s))

let request_existing t owner key operation fingerprint =
  match
    Sql.(
      run t.db
        (stmt "SELECT operation,fingerprint,accepted,deleted_at FROM advisor_requests WHERE owner_id=? AND key=?"
           (text @-> text @-> nil)
           (opt (row (text @-> text @-> text @-> nullable text @-> nil) (fun o f a d -> (o, f, a, d))))))
      (uid owner) (Idempotency_key.to_string key)
  with
  | None -> None
  | Some (o, f, a, d) ->
      if o <> operation || f <> fingerprint then reject A.Idempotency_conflict;
      if Option.is_some d then reject A.Not_found;
      let accepted = decode_accepted a in
      ignore (conversation t owner accepted.ids.conversation_id);
      Some { accepted with idempotent_repeat = true }

let record_request t owner key operation fingerprint (accepted : A.accepted) =
  Sql.(
    run t.db
      (stmt
         "INSERT INTO advisor_requests(owner_id,key,operation,fingerprint,conversation_id,accepted) VALUES \
          (?,?,?,?,?,?)"
         (text @-> text @-> text @-> text @-> text @-> text @-> nil)
         exec))
    (uid owner) (Idempotency_key.to_string key) operation fingerprint (cid accepted.ids.conversation_id)
    (encode_accepted accepted)

let insert_attempt t ~owner ~session ~now ~model ids ordinal =
  Sql.(
    run t.db
      (stmt "INSERT INTO advisor_attempts(id,turn_id,attempt_ordinal,assistant_message_id) VALUES (?,?,?,?)"
         (text @-> text @-> int64 @-> text @-> nil)
         exec))
    (A.Attempt_id.to_string ids.A.attempt_id)
    (A.Turn_id.to_string ids.turn_id) ordinal
    (A.Message_id.to_string ids.assistant_message_id);
  Sql.(
    run t.db
      (stmt
         "INSERT INTO \
          advisor_runs(id,attempt_id,conversation_id,owner_id,originating_session,model,started_at,updated_at,state) \
          VALUES (?,?,?,?,?,?,?,?,'queued')"
         (text @-> text @-> text @-> text @-> text @-> text @-> text @-> text @-> nil)
         exec))
    (rid ids.run_id) (A.Attempt_id.to_string ids.attempt_id) (cid ids.conversation_id) (uid owner)
    (A.Session_ref.to_hash session) model (stamp now) (stamp now)

let new_ids conversation turn user : A.identities =
  {
    conversation_id = conversation;
    turn_id = turn;
    user_message_id = user;
    attempt_id = require (A.Attempt_id.of_string (fresh ()));
    assistant_message_id = require (A.Message_id.of_string (fresh ()));
    run_id = require (A.Run_id.of_string (fresh ()));
  }

let bump t id version now =
  Sql.(
    run t.db
      (stmt "UPDATE advisor_conversations SET version=?,updated_at=? WHERE id=?" (int64 @-> text @-> text @-> nil) exec))
    version (stamp now) (cid id)

let check_version expected version =
  if expected <> version then raise (Reject { A.code = Version_conflict; context = Version version })

let accept t ~owner ~session ~now ~model ~source_finding (request : A.submit) =
  tx t (fun () ->
      (* Native requests use the v1 projection as their canonical payload fingerprint. *)
      let projected : W.submit =
        {
          conversation_id = Option.map cid request.conversation_id;
          scope =
            (match request.scope with Personal -> Personal | Household h -> Household (Household_id.to_string h));
          currency = { code = Currency.code request.currency; scale = Currency.scale request.currency };
          locale = request.locale;
          user_text = request.user_text;
          idempotency_key = Idempotency_key.to_string request.idempotency_key;
          expected_version = Int64.to_string request.expected_version;
          source_finding_key = request.source_finding_key;
        }
      in
      let fp = fingerprint (W.encode_submit projected) in
      (* Current owner authorization comes before repeat resolution; repeats precede current busy/version checks. *)
      Option.iter (fun id -> ignore (conversation t owner id)) request.conversation_id;
      match request_existing t owner request.idempotency_key "submit" fp with
      | Some a -> a
      | None ->
          if String.length request.user_text > A.limits.user_bytes then
            raise (Reject { code = A.Run_limit_reached; context = Limit User_bytes });
          if Result.is_error (W.decode_submit (W.encode_submit projected)) then reject A.Invalid_request;
          let id = Option.value request.conversation_id ~default:(require (A.Conversation_id.of_string (fresh ()))) in
          let version =
            match request.conversation_id with
            | None ->
                check_version request.expected_version 0L;
                0L
            | Some _ ->
                let s, _ = conversation t owner id in
                if
                  s.scope <> request.scope
                  || (not (Currency.equal s.currency request.currency))
                  || s.locale <> request.locale
                then reject A.Scope_mismatch;
                check_version request.expected_version s.version;
                s.version
          in
          capacity t owner id;
          (match request.conversation_id with
          | Some _ -> if source_finding <> None || request.source_finding_key <> None then reject A.Invalid_request
          | None ->
              (match (request.source_finding_key, source_finding) with
              | None, None -> ()
              | Some key, Some f when key = f.A.key && f.scope = request.scope -> ()
              | _ -> reject A.Invalid_request);
              let s : A.snapshot =
                {
                  conversation_id = id;
                  scope = request.scope;
                  currency = request.currency;
                  locale = request.locale;
                  source_finding;
                  turns = [];
                  active_run = None;
                  version = 0L;
                }
              in
              let projection = encode_descriptor s in
              if Result.is_error (W.decode_snapshot projection) then reject A.Invalid_request;
              Sql.(
                run t.db
                  (stmt
                     "INSERT INTO advisor_conversations(id,owner_id,projection,version,updated_at) VALUES (?,?,?,0,?)"
                     (text @-> text @-> text @-> text @-> nil)
                     exec))
                (cid id) (uid owner) projection (stamp now));
          let turn = require (A.Turn_id.of_string (fresh ())) and user = require (A.Message_id.of_string (fresh ())) in
          Sql.(
            run t.db
              (stmt
                 "INSERT INTO advisor_turns(id,conversation_id,ordinal,user_message_id,user_text) SELECT \
                  ?,?,coalesce(max(ordinal),0)+1,?,? FROM advisor_turns WHERE conversation_id=?"
                 (text @-> text @-> text @-> text @-> text @-> nil)
                 exec))
            (A.Turn_id.to_string turn) (cid id) (A.Message_id.to_string user) request.user_text (cid id);
          let ids = new_ids id turn user in
          insert_attempt t ~owner ~session ~now ~model ids 1L;
          let accepted : A.accepted = { ids; accepted_version = Int64.succ version; idempotent_repeat = false } in
          bump t id accepted.accepted_version now;
          record_request t owner request.idempotency_key "submit" fp accepted;
          accepted)

let retry t ~owner ~session ~now ~model (request : A.retry) =
  tx t (fun () ->
      let turn = A.Turn_id.to_string request.turn_id in
      let projection : W.retry =
        {
          turn_id = turn;
          expected_version = Int64.to_string request.expected_version;
          idempotency_key = Idempotency_key.to_string request.idempotency_key;
        }
      in
      let encoded = W.encode_retry projection in
      let fp = fingerprint encoded in
      let conversation_id =
        match
          Sql.(
            run t.db
              (stmt
                 "SELECT c.id FROM advisor_turns t JOIN advisor_conversations c ON c.id=t.conversation_id WHERE t.id=? \
                  AND c.owner_id=?"
                 (text @-> text @-> nil)
                 (opt (row (text @-> nil) Fun.id))))
            turn (uid owner)
        with
        | None ->
            (* Deleted retry keys remain tombstoned even though the turn is gone. *)
            ignore (request_existing t owner request.idempotency_key "retry" fp);
            reject A.Not_found
        | Some id -> require (A.Conversation_id.of_string id)
      in
      match request_existing t owner request.idempotency_key "retry" fp with
      | Some a -> a
      | None ->
          if Result.is_error (W.decode_retry encoded) then reject A.Invalid_request;
          let s, _ = conversation t owner conversation_id in
          check_version request.expected_version s.version;
          let latest =
            Sql.(
              run t.db
                (stmt "SELECT id FROM advisor_turns WHERE conversation_id=? ORDER BY ordinal DESC LIMIT 1" (text @-> nil)
                   (one (row (text @-> nil) Fun.id))))
              (cid conversation_id)
          in
          if latest <> turn then raise (Reject { A.code = Version_conflict; context = Version s.version });
          let old, ordinal =
            Sql.(
              run t.db
                (stmt
                   "SELECT r.id,a.attempt_ordinal FROM advisor_attempts a JOIN advisor_runs r ON r.attempt_id=a.id \
                    WHERE a.turn_id=? ORDER BY a.attempt_ordinal DESC LIMIT 1"
                   (text @-> nil)
                   (one (row (text @-> int64 @-> nil) (fun r o -> (require (A.Run_id.of_string r), o))))))
              turn
          in
          let old = get_run t old in
          (match old.state with Failed _ | Cancelled -> () | _ -> reject A.Invalid_transition);
          capacity t owner conversation_id;
          let ids = new_ids conversation_id request.turn_id old.ids.user_message_id in
          insert_attempt t ~owner ~session ~now ~model ids (Int64.succ ordinal);
          let accepted : A.accepted = { ids; accepted_version = Int64.succ s.version; idempotent_repeat = false } in
          bump t conversation_id accepted.accepted_version now;
          record_request t owner request.idempotency_key "retry" fp accepted;
          accepted)

let append_event t (event : A.event) =
  let encoded = encode_event event in
  if Result.is_error (W.decode_event encoded) then reject A.Invalid_request;
  if String.length encoded > A.limits.frame_bytes then
    raise (Reject { code = A.Run_limit_reached; context = Limit Frame_bytes });
  Sql.(
    run t.db
      (stmt "INSERT INTO advisor_events(run_id,sequence,projection) VALUES (?,?,?)"
         (text @-> int64 @-> text @-> nil)
         exec))
    (rid event.ids.run_id) event.sequence encoded;
  String.length encoded

let write_counters t (run : A.run) counters now =
  Sql.(
    run t.db
      (stmt
         "UPDATE advisor_runs SET \
          output_bytes=?,model_rounds=?,tool_calls=?,evidence_bytes=?,journal_bytes=?,last_sequence=?,updated_at=? \
          WHERE id=?"
         (int @-> int @-> int @-> int @-> int @-> int64 @-> text @-> text @-> nil)
         exec))
    counters.A.output_bytes counters.model_rounds counters.tool_calls counters.evidence_bytes counters.journal_bytes
    counters.last_sequence (stamp now) (rid run.ids.run_id)

let limit_exceeded kind = raise (Reject { A.code = Run_limit_reached; context = Limit kind })

(* Reserve space for the bounded identity/error-only terminal event, even at journal capacity. *)
let terminal_event_reserve_bytes = 4096

let append t ~run ~expected_sequence payloads =
  let now = t.clock () in
  tx t (fun () ->
      let run = get_run t run in
      (match run.state with Running _ -> () | _ -> reject A.Invalid_transition);
      if expected_sequence <> run.counters.last_sequence then
        raise (Reject { code = A.Cursor_gap; context = Cursor run.counters.last_sequence });
      let conversation, _ = conversation t run.owner_id run.ids.conversation_id in
      let counters = ref run.counters in
      let events =
        List.map
          (fun (payload : A.event_payload) ->
            (match payload with
            | Run_started | Run_completed | Run_cancelled | Run_failed _ -> reject A.Invalid_transition
            | _ -> ());
            let sequence = Int64.succ !counters.last_sequence in
            let event : A.event = { ids = run.ids; sequence; payload } in
            let bytes = append_event t event in
            let next = { !counters with journal_bytes = !counters.journal_bytes + bytes; last_sequence = sequence } in
            if next.journal_bytes > A.limits.journal_bytes - terminal_event_reserve_bytes then
              limit_exceeded A.Journal_bytes;
            let next =
              match payload with
              | Text_appended delta ->
                  let output = next.output_bytes + String.length delta.text in
                  if output > A.limits.output_bytes then limit_exceeded A.Output_bytes;
                  let id = A.Segment_id.to_string delta.segment_id in
                  let existing =
                    Sql.(
                      run t.db
                        (stmt "SELECT projection FROM advisor_segments WHERE run_id=? AND id=?"
                           (text @-> text @-> nil)
                           (opt (row (text @-> nil) Fun.id))))
                      (rid run.ids.run_id) id
                  in
                  let accumulated =
                    match existing with
                    | None -> delta.text
                    | Some p -> (
                        match (decode_event p).payload with
                        | Text_appended d -> d.text ^ delta.text
                        | _ -> raise (Sql.Sql_error "invalid segment"))
                  in
                  let projection =
                    encode_event { event with payload = Text_appended { delta with text = accumulated } }
                  in
                  Sql.(
                    run t.db
                      (stmt
                         "INSERT INTO advisor_segments(run_id,id,ordinal,projection) VALUES (?,?,?,?) ON \
                          CONFLICT(run_id,id) DO UPDATE SET projection=excluded.projection"
                         (text @-> text @-> int64 @-> text @-> nil)
                         exec))
                    (rid run.ids.run_id) id sequence projection;
                  { next with output_bytes = output }
              | Evidence_added evidence ->
                  if
                    evidence.scope <> conversation.scope || not (Currency.equal evidence.currency conversation.currency)
                  then reject A.Scope_mismatch;
                  let total = next.evidence_bytes + bytes in
                  if total > A.limits.evidence_bytes then limit_exceeded A.Evidence_bytes;
                  let exists =
                    Sql.(
                      run t.db
                        (stmt "SELECT 1 FROM advisor_evidence WHERE run_id=? AND id=?"
                           (text @-> text @-> nil)
                           (opt (row (int @-> nil) Fun.id))))
                      (rid run.ids.run_id) (A.Evidence_id.to_string evidence.id)
                  in
                  if Option.is_some exists then reject A.Invalid_request;
                  Sql.(
                    run t.db
                      (stmt "INSERT INTO advisor_evidence(run_id,id,ordinal,projection) VALUES (?,?,?,?)"
                         (text @-> text @-> int64 @-> text @-> nil)
                         exec))
                    (rid run.ids.run_id) (A.Evidence_id.to_string evidence.id) sequence (encode_event event);
                  { next with evidence_bytes = total }
              | Tool_started _ ->
                  if next.tool_calls >= A.limits.tool_calls then limit_exceeded A.Tool_calls;
                  { next with tool_calls = next.tool_calls + 1 }
              | _ -> next
            in
            counters := next;
            event)
          payloads
      in
      write_counters t run !counters now;
      events)

let state_name : A.run_state -> string = function
  | Queued -> "queued"
  | Running Awaiting_model -> "awaiting"
  | Running Streaming_text -> "streaming"
  | Running Running_tools -> "tools"
  | Cancel_requested -> "cancelling"
  | Completed -> "completed"
  | Cancelled -> "cancelled"
  | Failed _ -> "failed"

let transition_at t ~now ~run transition =
  let run = get_run t run in
  let state = match A.step run.state transition with Ok s -> s | Error e -> raise (Reject e) in
  let payload =
    match (transition, state) with
    | A.Start, _ -> Some A.Run_started
    | _, A.Completed -> Some A.Run_completed
    | _, A.Cancelled -> Some A.Run_cancelled
    | _, A.Failed e -> Some (A.Run_failed e)
    | _ -> None
  in
  let counters =
    match transition with
    | (A.Start | A.Set_phase A.Awaiting_model) when run.state <> A.Running A.Awaiting_model ->
        if run.counters.model_rounds >= A.limits.model_rounds then limit_exceeded A.Model_rounds;
        { run.counters with model_rounds = run.counters.model_rounds + 1 }
    | _ -> run.counters
  in
  let event =
    Option.map (fun payload -> { A.ids = run.ids; sequence = Int64.succ counters.last_sequence; payload }) payload
  in
  let counters =
    match event with
    | None -> counters
    | Some e ->
        let bytes = append_event t e in
        if counters.journal_bytes + bytes > A.limits.journal_bytes then limit_exceeded A.Journal_bytes;
        { counters with journal_bytes = counters.journal_bytes + bytes; last_sequence = e.sequence }
  in
  let terminal = if A.is_terminal state then Some (stamp now) else None in
  let failure = match (state, event) with Failed _, Some e -> Some (encode_event e) | _ -> None in
  Sql.(
    run t.db
      (stmt "UPDATE advisor_runs SET state=?,failure=?,terminal_at=? WHERE id=?"
         (text @-> nullable text @-> nullable text @-> text @-> nil)
         exec))
    (state_name state) failure terminal (rid run.ids.run_id);
  write_counters t run counters now;
  state

let transition t ~run change =
  let now = t.clock () in
  tx t (fun () -> transition_at t ~now ~run change)

let snapshot t ~owner id =
  read t (fun () ->
      let s, _ = conversation t owner id in
      let turns =
        Sql.(
          run t.db
            (stmt "SELECT id,user_message_id,user_text FROM advisor_turns WHERE conversation_id=? ORDER BY ordinal"
               (text @-> nil)
               (many (row (text @-> text @-> text @-> nil) (fun id message text -> (id, message, text))))))
          (cid id)
      in
      let turns =
        List.map
          (fun (turn, user_message_id, user_text) ->
            let attempts =
              Sql.(
                run t.db
                  (stmt
                     "SELECT r.id,a.attempt_ordinal FROM advisor_attempts a JOIN advisor_runs r ON r.attempt_id=a.id \
                      WHERE a.turn_id=? ORDER BY a.attempt_ordinal"
                     (text @-> nil)
                     (many (row (text @-> int64 @-> nil) (fun r o -> (r, o))))))
                turn
            in
            let attempts =
              List.map
                (fun (r, ordinal) ->
                  let run = get_run t (require (A.Run_id.of_string r)) in
                  let segments =
                    Sql.(
                      run t.db
                        (stmt
                           "SELECT projection FROM (SELECT ordinal,projection FROM advisor_segments WHERE run_id=? \
                            UNION ALL SELECT ordinal,projection FROM advisor_evidence WHERE run_id=?) ORDER BY ordinal"
                           (text @-> text @-> nil)
                           (many
                              (row (text @-> nil) (fun s ->
                                   match (decode_event s).payload with
                                   | Text_appended d -> A.Text_segment d
                                   | Evidence_added e -> A.Evidence_segment e
                                   | _ -> raise (Sql.Sql_error "invalid materialized segment"))))))
                      r r
                  in
                  { A.ids = run.ids; ordinal; state = run.state; segments; last_sequence = run.counters.last_sequence })
                attempts
            in
            {
              A.turn_id = require (A.Turn_id.of_string turn);
              user_message_id = require (A.Message_id.of_string user_message_id);
              user_text;
              attempts;
            })
          turns
      in
      { s with turns; active_run = active_id t id })

let replay t ~run ~after ~limit =
  read t (fun () ->
      let r = get_run t run in
      if after < 0L || limit < 1 then reject A.Invalid_request;
      let floor =
        Sql.(
          run t.db
            (stmt "SELECT replay_floor FROM advisor_runs WHERE id=?" (text @-> nil) (one (row (int64 @-> nil) Fun.id))))
          (rid run)
      in
      if after < floor then Resync_required r.counters.last_sequence
      else if after > r.counters.last_sequence then
        raise (Reject { code = A.Cursor_gap; context = Cursor r.counters.last_sequence })
      else
        Events
          (Sql.(
             run t.db
               (stmt "SELECT projection FROM advisor_events WHERE run_id=? AND sequence>? ORDER BY sequence LIMIT ?"
                  (text @-> int64 @-> int @-> nil)
                  (many (row (text @-> nil) decode_event))))
             (rid run) after (min 100 limit)))

let delete t ~owner ~now id =
  tx t (fun () ->
      ignore (conversation t owner id);
      let active =
        Sql.(
          run t.db
            (stmt "SELECT id FROM advisor_runs WHERE conversation_id=? AND terminal_at IS NULL ORDER BY id"
               (text @-> nil)
               (many (row (text @-> nil) (fun s -> require (A.Run_id.of_string s))))))
          (cid id)
      in
      Sql.(
        run t.db
          (stmt "UPDATE advisor_requests SET deleted_at=? WHERE owner_id=? AND conversation_id=? AND deleted_at IS NULL"
             (text @-> text @-> text @-> nil)
             exec))
        (stamp now) (uid owner) (cid id);
      Sql.(run t.db (stmt "DELETE FROM advisor_conversations WHERE id=? AND owner_id=?" (text @-> text @-> nil) exec))
        (cid id) (uid owner);
      active)

let recover t ~now =
  match
    tx t (fun () ->
        let ids =
          Sql.(
            run t.db
              (stmt "SELECT id FROM advisor_runs WHERE terminal_at IS NULL ORDER BY id" nil
                 (many (row (text @-> nil) (fun s -> require (A.Run_id.of_string s))))))
        in
        List.iter (fun run -> ignore (transition_at t ~now ~run A.Recover_interrupted)) ids)
  with
  | Ok () -> ()
  | Error _ -> raise (Sql.Sql_error "advisor recovery failed")

let prune t ~now =
  let cutoff = stamp (require (Ptime.sub_span now (Ptime.Span.of_int_s A.limits.replay_retention_seconds))) in
  match
    tx t (fun () ->
        Sql.(
          run t.db
            (stmt "DELETE FROM advisor_events WHERE run_id IN (SELECT id FROM advisor_runs WHERE terminal_at<=?)"
               (text @-> nil) exec))
          cutoff;
        Sql.(
          run t.db
            (stmt "UPDATE advisor_runs SET replay_floor=last_sequence,journal_bytes=0 WHERE terminal_at<=?"
               (text @-> nil) exec))
          cutoff;
        Sql.(run t.db (stmt "DELETE FROM advisor_requests WHERE deleted_at<=?" (text @-> nil) exec)) cutoff)
  with
  | Ok () -> ()
  | Error _ -> raise (Sql.Sql_error "advisor pruning failed")

let consent t scope =
  read t (fun () ->
      let sql, id =
        match scope with
        | User u -> ("SELECT enabled FROM advisor_user_consent WHERE user_id=?", uid u)
        | Household h -> ("SELECT enabled FROM advisor_household_consent WHERE household_id=?", Household_id.to_string h)
      in
      Option.value (Sql.(run t.db (stmt sql (text @-> nil) (opt (row (bool @-> nil) Fun.id)))) id) ~default:false)

let set_consent t scope ~enabled =
  tx t (fun () ->
      let sql, id =
        match scope with
        | User u ->
            ( "INSERT INTO advisor_user_consent(user_id,enabled) VALUES (?,?) ON CONFLICT(user_id) DO UPDATE SET \
               enabled=excluded.enabled",
              uid u )
        | Household h ->
            ( "INSERT INTO advisor_household_consent(household_id,enabled) VALUES (?,?) ON CONFLICT(household_id) DO \
               UPDATE SET enabled=excluded.enabled",
              Household_id.to_string h )
      in
      Sql.(run t.db (stmt sql (text @-> bool @-> nil) exec)) id enabled)

let account_alias t ~conversation account =
  tx t (fun () ->
      let exists =
        Sql.(
          run t.db
            (stmt "SELECT 1 FROM advisor_conversations WHERE id=?" (text @-> nil) (opt (row (int @-> nil) Fun.id))))
          (cid conversation)
      in
      if Option.is_none exists then reject A.Not_found;
      match
        Sql.(
          run t.db
            (stmt "SELECT alias FROM advisor_account_aliases WHERE conversation_id=? AND account_id=?"
               (text @-> text @-> nil)
               (opt (row (text @-> nil) Fun.id))))
          (cid conversation) (Account_id.to_string account)
      with
      | Some a -> a
      | None ->
          let ordinal =
            Sql.(
              run t.db
                (stmt "SELECT count(*)+1 FROM advisor_account_aliases WHERE conversation_id=?" (text @-> nil)
                   (one (row (int @-> nil) Fun.id))))
              (cid conversation)
          in
          let alias = "account_" ^ string_of_int ordinal in
          Sql.(
            run t.db
              (stmt "INSERT INTO advisor_account_aliases(conversation_id,account_id,alias) VALUES (?,?,?)"
                 (text @-> text @-> text @-> nil)
                 exec))
            (cid conversation) (Account_id.to_string account) alias;
          alias)

let resolve_account_alias t ~owner ~conversation:conversation_id ~alias =
  tx t (fun () ->
      ignore (conversation t owner conversation_id);
      match
        Sql.(
          run t.db
            (stmt "SELECT account_id FROM advisor_account_aliases WHERE conversation_id=? AND alias=?"
               (text @-> text @-> nil) (opt (row (text @-> nil) Fun.id))))
          (cid conversation_id) alias
      with
      | Some id -> (match Account_id.of_string id with Some id -> id | None -> reject A.Storage_unavailable)
      | None -> reject A.Not_found)
