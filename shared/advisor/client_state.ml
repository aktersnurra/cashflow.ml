module W = Wire

type notice =
  | Quiet
  | Interrupted
  | Request_failed
  | Choose_context
  | Busy
  | Invalid_state
  | Answer_completed
  | Answer_stopped
  | Answer_failed
  | Answer_running
  | Stopping

type token = { epoch : int; generation : int }
type failure = Session_lost | Access_lost | Transport | Rejected
type status = { available : bool; consent : bool }
type metadata = { conversation_id : string; version : string; updated_at : string }
type connection = Disconnected | Connecting | Connected | Reconnecting
type pending = Submit of W.submit | Retry of W.retry

type command =
  | Post_turn of token * W.submit
  | Retry_turn of token * W.retry
  | Cancel_run of token * W.identities
  | Fetch_snapshot of token * string
  | Subscribe of token * W.identities * string
  | Unsubscribe
  | Delete_conversation of token * string
  | Announce_status of notice
  | Get_status of token
  | Set_consent of token * bool * string option
  | List_conversations of token * int * int
  | Schedule_flush of token
  | Cancel_flush

type action =
  | Activate
  | Deactivate
  | Auth_lost
  | Leave_surface
  | Open_conversation of string
  | Open_latest
  | Open_empty of W.scope * W.currency * string
  | Open_finding of string * W.scope * W.currency * string
  | Draft_changed of string
  | Submit_draft of string
  | Retry_latest of string
  | Repeat_pending
  | Stop
  | Reconnect
  | Delete
  | Delete_owned of string
  | Consent of bool * string option
  | Load_history of int
  | Accepted of token * (W.accepted, failure) result
  | Snapshot of token * (W.snapshot, failure) result
  | Status of token * (status, failure) result
  | History of token * (metadata list, failure) result
  | Consent_result of token * (unit, failure) result
  | Deleted of token * (unit, failure) result
  | Stopped of token * (string, failure) result
  | Stream_invalid of token
  | Event of token * W.event
  | Connection of token * connection
  | Resync of token
  | Flush of token
  | Background of token
  | Foreground of token
  | Follow of bool

type view = {
  snapshot : W.snapshot option;
  draft : string;
  connection : connection;
  status : status option;
  history : metadata list;
  pending : pending option;
  notice : notice;
  following : bool;
  new_output : bool;
  stopping : bool;
  can_reconnect : bool;
}

type t = {
  v : view;
  epoch : int;
  serial : int;
  active : bool;
  snapshot_request : token option;
  write_request : token option;
  stop_request : token option;
  delete_request : token option;
  delete_target : string option;
  status_request : token option;
  consent_request : token option;
  history_request : token option;
  suspended : token option;
  stream : (token * W.identities) option;
  buffer : W.event list;
  bytes : int;
  latest : bool;
  selected : string option;
  context : (string option * W.scope * W.currency * string) option;
}

let blank =
  {
    snapshot = None;
    draft = "";
    connection = Disconnected;
    status = None;
    history = [];
    pending = None;
    notice = Quiet;
    following = true;
    new_output = false;
    stopping = false;
    can_reconnect = false;
  }

let empty =
  {
    v = blank;
    epoch = 0;
    serial = 0;
    active = false;
    snapshot_request = None;
    write_request = None;
    stop_request = None;
    delete_request = None;
    delete_target = None;
    status_request = None;
    consent_request = None;
    history_request = None;
    suspended = None;
    stream = None;
    buffer = [];
    bytes = 0;
    latest = false;
    selected = None;
    context = None;
  }

let view t = { t.v with can_reconnect = t.active && t.selected <> None && t.stream = None && t.snapshot_request = None }

let fresh t =
  let generation = t.serial + 1 in
  ({ t with serial = generation }, { epoch = t.epoch; generation })

let current t expected (tok : token) = t.active && tok.epoch = t.epoch && expected = Some tok
let terminal = function W.Completed | W.Cancelled | W.Failed _ -> true | _ -> false
let attempts (s : W.snapshot) = List.concat_map (fun (t : W.turn) -> t.attempts) s.turns

let find_attempt t ids =
  match t.v.snapshot with None -> None | Some s -> List.find_opt (fun (a : W.attempt) -> a.ids = ids) (attempts s)

let stream_current t tok = match t.stream with Some (x, _) -> current t (Some x) tok | None -> false
let clear t = { empty with epoch = t.epoch + 1; serial = t.serial; active = t.active }
let clear_commands = [ Unsubscribe; Cancel_flush ]

let fetch t id =
  let t, tok = fresh t in
  ( {
      t with
      snapshot_request = Some tok;
      suspended = None;
      stream = None;
      buffer = [];
      bytes = 0;
      v = { t.v with connection = Reconnecting };
    },
    [ Unsubscribe; Cancel_flush; Fetch_snapshot (tok, id) ] )

let resync t = match t.selected with None -> (t, []) | Some id -> fetch t id

let map_attempt (s : W.snapshot) ids f =
  {
    s with
    turns =
      List.map
        (fun (turn : W.turn) ->
          { turn with attempts = List.map (fun (a : W.attempt) -> if a.ids = ids then f a else a) turn.attempts })
        s.turns;
  }

let apply_event t (e : W.event) =
  match t.v.snapshot with
  | None -> t
  | Some s ->
      let update (a : W.attempt) =
        let state, segments =
          match e.payload with
          | W.Run_started -> (W.Running W.Awaiting_model, a.segments)
          | W.Text_appended d ->
              let found =
                List.exists (function W.Text_segment x -> x.segment_id = d.segment_id | _ -> false) a.segments
              in
              let segs =
                if found then
                  List.map
                    (function
                      | W.Text_segment x when x.segment_id = d.segment_id ->
                          W.Text_segment { x with text = x.text ^ d.text }
                      | x -> x)
                    a.segments
                else a.segments @ [ W.Text_segment d ]
              in
              (W.Running W.Streaming_text, segs)
          | W.Evidence_added e -> (a.state, a.segments @ [ W.Evidence_segment e ])
          | W.Tool_started _ -> (W.Running W.Running_tools, a.segments)
          | W.Tool_finished _ -> (a.state, a.segments)
          | W.Run_completed -> (W.Completed, a.segments)
          | W.Run_cancelled -> (W.Cancelled, a.segments)
          | W.Run_failed err -> (W.Failed err, a.segments)
        in
        { a with state; segments; last_sequence = e.sequence }
      in
      let s = map_attempt s e.ids update in
      let is_terminal =
        match e.payload with W.Run_completed | W.Run_cancelled | W.Run_failed _ -> true | _ -> false
      in
      let s = if is_terminal then { s with active_run = None } else s in
      { t with v = { t.v with snapshot = Some s; new_output = (not t.v.following) || t.v.new_output } }

let flush t = List.fold_left apply_event { t with buffer = []; bytes = 0 } t.buffer
let notice t message = ({ t with v = { t.v with notice = message } }, [ Announce_status message ])

let failure t = function
  | Session_lost -> ({ (clear t) with active = false }, clear_commands)
  | Access_lost -> (clear t, clear_commands)
  | Transport -> notice t Interrupted
  | Rejected -> notice t Request_failed

let request_pending t =
  match t.v.pending with
  | None -> (t, [])
  | Some p ->
      let t, tok = fresh t in
      let t = { t with write_request = Some tok } in
      (t, [ (match p with Submit p -> Post_turn (tok, p) | Retry p -> Retry_turn (tok, p)) ])

let reduce t action =
  match action with
  | Activate ->
      if t.active then (t, [])
      else
        let t, tok = fresh { t with active = true } in
        ({ t with status_request = Some tok }, [ Get_status tok ])
  | Deactivate | Auth_lost -> ({ (clear t) with active = false }, clear_commands)
  | Leave_surface -> (t, [])
  | Follow following ->
      ({ t with v = { t.v with following; new_output = (if following then false else t.v.new_output) } }, [])
  | Draft_changed draft when t.active && String.length draft <= 8192 -> ({ t with v = { t.v with draft } }, [])
  | Open_conversation id when t.active ->
      if t.selected = Some id && (t.snapshot_request <> None || t.v.snapshot <> None) then (t, [])
      else
        let t =
          {
            t with
            latest = false;
            context = None;
            suspended = None;
            status_request = None;
            consent_request = None;
            selected = Some id;
            write_request = None;
            stop_request = None;
            delete_request = None;
            history_request = None;
            v = { blank with draft = t.v.draft; status = t.v.status };
          }
        in
        fetch t id
  | Open_empty (scope, currency, locale) when t.active ->
      let status = t.v.status in
      let t = clear t in
      ({ t with context = Some (None, scope, currency, locale); v = { t.v with status } }, clear_commands)
  | Open_finding (key, scope, currency, locale) when t.active ->
      let status = t.v.status in
      let t = clear t in
      ({ t with context = Some (Some key, scope, currency, locale); v = { t.v with status } }, clear_commands)
  | Open_latest when t.active ->
      let t, tok = fresh t in
      ({ t with latest = true; history_request = Some tok }, [ List_conversations (tok, 1, 0) ])
  | Load_history offset when t.active && offset >= 0 ->
      let t, tok = fresh t in
      ({ t with latest = false; history_request = Some tok }, [ List_conversations (tok, 20, offset) ])
  | Submit_draft key when t.active && t.v.pending = None && t.v.draft <> "" && t.write_request = None -> (
      let context =
        match (t.v.snapshot, t.context) with Some s, _ -> Some (None, s.scope, s.currency, s.locale) | _, c -> c
      in
      match context with
      | None -> notice t Choose_context
      | Some (source_finding_key, scope, currency, locale) ->
          if match t.v.snapshot with Some s -> s.active_run <> None | _ -> false then notice t Busy
          else
            let p : W.submit =
              {
                conversation_id = t.selected;
                scope;
                currency;
                locale;
                user_text = t.v.draft;
                idempotency_key = key;
                expected_version = (match t.v.snapshot with Some s -> s.version | None -> "0");
                source_finding_key;
              }
            in
            request_pending { t with v = { t.v with pending = Some (Submit p) } })
  | Repeat_pending when t.active && t.write_request = None -> request_pending t
  | Retry_latest key when t.active && t.v.pending = None -> (
      match t.v.snapshot with
      | Some s when s.active_run = None -> (
          match List.rev s.turns with
          | turn :: _ -> (
              match List.rev turn.attempts with
              | a :: _ when match a.state with W.Failed _ | W.Cancelled -> true | _ -> false ->
                  let p : W.retry = { turn_id = turn.turn_id; expected_version = s.version; idempotency_key = key } in
                  request_pending { t with v = { t.v with pending = Some (Retry p) } }
              | _ -> (t, []))
          | _ -> (t, []))
      | _ -> (t, []))
  | Accepted (tok, result) when current t t.write_request tok -> (
      let t = { t with write_request = None } in
      match result with
      | Error e -> failure t e
      | Ok a ->
          let draft = match t.v.pending with Some (Submit p) when p.user_text = t.v.draft -> "" | _ -> t.v.draft in
          let t =
            {
              t with
              selected = Some a.ids.conversation_id;
              stop_request = None;
              v = { t.v with pending = None; draft; stopping = false };
            }
          in
          fetch t a.ids.conversation_id)
  | Snapshot (tok, result) when current t t.snapshot_request tok -> (
      let t = { t with snapshot_request = None } in
      match result with
      | Error e -> failure { t with v = { t.v with connection = Disconnected } } e
      | Ok s when Some s.conversation_id = t.selected -> (
          let snapshot_notice =
            match List.rev (attempts s) with
            | a :: _ -> (
                match a.state with
                | W.Completed -> Answer_completed
                | W.Cancelled -> Answer_stopped
                | W.Failed _ -> Answer_failed
                | _ -> Answer_running)
            | [] -> Quiet
          in
          let t = { t with v = { t.v with snapshot = Some s; connection = Disconnected; notice = snapshot_notice } } in
          match s.active_run with
          | None -> (t, [])
          | Some run -> (
              match
                List.find_opt (fun (a : W.attempt) -> a.ids.run_id = run && not (terminal a.state)) (attempts s)
              with
              | None -> notice t Invalid_state
              | Some a ->
                  let t, tok = fresh t in
                  ( { t with stream = Some (tok, a.ids); v = { t.v with connection = Connecting } },
                    [ Subscribe (tok, a.ids, a.last_sequence) ] )))
      | Ok _ -> (t, []))
  | Stream_invalid tok when stream_current t tok ->
      let t = flush t in
      ( {
          t with
          stream = None;
          buffer = [];
          bytes = 0;
          v = { t.v with connection = Disconnected; notice = Request_failed };
        },
        clear_commands @ [ Announce_status Request_failed ] )
  | Event (tok, e) when stream_current t tok -> (
      match t.stream with
      | Some (_, ids) when ids = e.ids -> (
          match find_attempt t ids with
          | None -> (t, [])
          | Some a ->
              let last = match List.rev t.buffer with x :: _ -> x.W.sequence | [] -> a.last_sequence in
              let seq = Int64.of_string e.sequence and prev = Int64.of_string last in
              if Int64.compare seq prev <= 0 || terminal a.state then (t, [])
              else if seq <> Int64.succ prev then resync (flush t)
              else if
                match (e.payload, t.v.snapshot) with
                | W.Evidence_added e, Some s -> e.scope <> s.scope || e.currency <> s.currency
                | _ -> false
              then resync (flush t)
              else
                let size = String.length (W.encode_event e) in
                if t.bytes + size > 65536 || List.length t.buffer >= 128 then resync (flush t)
                else
                  let is_terminal =
                    match e.payload with W.Run_completed | W.Run_cancelled | W.Run_failed _ -> true | _ -> false
                  in
                  let first = t.buffer = [] in
                  let t = { t with buffer = t.buffer @ [ e ]; bytes = t.bytes + size } in
                  if is_terminal then
                    let t = flush t in
                    let message =
                      match e.payload with
                      | W.Run_completed -> Answer_completed
                      | W.Run_cancelled -> Answer_stopped
                      | _ -> Answer_failed
                    in
                    ( {
                        t with
                        stream = None;
                        v = { t.v with connection = Disconnected; stopping = false; notice = message };
                      },
                      [ Unsubscribe; Cancel_flush; Announce_status message ] )
                  else (t, if first then [ Schedule_flush tok ] else []))
      | _ -> (t, []))
  | Flush tok when stream_current t tok -> (flush t, [])
  | Background tok when stream_current t tok ->
      let t = flush t in
      ({ t with suspended = Some tok; stream = None; v = { t.v with connection = Disconnected } }, clear_commands)
  | Foreground tok when t.active && t.suspended = Some tok -> resync { t with suspended = None }
  | Resync tok when stream_current t tok -> resync (flush t)
  | Connection (tok, Reconnecting) when stream_current t tok -> resync (flush t)
  | Connection (tok, connection) when stream_current t tok -> ({ t with v = { t.v with connection } }, [])
  | Reconnect when t.active -> resync (flush t)
  | Stop when t.active && not t.v.stopping -> (
      match t.v.snapshot with
      | None -> (t, [])
      | Some s -> (
          match List.find_opt (fun (a : W.attempt) -> Some a.ids.run_id = s.active_run) (attempts s) with
          | None -> (t, [])
          | Some a ->
              let t, tok = fresh t in
              ( { t with stop_request = Some tok; v = { t.v with stopping = true; notice = Stopping } },
                [ Cancel_run (tok, a.ids) ] )))
  | Stopped (tok, result) when current t t.stop_request tok -> (
      let t = { t with stop_request = None; v = { t.v with stopping = false } } in
      match result with Error e -> failure t e | Ok _ -> resync (flush t))
  | Delete when t.active -> (
      match t.selected with
      | None -> (t, [])
      | Some id ->
          let t, tok = fresh t in
          ({ t with delete_request = Some tok; delete_target = Some id }, [ Delete_conversation (tok, id) ]))
  | Delete_owned id when t.active ->
      let t, tok = fresh t in
      ({ t with delete_request = Some tok; delete_target = Some id }, [ Delete_conversation (tok, id) ])
  | Deleted (tok, result) when current t t.delete_request tok -> (
      let target = t.delete_target in
      let t = { t with delete_request = None; delete_target = None } in
      match result with
      | Error Access_lost when t.selected <> target -> notice t Request_failed
      | Error e -> failure t e
      | Ok () -> if t.selected = target then (clear t, clear_commands) else (t, []))
  | Consent (enabled, household) when t.active ->
      let t, tok = fresh t in
      ({ t with consent_request = Some tok }, [ Set_consent (tok, enabled, household) ])
  | Consent_result (tok, result) when current t t.consent_request tok -> (
      match result with
      | Error e -> failure { t with consent_request = None } e
      | Ok () ->
          let t = clear t in
          let t, tok = fresh t in
          ({ t with status_request = Some tok }, clear_commands @ [ Get_status tok ]))
  | Status (tok, result) when current t t.status_request tok -> (
      match result with
      | Error e -> failure { t with status_request = None } e
      | Ok status -> ({ t with status_request = None; v = { t.v with status = Some status } }, []))
  | History (tok, result) when current t t.history_request tok -> (
      match result with
      | Error e -> failure { t with history_request = None } e
      | Ok history -> (
          let latest = t.latest in
          let t = { t with latest = false; history_request = None; v = { t.v with history } } in
          match (latest, history) with
          | true, h :: _ ->
              fetch
                {
                  t with
                  write_request = None;
                  stop_request = None;
                  delete_request = None;
                  status_request = None;
                  consent_request = None;
                  selected = Some h.conversation_id;
                  context = None;
                  suspended = None;
                  v = { blank with draft = t.v.draft; status = t.v.status; history };
                }
                h.conversation_id
          | true, [] ->
              let t = clear t in
              ({ t with v = { t.v with notice = Choose_context } }, clear_commands)
          | _ -> (t, [])))
  | _ -> (t, [])

let command_is_current t = function
  | Post_turn (tok, _) | Retry_turn (tok, _) -> current t t.write_request tok
  | Fetch_snapshot (tok, _) -> current t t.snapshot_request tok
  | Cancel_run (tok, _) -> current t t.stop_request tok
  | Delete_conversation (tok, _) -> current t t.delete_request tok
  | Get_status tok -> current t t.status_request tok
  | Set_consent (tok, _, _) -> current t t.consent_request tok
  | List_conversations (tok, _, _) -> current t t.history_request tok
  | Subscribe (tok, ids, _) -> (
      stream_current t tok && match t.stream with Some (_, source) -> source = ids | None -> false)
  | Schedule_flush tok -> stream_current t tok
  | Announce_status n -> t.active && t.v.notice = n
  | Unsubscribe | Cancel_flush -> true
