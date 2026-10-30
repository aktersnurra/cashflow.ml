open Cashflow_core
module A = Advisor
module E = Cashflow_workflow.Advisor_effects
module W = Cashflow_workflow.Advisor_workflow
module P = Advisor_provider
module X = Advisor_access
module S = Advisor_store

let ( let* ) = Result.bind
let error code = A.{ code; context = No_context }
let limit kind = A.{ code = Run_limit_reached; context = Limit kind }

exception Stopped of A.error option
exception Round_closed

type clock = { now : unit -> Ptime.t; monotonic_s : unit -> float; sleep_s : float -> unit }
type delivery = Events of A.event list | Heartbeat | Resync_required of int64 | End

type t = {
  sw : Eio.Switch.t;
  service : Service.t;
  provider : P.t;
  store : S.t;
  clock : clock;
  registrations : (A.Run_id.t, registration) Hashtbl.t;
  changed : Eio.Condition.t;
  mutable subscribers : subscription list;
  mutable handoffs : int;
}

and registration = {
  accepted : A.accepted;
  principal : X.principal;
  owner : User_id.t;
  mutable started : bool;
  mutable sealed : bool;
  mutable run_switch : Eio.Switch.t option;
  mutable pending : A.event_payload list;
  mutable pending_bytes : int;
  mutable pending_since : float;
  mutable flush_scheduled : bool;
  mutable round : round option;
  mutable prepared : P.prepared_request option;
  mutable aliases : Advisor_projection.aliases option;
  deadline : float;
  mutable absent_since : float option;
  mutable problem : A.error option;
}

and round = {
  round_changed : Eio.Condition.t;
  mutable queue : (E.model_event * int) list;
  mutable bytes : int;
  mutable ended : bool;
  mutable failure : A.error option;
  mutable round_switch : Eio.Switch.t option;
  mutable round_closed : bool;
  mutable activity : float;
}

and subscription = {
  runtime : t;
  subscriber : X.principal;
  run : A.Run_id.t;
  mutable cursor : int64;
  mutable closed : bool;
  mutable busy : bool;
  mutable overflow : bool;
}

let protect t f =
  try Sql.transaction_result t.service.db f with Sql.Sql_error _ -> Error (error A.Storage_unavailable)

let find t r = S.find_run t.store ~owner:r.owner r.accepted.ids.run_id

let authority t r =
  let* run = X.authorize_run t.service ~subscriber:r.principal ~run:r.accepted.ids.run_id in
  if r.sealed || run.state = A.Cancel_requested || A.is_terminal run.state then Error (error A.Invalid_transition)
  else Ok run

let checked t r f =
  protect t (fun () ->
      let* run = authority t r in
      if
        t.clock.monotonic_s () >= r.deadline
        || Ptime.Span.to_float_s (Ptime.diff (t.clock.now ()) run.started_at) >= 120.
      then Error (limit A.Run_deadline)
      else f run)

let unread_bytes t run after =
  try
    Sql.read_snapshot t.service.db (fun () ->
        Ok
          (Sql.(
             run t.service.db
               (stmt
                  "SELECT coalesce(sum(length(CAST(projection AS BLOB))),0) FROM advisor_events WHERE run_id=? AND \
                   sequence>?"
                  (text @-> int64 @-> nil)
                  (one (row (int @-> nil) Fun.id))))
             (A.Run_id.to_string run) after))
  with Sql.Sql_error _ -> Error (error A.Storage_unavailable)

let publish t run _events =
  (* Notifications coalesce; durable bytes, including catch-up, define backlog. *)
  List.iter
    (fun (s : subscription) ->
      if (not s.closed) && s.run = run then
        match unread_bytes t run s.cursor with
        | Error _ -> ()
        | Ok bytes -> if bytes > A.limits.subscriber_bytes then s.overflow <- true)
    t.subscribers;
  Eio.Condition.broadcast t.changed

let notify t = Eio.Condition.broadcast t.changed

let transition t r change =
  let result = S.transition t.store ~run:r.accepted.ids.run_id change in
  (match result with Ok _ -> notify t | Error _ -> ());
  result

let flush_events t r ~draining =
  protect t (fun () ->
      let batch = r.pending in
      if batch = [] then Ok []
      else
        let* run = if draining then find t r else authority t r in
        r.pending <- [];
        r.pending_bytes <- 0;
        let append = if draining then S.flush_pending else S.append in
        append t.store ~run:run.ids.run_id ~expected_sequence:run.counters.last_sequence batch)

let flush t r ~draining =
  match flush_events t r ~draining with
  | Error e ->
      r.problem <- Some e;
      Error e
  | Ok events ->
      publish t r.accepted.ids.run_id events;
      Ok ()

let close_round r =
  match r.round with
  | None -> ()
  | Some round ->
      r.round <- None;
      round.round_closed <- true;
      round.queue <- [];
      round.bytes <- 0;
      Eio.Condition.broadcast round.round_changed;
      Option.iter (fun sw -> Eio.Switch.fail sw Round_closed) round.round_switch

let stop t r reason =
  r.sealed <- true;
  (match reason with Some _ -> r.problem <- reason | None -> ());
  close_round r;
  Option.iter (fun sw -> Eio.Switch.fail sw (Stopped reason)) r.run_switch;
  notify t

let cancel_transaction t r =
  r.sealed <- true;
  let* events = flush_events t r ~draining:true in
  let* run = find t r in
  let* state =
    if A.is_terminal run.state || run.state = A.Cancel_requested then Ok run.state
    else S.transition t.store ~run:run.ids.run_id A.Request_cancel
  in
  Ok (state, events)

let request_cancel t r =
  let* state, events = protect t (fun () -> cancel_transaction t r) in
  publish t r.accepted.ids.run_id events;
  Ok state

let signal_cancel t ids =
  List.iter (fun id -> Option.iter (fun r -> stop t r None) (Hashtbl.find_opt t.registrations id)) ids;
  notify t

let fresh_segment =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) in
  fun () -> Option.get (A.Segment_id.of_string (Uuidm.to_string (uuid ())))

let request_of_context (c : E.context) : P.request =
  let call (c : E.raw_call) : P.raw_call = { id = c.id; name = c.name; arguments = c.arguments; index = c.index } in
  let message = function
    | E.User s -> P.User s
    | E.Assistant (s, cs) -> P.Assistant (s, List.map call cs)
    | E.Tool { call_id; content } -> P.Tool { call_id; content }
  in
  (* Selection metadata is model-facing; internal identities never are. *)
  let source =
    match c.source_finding with None -> "" | Some f -> "\nSelected finding: " ^ f.title ^ "\n" ^ f.summary
  in
  {
    system =
      c.system_instructions ^ source
      ^ Printf.sprintf "\nHistory: %d included, %d omitted. Locale: %s; currency: %s." c.selection.selected_turns
          c.selection.omitted_turns c.locale (Currency.code c.currency);
    messages = List.map message c.messages;
  }

let load_context t r =
  let* snapshot, history, user_text =
    checked t r (fun run ->
        let* snapshot = S.snapshot t.store ~owner:r.owner run.ids.conversation_id in
        let* history = W.select_history snapshot in
        match List.find_opt (fun (turn : A.turn) -> turn.turn_id = run.ids.turn_id) snapshot.turns with
        | None -> Error (error A.Not_found)
        | Some turn -> Ok (snapshot, history, turn.user_text))
  in
  let* temporal_context =
    checked t r (fun _ ->
        let* access = Advisor_access.access t.service r.principal in
        Ok (Printf.sprintf "Current local date: %s; timezone: %s. Resolve relative dates from this date; ask if the intended year is unclear."
          (Cashflow_core.Date.to_string (Service.today t.service access))
          (Cashflow_core.Access.user access).timezone))
  in
  let* aliases =
    Advisor_projection.create_aliases t.service ~principal:r.principal ~scope:snapshot.scope
      ~conversation:snapshot.conversation_id
  in
  r.aliases <- Some aliases;
  (* Personal notes belong to personal conversations; a household conversation never carries them. *)
  let* notes =
    match snapshot.scope with
    | A.Personal -> Result.map (List.map (fun (m : S.memory) -> m.text)) (S.memories t.store ~owner:r.owner)
    | A.Household _ -> Ok []
  in
  let successful =
    List.fold_left
      (fun n (turn : A.turn) ->
        if List.exists (fun (a : A.attempt) -> a.state = A.Completed) turn.attempts then n + 1 else n)
      0 snapshot.turns
  in
  let context_token_budget = Option.map (fun (l : P.generation_limits) -> l.context_tokens) (P.limits t.provider) in
  Ok
    E.
      {
        system_instructions = temporal_context ^ W.memory_instructions notes;
        history;
        selection = { selected_turns = List.length history; omitted_turns = successful - List.length history };
        user_text;
        messages = [];
        source_finding = snapshot.source_finding;
        evidence = [];
        locale = snapshot.locale;
        currency = snapshot.currency;
        model_aliases = [];
        context_token_budget;
        segment_id = fresh_segment ();
      }

(* Charge envelopes as well as strings: tiny deltas must not create an
   unbounded number of retained queue cells below a payload-only byte cap. *)
let event_size = function
  | E.Text text -> 128 + String.length text
  | E.Round_finished _ -> 128
  | E.Complete_calls cs ->
      List.fold_left
        (fun n (c : E.raw_call) -> n + String.length c.id + String.length c.name + String.length c.arguments + 256)
        128 cs

let model_event = function
  | P.Text s -> E.Text s
  | P.Calls_ready cs ->
      E.Complete_calls
        (List.map (fun (c : P.raw_call) -> E.{ id = c.id; name = c.name; arguments = c.arguments; index = c.index }) cs)
  | P.Round_finished f ->
      E.Round_finished
        (match f with
        | P.Stop -> E.Stop
        | P.Tool_calls -> E.Tool_calls
        | P.Length -> E.Length
        | P.Content_filter -> E.Content_filter)

let authorized_subscribers t run =
  List.exists
    (fun (s : subscription) ->
      if s.closed || s.overflow || s.run <> run then false
      else match X.authorize_run t.service ~subscriber:s.subscriber ~run with Ok _ -> true | Error _ -> false)
    t.subscribers

let abandoned t r =
  if authorized_subscribers t r.accepted.ids.run_id then (
    r.absent_since <- None;
    false)
  else
    let now = t.clock.monotonic_s () in
    match r.absent_since with
    | None ->
        r.absent_since <- Some now;
        false
    | Some since -> now -. since >= 30.

let begin_round t r sw =
  let* _ = checked t r (fun run -> Ok run) in
  match r.prepared with
  | None -> Error (error A.Invalid_request)
  | Some prepared ->
      let round =
        {
          round_changed = Eio.Condition.create ();
          queue = [];
          bytes = 0;
          ended = false;
          failure = None;
          round_switch = None;
          round_closed = false;
          activity = t.clock.monotonic_s ();
        }
      in
      r.round <- Some round;
      Eio.Fiber.fork ~sw (fun () ->
          (try
             Eio.Switch.run (fun scope ->
                 round.round_switch <- Some scope;
                 let result =
                   let* _ = checked t r (fun run -> Ok run) in
                   if abandoned t r then (
                     stop t r None;
                     raise Round_closed);
                   P.stream_prepared t.provider prepared
                     ~on_activity:(fun () ->
                       let authorized = checked t r (fun _ -> Ok ()) in
                       (match authorized with Ok () -> () | Error e -> raise (Stopped (Some e)));
                       if round.round_closed then raise Round_closed;
                       let now = t.clock.monotonic_s () in
                       if now -. round.activity >= 30. then raise (Stopped (Some (limit A.Upstream_inactivity)));
                       round.activity <- now)
                     ~on_event:(fun event ->
                       let event = model_event event in
                       let bytes = event_size event in
                       if bytes > A.limits.subscriber_bytes then raise (Stopped (Some (limit A.Frame_bytes)));
                       Eio.Condition.loop_no_mutex round.round_changed (fun () ->
                           if round.round_closed || r.sealed then raise Round_closed;
                           if round.bytes + bytes > A.limits.subscriber_bytes then None else Some ());
                       let admitted =
                         checked t r (fun _ ->
                             if round.round_closed then Error (error A.Invalid_transition)
                             else (
                               round.queue <- round.queue @ [ (event, bytes) ];
                               round.bytes <- round.bytes + bytes;
                               Ok ()))
                       in
                       (match admitted with Ok () -> () | Error e -> raise (Stopped (Some e)));
                       Eio.Condition.broadcast round.round_changed)
                 in
                 match result with Ok _ -> () | Error e -> round.failure <- Some e)
           with
          | Round_closed -> ()
          | Stopped reason -> round.failure <- reason
          | Eio.Cancel.Cancelled _ as ex -> raise ex
          | _ -> round.failure <- Some (error A.Provider_unavailable));
          round.round_switch <- None;
          round.ended <- true;
          Eio.Condition.broadcast round.round_changed);
      Ok ()

let read_round t r =
  match r.round with
  | None -> Error (error A.Invalid_transition)
  | Some round ->
      Eio.Condition.loop_no_mutex round.round_changed (fun () ->
          match checked t r (fun _ -> Ok ()) with
          | Error e -> Some (Error e)
          | Ok () -> (
              match round.queue with
              | (event, bytes) :: rest ->
                  round.queue <- rest;
                  round.bytes <- round.bytes - bytes;
                  Eio.Condition.broadcast round.round_changed;
                  Some (Ok (Some event))
              | [] when round.ended -> Some (match round.failure with Some e -> Error e | None -> Ok None)
              | [] -> None))

let schedule_flush t r =
  if not r.flush_scheduled then
    match r.run_switch with
    | None -> ()
    | Some sw ->
        r.flush_scheduled <- true;
        Eio.Fiber.fork_daemon ~sw (fun () ->
            let rec loop () =
              if r.pending = [] then r.flush_scheduled <- false
              else (
                t.clock.sleep_s (max 0. (r.pending_since +. 0.1 -. t.clock.monotonic_s ()));
                (match flush t r ~draining:false with Ok () -> () | Error e -> stop t r (Some e));
                loop ())
            in
            loop ();
            `Stop_daemon)

let append t r payloads =
  let rec loop = function
    | [] -> Ok ()
    | A.Text_appended delta :: rest ->
        (* Bound individual pending chunks as well as aggregate batching. *)
        let rec text offset =
          if offset = String.length delta.text then Ok ()
          else
            let size = min (A.limits.flush_bytes - r.pending_bytes) (String.length delta.text - offset) in
            if size = 0 then
              let* () = flush t r ~draining:false in
              text offset
            else
              (* Cut only on UTF-8 boundaries. Incoming text itself is validated by the store. *)
              let end_ = ref (offset + size) in
              while
                !end_ < String.length delta.text && !end_ > offset && Char.code delta.text.[!end_] land 0xc0 = 0x80
              do
                decr end_
              done;
              if !end_ = offset then
                let* () = flush t r ~draining:false in
                text offset
              else
                let piece = String.sub delta.text offset (!end_ - offset) in
                let* () =
                  checked t r (fun _ ->
                      if r.pending = [] then r.pending_since <- t.clock.monotonic_s ();
                      r.pending <-
                        (match r.pending with
                        | [ A.Text_appended prior ] when prior.segment_id = delta.segment_id ->
                            [ A.Text_appended { prior with text = prior.text ^ piece } ]
                        | _ -> r.pending @ [ A.Text_appended { delta with text = piece } ]);
                      r.pending_bytes <- r.pending_bytes + String.length piece;
                      Ok ())
                in
                schedule_flush t r;
                let* () = if r.pending_bytes >= A.limits.flush_bytes then flush t r ~draining:false else Ok () in
                text !end_
        in
        let* () = text 0 in
        loop rest
    | payload :: rest ->
        let* () = flush t r ~draining:false in
        let* events =
          checked t r (fun run ->
              S.append t.store ~run:run.ids.run_id ~expected_sequence:run.counters.last_sequence [ payload ])
        in
        publish t r.accepted.ids.run_id events;
        loop rest
  in
  loop payloads

(* A tool that proposes a change records it, so the user's later click has something to apply. The proposal
   outlives the run (24 hours) and holds only typed values, never model prose. *)
let remember_proposal t r (run : A.run) (evidence : Cashflow_workflow.Advisor_tools.evidence) =
  match evidence.result with
  | Budget_proposal_result p ->
      let now = t.clock.now () in
      let id = A.Evidence_id.to_string evidence.id in
      S.add_proposal t.store ~owner:r.owner ~conversation_id:run.ids.conversation_id ~id ~kind:"set_budget"
        ~arguments:
          (`Assoc
            [
              ("category_id", `String (Cashflow_core.Account_id.to_string p.category.id));
              ("monthly_amount", `String (Int64.to_string p.proposed));
              ("rollover", `Bool p.rollover);
            ])
        ~precondition:
          (`Assoc
            [
              ("current_monthly", match p.current with Some n -> `String (Int64.to_string n) | None -> `Null);
              ("current_rollover", match p.current_rollover with Some b -> `Bool b | None -> `Null);
              ("from_cycle", `String p.from_cycle);
            ])
        ~now ~expires:(Option.value (Ptime.add_span now (Ptime.Span.of_int_s 86400)) ~default:now)
  | Proposal_card_result card ->
      let now = t.clock.now () in
      S.add_proposal t.store ~owner:r.owner ~conversation_id:run.ids.conversation_id ~id:(A.Evidence_id.to_string evidence.id) ~kind:card.kind
        ~arguments:(Yojson.Safe.from_string card.arguments) ~precondition:(Yojson.Safe.from_string card.precondition) ~now
        ~expires:(Option.value (Ptime.add_span now (Ptime.Span.of_int_s 86400)) ~default:now)
  | Categorize_proposal_result rows ->
      let now = t.clock.now () in
      let id = A.Evidence_id.to_string evidence.id in
      S.add_proposal t.store ~owner:r.owner ~conversation_id:run.ids.conversation_id ~id ~kind:"categorize"
        ~arguments:
          (`Assoc
            [
              ( "rows",
                `List
                  (List.map
                     (fun (x : Cashflow_workflow.Advisor_tools.categorize_row) ->
                       `Assoc
                         [
                           ("transfer_id", `String (Cashflow_core.Transfer_id.to_string x.transfer_id));
                           ("category_id", `String (Cashflow_core.Account_id.to_string x.category.id));
                         ])
                     rows) );
            ])
        ~precondition:
          (`Assoc
            [
              ( "rows",
                `List
                  (List.map
                     (fun (x : Cashflow_workflow.Advisor_tools.categorize_row) ->
                       `Assoc
                         [
                           ("transfer_id", `String (Cashflow_core.Transfer_id.to_string x.transfer_id));
                           ("revision", `Int x.revision);
                           ("current", `String (Cashflow_core.Account_id.to_string x.current));
                         ])
                     rows) );
            ])
        ~now ~expires:(Option.value (Ptime.add_span now (Ptime.Span.of_int_s 86400)) ~default:now)
  | _ -> Ok ()

let run_handler t r sw : E.handler =
  {
    handle =
      (fun (type a) (op : a E.operation) : a ->
        match op with
        | E.Check_authority -> Result.map (fun _ -> ()) (checked t r (fun run -> Ok run))
        | E.Now -> t.clock.now ()
        | E.Check_cancelled ->
            (r.sealed && r.problem = None)
            || Eio.Cancel.protect (fun () ->
                match find t r with Ok run -> run.state = A.Cancel_requested | Error _ -> true)
        | E.Load_context -> load_context t r
        | E.Allocate_segment -> checked t r (fun _ -> Ok (fresh_segment ()))
        | E.Measure_context c ->
            let* _ = checked t r (fun run -> Ok run) in
            let* prepared = P.prepare t.provider (request_of_context c) in
            let* limits = match P.limits t.provider with Some l -> Ok l | None -> Error (error A.Invalid_request) in
            r.prepared <- Some prepared;
            Ok
              E.
                {
                  serialized_bytes = P.serialized_bytes prepared;
                  conservative_tokens = P.conservative_tokens prepared;
                  output_reserve_tokens = limits.completion_tokens;
                }
        | E.Begin_model_round _ -> begin_round t r sw
        | E.Read_model_event -> read_round t r
        | E.Close_model_round -> close_round r
        | E.Parse_tool call ->
            let* _ = checked t r (fun run -> Ok run) in
            Advisor_projection.parse_call ~name:call.name ~arguments:call.arguments
        | E.Read_tool request -> (
            let* scope, currency =
              checked t r (fun run ->
                  let* s = S.metadata t.store ~owner:r.owner run.ids.conversation_id in
                  Ok (s.scope, s.currency))
            in
            match r.aliases with
            | None -> Error (error A.Invalid_request)
            | Some aliases -> (
                (* Filing asks the classifier: network I/O, so it runs before the run's transaction, holding none.
                   What it says is checked again row by row when the rows are filed, inside it. *)
                let* classified =
                  match request with
                  | Cashflow_workflow.Advisor_tools.File_inbox _ -> (
                      let owner =
                        try
                          Sql.read_snapshot t.service.db (fun () ->
                              let* () = Advisor_access.authorize_scope t.service r.principal scope in
                              let* access = Advisor_access.access t.service r.principal in
                              Ok (access, match scope with A.Personal -> Access.personal access | Household h -> Owner.Household h))
                        with Sql.Sql_error _ -> Error (error A.Storage_unavailable)
                      in
                      match owner with
                      | Ok (access, owner) -> Ok (Some (Autofile.classify t.service access ~owner))
                      | Error e -> Error e)
                  | _ -> Ok None
                in
                checked t r (fun run ->
                    let* filed =
                      match request, classified with
                      | Cashflow_workflow.Advisor_tools.File_inbox { keep_on }, Some c ->
                          let* access = Advisor_access.access t.service r.principal in
                          Ok (Some (Autofile.file t.service access ~keep_on c))
                      | _ -> Ok None
                    in
                    let* evidence = Advisor_projection.read ?filed t.service ~principal:r.principal ~scope ~aliases ~currency request in
                    let* () = remember_proposal t r run evidence in
                    Ok evidence)))
        | E.Select_evidence evidence -> (
            let* _ = checked t r (fun run -> Ok run) in
            match r.aliases with
            | None -> Error (error A.Invalid_request)
            | Some aliases ->
                let* json = Advisor_projection.model_json ~aliases evidence in
                let model_content = Yojson.Safe.to_string json in
                let* _ = checked t r (fun run -> Ok run) in
                Ok
                  E.
                    {
                      model_content;
                      display = Advisor_projection.display evidence;
                      serialized_bytes = String.length model_content;
                    })
        | E.Append_events payloads -> append t r payloads
        | E.Commit_terminal change ->
            Eio.Cancel.protect (fun () ->
                let change = match (change, r.problem) with A.Fail _, Some e -> A.Fail e | _ -> change in
                let cleanup = match change with A.Fail _ | A.Confirm_cancel -> true | _ -> false in
                let* () = flush t r ~draining:cleanup in
                if cleanup then transition t r change
                else
                  let* state = checked t r (fun run -> S.transition t.store ~run:run.ids.run_id change) in
                  notify t;
                  Ok state)
        | E.Request_cancel _ -> Eio.Cancel.protect (fun () -> request_cancel t r)
        | E.Persist_turn _ -> Error (error A.Invalid_request)
        | E.Persist_retry _ -> Error (error A.Invalid_request)
        | E.Delete_owned_conversation _ -> Error (error A.Invalid_request)
        | E.Start_run _ -> ());
  }

let finish_stopped t r reason =
  Eio.Cancel.protect (fun () ->
      r.sealed <- true;
      close_round r;
      ignore (flush t r ~draining:true);
      match find t r with
      | Error _ -> ()
      | Ok run when A.is_terminal run.state -> ()
      | Ok run -> (
          let change =
            if run.state = A.Cancel_requested then A.Confirm_cancel
            else match (r.problem, reason) with Some e, _ | _, Some e -> A.Fail e | None, None -> A.Request_cancel
          in
          match transition t r change with Ok A.Cancel_requested -> ignore (transition t r A.Confirm_cancel) | _ -> ()))

let watchdog t r =
  let rec loop () =
    (* Re-read the real expiry: ordinary authenticated activity may extend it, but
     these inspections never do. Wake no later than any known deadline. *)
    let expiry = match X.expiry t.service r.principal with Ok expiry -> expiry | Error _ -> raise (Stopped None) in
    let now = t.clock.monotonic_s () in
    let delay = min 0.05 (min (r.deadline -. now) (Ptime.Span.to_float_s (Ptime.diff expiry (t.clock.now ())))) in
    let delay = match r.round with None -> delay | Some round -> min delay (round.activity +. 30. -. now) in
    let delay = match r.absent_since with None -> delay | Some since -> min delay (since +. 30. -. now) in
    if delay > 0. then t.clock.sleep_s delay else Eio.Fiber.yield ();
    let run = match find t r with Ok run -> run | Error e -> raise (Stopped (Some e)) in
    if A.is_terminal run.state then ()
    else (
      if run.state = A.Cancel_requested then raise (Stopped None);
      (match checked t r (fun _ -> Ok ()) with
      | Ok () -> ()
      | Error ({ A.context = A.Limit A.Run_deadline; _ } as e) -> raise (Stopped (Some e))
      | Error _ -> raise (Stopped None));
      let now = t.clock.monotonic_s () in
      if now >= r.deadline || Ptime.Span.to_float_s (Ptime.diff (t.clock.now ()) run.started_at) >= 120. then
        raise (Stopped (Some (limit A.Run_deadline)));

      if abandoned t r then raise (Stopped None);

      (match r.round with
      | Some round when now -. round.activity >= 30. -> raise (Stopped (Some (limit A.Upstream_inactivity)))
      | _ -> ());
      loop ())
  in
  loop ()

let start t (accepted : A.accepted) =
  match Hashtbl.find_opt t.registrations accepted.ids.run_id with
  | None -> ()
  | Some r when r.started -> ()
  | Some r ->
      r.started <- true;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
          let reason = ref None in
          (try
             Eio.Switch.run (fun sw ->
                 r.run_switch <- Some sw;
                 if abandoned t r then raise (Stopped None);
                 Eio.Fiber.fork_daemon ~sw (fun () ->
                     (try watchdog t r with Stopped reason -> stop t r reason);
                     `Stop_daemon);
                 match find t r with
                 | Error e -> reason := Some e
                 | Ok run -> (
                     match E.run (run_handler t r sw) (fun () -> W.execute ~run) with
                     | Ok _ -> ()
                     | Error e -> reason := Some e))
           with
          | Stopped e -> reason := e
          | Eio.Cancel.Cancelled _ as ex ->
              finish_stopped t r None;
              raise ex
          | _ -> reason := Some (error A.Provider_unavailable));
          r.run_switch <- None;
          finish_stopped t r !reason;
          Hashtbl.remove t.registrations r.accepted.ids.run_id;
          notify t)

let register t principal (accepted : A.accepted) =
  let result =
    protect t (fun () ->
        let* user = X.user t.service principal in
        let* session = X.session t.service principal in
        let* run = S.find_run t.store ~owner:user.id accepted.A.ids.run_id in
        if Hashtbl.mem t.registrations run.ids.run_id then Ok None
        else if run.state = A.Cancel_requested then
          let* _ = S.transition t.store ~run:run.ids.run_id A.Confirm_cancel in
          Ok None
        else if run.state <> A.Queued then Ok None
        else if run.originating_session <> session then
          (* B cannot mint A or classify A's concurrent handoff as orphaned. *)
          Ok None
        else
          let r =
            {
              accepted = { accepted with ids = run.ids };
              principal;
              owner = user.id;
              started = false;
              sealed = false;
              run_switch = None;
              pending = [];
              pending_bytes = 0;
              pending_since = 0.;
              flush_scheduled = false;
              round = None;
              prepared = None;
              aliases = None;
              deadline =
                t.clock.monotonic_s ()
                +. max 0. (120. -. Ptime.Span.to_float_s (Ptime.diff (t.clock.now ()) run.started_at));
              (* The initial grace starts at acceptance, not this shielded handoff. *)
              absent_since =
                Some
                  (t.clock.monotonic_s () -. max 0. (Ptime.Span.to_float_s (Ptime.diff (t.clock.now ()) run.started_at)));
              problem = None;
            }
          in
          Ok (Some r))
  in
  let* registration = result in
  (* Bookkeeping follows commit while the in-flight marker protects the handoff.
     A racing Stop is read again by the one-shot worker. *)
  Option.iter
    (fun r ->
      if not (Hashtbl.mem t.registrations r.accepted.ids.run_id) then
        Hashtbl.add t.registrations r.accepted.ids.run_id r)
    registration;
  notify t;
  Ok accepted

let persist ~available t principal operation =
  (* Shield the commit-registration-start handoff; no request cancellation gap. *)
  Eio.Cancel.protect (fun () ->
      t.handoffs <- t.handoffs + 1;
      Fun.protect
        ~finally:(fun () -> t.handoffs <- t.handoffs - 1)
        (fun () ->
          let* accepted =
            protect t (fun () ->
                let* user = X.user t.service principal in
                let* session = X.session t.service principal in
                operation user session)
          in
          let registered = if not available then Ok accepted else register t principal accepted in
          (match registered with
          | Ok a -> if available then start t a
          | Error e ->
              ignore
                (protect t (fun () ->
                     if Hashtbl.mem t.registrations accepted.ids.run_id then Ok ()
                     else
                       let* runs = S.active_runs t.store in
                       match List.find_opt (fun (run : A.run) -> run.ids.run_id = accepted.ids.run_id) runs with
                       | None -> Ok ()
                       | Some run -> (
                           let change =
                             match run.state with
                             | A.Cancel_requested -> Some A.Confirm_cancel
                             | A.Queued -> Some (A.Fail e)
                             | _ -> None
                           in
                           match change with
                           | None -> Ok ()
                           | Some change -> Result.map (fun _ -> ()) (S.transition t.store ~run:run.ids.run_id change))));
              notify t);
          registered))

let cancel_live t principal id =
  let* state, events =
    protect t (fun () ->
        let* run = X.authorize_run t.service ~subscriber:principal ~run:id in
        if A.is_terminal run.state then Ok (run.state, [])
        else
          match Hashtbl.find_opt t.registrations id with
          | Some r -> cancel_transaction t r
          | None ->
              let* state =
                if run.state = A.Cancel_requested then Ok run.state else S.transition t.store ~run:id A.Request_cancel
              in
              Ok (state, []))
  in
  publish t id events;
  signal_cancel t [ id ];
  Ok state

let command_handler ?(available = true) t principal : E.handler =
  {
    handle =
      (fun (type a) (op : a E.operation) : a ->
        match op with
        | E.Check_authority -> Result.map (fun _ -> ()) (X.access t.service principal)
        | E.Persist_turn request ->
            persist ~available t principal (fun user session ->
                let* () = X.authorize_scope t.service principal request.scope in
                let resolve_source_finding () =
                  match request.source_finding_key with
                  | None -> Ok None
                  | Some key ->
                      Result.map Option.some
                        (Advisor_projection.resolve_finding t.service ~principal ~scope:request.scope ~key)
                in
                S.accept ~available ~resolve_source_finding t.store ~owner:user.id ~session ~now:(t.clock.now ())
                  ~model:(P.model t.provider) ~source_finding:None request)
        | E.Persist_retry request ->
            persist ~available t principal (fun user session ->
                let conversation =
                  Sql.(
                    run t.service.db
                      (stmt
                         "SELECT c.id FROM advisor_turns t JOIN advisor_conversations c ON c.id=t.conversation_id \
                          WHERE t.id=? AND c.owner_id=?"
                         (text @-> text @-> nil)
                         (opt (row (text @-> nil) Fun.id))))
                    (A.Turn_id.to_string request.turn_id) (User_id.to_string user.id)
                in
                let* conversation =
                  match conversation with
                  | Some id -> (
                      match A.Conversation_id.of_string id with
                      | Some id -> Ok id
                      | None -> Error (error A.Storage_unavailable))
                  | None -> Error (error A.Not_found)
                in
                let* () = X.authorize_history t.service principal conversation in
                S.retry ~available t.store ~owner:user.id ~session ~now:(t.clock.now ()) ~model:(P.model t.provider)
                  request)
        | E.Start_run accepted -> if available then start t accepted
        | E.Request_cancel id -> cancel_live t principal id
        | E.Delete_owned_conversation id ->
            let* ids = X.delete_conversation t.service principal id in
            signal_cancel t ids;
            notify t;
            Ok ()
        | E.Now -> t.clock.now ()
        | E.Check_cancelled -> false
        | E.Close_model_round -> ()
        | E.Load_context -> Error (error A.Invalid_request)
        | E.Allocate_segment -> Error (error A.Invalid_request)
        | E.Measure_context _ -> Error (error A.Invalid_request)
        | E.Begin_model_round _ -> Error (error A.Invalid_request)
        | E.Read_model_event -> Error (error A.Invalid_request)
        | E.Parse_tool _ -> Error (error A.Invalid_request)
        | E.Read_tool _ -> Error (error A.Invalid_request)
        | E.Select_evidence _ -> Error (error A.Invalid_request)
        | E.Append_events _ -> Error (error A.Invalid_request)
        | E.Commit_terminal _ -> Error (error A.Invalid_request));
  }

let submit ?(available = true) t ~principal request =
  E.run (command_handler ~available t principal) (fun () -> W.accept_turn request)

let retry ?(available = true) t ~principal request =
  E.run (command_handler ~available t principal) (fun () -> W.retry_turn request)

let cancel t ~principal id = E.run (command_handler t principal) (fun () -> W.cancel_run id)
let delete t ~principal id = E.run (command_handler t principal) (fun () -> W.delete_conversation id)

let close (s : subscription) =
  if not s.closed then (
    s.closed <- true;
    notify s.runtime);
  s.runtime.subscribers <- List.filter (fun other -> other != s) s.runtime.subscribers;
  if
    not
      (List.exists
         (fun (other : subscription) -> other.run = s.run && (not other.closed) && not other.overflow)
         s.runtime.subscribers)
  then
    Option.iter
      (fun r -> if r.absent_since = None then r.absent_since <- Some (s.runtime.clock.monotonic_s ()))
      (Hashtbl.find_opt s.runtime.registrations s.run)

let subscribe t ~principal ~run ~after =
  let* current = X.authorize_run t.service ~subscriber:principal ~run in
  if after < 0L || after > current.counters.last_sequence then Error (error A.Invalid_request)
  else
    let s = { runtime = t; subscriber = principal; run; cursor = after; closed = false; busy = false; overflow = false } in
    t.subscribers <- s :: t.subscribers;
    Option.iter (fun r -> r.absent_since <- None) (Hashtbl.find_opt t.registrations run);
    Ok s

let next (s : subscription) =
  if s.busy then Error (error A.Invalid_request)
  else (
    s.busy <- true;
    Fun.protect
      ~finally:(fun () -> s.busy <- false)
      (fun () ->
        let t = s.runtime in
        let poll () =
          if s.closed then Some (Ok End)
          else
            match X.authorize_run t.service ~subscriber:s.subscriber ~run:s.run with
            | Error e ->
                close s;
                Some (Error e)
            | Ok run -> (
                match unread_bytes t s.run s.cursor with
                | Error e ->
                    close s;
                    Some (Error e)
                | Ok bytes -> (
                    if s.overflow || bytes > A.limits.subscriber_bytes then (
                      close s;
                      Some (Ok (Resync_required run.counters.last_sequence)))
                    else
                      match S.replay t.store ~run:s.run ~after:s.cursor ~limit:1 with
                      | Error e ->
                          close s;
                          Some (Error e)
                      | Ok (S.Resync_required cursor) ->
                          close s;
                          Some (Ok (Resync_required cursor))
                      | Ok (S.Events (event :: _)) -> (
                          (* Authority rechecked after catch-up, immediately before handing detached bytes to the client. *)
                          match X.authorize_run t.service ~subscriber:s.subscriber ~run:s.run with
                          | Error e ->
                              close s;
                              Some (Error e)
                          | Ok _ ->
                              s.cursor <- event.sequence;
                              Some (Ok (Events [ event ])))
                      | Ok (S.Events []) when A.is_terminal run.state ->
                          close s;
                          Some (Ok End)
                      | Ok (S.Events []) -> None))
        in
        Eio.Fiber.first
          (fun () -> Eio.Condition.loop_no_mutex t.changed poll)
          (fun () ->
            t.clock.sleep_s 15.;
            match poll () with Some result -> result | None -> Ok Heartbeat)))

let create ~sw ~service ~provider ~clock =
  let service = { service with Service.now = clock.now } in
  let store = S.create ~clock:clock.now service.db in
  S.recover store ~now:(clock.now ());
  let t =
    {
      sw;
      service;
      provider;
      store;
      clock;
      registrations = Hashtbl.create 8;
      changed = Eio.Condition.create ();
      subscribers = [];
      handoffs = 0;
    }
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let next_prune = ref (clock.monotonic_s ()) in
      let sweep () =
        match S.active_runs store with
        | Error _ -> ()
        | Ok runs ->
            List.iter
              (fun (run : A.run) ->
                let changed =
                  protect t (fun () ->
                      if t.handoffs <> 0 || Hashtbl.mem t.registrations run.ids.run_id then Ok false
                      else
                        let* current = S.find_run store ~owner:run.owner_id run.ids.run_id in
                        if
                          current.state <> A.Queued
                          || Ptime.Span.to_float_s (Ptime.diff (clock.now ()) current.started_at) < 30.
                        then Ok false
                        else
                          Result.map
                            (fun _ -> true)
                            (S.transition store ~run:current.ids.run_id (A.Fail (error A.Invalid_request))))
                in
                (match changed with Ok true -> notify t | _ -> ());
                Eio.Fiber.yield ())
              runs
      in
      let rec maintenance () =
        sweep ();
        let more =
          if clock.monotonic_s () < !next_prune then false
          else
            match S.prune_batch store ~now:(clock.now ()) ~limit:64 with
            | Ok true -> true
            | Ok false | Error _ ->
                next_prune := clock.monotonic_s () +. 60.;
                false
        in
        if more then Eio.Fiber.yield () else clock.sleep_s 1.;
        maintenance ()
      in
      maintenance ());
  t
