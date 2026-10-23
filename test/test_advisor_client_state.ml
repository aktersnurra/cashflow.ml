module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

let ids : W.identities =
  {
    conversation_id = "c";
    turn_id = "t";
    attempt_id = "a";
    user_message_id = "u";
    assistant_message_id = "m";
    run_id = "r";
  }

let attempt : W.attempt = { ids; ordinal = "1"; state = W.Running W.Streaming_text; segments = []; last_sequence = "0" }

let snapshot : W.snapshot =
  {
    conversation_id = "c";
    scope = W.Personal;
    currency = { code = "SEK"; scale = 2 };
    locale = "en";
    source_finding = None;
    turns = [ { turn_id = "t"; user_message_id = "u"; user_text = "hej"; attempts = [ attempt ] } ];
    active_run = Some "r";
    version = "1";
  }

let apply s a = fst (C.reduce s a)

let opened () =
  let s = apply C.empty C.Activate in
  let s, cs = C.reduce s (C.Open_conversation "c") in
  let tok = List.find_map (function C.Fetch_snapshot (t, _) -> Some t | _ -> None) cs |> Option.get in
  let s, cs = C.reduce s (C.Snapshot (tok, Ok snapshot)) in
  let stream = List.find_map (function C.Subscribe (t, _, _) -> Some t | _ -> None) cs |> Option.get in
  (s, stream)

let event ?(ids = ids) n payload = { W.ids; sequence = string_of_int n; payload }

let text s =
  match (C.view s).snapshot with
  | None -> ""
  | Some s ->
      List.concat_map (fun (t : W.turn) -> t.attempts) s.turns
      |> List.concat_map (fun (a : W.attempt) -> a.segments)
      |> List.filter_map (function W.Text_segment d -> Some d.text | _ -> None)
      |> String.concat ""

let delta = W.Text_appended { segment_id = "s"; text = "héllo <img onerror=evil()>" }

let reduction () =
  for seed = 0 to 99 do
    let s, t = opened () in
    let s = apply s (C.Event (t, event 1 delta)) in
    let s = ref s in
    Random.init seed;
    for _ = 0 to 30 do
      s := apply !s (C.Event (t, event 1 delta))
    done;
    let s = apply !s (C.Event (t, event 2 W.Run_completed)) in
    Alcotest.(check string) "deduplicated UTF8 text" "héllo <img onerror=evil()>" (text s)
  done

let race () =
  let s, t = opened () in
  let s = apply s (C.Event (t, event 1 delta)) |> fun s -> apply s (C.Flush t) in
  let s, cs = C.reduce s (C.Resync t) in
  let q = List.find_map (function C.Fetch_snapshot (t, _) -> Some t | _ -> None) cs |> Option.get in
  let snap = Option.get (C.view s).snapshot in
  let s, cs = C.reduce s (C.Snapshot (q, Ok snap)) in
  Alcotest.(check int)
    "resume same run once" 1
    (List.length (List.filter (function C.Subscribe (_, i, "1") -> i.run_id = "r" | _ -> false) cs));
  let t = List.find_map (function C.Subscribe (t, _, _) -> Some t | _ -> None) cs |> Option.get in
  let s = apply s (C.Event (t, event 2 W.Run_completed)) in
  Alcotest.(check string) "terminal after snapshot" "héllo <img onerror=evil()>" (text s)

let isolation () =
  let s, t = opened () in
  let alien = { ids with attempt_id = "old" } in
  let s, cs = C.reduce s (C.Event (t, event ~ids:alien 99 W.Run_completed)) in
  Alcotest.(check int) "identity before gap" 0 (List.length cs);
  let s = apply s C.Auth_lost in
  let s = apply s (C.Event (t, event 1 delta)) in
  Alcotest.(check bool) "access loss clears" true ((C.view s).snapshot = None)

let gap () =
  let s, t = opened () in
  let _, cs = C.reduce s (C.Event (t, event 3 delta)) in
  Alcotest.(check bool)
    "gap fetches not posts" true
    (List.exists (function C.Fetch_snapshot _ -> true | _ -> false) cs
    && not (List.exists (function C.Post_turn _ -> true | _ -> false) cs))

let token_of f cs = List.find_map f cs |> Option.get
let snapshot_token cs = token_of (function C.Fetch_snapshot (t, _) -> Some t | _ -> None) cs

let stale_http () =
  let s = apply C.empty C.Activate in
  let s, cs = C.reduce s (C.Open_conversation "c") in
  let old = snapshot_token cs in
  let s, cs = C.reduce s (C.Open_conversation "other") in
  let newer = snapshot_token cs in
  let s = apply s (C.Snapshot (old, Ok snapshot)) in
  let s = apply s (C.Snapshot (old, Error C.Access_lost)) in
  Alcotest.(check bool) "stale snapshot ignored" true ((C.view s).snapshot = None);
  let other = { snapshot with conversation_id = "other"; active_run = None; turns = [] } in
  let s = apply s (C.Snapshot (newer, Ok other)) in
  Alcotest.(check bool) "new authorized selection survives" true ((C.view s).snapshot = Some other);
  let s = apply s C.Deactivate in
  let s = apply s (C.Snapshot (newer, Ok other)) in
  Alcotest.(check bool) "late success after shell deactivation" true ((C.view s).snapshot = None)

let pending () =
  let s = apply C.empty C.Activate in
  let s = apply s (C.Open_empty (W.Personal, snapshot.currency, "sv")) |> fun s -> apply s (C.Draft_changed "hej  ") in
  let s, cs = C.reduce s (C.Submit_draft "key") in
  let tok, p = token_of (function C.Post_turn (t, p) -> Some (t, p) | _ -> None) cs in
  let s = apply s (C.Accepted (tok, Error C.Transport)) |> fun s -> apply s (C.Draft_changed "edited") in
  let _, cs = C.reduce s C.Repeat_pending in
  let _, repeat = token_of (function C.Post_turn (t, p) -> Some (t, p) | _ -> None) cs in
  Alcotest.(check string) "immutable payload including whitespace" (W.encode_submit p) (W.encode_submit repeat)

let lifecycle () =
  let s, t = opened () in
  let s, cs = C.reduce s C.Leave_surface in
  Alcotest.(check int) "route leave has no commands" 0 (List.length cs);
  let s = apply s (C.Event (t, event 1 delta)) in
  let s, cs = C.reduce s (C.Background t) in
  Alcotest.(check bool) "background closes" true (List.mem C.Unsubscribe cs);
  let s = apply s (C.Event (t, event 2 delta)) |> fun s -> apply s (C.Flush t) in
  Alcotest.(check string) "old background callbacks cannot append" "héllo <img onerror=evil()>" (text s);
  let _, cs = C.reduce s (C.Foreground t) in
  Alcotest.(check bool)
    "foreground only fetches" true
    (List.exists (function C.Fetch_snapshot _ -> true | _ -> false) cs)

let stop_error () =
  let s, _ = opened () in
  let s, cs = C.reduce s C.Stop in
  let tok = token_of (function C.Cancel_run (t, _) -> Some t | _ -> None) cs in
  let s = apply s (C.Stopped (tok, Error C.Transport)) in
  Alcotest.(check bool)
    "Stop failure not cancellation" true
    ((C.view s).snapshot = Some snapshot && not (C.view s).stopping)

let retry_partials () =
  let s, t = opened () in
  let s = apply s (C.Event (t, event 1 delta)) |> fun s -> apply s (C.Event (t, event 2 W.Run_cancelled)) in
  let s, cs = C.reduce s (C.Retry_latest "new-key") in
  let tok = token_of (function C.Retry_turn (t, _) -> Some t | _ -> None) cs in
  let newids = { ids with attempt_id = "new"; run_id = "new-run"; assistant_message_id = "new-msg" } in
  let accepted : W.accepted = { ids = newids; accepted_version = "2"; idempotent_repeat = false } in
  let s, cs = C.reduce s (C.Accepted (tok, Ok accepted)) in
  let q = snapshot_token cs in
  let old = List.hd (Option.get (C.view s).snapshot).turns |> fun t -> List.hd t.attempts in
  let newer = { attempt with ids = newids; ordinal = "2" } in
  let snap =
    {
      snapshot with
      version = "2";
      active_run = Some "new-run";
      turns = [ { (List.hd snapshot.turns) with attempts = [ old; newer ] } ];
    }
  in
  let s, cs = C.reduce s (C.Snapshot (q, Ok snap)) in
  let stream = token_of (function C.Subscribe (t, _, _) -> Some t | _ -> None) cs in
  let s, cs = C.reduce s (C.Event (stream, event 99 W.Run_completed)) in
  Alcotest.(check int) "old identity ignored before sequence" 0 (List.length cs);
  Alcotest.(check string) "prior partial retained" "héllo <img onerror=evil()>" (text s)

let concurrent_and_consent () =
  let s, cs = C.reduce C.empty C.Activate in
  let status = token_of (function C.Get_status t -> Some t | _ -> None) cs in
  let s, cs = C.reduce s (C.Load_history 0) in
  let history = token_of (function C.List_conversations (t, _, _) -> Some t | _ -> None) cs in
  let s = apply s (C.Status (status, Ok { available = true; consent = true })) in
  let s = apply s (C.History (history, Ok [])) in
  Alcotest.(check bool) "concurrent request lanes coexist" true ((C.view s).status <> None);
  let s, cs = C.reduce s (C.Consent (true, Some "explicit-household")) in
  Alcotest.(check bool)
    "explicit consent target" true
    (List.exists (function C.Set_consent (_, true, Some "explicit-household") -> true | _ -> false) cs);
  let s, cs = C.reduce s (C.Open_conversation "household") in
  let s = apply s (C.Snapshot (snapshot_token cs, Error C.Access_lost)) in
  Alcotest.(check bool)
    "status200 never authorizes household" true
    ((C.view s).snapshot = None && (C.view s).status = None)

let list_races () =
  let s = apply C.empty C.Activate in
  let s, cs = C.reduce s C.Open_latest in
  let old = token_of (function C.List_conversations (t, 1, 0) -> Some t | _ -> None) cs in
  let s = apply s (C.Open_empty (W.Personal, snapshot.currency, "en")) in
  let s, cs = C.reduce s (C.History (old, Ok [ { conversation_id = "c"; version = "1"; updated_at = "now" } ])) in
  Alcotest.(check int) "stale latest cannot select" 0 (List.length cs);
  let s, cs = C.reduce s (C.Load_history 0) in
  let old = token_of (function C.List_conversations (t, _, _) -> Some t | _ -> None) cs in
  let s, cs = C.reduce s (C.Load_history 20) in
  let newer = token_of (function C.List_conversations (t, _, _) -> Some t | _ -> None) cs in
  let page : C.metadata list = [ { conversation_id = "current-page"; version = "2"; updated_at = "now" } ] in
  let s = apply s (C.History (old, Error C.Access_lost)) |> fun s -> apply s (C.History (newer, Ok page)) in
  let s = apply s (C.Draft_changed "still active") in
  Alcotest.(check string) "stale page denial preserves active draft" "still active" (C.view s).draft;
  Alcotest.(check bool) "current page applied" true ((C.view s).history = page);
  let _, cs = C.reduce s (C.Submit_draft "context-key") in
  Alcotest.(check bool)
    "explicit empty context survives page race" true
    (List.exists
       (function
         | C.Post_turn (_, p) -> p.scope = W.Personal && p.locale = "en" && p.conversation_id = None | _ -> false)
       cs)

let bounds () =
  let s, t = opened () in
  let s = ref s in
  let fetched = ref false in
  for n = 1 to 200 do
    let newer, cs =
      C.reduce !s (C.Event (t, event n (W.Text_appended { segment_id = "s"; text = String.make 1000 'x' })))
    in
    s := newer;
    if List.exists (function C.Fetch_snapshot _ -> true | _ -> false) cs then fetched := true
  done;
  Alcotest.(check bool) "bounded independent buffer resync" true !fetched;
  Alcotest.(check bool) "no unbounded accumulation" true (String.length (text !s) <= 65000)

let generated_out_of_order () =
  for seed = 0 to 199 do
    let random = Random.State.make [| seed |] in
    let ns = Array.init 8 (fun n -> n + 1) in
    for n = 7 downto 1 do
      let j = Random.State.int random (n + 1) in
      let x = ns.(n) in
      ns.(n) <- ns.(j);
      ns.(j) <- x
    done;
    let state, t = opened () in
    let state = ref state and fetches = ref 0 and posts = ref 0 in
    Array.iter
      (fun n ->
        let e = event n (W.Text_appended { segment_id = "s"; text = string_of_int n }) in
        for _ = 0 to Random.State.int random 5 do
          let next, cs = C.reduce !state (C.Event (t, e)) in
          state := next;
          List.iter
            (function C.Fetch_snapshot _ -> incr fetches | C.Post_turn _ | C.Retry_turn _ -> incr posts | _ -> ())
            cs
        done)
      ns;
    Alcotest.(check bool) "only one gap fetch per obsolete connection" true (!fetches <= 1);
    Alcotest.(check int) "no regeneration" 0 !posts;
    let old = { ids with run_id = "obsolete" } in
    ignore (C.reduce !state (C.Event (t, { W.ids = old; sequence = "not-a-number"; payload = W.Run_completed })))
  done

let versions_and_money () =
  Alcotest.(check bool)
    "unknown wire version rejected" true
    (W.decode_event "{\"version\":2,\"data\":{}}" = Error W.Unknown_version);
  let s, t = opened () in
  let evidence : W.evidence =
    {
      id = "e";
      scope = W.Personal;
      currency = snapshot.currency;
      sampled_at = "2026-10-22T00:00:00Z";
      title = "<script>";
      facts = [ { label = "exact"; value = W.Money { decimal = "9007199254740993.01"; currency = snapshot.currency } } ];
      source_references = [];
      import_status = W.Stale;
      reconciled = false;
      assumptions = [];
    }
  in
  let s = apply s (C.Event (t, event 1 (W.Evidence_added evidence))) |> fun s -> apply s (C.Flush t) in
  let e =
    List.hd (Option.get (C.view s).snapshot).turns |> fun t ->
    List.hd t.attempts |> fun a -> List.hd a.segments
  in
  let decimal = match e with W.Evidence_segment { facts = [ { value = W.Money m; _ } ]; _ } -> m.decimal | _ -> "" in
  Alcotest.(check string) "exact money" "9007199254740993.01" decimal;
  let s, cs = C.reduce s (C.Stream_invalid t) in
  Alcotest.(check bool)
    "invalid stream does not regenerate" true
    (not (List.exists (function C.Post_turn _ | C.Retry_turn _ | C.Fetch_snapshot _ -> true | _ -> false) cs));
  Alcotest.(check bool) "invalid stream retains partial" true ((C.view s).snapshot <> None)

let session_and_owned_deletion () =
  let s, _ = opened () in
  let s, cs = C.reduce s (C.Delete_owned "unrelated") in
  let token = token_of (function C.Delete_conversation (t, "unrelated") -> Some t | _ -> None) cs in
  let failed = apply s (C.Deleted (token, Error C.Access_lost)) in
  Alcotest.(check bool)
    "nonselected denial retains authorized transcript" true
    ((C.view failed).snapshot = Some snapshot);
  let success = apply s (C.Deleted (token, Ok ())) in
  Alcotest.(check bool)
    "nonselected deletion retains authorized transcript" true
    ((C.view success).snapshot = Some snapshot);
  let lost = apply s (C.Deleted (token, Error C.Session_lost)) in
  Alcotest.(check bool)
    "current401 clears all private state" true
    ((C.view lost).snapshot = None && (C.view lost).pending = None);
  let _, cs = C.reduce lost (C.Draft_changed "not signed in") in
  Alcotest.(check int) "current401 deactivates" 0 (List.length cs);
  let lost = apply lost (C.Draft_changed "not signed in") in
  Alcotest.(check string) "401 blocks draft" "" (C.view lost).draft;
  let fresh, cs = C.reduce lost C.Activate in
  let status = token_of (function C.Get_status t -> Some t | _ -> None) cs in
  let fresh = apply fresh (C.Status (status, Ok { available = true; consent = true })) in
  let fresh = apply fresh (C.Deleted (token, Error C.Session_lost)) in
  Alcotest.(check bool) "stale401 cannot deactivate new epoch" true ((C.view fresh).status <> None);
  let s, _ = opened () in
  let s, cs = C.reduce s C.Delete in
  let token = token_of (function C.Delete_conversation (t, _) -> Some t | _ -> None) cs in
  let s = apply s (C.Deleted (token, Error C.Access_lost)) in
  Alcotest.(check bool) "selected delete denial clears selected content" true ((C.view s).snapshot = None);
  let s, cs = C.reduce s (C.Delete_owned "c") in
  Alcotest.(check bool)
    "ownership-only deletion never fetches" true
    (List.exists (function C.Delete_conversation (_, "c") -> true | _ -> false) cs
    && not (List.exists (function C.Fetch_snapshot _ -> true | _ -> false) cs));
  let token = token_of (function C.Delete_conversation (t, _) -> Some t | _ -> None) cs in
  let s = apply s (C.Open_empty (W.Personal, snapshot.currency, "en")) |> fun s -> apply s (C.Deleted (token, Ok ())) in
  let s = apply s (C.Draft_changed "new selection") in
  Alcotest.(check string) "obsolete deletion cannot clear new selection" "new selection" (C.view s).draft

let delayed_readiness () =
  List.iter
    (fun (selection, latest_empty) ->
      List.iter
        (fun (status : C.status) ->
          let s, cs = C.reduce C.empty C.Activate in
          let original = token_of (function C.Get_status t -> Some t | _ -> None) cs in
          let s, cs = C.reduce s selection in
          let s, more =
            match selection with
            | C.Open_latest ->
                let t = token_of (function C.List_conversations (t, _, _) -> Some t | _ -> None) cs in
                C.reduce s
                  (C.History
                     ( t,
                       Ok
                         (if latest_empty then [] else [ { conversation_id = "c"; version = "1"; updated_at = "now" } ])
                     ))
            | _ -> (s, [])
          in
          let effective = List.fold_left (fun old -> function C.Get_status t -> t | _ -> old) original (cs @ more) in
          let s = apply s (C.Status (effective, Ok status)) in
          Alcotest.(check bool) "delayed initial readiness reaches view" true ((C.view s).status = Some status);
          let s = apply s (C.Status (original, Error C.Session_lost)) in
          Alcotest.(check bool) "obsolete selection status denial ignored" true ((C.view s).status = Some status);
          let s, cs = C.reduce (apply s C.Auth_lost) C.Activate in
          let current = token_of (function C.Get_status t -> Some t | _ -> None) cs in
          let s =
            apply s (C.Status (effective, Ok status)) |> fun s -> apply s (C.Status (effective, Error C.Session_lost))
          in
          Alcotest.(check bool) "old-session readiness cannot return" true ((C.view s).status = None);
          let s = apply s (C.Status (current, Ok status)) in
          Alcotest.(check bool) "new-session readiness still applies" true ((C.view s).status = Some status))
        [ { available = false; consent = true }; { available = true; consent = false } ])
    [
      (C.Open_empty (W.Personal, snapshot.currency, "en"), false);
      (C.Open_finding ("finding", W.Personal, snapshot.currency, "en"), false);
      (C.Open_conversation "c", false);
      (C.Open_latest, false);
      (C.Open_latest, true);
    ]

let definitive_post_rejection () =
  let s, t = opened () in
  let s = apply s (C.Event (t, event 1 W.Run_completed)) in
  let s = apply s (C.Draft_changed "unchanged draft  ") in
  let s, cs = C.reduce s (C.Submit_draft "old-key") in
  let old, p = token_of (function C.Post_turn (t, p) -> Some (t, p) | _ -> None) cs in
  let rejected, cs = C.reduce s (C.Accepted (old, Error (C.Not_accepted W.Version_conflict))) in
  Alcotest.(check bool) "definitive conflict resolves pending" true ((C.view rejected).pending = None);
  Alcotest.(check string) "conflict preserves draft" "unchanged draft  " (C.view rejected).draft;
  Alcotest.(check bool)
    "conflict never auto posts" false
    (List.exists (function C.Post_turn _ | C.Retry_turn _ -> true | _ -> false) cs);
  let q = snapshot_token cs in
  let _, early = C.reduce rejected (C.Submit_draft "too-early") in
  Alcotest.(check bool)
    "version recovery waits for authoritative snapshot" false
    (List.exists (function C.Post_turn _ -> true | _ -> false) early);
  let snap = { (Option.get (C.view rejected).snapshot) with version = "2" } in
  let refreshed = apply rejected (C.Snapshot (q, Ok snap)) in
  let fresh, cs = C.reduce refreshed (C.Submit_draft "fresh-key") in
  let tok, next = token_of (function C.Post_turn (t, p) -> Some (t, p) | _ -> None) cs in
  Alcotest.(check string) "fresh request uses current version" "2" next.expected_version;
  Alcotest.(check string) "fresh key is explicit" "fresh-key" next.idempotency_key;
  let fresh = apply fresh (C.Accepted (old, Error (C.Not_accepted W.Invalid_request))) in
  Alcotest.(check bool)
    "stale rejection cannot resolve fresh pending" true
    ((C.view fresh).pending = Some (C.Submit next));
  let accepted : W.accepted = { ids; accepted_version = "3"; idempotent_repeat = false } in
  let accepted = apply fresh (C.Accepted (tok, Ok accepted)) in
  Alcotest.(check bool) "fresh submit accepted" true ((C.view accepted).pending = None);
  let invalid, cs = C.reduce s (C.Accepted (old, Error (C.Not_accepted W.Invalid_request))) in
  Alcotest.(check bool)
    "invalid request resolves pending without IO retry" true
    ((C.view invalid).pending = None
    && not (List.exists (function C.Post_turn _ | C.Fetch_snapshot _ -> true | _ -> false) cs));
  List.iter
    (fun error ->
      let ambiguous = apply s (C.Accepted (old, Error error)) in
      let _, cs = C.reduce ambiguous C.Repeat_pending in
      let repeated = token_of (function C.Post_turn (_, p) -> Some p | _ -> None) cs in
      Alcotest.(check string) "ambiguous outcome preserves exact payload" (W.encode_submit p) (W.encode_submit repeated))
    [ C.Transport; C.Rejected ];
  let s, t = opened () in
  let s = apply s (C.Event (t, event 1 W.Run_cancelled)) in
  let s, cs = C.reduce s (C.Retry_latest "retry-old") in
  let tok = token_of (function C.Retry_turn (t, _) -> Some t | _ -> None) cs in
  let s, cs = C.reduce s (C.Accepted (tok, Error (C.Not_accepted W.Version_conflict))) in
  let snap = { (Option.get (C.view s).snapshot) with version = "2" } in
  let s = apply s (C.Snapshot (snapshot_token cs, Ok snap)) in
  let _, cs = C.reduce s (C.Retry_latest "retry-new") in
  Alcotest.(check bool)
    "explicit retry unlocked with fresh version" true
    (List.exists
       (function C.Retry_turn (_, p) -> p.expected_version = "2" && p.idempotency_key = "retry-new" | _ -> false)
       cs)

let () =
  Alcotest.run "advisor client state"
    [
      ( "snapshot_and_event_reduction",
        List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [
            ("definitive POST rejection", definitive_post_rejection);
            ("delayed readiness", delayed_readiness);
            ("session and owned deletion", session_and_owned_deletion);
            ("generated duplicate gap permutations", generated_out_of_order);
            ("versions and exact money", versions_and_money);
            ("generated duplicates", reduction);
            ("snapshot_between_last_delta_and_terminal", race);
            ("identity and auth isolation", isolation);
            ("gap resync", gap);
            ("stale HTTP", stale_http);
            ("immutable ambiguous POST", pending);
            ("surface background lifecycle", lifecycle);
            ("Stop errors", stop_error);
            ("retry prior attempts", retry_partials);
            ("request lanes and household consent", concurrent_and_consent);
            ("latest and pagination races", list_races);
            ("bounded buffering", bounds);
          ] );
    ]
