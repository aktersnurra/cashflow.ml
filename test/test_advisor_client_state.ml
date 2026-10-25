module C = Cashflow_advisor_protocol.Client_state
module W = Cashflow_advisor_protocol.Wire

let history_page ?next_offset conversations : C.history_page = {conversations; next_offset}

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
  let s = apply s (C.Status (status, Ok { available = true; consent = true; household = None; household_known = true })) in
  let s = apply s (C.History (history, Ok (history_page []))) in
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
  let s, cs = C.reduce s (C.History (old, Ok (history_page [{conversation_id="c"; version="1"; updated_at="now"; preview=None}]))) in
  Alcotest.(check int) "stale latest cannot select" 0 (List.length cs);
  let s, cs = C.reduce s (C.Load_history 0) in
  let old = token_of (function C.List_conversations (t, _, _) -> Some t | _ -> None) cs in
  let s, cs = C.reduce s (C.Load_history 0) in
  let newer = token_of (function C.List_conversations (t, _, _) -> Some t | _ -> None) cs in
  let page : C.metadata list = [{conversation_id="current-page"; version="2"; updated_at="now"; preview=None}] in
  let s = apply s (C.History (old, Error C.Access_lost)) |> fun s -> apply s (C.History (newer, Ok (history_page page))) in
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
  let fresh = apply fresh (C.Status (status, Ok { available = true; consent = true; household = None; household_known = true })) in
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
                         (history_page (if latest_empty then [] else [{conversation_id="c"; version="1"; updated_at="now"; preview=None}]))
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
        [ { available = false; consent = true; household = None; household_known = true }; { available = true; consent = false; household = None; household_known = true } ])
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

let failed_conflict_recovery () =
  List.iter
    (fun retry ->
      List.iter
        (fun error ->
          let s, t = opened () in
          let s =
            apply s (C.Event (t, event 1 delta)) |> fun s ->
            apply s (C.Event (t, event 2 (if retry then W.Run_cancelled else W.Run_completed)))
          in
          let s = apply s (C.Draft_changed "retained draft  ") in
          let operation key = if retry then C.Retry_latest key else C.Submit_draft key in
          let post_token cs = token_of (function C.Post_turn (t, _) | C.Retry_turn (t, _) -> Some t | _ -> None) cs in
          let is_post = function C.Post_turn _ | C.Retry_turn _ -> true | _ -> false in
          let s, cs = C.reduce s (operation "stale-key") in
          let s, cs = C.reduce s (C.Accepted (post_token cs, Error (C.Not_accepted W.Version_conflict))) in
          let obsolete = snapshot_token cs in
          let snap = { (Option.get (C.view s).snapshot) with version = "2" } in
          let s = apply s (C.Snapshot (obsolete, Error error)) in
          let _, blocked = C.reduce s (operation "too-early") in
          Alcotest.(check bool) "failed refresh cannot unlock known stale version" false (List.exists is_post blocked);
          Alcotest.(check bool) "explicit reconnect available without active run" true (C.view s).can_reconnect;
          Alcotest.(check string) "failed refresh retains draft" "retained draft  " (C.view s).draft;
          Alcotest.(check string) "failed refresh retains partials" "héllo <img onerror=evil()>" (text s);
          List.iter
            (fun selection ->
              let selected =
                apply s selection |> fun selected ->
                apply selected (C.Draft_changed "new context") |> fun selected ->
                apply selected (C.Snapshot (obsolete, Ok snap))
              in
              let _, commands = C.reduce selected (C.Submit_draft "new-context") in
              Alcotest.(check bool)
                "new explicit context does not inherit conflict recovery" true
                (List.exists
                   (function C.Post_turn (_, p) -> p.conversation_id = None && p.expected_version = "0" | _ -> false)
                   commands))
            [
              C.Open_empty (W.Personal, snapshot.currency, "en");
              C.Open_finding ("new-finding", W.Personal, snapshot.currency, "en");
            ];
          let signed_out = apply s C.Auth_lost in
          Alcotest.(check bool) "session loss clears partial content" true ((C.view signed_out).snapshot = None);
          let signed_in =
            apply signed_out C.Activate |> fun s ->
            apply s (C.Open_empty (W.Personal, snapshot.currency, "en")) |> fun s ->
            apply s (C.Draft_changed "new-session") |> fun s -> apply s (C.Snapshot (obsolete, Ok snap))
          in
          let _, commands = C.reduce signed_in (C.Submit_draft "new-session-key") in
          Alcotest.(check bool)
            "new session not locked by old recovery" true
            (List.exists
               (function C.Post_turn (_, p) -> p.user_text = "new-session" && p.expected_version = "0" | _ -> false)
               commands);
          let denied, commands = C.reduce s C.Reconnect in
          let denied = apply denied (C.Snapshot (snapshot_token commands, Error C.Access_lost)) in
          Alcotest.(check bool)
            "access loss clears private recovery content" true
            ((C.view denied).snapshot = None && (C.view denied).draft = "");
          let s, cs = C.reduce s C.Reconnect in
          let current = snapshot_token cs in
          Alcotest.(check bool) "explicit reconnect never posts" false (List.exists is_post cs);
          let s = apply s (C.Snapshot (obsolete, Ok snap)) in
          let _, blocked = C.reduce s (operation "stale-success") in
          Alcotest.(check bool) "obsolete success cannot unlock recovery" false (List.exists is_post blocked);
          let s = apply s (C.Snapshot (current, Ok { snap with conversation_id = "other"; turns = [] })) in
          let _, blocked = C.reduce s (operation "wrong-conversation") in
          Alcotest.(check bool) "wrong snapshot identity cannot unlock recovery" false (List.exists is_post blocked);
          let s, cs = C.reduce s C.Reconnect in
          let current = snapshot_token cs in
          let s, commands = C.reduce s (C.Snapshot (current, Ok snap)) in
          Alcotest.(check bool) "successful snapshot never auto posts" false (List.exists is_post commands);
          let _, cs = C.reduce s (operation "fresh-key") in
          Alcotest.(check bool)
            "matching success unlocks explicit fresh version and key" true
            (List.exists
               (function
                 | C.Post_turn (_, p) -> (not retry) && p.expected_version = "2" && p.idempotency_key = "fresh-key"
                 | C.Retry_turn (_, p) -> retry && p.expected_version = "2" && p.idempotency_key = "fresh-key"
                 | _ -> false)
               cs);
          ())
        [ C.Transport; C.Rejected ])
    [ false; true ]

let conversation_drafts () =
  let s, _ = opened () in
  let s = apply s (C.Draft_changed "personal private draft") in
  let s = apply s (C.Open_conversation "other") in
  Alcotest.(check string) "new conversation never copies draft" "" (C.view s).draft;
  let s = apply s (C.Draft_changed "other draft") in
  let s = apply s (C.Open_conversation "c") in
  Alcotest.(check string) "conversation draft restored" "personal private draft" (C.view s).draft;
  let s = apply s (C.Open_empty (W.Household "h", snapshot.currency, "en")) in
  Alcotest.(check string) "new scope never copies draft" "" (C.view s).draft;
  let s = apply s C.Auth_lost |> fun s -> apply s C.Activate |> fun s -> apply s (C.Open_conversation "c") in
  Alcotest.(check string) "auth loss forgets drafts" "" (C.view s).draft

let denied_ownership_handoff () =
  let s, _ = opened () in
  let s = apply s (C.Draft_changed "private draft") in
  let s, commands = C.reduce s C.Reconnect in
  let tok = List.find_map (function C.Fetch_snapshot (tok, _) -> Some tok | _ -> None) commands |> Option.get in
  let s = apply s (C.Snapshot (tok, Error C.Access_lost)) in
  let v = C.view s in
  Alcotest.(check bool) "denial clears source transcript and draft" true (v.snapshot = None && v.draft = "");
  Alcotest.(check bool) "only confirmed opaque ID retained" true (v.access_denied && v.denied_conversation = Some "c");
  let _, commands = C.reduce s (C.Delete_owned "c") in
  Alcotest.(check bool) "ownership delete never refetches denied contents" true
    (List.exists (function C.Delete_conversation (_, "c") -> true | _ -> false) commands
     && not (List.exists (function C.Fetch_snapshot _ -> true | _ -> false) commands));
  let s = apply s (C.Open_empty (W.Personal, snapshot.currency, "en")) in
  Alcotest.(check bool) "selection change invalidates denied handoff" true ((C.view s).denied_conversation = None);
  let s = apply s (C.Open_conversation "c") in
  Alcotest.(check string) "denial forgets old conversation drafts" "" (C.view s).draft

let first_acceptance_and_revocation () =
  let s = apply C.empty C.Activate |> fun s -> apply s (C.Open_empty (W.Personal, snapshot.currency, "en"))
    |> fun s -> apply s (C.Draft_changed "new conversation") in
  let s, commands = C.reduce s (C.Submit_draft "first") in
  let token = token_of (function C.Post_turn (token, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Accepted (token, Ok {W.ids; accepted_version="1"; idempotent_repeat=false})) in
  Alcotest.(check bool) "first acceptance is distinct from selection" true ((C.view s).first_acceptance = Some "c");
  let s = apply s (C.Open_conversation "history") in
  Alcotest.(check bool) "explicit history never signals first acceptance" true ((C.view s).first_acceptance = None);
  let s, _ = opened () in
  let s, commands = C.reduce s C.Refresh_status in
  let token = token_of (function C.Get_status token -> Some token | _ -> None) commands in
  let _, commands = C.reduce s (C.Status (token, Ok {C.available=true; consent=false; household=None; household_known=true})) in
  Alcotest.(check bool) "revoked readiness rechecks actual snapshot authority" true
    (List.exists (function C.Fetch_snapshot (_, "c") -> true | _ -> false) commands
     && not (List.exists (function C.Post_turn _ | C.Retry_turn _ -> true | _ -> false) commands));
  let s, commands = C.reduce s (C.Consent (false, None)) in
  let token = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Consent_result (token, Ok ())) in
  Alcotest.(check bool) "explicit opt-out clears private view and preserves only delete handoff" true
    ((C.view s).snapshot = None && (C.view s).access_denied && (C.view s).denied_conversation = Some "c")

let cached_ready_snapshot snap =
  let s, commands = C.reduce C.empty C.Activate in
  let token = token_of (function C.Get_status token -> Some token | _ -> None) commands in
  let readiness = {C.available=true; consent=true; household_known=true;
    household=Some {C.household_id="h"; enabled=true; can_manage=true}} in
  let s = apply s (C.Status (token, Ok readiness)) in
  let s, commands = C.reduce s (C.Open_conversation snap.W.conversation_id) in
  let s, commands = C.reduce s (C.Snapshot (snapshot_token commands, Ok snap)) in
  s, commands, readiness

let cached_readiness_revocation () =
  let completed = {snapshot with scope=W.Household "h"; active_run=None;
    turns=[{(List.hd snapshot.turns) with attempts=[{attempt with state=W.Completed}]}]} in
  List.iter (fun membership_lost ->
    let s, _, readiness = cached_ready_snapshot completed in
    let s = apply s (C.Draft_changed "private household draft") in
    let s, commands = C.reduce s C.Refresh_status in
    Alcotest.(check bool) "explicit refresh fetches despite cached successful readiness" true
      (List.exists (function C.Get_status _ -> true | _ -> false) commands);
    let old = token_of (function C.Get_status token -> Some token | _ -> None) commands in
    let s, commands = C.reduce s C.Refresh_status in
    let current = token_of (function C.Get_status token -> Some token | _ -> None) commands in
    let revoked = if membership_lost then {readiness with household=None}
      else {readiness with household=Some {C.household_id="h"; enabled=false; can_manage=true}} in
    let stale = apply s (C.Status (old, Error C.Session_lost)) in
    Alcotest.(check bool) "stale refresh401 cannot deactivate current session" true ((C.view stale).authenticated && (C.view stale).snapshot=Some completed);
    let stale = apply stale (C.Status (old, Ok revoked)) in
    Alcotest.(check bool) "stale refresh cannot clear current snapshot" true ((C.view stale).snapshot=Some completed);
    let s, commands = C.reduce s (C.Status (current, Ok revoked)) in
    Alcotest.(check bool) "readiness rechecks snapshot rather than authorizing content" true
      (List.exists (function C.Fetch_snapshot (_, "c") -> true | _ -> false) commands
       && not (List.exists (function C.Post_turn _ | C.Retry_turn _ -> true | _ -> false) commands));
    let s = apply s (C.Snapshot (snapshot_token commands, Error C.Access_lost)) in
    Alcotest.(check bool) "completed household denial clears private content and draft" true
      ((C.view s).access_denied && (C.view s).snapshot=None && (C.view s).draft="")) [false; true]

let scoped_consent_continuity () =
  let s, commands, _ = cached_ready_snapshot snapshot in
  let stream = token_of (function C.Subscribe (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Draft_changed "personal draft") in
  let s, commands = C.reduce s (C.Consent (true, Some "h")) in
  let consent = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
  let s, commands = C.reduce s (C.Consent_result (consent, Ok ())) in
  Alcotest.(check bool) "household enable preserves selected personal snapshot and draft" true
    ((C.view s).selected_conversation=Some "c" && (C.view s).snapshot=Some snapshot && (C.view s).draft="personal draft");
  Alcotest.(check bool) "household enable preserves same active subscription" true
    (C.command_is_current s (C.Subscribe (stream, ids, "0")) && not (List.mem C.Unsubscribe commands));
  let s = apply s (C.Event (stream, event 1 delta)) |> fun s -> apply s (C.Flush stream) in
  Alcotest.(check string) "same run still delivers" "héllo <img onerror=evil()>" (text s);
  let done_snapshot = {snapshot with active_run=None; turns=[]} in
  let s, _, _ = cached_ready_snapshot done_snapshot in
  let s = apply s (C.Draft_changed "immutable pending") in
  let s, _ = C.reduce s (C.Submit_draft "pending") in
  let pending = (C.view s).pending in
  Alcotest.(check bool) "personal submission is actually pending" true (Option.is_some pending);
  let s, commands = C.reduce s (C.Consent (true, Some "h")) in
  let consent = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Consent_result (consent, Ok ())) in
  Alcotest.(check bool) "unrelated consent preserves immutable pending write" true ((C.view s).pending=pending)

let scoped_cached_draft_revocation () =
  let personal = {snapshot with active_run=None; turns=[]} in
  let household = {personal with conversation_id="household"; scope=W.Household "h"} in
  let open_snapshot s snap =
    let s, commands = C.reduce s (C.Open_conversation snap.W.conversation_id) in
    let s, commands = if List.exists (function C.Fetch_snapshot _ -> true | _ -> false) commands
      then s, commands else C.reduce s C.Reconnect in
    apply s (C.Snapshot (snapshot_token commands, Ok snap)) in
  let s, _, readiness = cached_ready_snapshot personal in
  let s = apply s (C.Draft_changed "preserved personal") in
  let s = open_snapshot s household |> fun s -> apply s (C.Draft_changed "revoked household") in
  let s = open_snapshot s personal in
  List.iter (fun explicit ->
    let s = if explicit then
      let s, commands = C.reduce s (C.Consent (false, Some "h")) in
      let token = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
      apply s (C.Consent_result (token, Ok ()))
    else
      let s, commands = C.reduce s C.Refresh_status in
      let token = token_of (function C.Get_status token -> Some token | _ -> None) commands in
      apply s (C.Status (token, Ok {readiness with household=None})) in
    Alcotest.(check bool) "household loss retains unrelated personal view/draft" true
      ((C.view s).snapshot=Some personal && (C.view s).draft="preserved personal");
    let s = apply s (C.Open_conversation "household") in
    Alcotest.(check string) "revoked household cache cannot restore draft" "" (C.view s).draft)
    [false; true];
  let unknown, commands = C.reduce s C.Refresh_status in
  let token = token_of (function C.Get_status token -> Some token | _ -> None) commands in
  let unknown = apply unknown (C.Status (token, Ok {readiness with household=None; household_known=false})) in
  let unknown = apply unknown (C.Open_conversation "household") in
  Alcotest.(check string) "absent legacy metadata is not revocation" "revoked household" (C.view unknown).draft;
  let s = open_snapshot s household in
  let s, commands = C.reduce s (C.Consent (false, Some "h")) in
  let token = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Consent_result (token, Ok ())) in
  Alcotest.(check bool) "affected household view cleared" true ((C.view s).snapshot=None && (C.view s).draft="" && (C.view s).access_denied);
  let s = apply s (C.Open_conversation "c") in
  Alcotest.(check string) "affected household disable preserves personal cached draft" "preserved personal" (C.view s).draft;
  let s = open_snapshot s personal in
  let s, commands = C.reduce s (C.Consent (false, None)) in
  let token = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Consent_result (token, Ok ())) |> fun s -> apply s (C.Open_conversation "c") in
  Alcotest.(check string) "individual opt-out clears all cached drafts" "" (C.view s).draft

let pending_acceptance_consumption () =
  let s = apply C.empty C.Activate |> fun s -> apply s (C.Open_empty (W.Personal, snapshot.currency, "en"))
    |> fun s -> apply s (C.Draft_changed "pending URL") in
  let s, commands = C.reduce s (C.Submit_draft "pending URL") in
  let token = token_of (function C.Post_turn (token, _) -> Some token | _ -> None) commands in
  let s = apply s (C.Accepted (token, Ok {W.ids; accepted_version="1"; idempotent_repeat=false})) in
  let s = apply s C.Leave_surface |> fun s -> apply s (C.Consume_first_acceptance "other") in
  Alcotest.(check bool) "off-surface acceptance remains pending for selected ID" true ((C.view s).first_acceptance=Some "c");
  let consumed = apply s (C.Consume_first_acceptance "c") in
  Alcotest.(check bool) "matching selected acceptance consumed once" true ((C.view consumed).first_acceptance=None);
  let history = apply s (C.Open_conversation "other") in
  Alcotest.(check bool) "explicit history invalidates deferred URL" true ((C.view history).first_acceptance=None);
  let fresh = apply s (C.Open_empty (W.Personal, snapshot.currency, "en")) in
  Alcotest.(check bool) "explicit new context invalidates deferred URL" true ((C.view fresh).first_acceptance=None);
  let lost = apply s C.Auth_lost in
  Alcotest.(check bool) "session transition invalidates deferred URL" true ((C.view lost).first_acceptance=None)

let revoked_pending_household_context () =
  let s, _, readiness = cached_ready_snapshot {snapshot with active_run=None; turns=[]} in
  let s = apply s (C.Draft_changed "authorized personal cache") in
  let s = apply s (C.Open_empty (W.Household "h", snapshot.currency, "en"))
    |> fun s -> apply s (C.Draft_changed "private household pending") in
  let s, commands = C.reduce s (C.Submit_draft "household pending") in
  let write = token_of (function C.Post_turn (token, _) -> Some token | _ -> None) commands in
  let s, commands = C.reduce s C.Refresh_status in
  let status = token_of (function C.Get_status token -> Some token | _ -> None) commands in
  let s = apply s (C.Status (status, Ok {readiness with household=None})) in
  Alcotest.(check bool) "known household loss drops affected unaccepted text/pending operation" true
    ((C.view s).pending=None && (C.view s).draft="" && (C.view s).context_scope=None && (C.view s).access_denied);
  let s = apply s (C.Accepted (write, Ok {W.ids; accepted_version="1"; idempotent_repeat=false})) in
  Alcotest.(check bool) "late acceptance cannot revive revoked context" true ((C.view s).first_acceptance=None && (C.view s).snapshot=None);
  let s = apply s (C.Open_conversation "c") in
  Alcotest.(check string) "unrelated authorized personal cache retained" "authorized personal cache" (C.view s).draft

let history_append () =
  let s = apply C.empty C.Activate |> fun s -> apply s (C.Open_empty (W.Personal, snapshot.currency, "en")) in
  let entry id : C.metadata = {conversation_id=id; version="1"; updated_at="2026-10-25T12:00:00Z"; preview=None} in
  let s, commands = C.reduce s (C.Load_history 0) in
  let first = token_of (function C.List_conversations (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.History (first, Ok (history_page ~next_offset:20 [entry "newest"]))) in
  let s, commands = C.reduce s (C.Load_history 20) in
  let older = token_of (function C.List_conversations (token, _, _) -> Some token | _ -> None) commands in
  let _, duplicate = C.reduce s (C.Load_history 20) in
  Alcotest.(check int) "one request per older page" 0 (List.length duplicate);
  let s = apply s (C.History (older, Ok (history_page ~next_offset:40 [entry "newest"; entry "older"]))) in
  Alcotest.(check (list string)) "older rows append without replacing newer history" ["newest"; "older"]
    (List.map (fun (entry : C.metadata) -> entry.conversation_id) (C.view s).history);
  let _, skipped = C.reduce s (C.Load_history 999) in
  Alcotest.(check int) "cannot skip the known cursor" 0 (List.length skipped);
  let s, commands = C.reduce s (C.Load_history 0) in
  let refresh = token_of (function C.List_conversations (token, _, _) -> Some token | _ -> None) commands in
  let s = apply s (C.History (refresh, Error C.Transport)) in
  Alcotest.(check int) "failed refresh retains authorized history" 2 (List.length (C.view s).history);
  Alcotest.(check bool) "history error remains attributable to its own read lane" true (C.view s).history_failed

let history_preview_revocation () =
  List.iter (fun boundary ->
    let s, commands, readiness = cached_ready_snapshot snapshot in
    let stream = token_of (function C.Subscribe (token, _, _) -> Some token | _ -> None) commands in
    let s = apply s (C.Draft_changed "personal draft survives") in
    let entry id scope : C.metadata = {conversation_id=id; version="1"; updated_at="2026-10-25T12:00:00Z";
      preview=Some {C.title=id; scope}} in
    let personal = entry "personal preview" W.Personal and household = entry "private household preview" (W.Household "h") in
    let s, commands = C.reduce s (C.Load_history 0) in
    let token = token_of (function C.List_conversations (token, _, _) -> Some token | _ -> None) commands in
    let s = apply s (C.History (token, Ok (history_page ~next_offset:20 [personal; household]))) in
    let s, commands = C.reduce s (C.Load_history 20) in
    let late = token_of (function C.List_conversations (token, _, _) -> Some token | _ -> None) commands in
    let s = if boundary = "consent" then (
      let s, commands = C.reduce s (C.Consent (false, Some "h")) in
      let token = token_of (function C.Set_consent (token, _, _) -> Some token | _ -> None) commands in
      apply s (C.Consent_result (token, Ok ())))
    else (
      let s, commands = C.reduce s C.Refresh_status in
      let token = token_of (function C.Get_status token -> Some token | _ -> None) commands in
      apply s (C.Status (token, Ok {readiness with household=None; household_known=(boundary <> "legacy")}))) in
    let ids s = List.map (fun (entry : C.metadata) -> entry.conversation_id) (C.view s).history in
    if boundary = "legacy" then
      Alcotest.(check (list string)) "unknown legacy readiness is not revocation" ["personal preview"; "private household preview"] (ids s)
    else (
      Alcotest.(check (list string)) "known loss removes household previews immediately" ["personal preview"] (ids s);
      let s = apply s (C.History (late, Ok (history_page [household]))) in
      Alcotest.(check (list string)) "late page cannot restore revoked private titles" ["personal preview"] (ids s);
      Alcotest.(check bool) "revoked history cursor cleared" true ((C.view s).history_next_offset=None));
    Alcotest.(check string) "personal draft preserved" "personal draft survives" (C.view s).draft;
    let s = apply s (C.Event (stream, event 1 delta)) |> fun s -> apply s (C.Flush stream) in
    Alcotest.(check string) "personal stream remains current" "héllo <img onerror=evil()>" (text s)
  ) ["membership"; "consent"; "legacy"]

let () =
  Alcotest.run "advisor client state"
    [
      ( "snapshot_and_event_reduction",
        List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [
            ("private history preview revocation", history_preview_revocation);
            ("append older conversation history", history_append);
            ("revoked pending household context", revoked_pending_household_context);
            ("scoped cached draft revocation", scoped_cached_draft_revocation);
            ("pending acceptance consumption", pending_acceptance_consumption);
            ("cached readiness revocation", cached_readiness_revocation);
            ("scoped consent continuity", scoped_consent_continuity);
            ("first acceptance and revocation", first_acceptance_and_revocation);
            ("denied ownership handoff", denied_ownership_handoff);
            ("per conversation memory-only drafts", conversation_drafts);
            ("failed conflict recovery", failed_conflict_recovery);
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
