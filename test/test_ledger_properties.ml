open Hegel
open Test_support

(* A model-based test of the ledger: random batches of transfers, pending posts and voids,
   reversals and retries, over a few accounts with balance limits. Keys come from a small pool so
   that retries and key collisions are common, and amounts are small enough that the limits are
   hit. The invariants are checked after every step. *)

type state = {
  store : Mem_store.t;
  env : Ledger_core.env;
  accounts : Account_id.t array;
  transfers : Transfer_id.t Stateful.Pool.t;  (** every transfer created so far *)
  pendings : Transfer_id.t Stateful.Pool.t;  (** every pending transfer created so far *)
  succeeded : (Owner.t * Ledger_event.t list) Stateful.Pool.t;  (** batches in which every event succeeded *)
}

let init tc =
  let store = Mem_store.create () and env = env () in
  let no_overdraft = { Account.no_flags with credits_must_not_exceed_debits = true } in
  let accounts =
    [| open_account store env "checking" Asset ~flags:no_overdraft; open_account store env "savings" Asset;
       open_account store env "salary" Income; open_account store env "groceries" Expense;
       open_account store env "card" Liability ~flags:{ Account.no_flags with debits_must_not_exceed_credits = true };
       open_account store env "euros" Asset ~currency:eur |]
  in
  { store; env; accounts; transfers = Stateful.Pool.create tc; pendings = Stateful.Pool.create tc;
    succeeded = Stateful.Pool.create tc }

let draw_account tc state = state.accounts.(draw tc (integers ~min_value:0 ~max_value:(Array.length state.accounts - 1) ()))

(* An existing id from the pool, or occasionally an unknown one. *)
let draw_reference tc pool =
  if Stateful.Pool.size pool = 0 || draw tc (booleans ~p:0.1 ()) then Transfer_id.fresh ()
  else draw_silent tc (Stateful.Pool.values_reusable pool)

let draw_date tc = Date.add_days today (draw tc (integers ~min_value:(-3) ~max_value:3 ()))

let draw_event tc state : Ledger_event.t =
  let key = key (Printf.sprintf "k%d" (draw tc (integers ~min_value:0 ~max_value:15 ()))) in
  let linked = draw tc (booleans ~p:0.3 ()) in
  let body : Ledger_event.body =
    match draw tc (integers ~min_value:0 ~max_value:4 ()) with
    | 0 | 1 ->
        let phase : Ledger_event.phase =
          if draw tc (booleans ~p:0.3 ()) then Pending { expires_on = Some (draw_date tc) } else Posted
        in
        Transfer
          { debit_account_id = draw_account tc state; credit_account_id = draw_account tc state;
            amount = amount (draw tc (integers ~min_value:1 ~max_value:500 ())); phase; code = Purchase;
            memo = Transfer.empty_memo; value_date = draw_date tc }
    | 2 ->
        let amount = if draw tc (booleans ()) then None else Some (amount (draw tc (integers ~min_value:1 ~max_value:500 ()))) in
        Post_pending { pending_id = draw_reference tc state.pendings; amount; value_date = draw_date tc }
    | 3 -> Void_pending { pending_id = draw_reference tc state.pendings; value_date = draw_date tc }
    | _ -> Reverse { transfer_id = draw_reference tc state.transfers; value_date = None; memo = None }
  in
  { key; linked; body }

let created_transfer = function Ok (Ledger_core.Created (transfer : Transfer.t)) -> Some transfer | _ -> None

(* Every chain in a batch was either created in full or not at all. *)
let check_chains_are_atomic events results =
  let rec go events results chain =
    match events, results with
    | [], [] -> assert (chain = [] || List.for_all Option.is_none chain)
    | (event : Ledger_event.t) :: events, result :: results ->
        let chain = created_transfer result :: chain in
        if event.linked then go events results chain
        else (
          assert (List.for_all Option.is_some chain || List.for_all Option.is_none chain);
          go events results [])
    | _ -> assert false
  in
  go events results []

module%hegel_state_machine Ledger_machine = struct
  type nonrec state = state

  let submit tc state =
    let scope = if draw tc (booleans ~p:0.2 ()) then household else me in
    let events = List.init (draw tc (integers ~min_value:1 ~max_value:4 ())) (fun _ -> draw_event tc state) in
    let before = List.length (Mem_store.transfers state.store) in
    let results = submit_ok state.store state.env ~scope events in
    check_chains_are_atomic events results;
    (* The store holds exactly the transfers reported as created, so a failed chain left nothing behind. *)
    let reported = List.length (List.filter_map created_transfer results) in
    assert (List.length (Mem_store.transfers state.store) = before + reported);
    List.iter
      (fun result ->
        Option.iter
          (fun (transfer : Transfer.t) ->
            Stateful.Pool.add state.transfers tc transfer.id;
            match transfer.kind with Pending _ -> Stateful.Pool.add state.pendings tc transfer.id | _ -> ())
          (created_transfer result))
      results;
    if List.for_all Result.is_ok results then Stateful.Pool.add state.succeeded tc (scope, events)
  [@@rule { weight = 4.0 }]

  (* Resubmitting a batch that fully succeeded changes nothing and reports every event as existing. *)
  let retry tc state =
    let scope, events = draw_silent tc (Stateful.Pool.values_reusable state.succeeded) in
    let before = List.length (Mem_store.transfers state.store) in
    let results = submit_ok state.store state.env ~scope events in
    assert (List.for_all (function Ok (Ledger_core.Exists _) -> true | _ -> false) results);
    assert (List.length (Mem_store.transfers state.store) = before)

  let expire tc state =
    let today = draw_date tc in
    let before = List.length (Mem_store.transfers state.store) in
    let voided = L.void_expired state.store state.env ~today in
    assert (List.for_all (function _, Ok [ Ok (Ledger_core.Created _) ] -> true | _ -> false) voided);
    assert (List.length (Mem_store.transfers state.store) = before + List.length voided);
    assert (L.void_expired state.store state.env ~today = [])

  let balances_match_transfers _tc state = assert (L.verify state.store = [])
  [@@invariant { always_check = true }]

  (* Double entry: every amount is debited to one account and credited to another. *)
  let debits_equal_credits _tc state =
    let sum f = List.fold_left (fun total (account : Account.t) -> Int64.add total (f account.balances)) 0L (Mem_store.accounts state.store) in
    assert (Int64.equal (sum (fun b -> b.debits_posted)) (sum (fun b -> b.credits_posted)));
    assert (Int64.equal (sum (fun b -> b.debits_pending)) (sum (fun b -> b.credits_pending)))
  [@@invariant { always_check = true }]

  let limits_hold _tc state =
    List.iter
      (fun (account : Account.t) ->
        let b = account.balances in
        if account.flags.debits_must_not_exceed_credits then assert (Int64.add b.debits_posted b.debits_pending <= b.credits_posted);
        if account.flags.credits_must_not_exceed_debits then assert (Int64.add b.credits_posted b.credits_pending <= b.debits_posted))
      (Mem_store.accounts state.store)
  [@@invariant { always_check = true }]

  (* A pending transfer is resolved at most once, a transfer is reversed at most once, and a post
     never exceeds what was reserved. *)
  let resolved_and_reversed_at_most_once _tc state =
    let transfers = Mem_store.transfers state.store in
    let count_by f =
      let table = Hashtbl.create 16 in
      List.iter (fun t -> Option.iter (fun id -> Hashtbl.replace table id (1 + Option.value (Hashtbl.find_opt table id) ~default:0)) (f t)) transfers;
      Hashtbl.iter (fun _ count -> assert (count = 1)) table
    in
    count_by (fun (t : Transfer.t) -> match t.kind with Post_pending { pending_id } | Void_pending { pending_id } -> Some pending_id | _ -> None);
    count_by (fun (t : Transfer.t) -> t.reverses);
    List.iter
      (fun (t : Transfer.t) ->
        match t.kind with
        | Post_pending { pending_id } ->
            let pending = List.find (fun (p : Transfer.t) -> Transfer_id.equal p.id pending_id) transfers in
            assert (Amount.compare t.amount pending.amount <= 0)
        | _ -> ())
      transfers
  [@@invariant { always_check = true }]
end

let%hegel_test ledger_state_machine tc = Ledger_machine.run tc ~init:(init tc) [@@settings Settings.create ~test_cases:300 ()]

let () =
  Alcotest.run "ledger properties"
    [ ("state machine", [ Alcotest.test_case "batches, retries and expiry keep the ledger consistent" `Slow ledger_state_machine ]) ]
