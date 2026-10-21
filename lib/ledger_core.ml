module Account_map = Map.Make (Account_id)
module Transfer_map = Map.Make (Transfer_id)
module Key_map = Map.Make (String)

type view = {
  account : Account_id.t -> Account.t option;
  account_by_key : Owner.t -> Idempotency_key.t -> Account.t option;
  transfer : Transfer_id.t -> Transfer.t option;
  transfer_by_key : Owner.t -> Idempotency_key.t -> Transfer.t option;
  resolution : Transfer_id.t -> Transfer.t option;
  reversal : Transfer_id.t -> Transfer.t option;
}

type env = { now : unit -> Ptime.t; fresh_account_id : unit -> Account_id.t; fresh_transfer_id : unit -> Transfer_id.t }
type 'a outcome = Created of 'a | Exists of 'a
type applied = { transfers : Transfer.t list; accounts : Account.t list }

let transfers applied = applied.transfers
let accounts applied = applied.accounts
let max_batch_size = 8190
let ( let* ) = Result.bind

(* What the current batch has changed so far, layered over the store's view. The maps are
   persistent, so rolling back a failed chain is keeping the overlay from before it. *)
type overlay = {
  changed_accounts : Account.t Account_map.t;
  transfers_by_id : Transfer.t Transfer_map.t;
  transfers_by_key : Transfer.t Key_map.t;
  resolutions : Transfer.t Transfer_map.t;
  reversals : Transfer.t Transfer_map.t;
  created : Transfer.t list;  (** newest first *)
}

let empty_overlay =
  { changed_accounts = Account_map.empty; transfers_by_id = Transfer_map.empty; transfers_by_key = Key_map.empty;
    resolutions = Transfer_map.empty; reversals = Transfer_map.empty; created = [] }

let scoped_key owner key = Owner.to_string owner ^ "\x00" ^ Idempotency_key.to_string key

let layered find_local find_view key = match find_local key with Some _ as found -> found | None -> find_view key

let find_account view ov id = layered (fun id -> Account_map.find_opt id ov.changed_accounts) view.account id
let find_transfer view ov id = layered (fun id -> Transfer_map.find_opt id ov.transfers_by_id) view.transfer id
let find_resolution view ov id = layered (fun id -> Transfer_map.find_opt id ov.resolutions) view.resolution id
let find_reversal view ov id = layered (fun id -> Transfer_map.find_opt id ov.reversals) view.reversal id

let find_by_key view ov scope key =
  layered (fun key -> Key_map.find_opt (scoped_key scope key) ov.transfers_by_key) (view.transfer_by_key scope) key

let record_transfer ov (transfer : Transfer.t) =
  let resolutions =
    match transfer.kind with
    | Post_pending { pending_id } | Void_pending { pending_id } -> Transfer_map.add pending_id transfer ov.resolutions
    | Single | Pending _ -> ov.resolutions
  in
  let reversals =
    match transfer.reverses with Some id -> Transfer_map.add id transfer ov.reversals | None -> ov.reversals
  in
  { ov with
    transfers_by_id = Transfer_map.add transfer.id transfer ov.transfers_by_id;
    transfers_by_key = Key_map.add (scoped_key transfer.scope transfer.key) transfer ov.transfers_by_key;
    resolutions; reversals; created = transfer :: ov.created }

let record_accounts ov accounts =
  { ov with
    changed_accounts =
      List.fold_left (fun map (account : Account.t) -> Account_map.add account.id account map) ov.changed_accounts accounts }

(* Balance arithmetic. Every column only ever grows, except pending columns, which shrink by
   exactly what a pending transfer added to them, so only additions can overflow. *)

let add column amount = Option.to_result ~none:`Overflow (Amount.checked_add column (Amount.to_int64 amount))
let sub column amount = Int64.sub column (Amount.to_int64 amount)

let update (account : Account.t) f = { account with balances = f account.balances }

let post_amount (dr : Account.t) (cr : Account.t) amount =
  let* debits_posted = add dr.balances.debits_posted amount in
  let* credits_posted = add cr.balances.credits_posted amount in
  Ok (update dr (fun b -> { b with debits_posted }), update cr (fun b -> { b with credits_posted }))

let reserve_amount (dr : Account.t) (cr : Account.t) amount =
  let* debits_pending = add dr.balances.debits_pending amount in
  let* credits_pending = add cr.balances.credits_pending amount in
  Ok (update dr (fun b -> { b with debits_pending }), update cr (fun b -> { b with credits_pending }))

let release_amount (dr : Account.t) (cr : Account.t) amount =
  ( update dr (fun b -> { b with debits_pending = sub b.debits_pending amount }),
    update cr (fun b -> { b with credits_pending = sub b.credits_pending amount }) )

(* Only the debit side's debit limit and the credit side's credit limit can be breached by a
   transfer; the other two directions only move an account away from its limit. *)
let within_limits (dr : Account.t) (cr : Account.t) =
  let exceeds ~limited ~posted ~pending ~other =
    if not limited then Ok false
    else
      let* exposure = Option.to_result ~none:`Overflow (Amount.checked_add posted pending) in
      Ok (Int64.compare exposure other > 0)
  in
  let* debits_exceed =
    exceeds ~limited:dr.flags.debits_must_not_exceed_credits ~posted:dr.balances.debits_posted
      ~pending:dr.balances.debits_pending ~other:dr.balances.credits_posted
  in
  let* credits_exceed =
    exceeds ~limited:cr.flags.credits_must_not_exceed_debits ~posted:cr.balances.credits_posted
      ~pending:cr.balances.credits_pending ~other:cr.balances.debits_posted
  in
  if debits_exceed then Error `Exceeds_credits else if credits_exceed then Error `Exceeds_debits else Ok ()

let open_accounts view ov ~debit ~credit =
  if Account_id.equal debit credit then Error `Accounts_must_be_different
  else
    match find_account view ov debit, find_account view ov credit with
    | None, _ -> Error `Debit_account_not_found
    | _, None -> Error `Credit_account_not_found
    | Some (dr : Account.t), Some (cr : Account.t) ->
        if dr.closed then Error `Debit_account_closed
        else if cr.closed then Error `Credit_account_closed
        else if not (Currency.equal dr.currency cr.currency) then Error `Accounts_must_have_same_currency
        else Ok (dr, cr)

let open_pending view ov pending_id =
  match find_transfer view ov pending_id with
  | None -> Error `Pending_transfer_not_found
  | Some ({ kind = Pending _; _ } as pending : Transfer.t) -> (
      match find_resolution view ov pending_id with
      | None -> Ok pending
      | Some { kind = Post_pending _; _ } -> Error `Pending_transfer_already_posted
      | Some _ -> Error `Pending_transfer_already_voided)
  | Some _ -> Error `Pending_transfer_not_pending

(* Whether a retried event asks for exactly what the transfer already stored under its key. *)
let same_request view ov (existing : Transfer.t) (body : Ledger_event.body) =
  let optional equal requested actual = match requested with None -> true | Some value -> equal value actual in
  match body, existing.kind with
  | Transfer t, (Single | Pending _) ->
      let same_phase =
        match t.phase, existing.kind with
        | Posted, Single -> true
        | Pending { expires_on = a }, Pending { expires_on = b } -> Option.equal Date.equal a b
        | _ -> false
      in
      same_phase && Option.is_none existing.reverses
      && Account_id.equal t.debit_account_id existing.debit_account_id
      && Account_id.equal t.credit_account_id existing.credit_account_id
      && Amount.equal t.amount existing.amount && t.code = existing.code && Transfer.normalize_memo t.memo = existing.memo
      && Date.equal t.value_date existing.value_date
  | Post_pending { pending_id; amount; value_date }, Post_pending { pending_id = stored } ->
      let requested_amount =
        match amount with
        | Some amount -> Some amount
        | None -> Option.map (fun (pending : Transfer.t) -> pending.amount) (find_transfer view ov pending_id)
      in
      Transfer_id.equal pending_id stored && Date.equal value_date existing.value_date
      && Option.equal Amount.equal requested_amount (Some existing.amount)
  | Void_pending { pending_id; value_date }, Void_pending { pending_id = stored } ->
      Transfer_id.equal pending_id stored && Date.equal value_date existing.value_date
  | Reverse { transfer_id; value_date; memo }, (Single | Post_pending _) ->
      Option.equal Transfer_id.equal (Some transfer_id) existing.reverses
      && optional Date.equal value_date existing.value_date
      && optional (fun memo existing -> Transfer.normalize_memo memo = existing) memo existing.memo
  | (Transfer _ | Post_pending _ | Void_pending _ | Reverse _), _ -> false

let apply_new env view scope ov (event : Ledger_event.t) =
  let new_transfer ~debit ~credit ~amount ~kind ~code ~memo ~value_date ~reverses : Transfer.t =
    { id = env.fresh_transfer_id (); scope; key = event.key; debit_account_id = debit; credit_account_id = credit;
      amount; kind; code; memo = Transfer.normalize_memo memo; value_date; reverses; recorded_at = env.now () }
  in
  let posted ~debit ~credit ~amount ~code ~memo ~value_date ~reverses =
    let* dr, cr = open_accounts view ov ~debit ~credit in
    let* dr, cr = post_amount dr cr amount in
    let* () = within_limits dr cr in
    let transfer = new_transfer ~debit ~credit ~amount ~kind:Single ~code ~memo ~value_date ~reverses in
    Ok (transfer, [ dr; cr ])
  in
  let* transfer, accounts =
    match event.body with
    | Transfer { debit_account_id = debit; credit_account_id = credit; amount; phase = Posted; code; memo; value_date }
      ->
        posted ~debit ~credit ~amount ~code ~memo ~value_date ~reverses:None
    | Transfer { debit_account_id = debit; credit_account_id = credit; amount; phase = Pending { expires_on }; code;
                 memo; value_date } ->
        let* dr, cr = open_accounts view ov ~debit ~credit in
        let* dr, cr = reserve_amount dr cr amount in
        let* () = within_limits dr cr in
        let kind = Transfer.Pending { expires_on } in
        Ok (new_transfer ~debit ~credit ~amount ~kind ~code ~memo ~value_date ~reverses:None, [ dr; cr ])
    | Post_pending { pending_id; amount; value_date } ->
        let* pending = open_pending view ov pending_id in
        let amount = Option.value amount ~default:pending.amount in
        if Amount.compare amount pending.amount > 0 then Error `Exceeds_pending_transfer_amount
        else
          let debit = pending.debit_account_id and credit = pending.credit_account_id in
          let* dr, cr = open_accounts view ov ~debit ~credit in
          let dr, cr = release_amount dr cr pending.amount in
          let* dr, cr = post_amount dr cr amount in
          let kind = Transfer.Post_pending { pending_id } in
          Ok
            ( new_transfer ~debit ~credit ~amount ~kind ~code:pending.code ~memo:pending.memo ~value_date ~reverses:None,
              [ dr; cr ] )
    | Void_pending { pending_id; value_date } ->
        let* pending = open_pending view ov pending_id in
        let debit = pending.debit_account_id and credit = pending.credit_account_id in
        let* dr, cr = open_accounts view ov ~debit ~credit in
        let dr, cr = release_amount dr cr pending.amount in
        let kind = Transfer.Void_pending { pending_id } in
        Ok
          ( new_transfer ~debit ~credit ~amount:pending.amount ~kind ~code:pending.code ~memo:pending.memo ~value_date
              ~reverses:None,
            [ dr; cr ] )
    | Reverse { transfer_id; value_date; memo } -> (
        match find_transfer view ov transfer_id with
        | None -> Error `Transfer_not_found
        | Some ({ kind = Single | Post_pending _; reverses = None; _ } as target) ->
            if Option.is_some (find_reversal view ov transfer_id) then Error `Transfer_already_reversed
            else
              posted ~debit:target.credit_account_id ~credit:target.debit_account_id ~amount:target.amount
                ~code:Reversal
                ~memo:(Option.value memo ~default:target.memo)
                ~value_date:(Option.value value_date ~default:target.value_date)
                ~reverses:(Some transfer_id)
        | Some _ -> Error `Transfer_not_reversible)
  in
  Ok (Created transfer, record_accounts (record_transfer ov transfer) accounts)

let apply_event env view scope ov (event : Ledger_event.t) =
  match find_by_key view ov scope event.key with
  | Some existing ->
      if same_request view ov existing event.body then Ok (Exists existing, ov) else Error `Exists_with_different_fields
  | None -> apply_new env view scope ov event

(* Splits a batch into chains of linked events, each tagged with the index of its first event.
   A chain is open when its last event is still marked linked, which can only be the last chain. *)
let chains events =
  let close start chain acc = (start, List.rev chain) :: acc in
  let rec go index start chain acc = function
    | [] -> (match chain with [] -> List.rev acc | _ -> List.rev (close start chain acc))
    | (event : Ledger_event.t) :: rest ->
        let chain = event :: chain in
        if event.linked then go (index + 1) start chain acc rest else go (index + 1) (index + 1) [] (close start chain acc) rest
  in
  go 0 0 [] [] events

let apply env view scope events =
  let count = List.length events in
  if count = 0 then Error `Batch_empty
  else if count > max_batch_size then Error (`Batch_too_large max_batch_size)
  else
    let results = Array.make count (Error `Linked_event_failed) in
    let fail_chain start chain index error =
      List.iteri (fun offset _ -> results.(start + offset) <- Error (if start + offset = index then error else `Linked_event_failed)) chain
    in
    let apply_chain ov (start, chain) =
      let last = start + List.length chain - 1 in
      if (List.nth chain (List.length chain - 1)).Ledger_event.linked then (fail_chain start chain last `Linked_event_chain_open; ov)
      else
        let rec go chain_ov index outcomes = function
          | [] -> Ok (chain_ov, List.rev outcomes)
          | event :: rest -> (
              match apply_event env view scope chain_ov event with
              | Ok (outcome, chain_ov) -> go chain_ov (index + 1) (outcome :: outcomes) rest
              | Error error -> Error (index, error))
        in
        match go ov start [] chain with
        | Error (index, error) -> fail_chain start chain index error; ov
        | Ok (chain_ov, outcomes) ->
            let created = List.exists (function Created _ -> true | Exists _ -> false) outcomes in
            let existed = List.exists (function Exists _ -> true | Created _ -> false) outcomes in
            if created && existed then (
              (* A chain is atomic, so it either exists in full or is created in full. A partial
                 match means a key was reused across chains: report what exists and create nothing. *)
              List.iteri
                (fun offset outcome ->
                  results.(start + offset) <-
                    (match outcome with Exists _ -> Ok outcome | Created _ -> Error `Linked_event_failed))
                outcomes;
              ov)
            else (
              List.iteri (fun offset outcome -> results.(start + offset) <- Ok outcome) outcomes;
              chain_ov)
    in
    let ov = List.fold_left apply_chain empty_overlay (chains events) in
    let applied = { transfers = List.rev ov.created; accounts = List.map snd (Account_map.bindings ov.changed_accounts) } in
    Ok (applied, Array.to_list results)

let open_account env view key (spec : Account.spec) =
  let* spec = Account.validate_spec spec in
  match view.account_by_key spec.owner key with
  | Some existing -> if Account.matches_spec existing spec then Ok (Exists existing) else Error `Exists_with_different_fields
  | None ->
      Ok
        (Created
           { Account.id = env.fresh_account_id (); key; owner = spec.owner; kind = spec.kind; currency = spec.currency;
             name = spec.name; flags = spec.flags; closed = false; balances = Account.zero_balances;
             created_at = env.now () })

let close_account view id =
  match view.account id with
  | None -> Error `Account_not_found
  | Some (account : Account.t) ->
      if account.closed then Ok account
      else if Account.has_pending account then Error `Account_has_pending_transfers
      else if (account.kind = Asset || account.kind = Liability) && not (Int64.equal (Account.posted_balance account) 0L)
      then Error `Account_balance_not_zero
      else Ok { account with closed = true }

type mismatch = { account_id : Account_id.t; stored : Account.balances; expected : Account.balances }

(* Recomputes every balance from the transfers alone, in any order: a post or void subtracts the
   pending amount it resolved, which is looked up from the pending transfer itself. *)
let verify ~accounts ~transfers =
  let expected = Hashtbl.create 64 in
  List.iter (fun (account : Account.t) -> Hashtbl.replace expected account.id Account.zero_balances) accounts;
  let pending_amounts = Hashtbl.create 64 in
  List.iter
    (fun (transfer : Transfer.t) ->
      match transfer.kind with
      | Pending _ -> Hashtbl.replace pending_amounts transfer.id (Amount.to_int64 transfer.amount)
      | Single | Post_pending _ | Void_pending _ -> ())
    transfers;
  let adjust id f =
    let balances = Option.value (Hashtbl.find_opt expected id) ~default:Account.zero_balances in
    Hashtbl.replace expected id (f balances)
  in
  let apply (transfer : Transfer.t) ~posted ~pending =
    adjust transfer.debit_account_id (fun b ->
        { b with debits_posted = Int64.add b.debits_posted posted; debits_pending = Int64.add b.debits_pending pending });
    adjust transfer.credit_account_id (fun b ->
        { b with credits_posted = Int64.add b.credits_posted posted; credits_pending = Int64.add b.credits_pending pending })
  in
  let resolved pending_id = Int64.neg (Option.value (Hashtbl.find_opt pending_amounts pending_id) ~default:0L) in
  List.iter
    (fun (transfer : Transfer.t) ->
      let amount = Amount.to_int64 transfer.amount in
      match transfer.kind with
      | Single -> apply transfer ~posted:amount ~pending:0L
      | Pending _ -> apply transfer ~posted:0L ~pending:amount
      | Post_pending { pending_id } -> apply transfer ~posted:amount ~pending:(resolved pending_id)
      | Void_pending { pending_id } -> apply transfer ~posted:0L ~pending:(resolved pending_id))
    transfers;
  List.filter_map
    (fun (account : Account.t) ->
      let expected = Hashtbl.find expected account.id in
      if expected = account.balances then None else Some { account_id = account.id; stored = account.balances; expected })
    accounts
