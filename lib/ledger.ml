(* The ledger over some store: the SQLite store in production, the in-memory one in tests. *)
module type STORE = sig
  type t

  val view : t -> Ledger_core.view

  val transaction : t -> (unit -> 'a) -> 'a
  (** Runs the function with exclusive access to the store, so a batch is validated against the
      same state it is written to. *)

  val write : t -> Ledger_core.applied -> unit
  val save_account : t -> Account.t -> unit
  val accounts : t -> Account.t list
  val transfers : t -> Transfer.t list
  (** In the order they were recorded. *)

  val open_pending : t -> Transfer.t list
  (** Pending transfers that haven't been posted or voided. *)
end

module Make (Store : STORE) = struct
  let submit store env scope events =
    Store.transaction store (fun () ->
        match Ledger_core.apply env (Store.view store) scope events with
        | Error _ as error -> error
        | Ok (applied, results) ->
            Store.write store applied;
            Ok results)

  let open_account store env key spec =
    Store.transaction store (fun () ->
        match Ledger_core.open_account env (Store.view store) key spec with
        | Ok (Created account) as created -> Store.save_account store account; created
        | (Ok (Exists _) | Error _) as result -> result)

  let close_account store id =
    Store.transaction store (fun () ->
        match Ledger_core.close_account (Store.view store) id with
        | Ok account as closed -> Store.save_account store account; closed
        | Error _ as error -> error)

  (* Voids every open pending transfer whose expiry date is before [today]. The void's key is
     derived from the pending transfer, so running this twice voids nothing twice. *)
  let void_expired store env ~today =
    let expired (transfer : Transfer.t) =
      match transfer.kind with
      | Pending { expires_on = Some date } -> Date.compare date today < 0
      | Pending { expires_on = None } | Single | Post_pending _ | Void_pending _ -> false
    in
    Store.open_pending store |> List.filter expired
    |> List.map (fun (pending : Transfer.t) ->
           let key = Result.get_ok (Idempotency_key.of_string ("expire:" ^ Transfer_id.to_string pending.id)) in
           let event = { Ledger_event.key; linked = false; body = Void_pending { pending_id = pending.id; value_date = today } } in
           (pending, submit store env pending.scope [ event ]))

  let verify store = Ledger_core.verify ~accounts:(Store.accounts store) ~transfers:(Store.transfers store)
end
