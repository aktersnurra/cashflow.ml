(* An in-memory store for tests. *)
type t = {
  accounts : (Account_id.t, Account.t) Hashtbl.t;
  accounts_by_key : (string, Account.t) Hashtbl.t;
  transfers : (Transfer_id.t, Transfer.t) Hashtbl.t;
  transfers_by_key : (string, Transfer.t) Hashtbl.t;
  resolutions : (Transfer_id.t, Transfer.t) Hashtbl.t;
  reversals : (Transfer_id.t, Transfer.t) Hashtbl.t;
  mutable log : Transfer.t list;  (** newest first *)
}

let create () =
  { accounts = Hashtbl.create 16; accounts_by_key = Hashtbl.create 16; transfers = Hashtbl.create 64;
    transfers_by_key = Hashtbl.create 64; resolutions = Hashtbl.create 16; reversals = Hashtbl.create 16; log = [] }

let scoped_key owner key = Owner.to_string owner ^ "\x00" ^ Idempotency_key.to_string key

let view store : Ledger_core.view =
  { account = Hashtbl.find_opt store.accounts;
    account_by_key = (fun owner key -> Hashtbl.find_opt store.accounts_by_key (scoped_key owner key));
    transfer = Hashtbl.find_opt store.transfers;
    transfer_by_key = (fun owner key -> Hashtbl.find_opt store.transfers_by_key (scoped_key owner key));
    resolution = Hashtbl.find_opt store.resolutions; reversal = Hashtbl.find_opt store.reversals }

let transaction _store f = f ()

let save_account store (account : Account.t) =
  Hashtbl.replace store.accounts account.id account;
  Hashtbl.replace store.accounts_by_key (scoped_key account.owner account.key) account

let write store applied =
  List.iter
    (fun (transfer : Transfer.t) ->
      Hashtbl.replace store.transfers transfer.id transfer;
      Hashtbl.replace store.transfers_by_key (scoped_key transfer.scope transfer.key) transfer;
      (match transfer.kind with
      | Post_pending { pending_id } | Void_pending { pending_id } -> Hashtbl.replace store.resolutions pending_id transfer
      | Single | Pending _ -> ());
      Option.iter (fun id -> Hashtbl.replace store.reversals id transfer) transfer.reverses;
      store.log <- transfer :: store.log)
    (Ledger_core.transfers applied);
  List.iter (save_account store) (Ledger_core.accounts applied)

let accounts store = Hashtbl.to_seq_values store.accounts |> List.of_seq
let transfers store = List.rev store.log

let open_pending store =
  transfers store
  |> List.filter (fun (transfer : Transfer.t) ->
         match transfer.kind with
         | Pending _ -> not (Hashtbl.mem store.resolutions transfer.id)
         | Single | Post_pending _ | Void_pending _ -> false)
