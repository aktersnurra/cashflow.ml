(* The danger zone: wipe what statements imported. The ledger never forgets, so this first undoes every
   active import, newest first, exactly as the Accounts page does (a partner row in the other scope goes back to
   its inbox). Then it deletes what those imports and their undoing left behind: the rows, their reversals, the
   inbox and the bookkeeping, and sets the stored balances of the accounts they touched from what remains.
   Accounts, categories, rules, budgets, goals, opening balances and anything entered by hand stay. *)
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id

open Sql

type summary = { imports : int; transfers : int }

let insert_batch = stmt "INSERT OR IGNORE INTO reset_batches (id) VALUES (?)" (text @-> nil) exec

(* What the imports made: their new rows, what triage posted about those, and the reversals of both. Only
   a transfer that is reversed, or is a reversal, is ever deleted, so what stays still balances. *)
let collect =
  [ "INSERT OR IGNORE INTO reset_transfers (id) SELECT transfer_id FROM import_rows \
     WHERE batch_id IN (SELECT id FROM reset_batches) AND status = 'new' AND transfer_id IS NOT NULL";
    "INSERT OR IGNORE INTO reset_transfers (id) SELECT transfer_id FROM triage_transfers \
     WHERE item_transfer_id IN (SELECT id FROM reset_transfers)";
    "INSERT OR IGNORE INTO reset_transfers (id) SELECT id FROM transfers WHERE reverses IN (SELECT id FROM reset_transfers)";
    "DELETE FROM reset_transfers WHERE id NOT IN (SELECT reverses FROM transfers WHERE reverses IS NOT NULL) \
     AND id NOT IN (SELECT id FROM transfers WHERE reverses IS NOT NULL)";
    "INSERT OR IGNORE INTO reset_accounts (id) SELECT debit_account_id FROM transfers WHERE id IN (SELECT id FROM reset_transfers)";
    "INSERT OR IGNORE INTO reset_accounts (id) SELECT credit_account_id FROM transfers WHERE id IN (SELECT id FROM reset_transfers)" ]

let delete =
  [ "DELETE FROM triage_transfers WHERE transfer_id IN (SELECT id FROM reset_transfers) OR item_transfer_id IN (SELECT id FROM reset_transfers)";
    "DELETE FROM inbox_items WHERE transfer_id IN (SELECT id FROM reset_transfers)";
    "DELETE FROM statement_legs WHERE batch_id IN (SELECT id FROM reset_batches)";
    "DELETE FROM transfer_tags WHERE transfer_id IN (SELECT id FROM reset_transfers)";
    "DELETE FROM import_rows WHERE batch_id IN (SELECT id FROM reset_batches) OR transfer_id IN (SELECT id FROM reset_transfers)";
    "DELETE FROM reconciliation_checkpoints WHERE import_batch_id IN (SELECT id FROM reset_batches)";
    "DELETE FROM transfers WHERE id IN (SELECT id FROM reset_transfers)";
    "DELETE FROM import_batches WHERE id IN (SELECT id FROM reset_batches)";
    (* A pending transfer's columns are untouched: only posted pairs were deleted. *)
    "UPDATE accounts SET \
     debits_posted = coalesce((SELECT sum(amount) FROM transfers WHERE debit_account_id = accounts.id AND kind IN ('single', 'post_pending')), 0), \
     credits_posted = coalesce((SELECT sum(amount) FROM transfers WHERE credit_account_id = accounts.id AND kind IN ('single', 'post_pending')), 0) \
     WHERE id IN (SELECT id FROM reset_accounts)" ]

let count = stmt "SELECT count(*) FROM reset_transfers" nil (one (row (int @-> nil) Fun.id))

let purge db batches =
  Sql.transaction db (fun () ->
      List.iter (exec_script db)
        [ "CREATE TEMP TABLE reset_batches (id TEXT PRIMARY KEY)"; "CREATE TEMP TABLE reset_transfers (id TEXT PRIMARY KEY)";
          "CREATE TEMP TABLE reset_accounts (id TEXT PRIMARY KEY)" ];
      Fun.protect
        ~finally:(fun () -> List.iter (exec_script db) [ "DROP TABLE reset_batches"; "DROP TABLE reset_transfers"; "DROP TABLE reset_accounts" ])
        (fun () ->
          List.iter (run db insert_batch) batches;
          List.iter (exec_script db) collect;
          let transfers = run db count in
          List.iter (exec_script db) delete;
          transfers))

(* Reset every import into [accounts]. The undoing can refuse (a later import elsewhere depends on one); then
   what was undone stays undone and nothing is deleted. *)
let imports (ctx : Import.ctx) ~(accounts : Account_id.t list) =
  let db = ctx.service.db in
  let batches =
    List.concat_map (fun account_id -> Import_store.batches db ~account_id ~limit:max_int) accounts
    |> List.sort (fun (a : Import_store.batch) (b : Import_store.batch) -> Ptime.compare b.created_at a.created_at)
  in
  let rec undo_all = function
    | [] -> Ok ()
    | (b : Import_store.batch) :: rest when b.undone -> undo_all rest
    | b :: rest -> (
        match Import.undo ctx ~batch_id:b.batch_id with Ok _ -> undo_all rest | Error e -> Error e)
  in
  match undo_all batches with
  | Error e -> Error e
  | Ok () ->
      let ids = List.map (fun (b : Import_store.batch) -> b.batch_id) batches in
      Ok { imports = List.length batches; transfers = purge db ids }
