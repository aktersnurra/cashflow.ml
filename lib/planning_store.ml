(* Budgets, savings goals and reconciliation checkpoints. *)
open Sql

let budget_row =
  row Codec.(account_id @-> text @-> int64 @-> bool @-> nil) (fun category_id effective_from monthly_amount rollover ->
      (effective_from, { Analytics.category_id; monthly_amount; rollover }))

let budget_versions_of =
  stmt
    "SELECT category_account_id, effective_from, monthly_amount, rollover FROM budget_versions \
     WHERE owner_kind = ? AND owner_id = ? ORDER BY effective_from"
    (text @-> text @-> nil) (many budget_row)

let upsert_budget =
  stmt
    "INSERT INTO budget_versions (owner_kind, owner_id, category_account_id, effective_from, monthly_amount, rollover, created_at) \
     VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT (category_account_id, effective_from) DO UPDATE SET \
     monthly_amount = excluded.monthly_amount, rollover = excluded.rollover"
    Codec.(text @-> text @-> account_id @-> text @-> int64 @-> bool @-> time @-> nil)
    exec

(* Every version for an owner, as a lookup from (cycle label, category) to the version in force. *)
let budgets db owner =
  let versions = run db budget_versions_of (Codec.owner_kind owner) (Codec.owner_id owner) in
  let budget_for (cycle : Cycle.t) category =
    List.fold_left
      (fun found (effective_from, (budget : Analytics.budget)) ->
        if Account_id.equal budget.category_id category && String.compare effective_from (Cycle.label cycle) <= 0 then Some budget
        else found)
      None versions
    |> Option.to_list
    |> List.filter (fun (b : Analytics.budget) -> Int64.compare b.monthly_amount 0L > 0)
    |> fun found -> List.nth_opt found 0
  in
  let in_force cycle =
    List.sort_uniq Account_id.compare (List.map (fun (_, (b : Analytics.budget)) -> b.category_id) versions)
    |> List.filter_map (budget_for cycle)
  in
  (in_force, budget_for)

let set_budget db ~now owner ~category_id ~effective_from ~monthly_amount ~rollover =
  run db upsert_budget (Codec.owner_kind owner) (Codec.owner_id owner) category_id effective_from monthly_amount rollover now

type goal = {
  goal_id : string;
  goal_owner : Owner.t;
  name : string;
  currency : string;
  target_amount : int64;
  target_date : Date.t option;
  monthly_amount : int64 option;
  earmarks : (Account_id.t * int64) list;
}

let goal_row =
  row
    Codec.(text @-> text @-> text @-> text @-> text @-> int64 @-> nullable date @-> nullable int64 @-> nil)
    (fun goal_id kind id name currency target_amount target_date monthly_amount ->
      { goal_id; goal_owner = Codec.owner kind id; name; currency; target_amount; target_date; monthly_amount; earmarks = [] })

let goals_of =
  stmt
    "SELECT id, owner_kind, owner_id, name, currency, target_amount, target_date, monthly_amount FROM savings_goals \
     WHERE owner_kind = ? AND owner_id = ? AND archived = 0 ORDER BY created_at"
    (text @-> text @-> nil) (many goal_row)

let goal_by_id =
  stmt
    "SELECT id, owner_kind, owner_id, name, currency, target_amount, target_date, monthly_amount FROM savings_goals WHERE id = ?"
    (text @-> nil) (opt goal_row)

let earmarks_of =
  stmt "SELECT account_id, amount FROM goal_earmarks WHERE goal_id = ? AND amount > 0" (text @-> nil)
    (many (row Codec.(account_id @-> int64 @-> nil) (fun id amount -> (id, amount))))

let earmarked_on =
  stmt "SELECT coalesce(sum(amount), 0) FROM goal_earmarks e JOIN savings_goals g ON g.id = e.goal_id \
        WHERE e.account_id = ? AND g.archived = 0 AND e.goal_id <> ?"
    Codec.(account_id @-> text @-> nil) (one (row (int64 @-> nil) Fun.id))

let insert_goal =
  stmt
    "INSERT INTO savings_goals (id, owner_kind, owner_id, name, currency, target_amount, target_date, monthly_amount, created_at) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (id) DO UPDATE SET name = excluded.name, \
     target_amount = excluded.target_amount, target_date = excluded.target_date, monthly_amount = excluded.monthly_amount"
    Codec.(text @-> text @-> text @-> text @-> text @-> int64 @-> nullable date @-> nullable int64 @-> time @-> nil)
    exec

let upsert_earmark =
  stmt "INSERT INTO goal_earmarks (goal_id, account_id, amount) VALUES (?, ?, ?) \
        ON CONFLICT (goal_id, account_id) DO UPDATE SET amount = excluded.amount"
    Codec.(text @-> account_id @-> int64 @-> nil)
    exec

let archive = stmt "UPDATE savings_goals SET archived = 1 WHERE id = ?" (text @-> nil) exec
let with_earmarks db goal = { goal with earmarks = run db earmarks_of goal.goal_id }
let goals db owner = List.map (with_earmarks db) (run db goals_of (Codec.owner_kind owner) (Codec.owner_id owner))
let find_goal db id = Option.map (with_earmarks db) (run db goal_by_id id)

let save_goal db ~now goal =
  run db insert_goal goal.goal_id (Codec.owner_kind goal.goal_owner) (Codec.owner_id goal.goal_owner) goal.name goal.currency
    goal.target_amount goal.target_date goal.monthly_amount now

let earmarked_elsewhere db ~account_id ~goal_id = run db earmarked_on account_id goal_id
let set_earmark db ~goal_id ~account_id ~amount = run db upsert_earmark goal_id account_id amount
let archive_goal db id = run db archive id

type checkpoint = {
  checkpoint_id : string;
  account_id : Account_id.t;
  on_date : Date.t;
  statement_balance : int64;
  ledger_balance : int64;
  source : string;
}

let checkpoint_row =
  row Codec.(text @-> account_id @-> date @-> int64 @-> int64 @-> text @-> nil)
    (fun checkpoint_id account_id on_date statement_balance ledger_balance source ->
      { checkpoint_id; account_id; on_date; statement_balance; ledger_balance; source })

let insert_checkpoint =
  stmt
    "INSERT INTO reconciliation_checkpoints (id, account_id, on_date, statement_balance, ledger_balance, source, \
     import_batch_id, created_by, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    Codec.(text @-> account_id @-> date @-> int64 @-> int64 @-> text @-> nullable text @-> user_id @-> time @-> nil)
    exec

let checkpoints_of =
  stmt
    "SELECT id, account_id, on_date, statement_balance, ledger_balance, source FROM reconciliation_checkpoints \
     WHERE account_id = ? ORDER BY on_date, created_at"
    Codec.(account_id @-> nil) (many checkpoint_row)

let add_checkpoint db ~now ~user_id ?import_batch_id ~source (c : checkpoint) =
  run db insert_checkpoint c.checkpoint_id c.account_id c.on_date c.statement_balance c.ledger_balance source import_batch_id user_id now

let checkpoints db account_id = run db checkpoints_of account_id

(* The latest date up to which the account's balance matched the bank: the last matching
   checkpoint, unless a later one disagreed. *)
let reconciled_through db account_id =
  List.fold_left
    (fun through c -> if Int64.equal c.statement_balance c.ledger_balance then Some c.on_date else None)
    None (checkpoints db account_id)
