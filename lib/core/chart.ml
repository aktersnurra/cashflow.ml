(* The starter chart of accounts a new user or household gets. Keys are derived from the slug, so
   seeding twice opens nothing twice; the names are the category.<slug> messages in locales/. *)
type scope = Personal | Shared | Both

type entry = { slug : string; kind : Account.kind; role : Account_info.role option; scope : scope }

let entry ?role ?(scope = Both) slug kind = { slug; kind; role; scope }

let entries =
  [ entry "uncategorized-expense" Expense ~role:Uncategorized;
    entry "uncategorized-income" Income ~role:Uncategorized;
    entry "opening-balances" Equity ~role:Opening;
    entry "unrealized-gains" Equity ~role:Unrealized ~scope:Personal;
    entry "untracked-cash" Expense ~role:Untracked_cash ~scope:Personal;
    entry "salary" Income ~scope:Personal;
    entry "other-income" Income;
    entry "interest" Income ~scope:Personal;
    entry "housing" Expense;
    entry "utilities" Expense;
    entry "groceries" Expense;
    entry "restaurants" Expense;
    entry "transport" Expense;
    entry "subscriptions" Expense;
    entry "insurance" Expense;
    entry "health" Expense;
    entry "clothes" Expense;
    entry "shopping" Expense;
    entry "children" Expense ~scope:Shared;
    entry "entertainment" Expense;
    entry "travel" Expense;
    entry "gifts" Expense;
    entry "fees" Expense;
    entry "other" Expense ]

let applies scope entry =
  match entry.scope, scope with Both, _ | Personal, `Personal | Shared, `Shared -> true | _ -> false

let key entry = Result.get_ok (Idempotency_key.of_string ("seed:" ^ entry.slug))
