(* The starter chart of accounts a new user or household gets, named in the creator's language.
   Keys are derived from the slug, so seeding twice opens nothing twice. *)
type scope = Personal | Shared | Both

type entry = { slug : string; kind : Account.kind; role : Account_info.role option; scope : scope; en : string; sv : string }

let entry ?role ?(scope = Both) slug kind en sv = { slug; kind; role; scope; en; sv }

let entries =
  [ entry "uncategorized-expense" Expense ~role:Uncategorized "Uncategorized" "Okategoriserat";
    entry "uncategorized-income" Income ~role:Uncategorized "Uncategorized income" "Okategoriserade inkomster";
    entry "opening-balances" Equity ~role:Opening "Opening balances" "Ingående saldon";
    entry "unrealized-gains" Equity ~role:Unrealized ~scope:Personal "Unrealized gains" "Orealiserade vinster";
    entry "untracked-cash" Expense ~role:Untracked_cash ~scope:Personal "Untracked cash" "Ospårade kontanter";
    entry "salary" Income ~scope:Personal "Salary" "Lön";
    entry "other-income" Income "Other income" "Övriga inkomster";
    entry "interest" Income ~scope:Personal "Interest" "Ränta";
    entry "housing" Expense "Housing" "Boende";
    entry "utilities" Expense "Utilities" "El, vatten & värme";
    entry "groceries" Expense "Groceries" "Mat & hushåll";
    entry "restaurants" Expense "Restaurants & cafés" "Restaurang & café";
    entry "transport" Expense "Transport" "Transport";
    entry "subscriptions" Expense "Subscriptions" "Abonnemang";
    entry "insurance" Expense "Insurance" "Försäkringar";
    entry "health" Expense "Health" "Hälsa";
    entry "clothes" Expense "Clothes" "Kläder";
    entry "shopping" Expense "Shopping" "Shopping";
    entry "children" Expense ~scope:Shared "Children" "Barn";
    entry "entertainment" Expense "Entertainment" "Nöje";
    entry "travel" Expense "Travel" "Resor";
    entry "gifts" Expense "Gifts" "Presenter";
    entry "fees" Expense "Fees & interest" "Avgifter & räntor";
    entry "other" Expense "Other" "Övrigt" ]

let name locale entry = if String.starts_with ~prefix:"sv" locale then entry.sv else entry.en

let applies scope entry =
  match entry.scope, scope with Both, _ | Personal, `Personal | Shared, `Shared -> true | _ -> false

let key entry = Result.get_ok (Idempotency_key.of_string ("seed:" ^ entry.slug))

(* A starter account still bearing a starter name (in any language) follows the language; one
   someone renamed keeps its name. Accounts opened later for another currency carry it in their
   key and name: "seed:uncategorized-expense-EUR", "Uncategorized (EUR)". *)
let localized_name ~locale ~key ~name =
  List.find_map
    (fun entry ->
      let base = "seed:" ^ entry.slug in
      let follows label = if name = label entry.en || name = label entry.sv then Some (label (if String.starts_with ~prefix:"sv" locale then entry.sv else entry.en)) else None in
      if key = base then follows Fun.id
      else if String.starts_with ~prefix:(base ^ "-") key then
        let currency = String.sub key (String.length base + 1) (String.length key - String.length base - 1) in
        follows (fun n -> n ^ " (" ^ currency ^ ")")
      else None)
    entries
