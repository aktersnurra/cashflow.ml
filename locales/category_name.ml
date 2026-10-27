(* The starter categories' names, by the slugs in lib/core/chart.ml, for the server and the
   browser alike. *)

let base slug locale =
  match slug with
  | "uncategorized-expense" -> Msg.category_uncategorized_expense locale
  | "uncategorized-income" -> Msg.category_uncategorized_income locale
  | "opening-balances" -> Msg.category_opening_balances locale
  | "unrealized-gains" -> Msg.category_unrealized_gains locale
  | "untracked-cash" -> Msg.category_untracked_cash locale
  | "salary" -> Msg.category_salary locale
  | "other-income" -> Msg.category_other_income locale
  | "interest" -> Msg.category_interest locale
  | "housing" -> Msg.category_housing locale
  | "utilities" -> Msg.category_utilities locale
  | "groceries" -> Msg.category_groceries locale
  | "restaurants" -> Msg.category_restaurants locale
  | "transport" -> Msg.category_transport locale
  | "subscriptions" -> Msg.category_subscriptions locale
  | "insurance" -> Msg.category_insurance locale
  | "health" -> Msg.category_health locale
  | "clothes" -> Msg.category_clothes locale
  | "shopping" -> Msg.category_shopping locale
  | "children" -> Msg.category_children locale
  | "entertainment" -> Msg.category_entertainment locale
  | "travel" -> Msg.category_travel locale
  | "gifts" -> Msg.category_gifts locale
  | "fees" -> Msg.category_fees locale
  | "other" -> Msg.category_other locale
  | _ -> invalid_arg ("Category_name: no message for " ^ slug)

let name ~slug ~currency locale =
  let base = base slug locale in
  match currency with None -> base | Some currency -> Msg.category_in_currency ~name:base ~currency locale
