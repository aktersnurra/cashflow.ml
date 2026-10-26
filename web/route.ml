open! Core

(* Transactions carries its filters, the pay cycle (0 the current one, -1 the one before) and
   optionally a category, so Back and links keep them. *)
type t = Home | Advisor of string option | Cashflow | Transactions of { offset : int; category : string option } | Inbox | Accounts | Account of string | Budgets | Forecast | Household | Insights | Settings | Add [@@deriving equal, sexp]

let to_path = function
  | Home -> "/"
  | Advisor None -> "/advisor"
  | Advisor (Some id) -> "/advisor/" ^ id
  | Cashflow -> "/cashflow"
  | Transactions { offset = 0; category = None } -> "/transactions"
  | Transactions { offset; category } -> "/transactions/" ^ Int.to_string offset ^ Option.value_map category ~default:"" ~f:(fun c -> "/" ^ c)
  | Inbox -> "/inbox"
  | Accounts -> "/accounts"
  | Account id -> "/accounts/" ^ id
  | Budgets -> "/budgets"
  | Forecast -> "/forecast"
  | Household -> "/household"
  | Insights -> "/insights"
  | Settings -> "/settings"
  | Add -> "/add"

let of_path path =
  match String.split (String.strip ~drop:(Char.equal '/') path) ~on:'/' with
  | [ "advisor" ] -> Advisor None
  | [ "advisor"; id ] -> Advisor (Some id)
  | [ "cashflow" ] -> Cashflow
  | [ "transactions" ] -> Transactions { offset = 0; category = None }
  | [ "transactions"; offset ] -> Transactions { offset = Option.value (Int.of_string_opt offset) ~default:0; category = None }
  | [ "transactions"; offset; category ] -> Transactions { offset = Option.value (Int.of_string_opt offset) ~default:0; category = Some category }
  | [ "inbox" ] -> Inbox
  | [ "accounts" ] -> Accounts
  | [ "accounts"; id ] -> Account id
  | [ "budgets" ] -> Budgets
  | [ "forecast" ] -> Forecast
  | [ "household" ] -> Household
  | [ "insights" ] -> Insights
  | [ "settings" ] -> Settings
  | [ "add" ] -> Add
  | _ -> Home

let title locale = function
  | Home -> Msg.nav_home locale
  | Advisor _ -> Msg.nav_advisor locale
  | Cashflow -> Msg.nav_cashflow locale
  | Transactions _ -> Msg.nav_transactions locale
  | Inbox -> Msg.nav_inbox locale
  | Accounts | Account _ -> Msg.nav_accounts locale
  | Budgets -> Msg.nav_budgets locale
  | Forecast -> Msg.nav_forecast locale
  | Household -> Msg.nav_household locale
  | Insights -> Msg.nav_insights locale
  | Settings -> Msg.nav_settings locale
  | Add -> Msg.nav_add locale
