open! Core

type t = Home | Inbox | Accounts | Account of string | Budgets | Household | Insights | Settings | Add [@@deriving equal, sexp]

let to_path = function
  | Home -> "/"
  | Inbox -> "/inbox"
  | Accounts -> "/accounts"
  | Account id -> "/accounts/" ^ id
  | Budgets -> "/budgets"
  | Household -> "/household"
  | Insights -> "/insights"
  | Settings -> "/settings"
  | Add -> "/add"

let of_path path =
  match String.split (String.strip ~drop:(Char.equal '/') path) ~on:'/' with
  | [ "inbox" ] -> Inbox
  | [ "accounts" ] -> Accounts
  | [ "accounts"; id ] -> Account id
  | [ "budgets" ] -> Budgets
  | [ "household" ] -> Household
  | [ "insights" ] -> Insights
  | [ "settings" ] -> Settings
  | [ "add" ] -> Add
  | _ -> Home

let title locale = function
  | Home -> Msg.nav_home locale
  | Inbox -> Msg.nav_inbox locale
  | Accounts | Account _ -> Msg.nav_accounts locale
  | Budgets -> Msg.nav_budgets locale
  | Household -> Msg.nav_household locale
  | Insights -> Msg.nav_insights locale
  | Settings -> Msg.nav_settings locale
  | Add -> Msg.nav_add locale
