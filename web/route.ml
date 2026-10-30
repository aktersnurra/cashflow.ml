open! Core

(* Transactions carries its filters, the pay cycle (0 the current one, -1 the one before) and
   optionally a category, so Back and links keep them. *)
(* Four places: the Overview (where things stand, what is worth knowing, what is ahead: the forecast, goals and
   budgets), the Advisor, Money (transactions and accounts, with the inbox, an account and adding by hand as views
   within it) and Settings (the household among them). *)
type t = Home | Advisor of string option | Transactions of { offset : int; category : string option } | Inbox | Accounts | Account of string | Settings | Add [@@deriving equal, sexp]

let to_path = function
  | Home -> "/"
  | Advisor None -> "/advisor"
  | Advisor (Some id) -> "/advisor/" ^ id
  | Transactions { offset = 0; category = None } -> "/transactions"
  | Transactions { offset; category } -> "/transactions/" ^ Int.to_string offset ^ Option.value_map category ~default:"" ~f:(fun c -> "/" ^ c)
  | Inbox -> "/inbox"
  | Accounts -> "/accounts"
  | Account id -> "/accounts/" ^ id
  | Settings -> "/settings"
  | Add -> "/add"

let of_path path =
  match String.split (String.strip ~drop:(Char.equal '/') path) ~on:'/' with
  | [ "advisor" ] -> Advisor None
  | [ "advisor"; id ] -> Advisor (Some id)
  | [ "transactions" ] -> Transactions { offset = 0; category = None }
  | [ "transactions"; offset ] -> Transactions { offset = Option.value (Int.of_string_opt offset) ~default:0; category = None }
  | [ "transactions"; offset; category ] -> Transactions { offset = Option.value (Int.of_string_opt offset) ~default:0; category = Some category }
  | [ "inbox" ] -> Inbox
  | [ "accounts" ] -> Accounts
  | [ "accounts"; id ] -> Account id
  (* Pages that were folded into others: their old addresses lead there. *)
  | [ "household" ] -> Settings
  | [ "settings" ] -> Settings
  | [ "add" ] -> Add
  | _ -> Home

let title locale = function
  | Home -> Msg.nav_home locale
  | Advisor _ -> Msg.nav_advisor locale
  | Transactions _ -> Msg.nav_transactions locale
  | Inbox -> Msg.nav_inbox locale
  | Accounts | Account _ -> Msg.nav_accounts locale
  | Settings -> Msg.nav_settings locale
  | Add -> Msg.nav_add locale

(* Money is the one section with more than one page: a strip above it switches between transactions and
   accounts. Each keeps its own address. *)
type section = Money [@@deriving equal]

let section = function Transactions _ | Inbox | Accounts | Account _ | Add -> Some Money | Home | Advisor _ | Settings -> None
let section_entry = function Money -> Transactions { offset = 0; category = None }
let section_pages = function Money -> [ Transactions { offset = 0; category = None }; Accounts ]

(* The page a strip highlights: the inbox and adding by hand are part of Transactions, an account part of Accounts. *)
let section_page = function
  | Inbox | Add | Transactions _ -> Transactions { offset = 0; category = None }
  | Account _ -> Accounts
  | route -> route

let section_title locale = function Money -> Msg.nav_money locale
