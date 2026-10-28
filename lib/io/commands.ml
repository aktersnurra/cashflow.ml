(* Every use case the outside world can call, in one table. MCP tools are generated from it, and
   the web UI calls the same commands through /api/commands/<name>, so a use case exists once and
   both clients get the same validation and the same JSON. Analytics queries (Query.all) are
   included as read commands. *)
module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Household = Cashflow_core.Household
module Household_id = Cashflow_core.Household_id
module I18n_locale = Cashflow_core.I18n_locale
module Idempotency_key = Cashflow_core.Idempotency_key
module Owner = Cashflow_core.Owner
module Pay_cycle_setting = Cashflow_core.Pay_cycle_setting
module Suggest = Cashflow_core.Suggest
module Time = Cashflow_core.Time
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id

let ( let* ) = Result.bind

type scope = Read | Write | Admin

let scope_name = function Read -> "ledger:read" | Write -> "ledger:write" | Admin -> "ledger:admin"

type command = {
  name : string;
  scope : scope;
  description : string;
  properties : (string * Yojson.Safe.t) list;
  required : string list;
  mcp : bool;  (** offered to MCP clients; some commands only make sense in the web UI *)
  run : Service.t -> Access.t -> Query.Args.fields -> (Yojson.Safe.t, Import.error) result;
}

module A = Query.Args

let invalid reason = Error (`Invalid reason)
let arg r = Result.map_error (fun reason -> `Invalid reason) r

let required_string name fields =
  let* value = arg (A.string name fields) in
  match value with Some v when String.trim v <> "" -> Ok v | _ -> invalid (name ^ " is required")

let key name fields =
  let* text = required_string name fields in
  arg (Result.map_error (fun (`Invalid_idempotency_key r) -> r) (Idempotency_key.of_string text))

let id of_string label name fields =
  let* text = required_string name fields in
  match of_string text with Some id -> Ok id | None -> invalid (Printf.sprintf "%s must be %s id" name label)

let account_id = id Account_id.of_string "an account"
let transfer_id = id Transfer_id.of_string "a transfer"

let optional_account_id name fields =
  let* text = arg (A.string name fields) in
  match text with None -> Ok None | Some t -> (match Account_id.of_string t with Some id -> Ok (Some id) | None -> invalid (name ^ " must be an account id"))

let date_or_today service access name fields =
  let* date = arg (A.date name fields) in
  Ok (Option.value date ~default:(Service.today service access))

let required_date name fields =
  let* date = arg (A.date name fields) in
  match date with Some d -> Ok d | None -> invalid (name ^ " is required, as YYYY-MM-DD")

(* Amounts are decimal strings in the account's currency ("1240.50"); plain JSON numbers are
   accepted too, as long as they have no more decimals than the currency. A number is read as the
   shortest decimal that is the same float, never rounded: one that can't be written with the
   currency's decimals is refused. *)
let decimal_text name fields =
  match List.assoc_opt name fields with
  | Some (`String s) -> Ok (Some s)
  | Some (`Int n) -> Ok (Some (string_of_int n))
  | Some (`Intlit s) -> Ok (Some s)
  | Some (`Float f) ->
      let exact digits = let text = Printf.sprintf "%.*g" digits f in if float_of_string text = f then Some text else None in
      Ok (List.find_map exact [ 15; 16; 17 ])
  | None | Some `Null -> Ok None
  | Some _ -> invalid (name ^ " must be a decimal amount")

let signed_amount (currency : Currency.t) name fields =
  let* text = decimal_text name fields in
  match text with
  | None -> Ok None
  | Some text -> (
      match Json.parse_decimal ~scale:(Currency.scale currency) text with
      | Some v -> Ok (Some v)
      | None -> invalid (Printf.sprintf "%s must be a decimal amount with at most %d decimals" name (Currency.scale currency)))

let required_amount currency name fields =
  let* value = signed_amount currency name fields in
  match value with
  | Some v when Int64.compare v 0L > 0 -> Ok (Option.get (Amount.of_int64 v))
  | Some _ -> invalid (name ^ " must be positive")
  | None -> invalid (name ^ " is required")

let optional_amount currency name fields =
  let* value = signed_amount currency name fields in
  match value with
  | None -> Ok None
  | Some v when Int64.compare v 0L > 0 -> Ok (Some (Option.get (Amount.of_int64 v)))
  | Some _ -> invalid (name ^ " must be positive")

let strings name fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok []
  | Some (`List items) ->
      List.fold_right (fun item acc -> let* acc = acc in match item with `String s -> Ok (String.trim s :: acc) | _ -> invalid (name ^ " must be a list of strings")) items (Ok [])
  | Some _ -> invalid (name ^ " must be a list of strings")

let currency_of_account service access id =
  let* info = Service.find_account service access id in
  Ok info.account.currency

let prop = Query.prop
let string_prop name description = prop name "string" description
let key_prop = string_prop "idempotency_key" "A key you choose for this write, unique per change. Retrying with the same key and arguments is safe and returns the original result."
let amount_prop name description = string_prop name (description ^ " A decimal amount such as \"1240.50\", in the account's currency.")
let date_prop name description = string_prop name (description ^ " YYYY-MM-DD.")
let list_prop name description = (name, `Assoc [ ("type", `String "array"); ("items", `Assoc [ ("type", `String "string") ]); ("description", `String description) ])

let command ?(mcp = true) ?(required = []) name scope description properties run = { name; scope; description; properties; required; mcp; run }

let one_outcome = function Ok result -> Ok (Json.outcome result) | Error e -> Error e
let outcomes = function Ok results -> Ok (`Assoc [ ("results", `List (List.map Json.outcome results)) ]) | Error e -> Error e
let lift r = Result.map_error (fun e -> (e :> Import.error)) r

(* Accounts *)

let list_accounts =
  command "list_accounts" Read
    "Your accounts and, if you are in a household, the household's: bank accounts, cards, categories and the rest, with balances. Use the ids with the other tools."
    [ ("include_closed", `Assoc [ ("type", `String "boolean"); ("description", `String "Also list closed accounts.") ]) ]
    (fun service access fields ->
      let* closed = arg (A.bool "include_closed" ~default:false fields) in
      let accounts = List.filter (fun (i : Account_info.t) -> closed || not i.account.closed) (Service.accounts service access) in
      Ok (`Assoc [ ("accounts", `List (List.map Json.account accounts)) ]))

let kind_of_string = function
  | "asset" -> Ok Account.Asset
  | "liability" -> Ok Liability
  | "income" -> Ok Income
  | "expense" -> Ok Expense
  | "equity" -> Ok Equity
  | other -> invalid ("unknown account kind " ^ other)

let owner_arg access fields =
  let* owner = arg (A.string "owner" fields) in
  match owner, Access.household access with
  | (None | Some "personal"), _ -> Ok (Access.personal access)
  | Some "household", Some h -> Ok (Owner.Household h.id)
  | Some "household", None -> invalid "you don't belong to a household"
  | Some other, _ -> invalid ("owner must be personal or household, not " ^ other)

let details_args fields (base : Account_info.details) =
  let* institution = arg (A.string "institution" fields) in
  let* number_last4 = arg (A.string "number_last4" fields) in
  let* statement_backed = arg (A.bool "statement_backed" ~default:base.statement_backed fields) in
  let* tags = strings "tags" fields in
  Ok
    { base with
      institution = (match institution with Some _ -> institution | None -> base.institution);
      number_last4 = (match number_last4 with Some _ -> number_last4 | None -> base.number_last4);
      statement_backed;
      tags = (if List.mem_assoc "tags" fields then tags else base.tags) }

let account_detail_props =
  [ string_prop "institution" "The bank, e.g. Handelsbanken.";
    string_prop "number_last4" "The last four digits of the account number, for recognising statements. The full number is never stored.";
    ("statement_backed", `Assoc [ ("type", `String "boolean"); ("description", `String "Whether bank statements will be imported for this account. Manual entries on such accounts stay pending until a statement confirms them.") ]);
    list_prop "tags" "Labels; tag investment accounts \"investment\" to leave them out of runway." ]

let open_account =
  command "open_account" Write "Open an account: a bank account or card (asset, liability), or a category (expense, income)."
    ~required:[ "idempotency_key"; "kind"; "name" ]
    ([ key_prop; string_prop "kind" "asset, liability, income, expense or equity."; string_prop "name" "Display name.";
       string_prop "owner" "personal (default) or household.";
       string_prop "currency" "ISO 4217 code; defaults to your default currency.";
       ("no_overdraft", `Assoc [ ("type", `String "boolean"); ("description", `String "For assets: refuse transfers that would take the balance below zero.") ]) ]
    @ account_detail_props)
    (fun service access fields ->
      let* key = key "idempotency_key" fields in
      let* kind = Result.bind (required_string "kind" fields) kind_of_string in
      let* name = required_string "name" fields in
      let* owner = owner_arg access fields in
      let* currency = arg (A.string "currency" fields) in
      let currency = Option.value currency ~default:(Access.user access).default_currency in
      let* no_overdraft = arg (A.bool "no_overdraft" ~default:false fields) in
      let flags = { Account.no_flags with credits_must_not_exceed_debits = no_overdraft && kind = Asset } in
      let* details = details_args fields Account_info.no_details in
      let* result = lift (Service.open_account service access ~key ~owner ~kind ~name ~currency ~flags ~details) in
      match result with
      | `Created info -> Ok (`Assoc [ ("status", `String "created"); ("account", Json.account info) ])
      | `Exists info -> Ok (`Assoc [ ("status", `String "exists"); ("account", Json.account info) ]))

let update_account =
  command "update_account" Write "Rename an account or change its display details." ~required:[ "account_id" ]
    ([ string_prop "account_id" "The account."; string_prop "name" "A new name." ] @ account_detail_props)
    (fun service access fields ->
      let* id = account_id "account_id" fields in
      let* info = lift (Service.find_account service access id) in
      let* name = arg (A.string "name" fields) in
      let* details = details_args fields info.details in
      let* info = lift (Service.update_account service access id ~name ~details) in
      Ok (Json.account info))

let close_account =
  command "close_account" Admin "Close an account. Bank accounts and cards must be at zero; categories keep their history." ~required:[ "account_id" ]
    [ string_prop "account_id" "The account." ]
    (fun service access fields ->
      let* id = account_id "account_id" fields in
      let* info = lift (Service.close_account service access id) in
      Ok (Json.account info))

let set_opening_balance =
  command "set_opening_balance" Write "Set an account's balance on the day you start tracking it, against opening balances." ~required:[ "account_id"; "balance"; "date" ]
    [ string_prop "account_id" "The account."; amount_prop "balance" "The balance, negative for an overdrawn account or a card debt shown as negative."; date_prop "date" "The day of that balance." ]
    (fun service access fields ->
      let* id = account_id "account_id" fields in
      let* currency = lift (currency_of_account service access id) in
      let* balance = signed_amount currency "balance" fields in
      let* balance = Option.to_result ~none:(`Invalid "balance is required") balance in
      let* value_date = required_date "date" fields in
      one_outcome (lift (Service.set_opening_balance service access ~account_id:id ~balance ~value_date)))

let set_market_value =
  command "set_market_value" Write "Record an investment account's market value; the change is posted as an unrealized gain or loss." ~required:[ "account_id"; "value" ]
    [ string_prop "account_id" "The investment account."; amount_prop "value" "Its market value."; date_prop "date" "The day of the valuation; defaults to today." ]
    (fun service access fields ->
      let* id = account_id "account_id" fields in
      let* currency = lift (currency_of_account service access id) in
      let* value = signed_amount currency "value" fields in
      let* value = Option.to_result ~none:(`Invalid "value is required") value in
      let* on_date = date_or_today service access "date" fields in
      let* result = lift (Valuation.set_market_value service access ~account_id:id ~value ~on_date) in
      match result with None -> Ok (`Assoc [ ("status", `String "unchanged") ]) | Some r -> Ok (Json.outcome r))

(* Transactions *)

let code_of_string = function
  | None -> Ok None
  | Some s -> (
      match List.find_opt (fun c -> Transfer.code_to_string c = s) Transfer.[ Purchase; Income; Internal; Contribution; Reclassify; Reversal; Revaluation; Opening; Settlement; Adjustment ] with
      | Some c -> Ok (Some c)
      | None -> invalid ("unknown code " ^ s))

let merchant_json service access (v : Service.transfer_view) =
  let visible_owners = List.filter_map (function Service.Own info -> Some info.account.owner | Hidden _ -> None)
    [v.debit; v.credit] in
  let owner = if List.exists (Owner.equal v.transfer.scope) visible_owners then Some v.transfer.scope
    else List.nth_opt visible_owners 0 in
  let item = Import_store.find_item service.Service.db v.transfer.id in
  let key = match item with Some item -> item.merchant_key | None -> Analytics.merchant_key v.transfer in
  let merchant = Option.map (fun owner -> Merchant_logos.view service access ~owner ~merchant_key:key) owner in
  let logo = Option.bind merchant (fun (v : Merchant_logos.view) -> v.logo) in
  `Assoc ["key", `String key; "logo", Json.option (fun s -> `String s) logo;
          "can_retry", `Bool (Option.exists (fun (v : Merchant_logos.view) -> v.can_retry) merchant);
          "owner", Json.option (function Owner.User _ -> `String "personal" | Owner.Household _ -> `String "household") owner]

let query_transactions =
  command "query_transactions" Read "List transactions you can see, newest first, filtered by date, account, text, tag or code."
    [ date_prop "from_date" "Earliest date, inclusive."; date_prop "to_date" "Latest date, inclusive."; string_prop "account_id" "Only transactions touching this account.";
      string_prop "text" "Matched against description and counterparty."; string_prop "tag" "Only transactions with this tag.";
      string_prop "code" "purchase, income, internal, contribution, reclassify, reversal, revaluation, opening, settlement or adjustment.";
      prop "limit" "integer" "At most this many (1-500, default 100).";
      ( "fold_triage",
        `Assoc
          [ ("type", `String "boolean");
            ( "description",
              `String
                "List each imported row once, with the category it has now (category) and its inbox status (triage), leaving out the \
                 transfers that categorising, sharing or pairing posted." ) ] ) ]
    (fun service access fields ->
      let* from_date = arg (A.date "from_date" fields) in
      let* to_date = arg (A.date "to_date" fields) in
      let* account_id = optional_account_id "account_id" fields in
      let* text = arg (A.string "text" fields) in
      let* tag = arg (A.string "tag" fields) in
      let* code = Result.bind (arg (A.string "code" fields)) code_of_string in
      let* limit = arg (A.int "limit" ~default:100 ~min:1 ~max:500 fields) in
      let* fold = arg (A.bool "fold_triage" ~default:false fields) in
      let views = Service.query_transfers service access { from_date; to_date; account_id; text; tag; code; without_triage = fold; limit } in
      if not fold then Ok (`Assoc [ ("transactions", `List (List.map (fun v -> match Json.transfer v with
        | `Assoc fields -> `Assoc (fields @ ["merchant", merchant_json service access v])
        | json -> json) views)) ])
      else
        (* An imported row stays where the bank put it; triage moves its amount on with separate
           transfers. Folded, the row carries where it sits now, and the query leaves those transfers
           out before the limit counts. *)
        let row (v : Service.transfer_view) =
          let json = match Json.transfer v with
            | `Assoc fields -> `Assoc (fields @ ["merchant", merchant_json service access v])
            | json -> json in
          match Import_store.find_item service.db v.transfer.id, json with
          | Some item, `Assoc fields ->
              let category = Result.to_option (Service.find_account service access item.current_account_id) in
              `Assoc (fields @ [ ("category", Json.option Json.account_ref category); ("triage", `String item.status) ])
          | _, json -> json
        in
        Ok (`Assoc [ ("transactions", `List (List.map row views)) ]))

let logo_error e : Import.error = `Logo (Merchant_logos.error_code e, Merchant_logos.error_message e)
let logo_arg code = Result.map_error (fun (`Invalid reason) -> (`Logo (code, reason) : Import.error))
let logo_view (v : Merchant_logos.view) =
  `Assoc ["key", `String v.key; "logo", Json.option (fun s -> `String s) v.logo; "can_retry", `Bool v.can_retry]

let set_merchant_logo =
  command "set_merchant_logo" Write "Save a merchant logo from a website or image link."
    ~required:["merchant_key"; "link"]
    [string_prop "merchant_key" "Merchant identity."; string_prop "owner" "personal (default) or household.";
     string_prop "link" "HTTPS website or image link."]
    (fun service access fields ->
      let* merchant_key = logo_arg "invalid_merchant_key" (required_string "merchant_key" fields) in
      let* owner = logo_arg "invalid_owner" (owner_arg access fields) in
      let* link = logo_arg "invalid_link" (required_string "link" fields) in
      let* view = Result.map_error logo_error (Merchant_logos.set service access ~owner ~merchant_key ~link) in
      Ok (logo_view view))

let clear_merchant_logo =
  command "clear_merchant_logo" Write "Remove a logo or use automatic resolution."
    ~required:["merchant_key"; "mode"]
    [string_prop "merchant_key" "Merchant identity."; string_prop "owner" "personal (default) or household.";
     ("mode", `Assoc ["type", `String "string"; "enum", `List [`String "none"; `String "automatic"]])]
    (fun service access fields ->
      let* merchant_key = logo_arg "invalid_merchant_key" (required_string "merchant_key" fields) in
      let* owner = logo_arg "invalid_owner" (owner_arg access fields) in
      let* mode = logo_arg "invalid_mode" (required_string "mode" fields) in
      let* mode = match mode with "none" -> Ok Merchant_logos.None_selected | "automatic" -> Ok Automatic
        | _ -> Error (`Logo ("invalid_mode", "mode must be none or automatic")) in
      let* view = Result.map_error logo_error (Merchant_logos.clear service access ~owner ~merchant_key ~mode) in
      Ok (logo_view view))

let get_transaction =
  command "get_transaction" Read "One transaction, with what resolved or reversed it." ~required:[ "id" ] [ string_prop "id" "The transaction id." ]
    (fun service access fields ->
      let* id = transfer_id "id" fields in
      let* view = lift (Service.get_transfer service access id) in
      Ok (Json.transfer view))

let add_transaction =
  command "add_transaction" Write
    "Record money moving from one account to another: a purchase is from a bank account to an expense category, income is from an \
     income category to a bank account. On an account whose bank statements are imported, the entry stays pending until the statement \
     confirms it, so it is never counted twice."
    ~required:[ "idempotency_key"; "from_account_id"; "to_account_id"; "amount" ]
    [ key_prop; string_prop "from_account_id" "Where the money comes from."; string_prop "to_account_id" "Where it goes.";
      amount_prop "amount" "How much, positive."; date_prop "date" "When it happened; defaults to today.";
      string_prop "description" "What it was."; string_prop "counterparty" "The shop, employer or person."; list_prop "tags" "Labels, e.g. vacation-2026.";
      string_prop "code" "Override the inferred purpose (purchase, income, internal, contribution...)." ]
    (fun service access fields ->
      let* key = key "idempotency_key" fields in
      let* from_account = account_id "from_account_id" fields in
      let* to_account = account_id "to_account_id" fields in
      let* currency = lift (currency_of_account service access from_account) in
      let* amount = required_amount currency "amount" fields in
      let* value_date = date_or_today service access "date" fields in
      let* description = arg (A.string "description" fields) in
      let* counterparty = arg (A.string "counterparty" fields) in
      let* tags = strings "tags" fields in
      let* code = Result.bind (arg (A.string "code" fields)) code_of_string in
      one_outcome
        (lift
           (Service.add_entry service access
              { key; from_account; to_account; amount; value_date; code; scope = None;
                memo = { description = Option.value description ~default:""; counterparty; tags } })))

let reverse_transaction =
  command "reverse_transaction" Write
    "Undo a transaction by posting its mirror image; transactions are never edited or deleted. Confirm with the user before calling this."
    ~required:[ "idempotency_key"; "id" ]
    [ key_prop; string_prop "id" "The transaction to reverse."; date_prop "date" "The reversal's date; defaults to the original's." ]
    (fun service access fields ->
      let* key = key "idempotency_key" fields in
      let* id = transfer_id "id" fields in
      let* value_date = arg (A.date "date" fields) in
      one_outcome (lift (Service.reverse service access ~key ~transfer_id:id ?value_date ())))

let correct_transaction =
  command "correct_transaction" Write
    "Correct a transaction: reverses it and posts the corrected version, linked so both happen or neither. Fields you leave out keep \
     their original values. Confirm the change with the user before calling this."
    ~required:[ "idempotency_key"; "id" ]
    [ key_prop; string_prop "id" "The transaction to correct."; amount_prop "amount" "The right amount."; string_prop "from_account_id" "The right source account.";
      string_prop "to_account_id" "The right destination account."; date_prop "date" "The right date."; string_prop "description" "The right description." ]
    (fun service access fields ->
      let* key = key "idempotency_key" fields in
      let* id = transfer_id "id" fields in
      let* original = lift (Service.get_transfer service access id) in
      let* currency =
        match original.debit, original.credit with
        | Own i, _ | _, Own i -> Ok i.account.currency
        | Hidden _, Hidden _ -> invalid "transaction not found"
      in
      let* amount = optional_amount currency "amount" fields in
      let* from_account = optional_account_id "from_account_id" fields in
      let* to_account = optional_account_id "to_account_id" fields in
      let* value_date = arg (A.date "date" fields) in
      let* description = arg (A.string "description" fields) in
      let memo = Option.map (fun description -> { original.transfer.memo with description }) description in
      outcomes (lift (Service.correct service access ~key ~transfer_id:id { amount; from_account; to_account; value_date; memo })))

let post_pending =
  command "post_pending" Write "Confirm a pending transaction, optionally for a smaller amount (the rest is released)." ~required:[ "idempotency_key"; "id" ]
    [ key_prop; string_prop "id" "The pending transaction."; amount_prop "amount" "The final amount, at most the pending one; defaults to all of it."; date_prop "date" "Defaults to today." ]
    (fun service access fields ->
      let* key = key "idempotency_key" fields in
      let* id = transfer_id "id" fields in
      let* view = lift (Service.get_transfer service access id) in
      let currency = Json.currency_of_view view in
      let* amount = optional_amount currency "amount" fields in
      let* value_date = date_or_today service access "date" fields in
      one_outcome (lift (Service.post_pending service access ~key ~pending_id:id ?amount ~value_date ())))

let void_pending =
  command "void_pending" Write "Cancel a pending transaction, releasing its amount." ~required:[ "idempotency_key"; "id" ]
    [ key_prop; string_prop "id" "The pending transaction."; date_prop "date" "Defaults to today." ]
    (fun service access fields ->
      let* key = key "idempotency_key" fields in
      let* id = transfer_id "id" fields in
      let* value_date = date_or_today service access "date" fields in
      one_outcome (lift (Service.void_pending service access ~key ~pending_id:id ~value_date)))

(* Inbox *)

let item_json (v : Inbox.item_view) =
  `Assoc
    [ ("transaction", Json.transfer v.transfer); ("suggested_category", Json.option Json.account_ref v.suggested);
      ("suggestion_source", Json.option (fun s -> `String s) v.item.suggestion_source);
      ("confidence", Json.option (fun c -> `Int c) v.item.suggestion_confidence);
      ("pending_candidate", Json.option Json.transfer v.candidate) ]

let list_inbox =
  command "list_inbox" Read "Imported transactions waiting for a category, newest first, with a suggested category where there is one."
    [ prop "limit" "integer" "At most this many (1-500, default 50)." ]
    (fun service access fields ->
      let* limit = arg (A.int "limit" ~default:50 ~min:1 ~max:500 fields) in
      Ok (`Assoc [ ("count", `Int (Inbox.count service access)); ("items", `List (List.map item_json (Inbox.list service access ~limit))) ]))

let categorize =
  command "categorize" Write "Put an inbox transaction in a category (or a balance-sheet account such as cash)." ~required:[ "id"; "category_id" ]
    [ string_prop "id" "The inbox transaction."; string_prop "category_id" "The category account."; list_prop "tags" "Tags to add." ]
    (fun service access fields ->
      let* id = transfer_id "id" fields in
      let* category_id = account_id "category_id" fields in
      let* tags = strings "tags" fields in
      let* result = lift (Inbox.categorize service access ~transfer_id:id ~category_id ~tags ()) in
      match result with None -> Ok (`Assoc [ ("status", `String "unchanged") ]) | Some r -> Ok (Json.outcome r))

let share =
  command "share_with_household" Write
    "A shared cost you paid from a personal account: the household's books get the expense and owe you for it." ~required:[ "id"; "category_id" ]
    [ string_prop "id" "The inbox transaction, from one of your personal accounts."; string_prop "category_id" "One of the household's expense categories." ]
    (fun service access fields ->
      let* id = transfer_id "id" fields in
      let* category = account_id "category_id" fields in
      outcomes (lift (Inbox.share service access ~transfer_id:id ~household_category_id:category)))

let pair =
  command "pair_transfers" Write "Two inbox transactions that are one move between your own accounts (or to the household)." ~required:[ "out_id"; "in_id" ]
    [ string_prop "out_id" "The side where money left."; string_prop "in_id" "The side where it arrived." ]
    (fun service access fields ->
      let* out_id = transfer_id "out_id" fields in
      let* in_id = transfer_id "in_id" fields in
      one_outcome (lift (Inbox.pair service access ~out_id ~in_id)))

let pair_all_moves =
  command "pair_all_moves" Write
    "Find every move between your own accounts (or to the household) among the inbox transactions and pair them, so they stop counting as income and spending. \
     Clear matches are paired by rule, unsure ones by the assistant when one is configured; whatever is still unsure stays for you."
    []
    (fun service access _ ->
      let paired_by_rule, paired_by_assistant, still_unsure = Inbox.pair_all service access in
      Ok (`Assoc [ ("paired_by_rule", `Int paired_by_rule); ("paired_by_assistant", `Int paired_by_assistant); ("still_unsure", `Int still_unsure) ]))

let pair_candidates =
  command "inbox_pair_candidates" Read "Inbox transactions that could be the other side of a move: the same amount the other way within three days." ~required:[ "id" ]
    [ string_prop "id" "The inbox transaction." ]
    (fun service access fields ->
      let* id = transfer_id "id" fields in
      let* candidates = lift (Inbox.pair_candidates service access ~transfer_id:id) in
      Ok (`Assoc [ ("candidates", `List (List.map item_json candidates)) ]))

let accept_candidate =
  command "accept_pending_candidate" Write "The inbox transaction is the bank's version of a pending manual entry with a different amount: void the entry and categorise the transaction like it." ~required:[ "id" ]
    [ string_prop "id" "The inbox transaction." ]
    (fun service access fields ->
      let* id = transfer_id "id" fields in
      one_outcome (lift (Inbox.accept_candidate service access ~transfer_id:id)))

let dismiss =
  command "dismiss_inbox_item" Write "Leave an inbox transaction uncategorised and stop asking about it." ~required:[ "id" ] [ string_prop "id" "The inbox transaction." ]
    (fun service access fields ->
      let* id = transfer_id "id" fields in
      let* () = lift (Inbox.dismiss service access ~transfer_id:id) in
      Ok (`Assoc [ ("status", `String "dismissed") ]))

(* Imports *)

let planned_json currency (p : Import.planned) =
  `Assoc
    [ ("row", `Int (p.index + 1)); ("date", Json.date p.row.booking_date); ("description", `String p.row.description);
      ("amount", Json.money currency p.row.amount);
      ( "status",
        `String
          (match p.status with
          | New -> "new"
          | Duplicate _ | Decided_duplicate _ -> "duplicate"
          | Matched _ | Confirms_leg _ -> "confirms_pending"
          | Ambiguous _ -> "ambiguous") );
      ("already_imported_alike", match p.status with Ambiguous existing -> `Int (List.length existing) | _ -> `Null);
      ("suggested_category_id", Json.option (fun (s : Suggest.suggestion) -> `String (Account_id.to_string s.category_id)) p.suggestion);
      ("auto_categorized", `Bool (match p.suggestion with Some s -> s.auto_apply | None -> false));
      ("near_pending_id", Json.option (fun (t : Transfer.t) -> `String (Transfer_id.to_string t.id)) p.near_pending) ]

let balance_json currency = function
  | None -> `Null
  | Some (b : Import.balance_check) ->
      `Assoc
        [ ("date", Json.date b.on_date); ("statement_balance", Json.money currency b.statement_balance); ("ledger_balance", Json.money currency b.ledger_balance);
          ("agrees", `Bool (Int64.equal b.statement_balance b.ledger_balance)) ]

(* [{"row": 3, "decision": "new"}, ...], rows numbered from 1 as the preview shows them. *)
let decisions fields =
  let one = function
    | `Assoc d -> (
        match List.assoc_opt "row" d, List.assoc_opt "decision" d with
        | Some (`Int row), Some (`String "duplicate") when row >= 1 -> Ok (row - 1, Import.Is_duplicate)
        | Some (`Int row), Some (`String "new") when row >= 1 -> Ok (row - 1, Import.Is_new)
        | _ -> invalid "each decision is {\"row\": <row>, \"decision\": \"duplicate\" or \"new\"}")
    | _ -> invalid "decisions must be a list of objects"
  in
  match List.assoc_opt "decisions" fields with
  | None | Some `Null -> Ok []
  | Some (`List items) -> List.fold_right (fun item acc -> let* acc = acc in let* d = one item in Ok (d :: acc)) items (Ok [])
  | Some _ -> invalid "decisions must be a list"

(* The file as text (CSV), or base64 for a binary one (an .xlsx). *)
let file_content fields =
  let* text = arg (A.string "content" fields) in
  let* encoded = arg (A.string "content_base64" fields) in
  let* content =
    match text, encoded with
    | Some text, None -> Ok text
    | None, Some b64 -> (match Base64.decode (String.trim b64) with Ok bytes -> Ok bytes | Error (`Msg _) -> invalid "content_base64 isn't valid base64")
    | None, None -> invalid "content or content_base64 is required"
    | Some _, Some _ -> invalid "give content or content_base64, not both"
  in
  Ok content

(* Into an existing account, or into a new one the import opens. *)
let import_target access fields =
  match List.assoc_opt "account_id" fields, List.assoc_opt "new_account" fields with
  | (None | Some `Null), Some (`Assoc n) ->
      let* name = required_string "name" n in
      let* kind = arg (A.string "kind" n) in
      let* kind = match kind with None | Some "asset" -> Ok Account.Asset | Some "liability" -> Ok Liability | Some _ -> invalid "new_account.kind must be asset or liability" in
      let* owner = owner_arg access n in
      let* institution = arg (A.string "institution" n) in
      let* number_last4 = arg (A.string "number_last4" n) in
      Ok (Import.Create { name; kind; owner; institution; number_last4 })
  | Some _, Some (`Assoc _) -> invalid "give account_id or new_account, not both"
  | _ ->
      let* id = account_id "account_id" fields in
      Ok (Import.Existing id)

let import_request access fields =
  let* target = import_target access fields in
  let* content = file_content fields in
  let* file_name = arg (A.string "file_name" fields) in
  let* decisions = decisions fields in
  Ok { Import.target; file_name; content; decisions }

let batch_json (b : Import_store.batch) =
  `Assoc
    [ ("id", `String b.batch_id); ("file_name", Json.option (fun s -> `String s) b.file_name); ("format", `String b.format);
      ("rows", `Int b.row_count); ("new", `Int b.new_count); ("duplicates", `Int b.duplicate_count); ("confirmed_pending", `Int b.matched_count);
      ("first_date", Json.option Json.date b.first_date); ("last_date", Json.option Json.date b.last_date); ("imported_at", Json.time b.created_at);
      ("undone", `Bool b.undone) ]

let identify_statement =
  command "identify_statement" Read
    "Read a bank statement (CSV, or a Handelsbanken .xlsx export) without importing it: its format, what it says about its account \
     (bank, account name, last four digits of the number), the account it belongs to if one of yours has those digits, and the bank \
     accounts and cards it could go into. Then import it with import_statement, into that account or, when none matches, into a \
     new_account named from the file. A file in a layout cashflow doesn't know is read by the assistant, when one is configured: \
     it sees the first rows with every digit masked and says which column holds what; the rows are then read from the file."
    [ string_prop "content" "A CSV file's text."; string_prop "content_base64" "An .xlsx file, base64-encoded (instead of content)." ]
    (fun service access fields ->
      let* content = file_content fields in
      let* (i : Import.identified) = Import.identify (Import.context service access) content in
      let text = Json.option (fun s -> `String s) in
      Ok
        (`Assoc
           [ ("format", `String i.format); ("institution", text i.identity.institution); ("account_name", text i.identity.account_name);
             ("number_last4", text i.identity.last4); ("account", match i.matches with [ only ] -> Json.account only | _ -> `Null);
             ("accounts", `List (List.map Json.account i.candidates)) ]))

let import_statement =
  command "import_statement" Write
    "Import a bank statement (CSV, or a Handelsbanken .xlsx export) into an account, or with new_account instead of account_id into \
     a bank account or card the import opens (with the bank's opening balance, when the file shows it). Without confirm it only previews: which rows are new, which are duplicates of earlier \
     imports, which confirm pending manual entries, and whether the balance will agree with the bank. With confirm: true it posts the \
     new rows; uncategorised ones go to the inbox. A file without running balances can have ambiguous rows: on its first or last day, \
     identical (same day, text and amount) to rows already imported. Ask the user whether each is already imported or a new \
     purchase, and pass the answers as decisions; confirm refuses until every ambiguous row is decided. For an account with nothing \
     before the file, the preview suggests an opening balance from the bank's running balance; offer to set it with \
     set_opening_balance so the balance check agrees."
    [ string_prop "account_id" "The bank account or card the statement belongs to.";
      ( "new_account",
        `Assoc
          [ ("type", `String "object");
            ( "properties",
              `Assoc
                [ ("name", `Assoc [ ("type", `String "string") ]); ("kind", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "asset"; `String "liability" ]) ]);
                  ("owner", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "personal"; `String "household" ]) ]);
                  ("institution", `Assoc [ ("type", `String "string") ]); ("number_last4", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "name" ]);
            ("description", `String "Instead of account_id: the bank account (asset) or card (liability) to open for this statement.") ] );
      string_prop "content" "A CSV file's text.";
      string_prop "content_base64" "An .xlsx file, base64-encoded (instead of content).";
      string_prop "file_name" "The file's name, for the import history.";
      ( "decisions",
        `Assoc
          [ ("type", `String "array");
            ( "items",
              `Assoc
                [ ("type", `String "object");
                  ( "properties",
                    `Assoc [ ("row", `Assoc [ ("type", `String "integer") ]); ("decision", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "duplicate"; `String "new" ]) ]) ] );
                  ("required", `List [ `String "row"; `String "decision" ]) ] );
            ("description", `String "For each ambiguous row of the preview: duplicate (already imported) or new.") ] );
      ("confirm", `Assoc [ ("type", `String "boolean"); ("description", `String "Post the rows; otherwise only preview.") ]) ]
    (fun service access fields ->
      let* request = import_request access fields in
      let* confirm = arg (A.bool "confirm" ~default:false fields) in
      let ctx = Import.context service access in
      if not confirm then
        let* p = Import.preview ctx request in
        let currency = p.decoded_file.account.account.currency in
        Ok
          (`Assoc
             [ ("status", `String (if Option.is_some p.already_imported then "already_imported" else "preview")); ("format", `String p.decoded_file.format);
               ("rows", `List (List.map (planned_json currency) p.rows)); ("balance_check", balance_json currency p.balance);
               ( "suggested_opening_balance",
                 Json.option (fun (date, balance) -> `Assoc [ ("date", Json.date date); ("balance", Json.money currency balance) ]) p.opening ) ])
      else
        let* result = Import.confirm ctx request in
        match result with
        | `Already_imported batch -> Ok (`Assoc [ ("status", `String "already_imported"); ("import", batch_json batch) ])
        | `Imported (s : Import.summary) ->
            Ok
              (`Assoc
                 [ ("status", `String "imported"); ("account", Json.account s.account); ("import", batch_json s.batch); ("inbox", `Int s.inbox_count);
                   ("balance_check", balance_json s.account.account.currency s.balance) ]))

let undo_import =
  command "undo_import" Write "Reverse everything an import posted, including categorisations made since." ~required:[ "import_id" ] [ string_prop "import_id" "The import." ]
    (fun service access fields ->
      let* id = required_string "import_id" fields in
      let* batch = Import.undo (Import.context service access) ~batch_id:id in
      Ok (batch_json batch))

(* Not offered over MCP: wiping data is something a person does on purpose, on the page. *)
let reset_imports =
  command ~mcp:false "reset_imports" Write
    "Danger zone: undo every import into your accounts (or the household's) and delete what they made, so statements can be uploaded again from scratch. Accounts, categories, rules and anything entered by hand stay."
    ~required:[ "confirm" ]
    [ string_prop "owner" "personal (default) or household. Only the household's owner may reset the household.";
      string_prop "confirm" "Must be the word reset." ]
    (fun service access fields ->
      let* owner = owner_arg access fields in
      let* confirm = required_string "confirm" fields in
      if not (String.equal confirm "reset") then invalid "type the word reset to confirm"
      else
        let* () = match owner with Owner.Household _ -> lift (Service.require_owner access) | Owner.User _ -> Ok () in
        let accounts =
          List.filter_map (fun (i : Account_info.t) -> if Owner.equal i.account.owner owner then Some i.account.id else None) (Service.accounts service access)
        in
        let* summary = Reset.imports (Import.context service access) ~accounts in
        Ok (`Assoc [ ("imports", `Int summary.imports); ("transfers", `Int summary.transfers) ]))

let list_imports =
  command "list_imports" Read "An account's recent statement imports." ~required:[ "account_id" ] [ string_prop "account_id" "The account." ]
    (fun service access fields ->
      let* id = account_id "account_id" fields in
      let* _ = lift (Service.find_account service access id) in
      let last = Import_store.last_import_at service.db id in
      Ok
        (`Assoc
           [ ("last_import_at", Json.option Json.time last);
             ("imports", `List (List.map batch_json (Import_store.batches service.db ~account_id:id ~limit:50))) ]))

let reconcile =
  command "reconcile" Write
    "Check an account against the bank's balance on a date. With adjust, a difference is posted so the ledger agrees (against untracked \
     cash for a cash account, uncategorized otherwise); ask the user first."
    ~required:[ "account_id"; "date"; "statement_balance" ]
    [ string_prop "account_id" "The account."; date_prop "date" "The statement's date."; amount_prop "statement_balance" "The bank's balance that day.";
      ("adjust", `Assoc [ ("type", `String "boolean"); ("description", `String "Post the difference.") ]) ]
    (fun service access fields ->
      let* id = account_id "account_id" fields in
      let* currency = lift (currency_of_account service access id) in
      let* on_date = required_date "date" fields in
      let* statement = signed_amount currency "statement_balance" fields in
      let* statement_balance = Option.to_result ~none:(`Invalid "statement_balance is required") statement in
      let* adjust = arg (A.bool "adjust" ~default:false fields) in
      let* `Difference difference, `Adjusted adjusted, `Ledger_balance ledger =
        lift (Inbox.reconcile service access ~account_id:id ~on_date ~statement_balance ~adjust)
      in
      Ok
        (`Assoc
           [ ("difference", Json.money currency difference); ("agrees", `Bool (Int64.equal difference 0L)); ("adjusted", `Bool (Option.is_some adjusted));
             ("ledger_balance", Json.money currency ledger) ]))

(* Planning *)

let insights_ctx service access fields =
  let* common = arg (A.common fields) in
  lift (Insights.context service access ~scope:common.scope ?currency:common.currency ())

let set_budget =
  command "set_budget" Write "Set a category's monthly budget from a pay cycle onward (earlier cycles keep theirs). 0 removes it." ~required:[ "category_id"; "monthly_amount" ]
    (Query.common_props
    @ [ string_prop "category_id" "The expense category."; amount_prop "monthly_amount" "The budget per pay cycle.";
        ("rollover", `Assoc [ ("type", `String "boolean"); ("description", `String "Carry what's left (or overspent) into the next cycle.") ]);
        string_prop "effective_from" "The first cycle it applies to, as YYYY-MM; defaults to the current one." ])
    (fun service access fields ->
      let* ctx = insights_ctx service access fields in
      let* category_id = account_id "category_id" fields in
      let* amount = signed_amount ctx.currency "monthly_amount" fields in
      let* monthly_amount = match amount with Some v when Int64.compare v 0L >= 0 -> Ok v | _ -> invalid "monthly_amount must be zero or more" in
      let* rollover = arg (A.bool "rollover" ~default:false fields) in
      let* effective_from = arg (A.string "effective_from" fields) in
      let* effective_from = lift (Insights.set_budget ctx ~category_id ~monthly_amount ~rollover ~effective_from) in
      Ok (`Assoc [ ("effective_from", `String effective_from) ]))

let save_goal =
  command "save_savings_goal" Write "Create or update a savings goal (a buffer, a vacation, a car)." ~required:[ "name"; "target_amount" ]
    (Query.common_props
    @ [ string_prop "goal_id" "To update an existing goal."; string_prop "name" "What it's for."; amount_prop "target_amount" "How much.";
        date_prop "target_date" "When by."; amount_prop "monthly_amount" "How much you plan to set aside each cycle." ])
    (fun service access fields ->
      let* ctx = insights_ctx service access fields in
      let* goal_id = arg (A.string "goal_id" fields) in
      let* name = required_string "name" fields in
      let* target = required_amount ctx.currency "target_amount" fields in
      let* target_date = arg (A.date "target_date" fields) in
      let* monthly = optional_amount ctx.currency "monthly_amount" fields in
      let* progress =
        lift (Insights.save_goal ctx ?goal_id ~name ~target_amount:(Amount.to_int64 target) ~target_date ~monthly_amount:(Option.map Amount.to_int64 monthly) ())
      in
      Ok (`Assoc [ ("id", `String progress.goal.goal_id); ("saved", Json.money ctx.currency progress.saved); ("on_track", `Bool progress.on_track) ]))

let earmark =
  command "earmark_savings" Write "Set how much of a savings account is set aside for a goal." ~required:[ "goal_id"; "account_id"; "amount" ]
    (Query.common_props @ [ string_prop "goal_id" "The goal."; string_prop "account_id" "The savings account."; amount_prop "amount" "How much of it is for this goal." ])
    (fun service access fields ->
      let* ctx = insights_ctx service access fields in
      let* goal_id = required_string "goal_id" fields in
      let* account_id = account_id "account_id" fields in
      (* In the account's currency, which the goal's must match. *)
      let* currency = lift (currency_of_account service access account_id) in
      let* ctx = lift (Insights.context service access ~scope:ctx.scope ~currency:(Currency.code currency) ()) in
      let* amount = signed_amount ctx.currency "amount" fields in
      let* amount = match amount with Some v when Int64.compare v 0L >= 0 -> Ok v | _ -> invalid "amount must be zero or more" in
      let* progress = lift (Insights.earmark ctx ~goal_id ~account_id ~amount) in
      Ok (`Assoc [ ("saved", Json.money ctx.currency progress.saved); ("remaining", Json.money ctx.currency progress.remaining) ]))

(* Rules *)

let rule_json (r : Suggest.rule) =
  `Assoc
    [ ("id", `String r.rule_id); ("pattern", `String r.pattern); ("category_id", `String (Account_id.to_string r.category_id));
      ("direction", Json.option (fun d -> `String (match d with `In -> "in" | `Out -> "out")) r.direction); ("tags", Json.strings r.tags);
      ("shared", `Bool r.shared); ("auto_apply", `Bool r.auto_apply) ]

let list_rules =
  command "list_rules" Read "Categorisation rules: text patterns that suggest (or apply) a category on import." []
    (fun service access _ -> Ok (`Assoc [ ("rules", `List (List.map rule_json (Inbox.rules service access))) ]))

let add_rule =
  command "add_rule" Write "Add a categorisation rule." ~required:[ "pattern"; "category_id" ]
    [ string_prop "pattern" "Text matched case-insensitively in the description or counterparty."; string_prop "category_id" "The category to suggest.";
      string_prop "direction" "in or out, to match only money in or out."; amount_prop "min_amount" "Only at least this much."; amount_prop "max_amount" "Only at most this much.";
      string_prop "account_id" "Only rows from this bank account."; list_prop "tags" "Tags to add.";
      ("auto_apply", `Assoc [ ("type", `String "boolean"); ("description", `String "Categorise matching rows on import without asking.") ]) ]
    (fun service access fields ->
      let* pattern = required_string "pattern" fields in
      let* category_id = account_id "category_id" fields in
      let* category = lift (Service.find_account service access category_id) in
      let currency = category.account.currency in
      let* direction =
        match A.string "direction" fields with
        | Ok None -> Ok None
        | Ok (Some "in") -> Ok (Some `In)
        | Ok (Some "out") -> Ok (Some `Out)
        | _ -> invalid "direction must be in or out"
      in
      let* min_amount = optional_amount currency "min_amount" fields in
      let* max_amount = optional_amount currency "max_amount" fields in
      let* bank_account_id = optional_account_id "account_id" fields in
      let* tags = strings "tags" fields in
      let* auto_apply = arg (A.bool "auto_apply" ~default:false fields) in
      let* rule =
        lift
          (Inbox.add_rule service access ~owner:category.account.owner ~pattern ~direction ~min_amount:(Option.map Amount.to_int64 min_amount)
             ~max_amount:(Option.map Amount.to_int64 max_amount) ~bank_account_id ~category_id ~tags ~shared:false ~auto_apply)
      in
      Ok (rule_json rule))

let auto_apply_settings =
  command ~mcp:false "list_auto_apply" Read "Bank accounts that apply confident category suggestions on import without asking." []
    (fun service access _ ->
      let entry (id, n) = `Assoc [ ("account_id", `String (Account_id.to_string id)); ("min_confidence", `Int n) ] in
      Ok (`Assoc [ ("accounts", `List (List.map entry (Inbox.auto_apply_settings service access))) ]))

let set_auto_apply =
  command ~mcp:false "set_auto_apply" Write "Choose how confident a category suggestion must be to be applied on import without asking, or turn it off."
    ~required:[ "account_id" ]
    [ string_prop "account_id" "The bank account."; ("min_confidence", `Assoc [ ("type", `String "integer"); ("description", `String "50 to 100. Leave out to turn off.") ]) ]
    (fun service access fields ->
      let* account_id = account_id "account_id" fields in
      let* min_confidence = arg (A.int "min_confidence" ~default:0 ~min:50 ~max:100 fields) in
      let min_confidence = if min_confidence = 0 then None else Some min_confidence in
      let* () = lift (Inbox.set_auto_apply service access ~account_id ~min_confidence) in
      Ok (`Assoc [ ("status", `String "saved") ]))

let remove_rule =
  command "remove_rule" Write "Remove a categorisation rule." ~required:[ "rule_id" ] [ string_prop "rule_id" "The rule." ]
    (fun service access fields ->
      let* id = required_string "rule_id" fields in
      let* () = lift (Inbox.remove_rule service access ~rule_id:id) in
      Ok (`Assoc [ ("status", `String "removed") ]))

(* People *)

let session_json (service : Service.t) access =
  Sql.read_snapshot service.db (fun () ->
  let user = Access.user access in
  let defaults = user.default_currency :: (match Access.household access with None -> [] | Some h -> [h.default_currency]) in
  let codes = List.sort_uniq String.compare defaults in
  let currency_metadata = List.map (fun code -> match Service.currency service code with
    | Ok currency -> `Assoc [("code", `String code); ("scale", `Int (Currency.scale currency))]
    | Error _ -> `Null) codes in
  `Assoc
    [ ("user", Json.user user); ("household", Json.option Json.household (Access.household access));
      ("currency_metadata", `List currency_metadata);
      ("members", match Service.members service access with Ok members -> `List (List.map Json.member members) | Error _ -> `List []);
      ("invitations", `List (List.map Json.household (Service.invitations service access)));
      ("inbox_count", `Int (Inbox.count service access));
      ("locales", Json.strings I18n_locale.supported); ("timezones", Json.strings Time.supported_zone_names) ])

let get_session = command ~mcp:false "get_session" Read "Who you are, your household and pending invitations." [] (fun service access _ -> Ok (session_json service access))

let pay_cycle_arg fields ~default =
  match List.assoc_opt "pay_cycle" fields with
  | None | Some `Null -> Ok default
  | Some (`Assoc f) ->
      let* start =
        match List.assoc_opt "start" f with
        | Some (`Int day) -> Ok (Pay_cycle_setting.Day day)
        | Some (`String "last_banking_day") -> Ok Last_banking_day
        | None -> Ok default.Pay_cycle_setting.start
        | _ -> invalid "pay_cycle.start must be a day from 1 to 28 or \"last_banking_day\""
      in
      let* adjustment =
        match List.assoc_opt "adjustment" f with
        | Some (`String "previous_banking_day") -> Ok Pay_cycle_setting.Previous_banking_day
        | Some (`String "next_banking_day") -> Ok Next_banking_day
        | Some (`String "none") -> Ok No_adjustment
        | None -> Ok default.adjustment
        | _ -> invalid "pay_cycle.adjustment must be previous_banking_day, next_banking_day or none"
      in
      let* naming =
        match List.assoc_opt "naming" f with
        | Some (`String "mostly_covered_month") -> Ok Pay_cycle_setting.Mostly_covered_month
        | Some (`String "starting_month") -> Ok Starting_month
        | None -> Ok default.naming
        | _ -> invalid "pay_cycle.naming must be mostly_covered_month or starting_month"
      in
      let calendar = match List.assoc_opt "calendar" f with Some (`String c) -> c | _ -> default.calendar in
      Result.map_error (fun (`Invalid_pay_cycle r) -> `Invalid r) (Pay_cycle_setting.make ~start ~adjustment ~calendar ~naming)
  | Some _ -> invalid "pay_cycle must be an object"

let pay_cycle_prop =
  ( "pay_cycle",
    `Assoc
      [ ("type", `String "object");
        ("description", `String "start (a day 1-28 or \"last_banking_day\"), adjustment (previous_banking_day, next_banking_day, none), naming (mostly_covered_month, starting_month).") ] )

let update_settings =
  command ~mcp:false "update_settings" Write "Change your language, time zone, default currency or pay cycle."
    [ string_prop "locale" "en or sv."; string_prop "timezone" "e.g. Europe/Stockholm."; string_prop "default_currency" "ISO 4217."; pay_cycle_prop ]
    (fun service access fields ->
      let user = Access.user access in
      let* locale = arg (A.string "locale" fields) in
      let* timezone = arg (A.string "timezone" fields) in
      let* default_currency = arg (A.string "default_currency" fields) in
      let* pay_cycle = pay_cycle_arg fields ~default:user.pay_cycle in
      let* access =
        lift
          (Service.update_settings service access
             { locale = Option.value locale ~default:user.locale; timezone = Option.value timezone ~default:user.timezone;
               default_currency = Option.value default_currency ~default:user.default_currency; pay_cycle })
      in
      Ok (session_json service access))

let household_id_arg fields =
  let* text = required_string "household_id" fields in
  match Household_id.of_string text with Some id -> Ok id | None -> invalid "household_id must be a household id"

let respond_to_invitation =
  command ~mcp:false "respond_to_invitation" Write "Accept or decline an invitation to a household." ~required:[ "household_id"; "accept" ]
    [ string_prop "household_id" "The household."; ("accept", `Assoc [ ("type", `String "boolean") ]) ]
    (fun service access fields ->
      let* household_id = household_id_arg fields in
      let* accept = arg (A.bool "accept" ~default:false fields) in
      let* access = lift (Service.respond_to_invitation service access ~household_id ~accept) in
      Ok (session_json service access))

let leave_household =
  command ~mcp:false "leave_household" Write "Leave your household." []
    (fun service access _ -> let* access = lift (Service.leave_household service access) in Ok (session_json service access))

let create_household =
  command ~mcp:false "create_household" Admin "Create a household with shared accounts and categories." ~required:[ "name" ]
    [ string_prop "name" "e.g. Home."; string_prop "default_currency" "ISO 4217; defaults to yours." ]
    (fun service access fields ->
      let* name = required_string "name" fields in
      let* currency = arg (A.string "default_currency" fields) in
      let* _ = lift (Service.create_household service access ~name ~default_currency:(Option.value currency ~default:(Access.user access).default_currency)) in
      Ok (session_json service (Service.access_for service (Access.user access))))

let invite_member =
  command "invite_member" Admin "Invite someone to your household by email. They need to have signed in once." ~required:[ "email" ] [ string_prop "email" "Their email." ]
    (fun service access fields ->
      let* email = required_string "email" fields in
      let* invitee = lift (Service.invite_member service access ~email) in
      Ok (`Assoc [ ("invited", `String invitee.display_name) ]))

let update_household =
  command ~mcp:false "update_household" Admin "Rename the household, change its split for \"my share\" reports, or its pay cycle."
    [ string_prop "name" "A new name."; prop "report_split_percent" "integer" "Your share in percent (default 50)."; pay_cycle_prop ]
    (fun service access fields ->
      let* household = Option.to_result ~none:(`Invalid "you don't belong to a household") (Access.household access) in
      let* name = arg (A.string "name" fields) in
      let* percent = arg (A.int "report_split_percent" ~default:(household.report_split_bp / 100) ~min:0 ~max:100 fields) in
      let* pay_cycle = pay_cycle_arg fields ~default:household.pay_cycle in
      let* _ = lift (Service.update_household service access ~name ~report_split_bp:(percent * 100) ~pay_cycle) in
      Ok (session_json service (Service.access_for service (Access.user access))))

let queries =
  List.map
    (fun (Query.Any d as q) ->
      command d.name Read d.description d.properties (fun service access fields -> lift (Query.execute service access q fields)))
    Query.all

let base =
  [ get_session; list_accounts; open_account; update_account; close_account; set_opening_balance; set_market_value; query_transactions;
    set_merchant_logo; clear_merchant_logo; get_transaction; add_transaction; reverse_transaction; correct_transaction; post_pending; void_pending; list_inbox; categorize; share;
    pair; pair_all_moves; pair_candidates; accept_candidate; dismiss; identify_statement; import_statement; undo_import; reset_imports; list_imports; reconcile; set_budget; save_goal; earmark;
    list_rules; add_rule; remove_rule; auto_apply_settings; set_auto_apply; update_settings; respond_to_invitation; leave_household; create_household; invite_member;
    update_household ]
  @ queries

let schema c =
  `Assoc
    [ ("type", `String "object"); ("properties", `Assoc c.properties); ("required", `List (List.map (fun n -> `String n) c.required));
      ("additionalProperties", `Bool false) ]

let allowed ~scopes c =
  match c.scope with
  | Read -> List.mem "ledger:read" scopes || List.mem "ledger:write" scopes
  | Write -> List.mem "ledger:write" scopes
  | Admin -> List.mem "ledger:admin" scopes

let execute service access ~scopes c (args : Yojson.Safe.t) =
  if not (allowed ~scopes c) then Error (`Forbidden ("this needs the " ^ scope_name c.scope ^ " scope"))
  else
    match args with
    | `Assoc fields -> (
        match List.find_opt (fun (name, _) -> not (List.mem_assoc name c.properties)) fields with
        | Some (name, _) -> invalid ("unknown argument " ^ name)
        | None -> c.run service access fields)
    | `Null -> c.run service access []
    | _ -> invalid "arguments must be a JSON object"
