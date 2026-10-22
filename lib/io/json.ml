(* JSON for domain values, shared by MCP and the web API. Money is an exact decimal string in the
   currency's major unit ("1240.50"); the minor-unit integer is never exposed to callers, so
   nobody has to know a currency's scale. *)
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Currency = Cashflow_core.Currency
module Cycle = Cashflow_core.Cycle
module Date = Cashflow_core.Date
module Decimal = Cashflow_core.Decimal
module Household = Cashflow_core.Household
module Household_id = Cashflow_core.Household_id
module Ledger_core = Cashflow_core.Ledger_core
module Ledger_error = Cashflow_core.Ledger_error
module Pay_cycle_setting = Cashflow_core.Pay_cycle_setting
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id
module User = Cashflow_core.User
module User_id = Cashflow_core.User_id

type t = Yojson.Safe.t

let decimal = Decimal.format
let parse_decimal ~scale text = Result.to_option (Decimal.parse ~scale text)

let money currency minor = `String (decimal ~scale:(Currency.scale currency) minor)
let option f = function None -> `Null | Some value -> f value
let date d = `String (Date.to_string d)
let time t = `String (Ptime.to_rfc3339 ~tz_offset_s:0 t)
let strings values = `List (List.map (fun s -> `String s) values)

let cycle (c : Cycle.t) = `Assoc [ ("cycle", `String (Cycle.label c)); ("first", date c.first); ("last", date c.last) ]

let account (info : Account_info.t) =
  let a = info.account in
  `Assoc
    [ ("id", `String (Account_id.to_string a.id)); ("name", `String a.name); ("kind", `String (Account.kind_to_string a.kind));
      ("owner", `String (match a.owner with User _ -> "personal" | Household _ -> "household"));
      ("currency", `String (Currency.code a.currency)); ("balance", money a.currency (Account.posted_balance a));
      ("pending", money a.currency (Account.pending_balance a)); ("closed", `Bool a.closed);
      ("institution", option (fun s -> `String s) info.details.institution);
      ("number", option (fun s -> `String s) (Account_info.masked_number info));
      ("role", option (fun r -> `String (Account_info.role_to_string r)) info.details.role);
      ("statement_backed", `Bool info.details.statement_backed); ("tags", strings info.details.tags) ]

let account_ref (info : Account_info.t) =
  `Assoc [ ("id", `String (Account_id.to_string info.account.id)); ("name", `String info.account.name) ]

let side = function
  | Service.Own info -> account_ref info
  | Hidden { owner_name } -> `Assoc [ ("hidden", `Bool true); ("owner", `String owner_name) ]

let currency_of_view (view : Service.transfer_view) =
  match view.debit, view.credit with
  | Own info, _ | _, Own info -> info.account.currency
  | Hidden _, Hidden _ -> Result.get_ok (Currency.make ~code:"XXX" ~scale:2)

let transfer_kind (t : Transfer.t) =
  match t.kind with
  | Single -> `String "posted"
  | Pending _ -> `String "pending"
  | Post_pending _ -> `String "post_pending"
  | Void_pending _ -> `String "void_pending"

(* "from" is the credited account and "to" the debited one: money moves from one to the other. *)
let transfer (view : Service.transfer_view) =
  let t = view.transfer in
  let currency = currency_of_view view in
  `Assoc
    [ ("id", `String (Transfer_id.to_string t.id)); ("date", date t.value_date); ("from", side view.credit); ("to", side view.debit);
      ("amount", money currency (Amount.to_int64 t.amount)); ("currency", `String (Currency.code currency));
      ("kind", transfer_kind t); ("code", `String (Transfer.code_to_string t.code));
      ("description", `String t.memo.description); ("counterparty", option (fun s -> `String s) t.memo.counterparty);
      ("tags", strings t.memo.tags);
      ("pending_id", option (fun id -> `String (Transfer_id.to_string id))
                       (match t.kind with Post_pending { pending_id } | Void_pending { pending_id } -> Some pending_id | _ -> None));
      ("resolved_by", option (fun (r : Transfer.t) -> `String (Transfer_id.to_string r.id)) view.resolved_by);
      ("reverses", option (fun id -> `String (Transfer_id.to_string id)) t.reverses);
      ("reversed_by", option (fun id -> `String (Transfer_id.to_string id)) view.reversed_by);
      ("recorded_at", time t.recorded_at) ]

let outcome = function
  | Ok (Ledger_core.Created view) -> `Assoc [ ("status", `String "created"); ("transfer", transfer view) ]
  | Ok (Exists view) -> `Assoc [ ("status", `String "exists"); ("transfer", transfer view) ]
  | Error error -> `Assoc [ ("status", `String "error"); ("error", `String (Ledger_error.transfer_to_string error)) ]

let user (user : User.t) =
  `Assoc
    [ ("id", `String (User_id.to_string user.id)); ("display_name", `String user.display_name);
      ("email", option (fun s -> `String s) user.email); ("locale", `String user.locale); ("timezone", `String user.timezone);
      ("default_currency", `String user.default_currency); ("pay_cycle", `String (Pay_cycle_setting.to_string user.pay_cycle));
      ("is_admin", `Bool user.is_admin) ]

let household (h : Household.t) =
  `Assoc
    [ ("id", `String (Household_id.to_string h.id)); ("name", `String h.name); ("default_currency", `String h.default_currency);
      ("report_split_bp", `Int h.report_split_bp); ("pay_cycle", `String (Pay_cycle_setting.to_string h.pay_cycle)) ]

let member (m : Household.member) =
  `Assoc [ ("user_id", `String (User_id.to_string m.user_id)); ("display_name", `String m.display_name);
           ("status", `String (Household.status_to_string m.status)) ]
