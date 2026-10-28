(* Starter categories: the chart's accounts, recognised by their keys. While one still bears a
   starter name, in any locale, each viewer sees it in their own language; once someone renamed
   it, everyone sees what they typed. *)
module Account_info = Cashflow_core.Account_info
module Chart = Cashflow_core.Chart
module Idempotency_key = Cashflow_core.Idempotency_key

type t = { entry : Chart.entry; currency : string option }

let is_currency code = String.length code = 3 && String.for_all (fun c -> c >= 'A' && c <= 'Z') code

(* "seed:<slug>", or "seed:<slug>-<CUR>" for one opened later in another currency. An exact
   slug wins: "seed:other-income" is its own category, not "other" in a currency "income". *)
let of_key key =
  let key = Idempotency_key.to_string key in
  let base (entry : Chart.entry) = "seed:" ^ entry.slug in
  match List.find_opt (fun entry -> key = base entry) Chart.entries with
  | Some entry -> Some { entry; currency = None }
  | None ->
      List.find_map
        (fun entry ->
          let prefix = base entry ^ "-" in
          if String.starts_with ~prefix key then
            let code = String.sub key (String.length prefix) (String.length key - String.length prefix) in
            if is_currency code then Some { entry; currency = Some code } else None
          else None)
        Chart.entries

let name ~locale starter = Category_name.name ~slug:starter.entry.slug ~currency:starter.currency locale

let untouched (info : Account_info.t) =
  match of_key info.account.key with
  | Some starter when List.exists (fun locale -> name ~locale starter = info.account.name) Msg.locales -> Some starter
  | _ -> None

let display_name ~locale (info : Account_info.t) = match untouched info with Some starter -> name ~locale starter | None -> info.account.name

(* Every name a category goes by: what is stored, and for an untouched starter its name in each language, because each
   reader sees it in theirs. *)
let names (info : Account_info.t) =
  match untouched info with
  | Some starter -> info.account.name :: List.map (fun locale -> name ~locale starter) Msg.locales
  | None -> [ info.account.name ]
