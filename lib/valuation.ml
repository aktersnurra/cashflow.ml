(* Investment accounts (Avanza, Montrose) are tracked by contributions plus an occasional market
   value: the difference to the ledger is posted against unrealized gains (§3.7), so net worth
   follows the market while income and spending never do. *)
let ( let* ) = Result.bind

let set_market_value (service : Service.t) access ~account_id ~value ~on_date =
  let* info = Service.find_account service access account_id in
  let account = info.account in
  if account.kind <> Asset then Error (`Invalid "only asset accounts have a market value")
  else
    let current = Import_store.posted_balance_on service.db account on_date in
    let difference = Int64.sub value current in
    if Int64.equal difference 0L then Ok None
    else
      let* gains =
        Inbox.role_account service access account.owner Unrealized Equity ~currency:(Currency.code account.currency) ~name:"Unrealized gains"
          ~member_id:None
      in
      let debit, credit = if Int64.compare difference 0L > 0 then (account.id, gains.account.id) else (gains.account.id, account.id) in
      let key =
        Result.get_ok (Idempotency_key.of_string (Printf.sprintf "revalue:%s:%s:%Ld" (Account_id.to_string account.id) (Date.to_string on_date) value))
      in
      let* results =
        Service.submit service access ~scope:account.owner
          [ { key; linked = false;
              body =
                Transfer
                  { debit_account_id = debit; credit_account_id = credit; amount = Option.get (Amount.of_int64 (Int64.abs difference));
                    phase = Posted; code = Revaluation; memo = { Transfer.empty_memo with description = "Market value" }; value_date = on_date } } ]
      in
      match results with [ result ] -> Ok (Some result) | _ -> Ok None
