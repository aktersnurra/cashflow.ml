module Account_info = Cashflow_core.Account_info
module Analytics = Cashflow_core.Analytics
module Date = Cashflow_core.Date

let reconciled_through db ~accounts ~(book : Analytics.book) =
  let statement_accounts =
    List.filter
      (fun (info : Account_info.t) -> info.details.statement_backed && Option.is_some (book.inside info.account.id))
      accounts
  in
  List.fold_left
    (fun through (info : Account_info.t) ->
      match through, Planning_store.reconciled_through db info.account.id ~ledger_balance_on:(Import_store.posted_balance_on db info.account) with
      | Some (Some a), Some b -> Some (Some (Date.min a b))
      | None, b -> Some b
      | _, None | Some None, _ -> Some None)
    None statement_accounts
  |> Option.join
