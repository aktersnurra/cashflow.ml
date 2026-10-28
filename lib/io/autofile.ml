(* Filing the inbox without asking, when a person asked for it (the advisor's file_inbox): every waiting row
   cashflow is sure enough of goes to its category, the rest stay for the person with cashflow's best guess.
   Sure enough is [threshold]: a rule, a merchant the person always filed the same way, or the classifier's
   calibrated probability. Each row is filed through the inbox like a person's choice (one transfer, undone
   by recategorising it), but marked as cashflow's, so it teaches nothing.

   [classify] reads and asks the classifier, holding no transaction (it is network I/O); [file] then files
   inside the caller's transaction, checking each row again as it is then. *)

module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Currency = Cashflow_core.Currency
module Owner = Cashflow_core.Owner
module Statement_format = Cashflow_core.Statement_format
module Suggest = Cashflow_core.Suggest
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id

let threshold = 80
let max_rows = 200

type guess = { category_id : Account_id.t; confidence : int; source : string }
type classified = { owner : Owner.t; rows : (Transfer_id.t * guess option) list }

type row = {
  transfer : Transfer.t;
  outflow : bool;
  guess : (Account_info.t * int * string) option;  (** the category, how sure, and where the guess came from *)
}

type report = { filed : row list; unsure : row list; kept_on : bool }

let row_of (t : Transfer.t) ~outflow : Statement_format.row =
  let amount = Amount.to_int64 t.amount in
  { booking_date = t.value_date; transaction_date = None; description = t.memo.description; counterparty = t.memo.counterparty;
    amount = (if outflow then Int64.neg amount else amount); balance = None; reference = None }

let classify (service : Service.t) access ~owner =
  let waiting =
    Sql.read_snapshot service.db (fun () ->
        List.filter_map
          (fun (item : Import_store.item) ->
            match Service.find_account service access item.bank_account_id, Service.get_transfer service access item.transfer_id with
            | Ok bank, Ok view when Owner.equal bank.account.owner owner ->
                let outflow = Inbox.outflow item view.transfer in
                Some (item, bank, row_of view.transfer ~outflow)
            | _ -> None)
          (List.filteri (fun i _ -> i < max_rows) (Import_store.open_items service.db access ~limit:max_rows)))
  in
  (* Rules and the person's own history first; what they don't know goes to the classifier, a bank account
     at a time (its owner's categories are the choices). *)
  let suggester = Import.default_suggester service access in
  let known, unknown =
    List.partition_map
      (fun ((item : Import_store.item), (bank : Account_info.t), row) ->
        match suggester { bank_account = bank; row; merchant = item.merchant_key } with
        | Some s -> Left (item.transfer_id, Some { category_id = s.category_id; confidence = s.confidence; source = Suggest.source_to_string s.source })
        | None -> Right (item, bank, row))
      waiting
  in
  let banks = List.sort_uniq Account_id.compare (List.map (fun (_, (b : Account_info.t), _) -> b.account.id) unknown) in
  let asked =
    List.concat_map
      (fun bank_id ->
        let rows = List.filter (fun (_, (b : Account_info.t), _) -> Account_id.equal b.account.id bank_id) unknown in
        let bank = match rows with (_, b, _) :: _ -> b | [] -> assert false in
        let indexed = List.mapi (fun i (_, _, row) -> (i, row)) rows in
        let answers = Llm_features.suggest_categories service access ~bank_account:bank indexed in
        List.mapi
          (fun i ((item : Import_store.item), _, _) ->
            ( item.transfer_id,
              Option.map
                (fun (s : Suggest.suggestion) -> { category_id = s.category_id; confidence = s.confidence; source = Suggest.source_to_string s.source })
                (List.assoc_opt i answers) ))
          rows)
      banks
  in
  { owner; rows = known @ asked }

(* A category the row can be filed under without asking: the bank account's own owner and currency, the
   direction the money went, a person's category (never a system one), open, and not where it is now. *)
let fits (bank : Account_info.t) (item : Import_store.item) ~outflow (c : Account_info.t) =
  Owner.equal c.account.owner bank.account.owner
  && Currency.equal c.account.currency bank.account.currency
  && (if outflow then c.account.kind = Expense else c.account.kind = Income)
  && Option.is_none c.details.role && (not c.account.closed)
  && not (Account_id.equal c.account.id item.current_account_id)

let file (service : Service.t) access ~keep_on (c : classified) =
  let filed, unsure =
    List.fold_left
      (fun (filed, unsure) (transfer_id, guess) ->
        match Import_store.find_item service.db transfer_id with
        | Some ({ status = "open"; _ } as item) -> (
            match Service.find_account service access item.bank_account_id, Service.get_transfer service access transfer_id with
            | Ok bank, Ok view -> (
                let outflow = Inbox.outflow item view.transfer in
                let category = Option.bind guess (fun g -> Result.to_option (Service.find_account service access g.category_id)) in
                let row g c = { transfer = view.transfer; outflow; guess = Some (c, g.confidence, g.source) } in
                match guess, category with
                | Some g, Some c when fits bank item ~outflow c && g.confidence >= threshold -> (
                    match Inbox.categorize service access ~transfer_id ~category_id:c.account.id ~by:`Auto () with
                    | Ok (Some (Ok _)) -> (row g c :: filed, unsure)
                    | _ -> (filed, row g c :: unsure))
                | Some g, Some c when fits bank item ~outflow c -> (filed, row g c :: unsure)
                | _ -> (filed, { transfer = view.transfer; outflow; guess = None } :: unsure))
            | _ -> (filed, unsure))
        | _ -> (filed, unsure))
      ([], []) c.rows
  in
  (* Later imports into the owner's bank accounts file at the same threshold. *)
  let kept_on =
    keep_on
    && List.fold_left
         (fun any (i : Account_info.t) ->
           if Owner.equal i.account.owner c.owner && i.details.statement_backed && not i.account.closed then
             Result.is_ok (Inbox.set_auto_apply service access ~account_id:i.account.id ~min_confidence:(Some threshold)) || any
           else any)
         false (Service.accounts service access)
  in
  { filed = List.rev filed; unsure = List.rev unsure; kept_on }
