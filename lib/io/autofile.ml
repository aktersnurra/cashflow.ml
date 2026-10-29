(* Filing the inbox without asking, when a person asked for it (the advisor's file_inbox): every waiting row
   cashflow is sure enough of goes to its category, the rest stay for the person with cashflow's best guess.
   Sure enough is [threshold]: a rule, a merchant the person always filed the same way, or the classifier's
   calibrated probability. Each row is filed through the inbox like a person's choice (one transfer, undone
   by recategorising it), but marked as cashflow's, so it teaches nothing.

   [classify] reads and asks the classifier, holding no transaction (it is network I/O); [file] then files
   inside the caller's transaction, checking each row again as it is then.

   [sweep] does the same without being asked, after an import and when the Overview finds rows waiting: the
   books shouldn't wait on a person sorting coffee. It files at each bank account's own threshold (on by
   default), and a small row it isn't sure of is filed with its best guess and marked as guessed rather than
   asked about: its category changes no number a person would act on. What is left, the big or recurring rows
   cashflow can't place, is what the Overview asks about (Attention), one merchant at a time. *)

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

type report = { filed : row list; unsure : row list; kept_on : bool; guessed : row list }

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

(* [per_bank]: each bank account's own threshold (off files nothing) rather than [threshold]. [guess_below]: a
   row under this amount (in its bank account's currency) whose best guess is not sure enough is filed with it,
   marked as guessed. *)
let file ?(per_bank = false) ?guess_below (service : Service.t) access ~keep_on (c : classified) =
  let filed, unsure, guessed =
    List.fold_left
      (fun (filed, unsure, guessed) (transfer_id, guess) ->
        match Import_store.find_item service.db transfer_id with
        | Some ({ status = "open"; _ } as item) -> (
            match Service.find_account service access item.bank_account_id, Service.get_transfer service access transfer_id with
            | Ok bank, Ok view -> (
                let outflow = Inbox.outflow item view.transfer in
                let category = Option.bind guess (fun g -> Result.to_option (Service.find_account service access g.category_id)) in
                let row g c = { transfer = view.transfer; outflow; guess = Some (c, g.confidence, g.source) } in
                let threshold = if per_bank then Import_store.auto_apply_threshold service.db bank.account.id else Some threshold in
                let small =
                  match guess_below with Some limit -> Int64.compare (Amount.to_int64 view.transfer.amount) (limit bank.account.currency) < 0 | None -> false
                in
                let file_as (c : Account_info.t) = Inbox.categorize service access ~transfer_id ~category_id:c.account.id ~by:`Auto () in
                match guess, category, threshold with
                | Some g, Some c, Some threshold when fits bank item ~outflow c && g.confidence >= threshold -> (
                    match file_as c with
                    | Ok (Some (Ok _)) -> (row g c :: filed, unsure, guessed)
                    | _ -> (filed, row g c :: unsure, guessed))
                | Some g, Some c, Some _ when fits bank item ~outflow c && small -> (
                    match file_as c with
                    | Ok (Some (Ok _)) ->
                        Import_store.set_guessed service.db transfer_id;
                        (filed, unsure, row g c :: guessed)
                    | _ -> (filed, row g c :: unsure, guessed))
                | Some g, Some c, _ when fits bank item ~outflow c -> (filed, row g c :: unsure, guessed)
                | _ -> (filed, { transfer = view.transfer; outflow; guess = None } :: unsure, guessed))
            | _ -> (filed, unsure, guessed))
        | _ -> (filed, unsure, guessed))
      ([], [], []) c.rows
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
  { filed = List.rev filed; unsure = List.rev unsure; kept_on; guessed = List.rev guessed }

(* Under this, a row's category changes nothing a person would act on (300 in the currency). *)
let guess_limit currency = Int64.mul 300L (Int64.of_float (10. ** float_of_int (Currency.scale currency)))

(* One sweep at a time per person, and not again within [rest] unless an import asked for it. *)
let sweeping : (string, unit) Hashtbl.t = Hashtbl.create 8
let last_swept : (string, float) Hashtbl.t = Hashtbl.create 8
let rest = 1800.

let sweep (service : Service.t) access =
  let user = Cashflow_core.User_id.to_string (Access.user access).id in
  if not (Hashtbl.mem sweeping user) then begin
    Hashtbl.replace sweeping user ();
    Fun.protect ~finally:(fun () -> Hashtbl.remove sweeping user; Hashtbl.replace last_swept user (Unix.gettimeofday ()))
      (fun () ->
        let total_filed = ref 0 and total_guessed = ref 0 in
        List.iter
          (fun owner ->
            (* A chunk at a time until a pass changes nothing. *)
            let rec pass n =
              if n > 0 then
                let c = classify service access ~owner in
                if c.rows <> [] then
                  let r = file ~per_bank:true ~guess_below:guess_limit service access ~keep_on:false c in
                  let moved = List.length r.filed + List.length r.guessed in
                  total_filed := !total_filed + List.length r.filed;
                  total_guessed := !total_guessed + List.length r.guessed;
                  if moved > 0 then pass (n - 1)
            in
            pass 10)
          (Access.owners access);
        if !total_filed + !total_guessed > 0 then
          Import_store.record_run service.db (Access.user access).id ~at:(service.now ()) ~filed:!total_filed ~guessed:!total_guessed)
  end

(* After an import, at once; when the Overview finds rows waiting, at most every half hour. *)
let schedule_sweep ?(now = false) (service : Service.t) access =
  let user = Cashflow_core.User_id.to_string (Access.user access).id in
  let recent = match Hashtbl.find_opt last_swept user with Some t -> Unix.gettimeofday () -. t < rest | None -> false in
  if (now || not recent) && not (Hashtbl.mem sweeping user) then
    try service.schedule (fun () -> try sweep service access with exn -> Logs.err (fun m -> m "inbox sweep failed: %s" (Printexc.to_string exn)))
    with exn -> Logs.err (fun m -> m "inbox sweep not scheduled: %s" (Printexc.to_string exn))
