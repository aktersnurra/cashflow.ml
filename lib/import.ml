(* Importing a bank statement, as a pipeline of stages composed with Kleisli composition:

     decode >=> fingerprint >=> classify >=> suggest >=> check_balance

   Each stage takes the previous one's result and either refines it or stops with an error, so a
   stage can be tested on its own, and a bank sync can reuse everything after [decode]. Running
   the pipeline is the preview; [confirm] runs it again inside a transaction and posts the result. *)
let ( let* ) = Result.bind
let ( >=> ) f g x = Result.bind (f x) g

type error = [ Service.error | `Unreadable of string ]

let error_to_string : [< error ] -> string = function
  | `Unreadable reason -> "the file couldn't be read: " ^ reason
  | #Service.error as e -> Service.error_to_string e

type request = { account_id : Account_id.t; file_name : string option; content : string }

type ctx = { service : Service.t; access : Access.t; suggester : Suggest.t }

type decoded = { ctx : ctx; request : request; account : Account_info.t; format : string; sha256 : string; rows : Statement_format.row list }

type keyed = { decoded : decoded; keyed_rows : (int * Statement_format.row * Idempotency_key.t) list }

type status =
  | New
  | Duplicate of Transfer_id.t
  | Matched of Transfer.t  (** confirms a pending manual entry with the same date and amount *)

type planned = {
  index : int;
  row : Statement_format.row;
  key : Idempotency_key.t;
  status : status;
  near_pending : Transfer.t option;  (** a pending entry on the same day with a different amount *)
  suggestion : Suggest.suggestion option;
}

type balance_check = { on_date : Date.t; statement_balance : int64; ledger_balance : int64 }

type preview = {
  decoded_file : decoded;
  rows : planned list;
  already_imported : Import_store.batch option;
  balance : balance_check option;
}

let max_file_bytes = 5 * 1024 * 1024

(* Stage 1: read the file and find its format. *)
let decode ctx (request : request) =
  if String.length request.content > max_file_bytes then Error (`Invalid "the file is larger than 5 MB")
  else
    let* account = Service.find_account ctx.service ctx.access request.account_id in
    if account.account.kind <> Asset && account.account.kind <> Liability then Error (`Invalid "statements import into bank accounts or cards")
    else
      let table = Csv.parse request.content in
      match Statement_format.detect table with
      | None -> Error (`Unreadable "no header row with a date and an amount column was found")
      | Some (module F) -> (
          match F.parse ~scale:(Currency.scale account.account.currency) table with
          | Error reason -> Error (`Unreadable reason)
          | Ok [] -> Error (`Unreadable "the file has no transactions")
          | Ok rows ->
              Ok { ctx; request; account; format = F.name; sha256 = Digestif.SHA256.(to_hex (digest_string request.content)); rows })

let normalize_text s = String.lowercase_ascii s |> String.split_on_char ' ' |> List.filter (( <> ) "") |> String.concat " "

(* Stage 2: a fingerprint per row (§6.3). The running balance tells identical rows apart; without
   one, so does the row's position among identical rows on that day in this file. *)
let fingerprint (d : decoded) =
  let occurrences = Hashtbl.create 64 in
  let keyed_rows =
    List.mapi
      (fun index (row : Statement_format.row) ->
        let base =
          String.concat "\x00"
            [ Account_id.to_string d.account.account.id; Date.to_string row.booking_date; Int64.to_string row.amount;
              normalize_text row.description; Option.fold ~none:"" ~some:Int64.to_string row.balance;
              Option.value row.reference ~default:"" ]
        in
        let n = Option.value (Hashtbl.find_opt occurrences base) ~default:0 in
        Hashtbl.replace occurrences base (n + 1);
        let hash = Digestif.SHA256.(to_hex (digest_string (base ^ "\x00" ^ string_of_int n))) in
        (index, row, Result.get_ok (Idempotency_key.of_string ("import:" ^ hash))))
      d.rows
  in
  Ok { decoded = d; keyed_rows }

(* A key whose transfer was reversed (an undone import) is free to use again with a suffix. *)
let rec usable_key view scope key n =
  let candidate = if n = 0 then key else Result.get_ok (Idempotency_key.of_string (Idempotency_key.to_string key ^ "#" ^ string_of_int n)) in
  match view.Ledger_core.transfer_by_key scope candidate with
  | Some (t : Transfer.t) when Option.is_some (view.reversal t.id) -> usable_key view scope key (n + 1)
  | Some t -> `Exists (candidate, t)
  | None -> `Free candidate

let direction_matches (account_id : Account_id.t) (row : Statement_format.row) (t : Transfer.t) =
  if Int64.compare row.amount 0L < 0 then Account_id.equal t.credit_account_id account_id else Account_id.equal t.debit_account_id account_id

(* Stage 3: duplicates, and pending manual entries the bank now confirms (§6.7): the same account,
   direction, purchase date and amount. *)
let classify (k : keyed) =
  let d = k.decoded in
  let account = d.account.account in
  let view = Store_sqlite.view d.ctx.service.db in
  let pendings =
    List.filter
      (fun (t : Transfer.t) -> Account_id.equal t.debit_account_id account.id || Account_id.equal t.credit_account_id account.id)
      (Store_sqlite.open_pending d.ctx.service.db)
  in
  let claimed = Hashtbl.create 8 in
  let rows =
    List.map
      (fun (index, (row : Statement_format.row), key) ->
        let purchase_date = Option.value row.transaction_date ~default:row.booking_date in
        let same_day (t : Transfer.t) = Date.equal t.value_date purchase_date && direction_matches account.id row t && not (Hashtbl.mem claimed t.id) in
        let key, status =
          match usable_key view account.owner key 0 with
          | `Exists (key, t) -> (key, Duplicate t.id)
          | `Free key -> (
              match List.filter (fun (t : Transfer.t) -> same_day t && Int64.equal (Amount.to_int64 t.amount) (Int64.abs row.amount)) pendings with
              | [ pending ] -> Hashtbl.replace claimed pending.id (); (key, Matched pending)
              | _ -> (key, New))
        in
        let near_pending =
          match status with
          | New -> List.find_opt (fun (t : Transfer.t) -> same_day t && not (Int64.equal (Amount.to_int64 t.amount) (Int64.abs row.amount))) pendings
          | Duplicate _ | Matched _ -> None
        in
        { index; row; key; status; near_pending; suggestion = None })
      k.keyed_rows
  in
  Ok (d, rows)

(* Stage 4: suggestions for new rows. *)
let suggest (d, rows) =
  let rows =
    List.map
      (fun planned ->
        match planned.status with
        | New ->
            let input = { Suggest.bank_account = d.account; row = planned.row; merchant = Suggest.merchant_key planned.row } in
            { planned with suggestion = d.ctx.suggester input }
        | Duplicate _ | Matched _ -> planned)
      rows
  in
  Ok (d, rows)

(* The balance after the file's last day, from the running-balance column. Banks list rows newest
   or oldest first, so the closing row is the newest-dated one at whichever end comes later. *)
let closing_balance (rows : Statement_format.row list) =
  match rows, List.rev rows with
  | first :: _, last :: _ ->
      let newest_first = Date.compare first.booking_date last.booking_date >= 0 in
      let closing = if newest_first then first else last in
      Option.map (fun balance -> (closing.booking_date, balance)) closing.balance
  | _ -> None

(* Stage 5: will the ledger agree with the bank once the new rows are posted? *)
let check_balance (d, rows) =
  let already_imported = Import_store.find_active_batch d.ctx.service.db ~account_id:d.account.account.id ~sha:d.sha256 in
  let balance =
    Option.map
      (fun (on_date, statement_balance) ->
        let existing = Import_store.posted_balance_on d.ctx.service.db d.account.account on_date in
        let sign = if Account.debit_normal d.account.account.kind then 1L else -1L in
        let added =
          List.fold_left
            (fun sum p ->
              match p.status with
              | (New | Matched _) when Date.compare p.row.booking_date on_date <= 0 -> Int64.add sum (Int64.mul sign p.row.amount)
              | _ -> sum)
            0L rows
        in
        { on_date; statement_balance = Int64.mul sign statement_balance; ledger_balance = Int64.add existing added })
      (closing_balance d.rows)
  in
  Ok { decoded_file = d; rows; already_imported; balance }

let pipeline ctx = decode ctx >=> fingerprint >=> classify >=> suggest >=> check_balance
let preview ctx request = pipeline ctx request

type summary = { batch : Import_store.batch; inbox_count : int; balance : balance_check option }

exception Abort of error

(* The account an imported row is posted against until someone categorises it. *)
let uncategorized ctx (account : Account.t) kind =
  let currency = Currency.code account.currency in
  match Service.role_account ctx.service account.owner Uncategorized kind ~currency with
  | Some info -> Ok info
  | None -> (
      let key = Result.get_ok (Idempotency_key.of_string ("seed:uncategorized-" ^ Account.kind_to_string kind ^ "-" ^ currency)) in
      let name = if kind = Account.Expense then "Uncategorized" else "Uncategorized income" in
      match
        Service.open_account ctx.service ctx.access ~key ~owner:account.owner ~kind ~name:(name ^ " (" ^ currency ^ ")") ~currency
          ~flags:Account.no_flags ~details:{ Account_info.no_details with role = Some Uncategorized }
      with
      | Ok (`Created info | `Exists info) -> Ok info
      | Error e -> Error e)

let event_for ctx (account : Account.t) (p : planned) =
  match p.status with
  | Duplicate _ -> Ok None
  | Matched pending ->
      Ok (Some ({ Ledger_event.key = p.key; linked = false; body = Post_pending { pending_id = pending.id; amount = None; value_date = p.row.booking_date } }, None))
  | New ->
      let out = Int64.compare p.row.amount 0L < 0 in
      let* target =
        match p.suggestion with
        | Some s when s.auto_apply && not s.shared -> Ok (s.category_id, `Categorized)
        | _ ->
            let* info = uncategorized ctx account (if out then Expense else Income) in
            Ok (info.account.id, `Inbox info.account.id)
      in
      let category, inbox = target in
      let amount = Option.get (Amount.of_int64 (Int64.abs p.row.amount)) in
      let debit, credit = if out then (category, account.id) else (account.id, category) in
      let tags = match p.suggestion with Some s when s.auto_apply -> s.tags | _ -> [] in
      let event : Ledger_event.t =
        { key = p.key; linked = false;
          body =
            Transfer
              { debit_account_id = debit; credit_account_id = credit; amount; phase = Posted; code = (if out then Purchase else Income);
                memo = { description = p.row.description; counterparty = p.row.counterparty; tags }; value_date = p.row.booking_date } }
      in
      Ok (Some (event, match inbox with `Inbox current -> Some current | `Categorized -> None))

let confirm ctx request =
  let db = ctx.service.db in
  let now = ctx.service.now () in
  try
    Sql.transaction db (fun () ->
        let fail e = raise (Abort e) in
        let p = match preview ctx request with Ok p -> p | Error e -> fail e in
        match p.already_imported with
        | Some batch -> Ok (`Already_imported batch)
        | None ->
            let d = p.decoded_file in
            let account = d.account.account in
            let planned_events =
              List.filter_map
                (fun planned -> match event_for ctx account planned with Ok (Some e) -> Some (planned, e) | Ok None -> None | Error e -> fail e)
                p.rows
            in
            let results =
              match planned_events with
              | [] -> []
              | _ -> (
                  match Service.submit ctx.service ctx.access ~scope:account.owner (List.map (fun (_, (e, _)) -> e) planned_events) with
                  | Ok results -> results
                  | Error e -> fail (e :> error))
            in
            let posted =
              List.map2
                (fun (planned, (_, inbox)) result ->
                  match result with
                  | Ok (Ledger_core.Created (v : Service.transfer_view) | Exists v) -> (planned, v.transfer.id, inbox)
                  | Error e ->
                      fail (`Invalid (Printf.sprintf "row %d (%s): %s" (planned.index + 1) planned.row.description (Ledger_error.transfer_to_string e))))
                planned_events results
            in
            let count f = List.length (List.filter f p.rows) in
            let dates = List.map (fun (r : Statement_format.row) -> r.booking_date) d.rows in
            let batch : Import_store.batch =
              { batch_id = Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()); account_id = account.id;
                file_sha256 = d.sha256; file_name = request.file_name; format = d.format; row_count = List.length p.rows;
                new_count = count (fun r -> r.status = New); duplicate_count = count (fun r -> match r.status with Duplicate _ -> true | _ -> false);
                matched_count = count (fun r -> match r.status with Matched _ -> true | _ -> false);
                first_date = List.fold_left (fun a d -> Some (match a with None -> d | Some a -> Date.min a d)) None dates;
                last_date = List.fold_left (fun a d -> Some (match a with None -> d | Some a -> Date.max a d)) None dates;
                created_at = now; undone = false }
            in
            let user_id = (Access.user ctx.access).id in
            Import_store.save_batch db ~user_id batch;
            List.iter
              (fun planned ->
                let transfer_id, status =
                  match planned.status with
                  | Duplicate id -> (Some id, "duplicate")
                  | Matched _ | New ->
                      ( Option.map (fun (_, id, _) -> id) (List.find_opt (fun (q, _, _) -> q.index = planned.index) posted),
                        if planned.status = New then "new" else "matched" )
                in
                Import_store.save_row db ~batch_id:batch.batch_id ~index:planned.index ~fingerprint:(Idempotency_key.to_string planned.key) ~transfer_id ~status)
              p.rows;
            let inbox_count = ref 0 in
            List.iter
              (fun (planned, transfer_id, inbox) ->
                match inbox with
                | None -> ()
                | Some current ->
                    incr inbox_count;
                    Import_store.add_item db ~now
                      { transfer_id; bank_account_id = account.id; current_account_id = current; revision = 0;
                        suggested_account_id = Option.map (fun (s : Suggest.suggestion) -> s.category_id) planned.suggestion;
                        suggestion_source = Option.map (fun (s : Suggest.suggestion) -> Suggest.source_to_string s.source) planned.suggestion;
                        suggestion_confidence = Option.map (fun (s : Suggest.suggestion) -> s.confidence) planned.suggestion;
                        candidate_pending_id = Option.map (fun (t : Transfer.t) -> t.id) planned.near_pending;
                        merchant_key = Suggest.merchant_key planned.row; status = "open" })
              posted;
            if not d.account.details.statement_backed then
              Account_store.save_details db account.id { d.account.details with statement_backed = true };
            let balance =
              Option.map
                (fun (b : balance_check) ->
                  let ledger_balance = Import_store.posted_balance_on db account b.on_date in
                  Planning_store.add_checkpoint db ~now ~user_id ~import_batch_id:batch.batch_id ~source:"import"
                    { checkpoint_id = Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()); account_id = account.id;
                      on_date = b.on_date; statement_balance = b.statement_balance; ledger_balance; source = "import" };
                  { b with ledger_balance })
                p.balance
            in
            Ok (`Imported { batch; inbox_count = !inbox_count; balance }))
  with Abort e -> Error e

(* Undoing an import reverses everything it posted, including categorisations made since. *)
let undo ctx ~batch_id =
  let db = ctx.service.db in
  match Import_store.find_batch db batch_id with
  | None -> Error (`Not_found "import")
  | Some batch when batch.undone -> Ok batch
  | Some batch -> (
      let* account = Service.find_account ctx.service ctx.access batch.account_id in
      let scope = account.account.owner in
      let view = Store_sqlite.view db in
      let transfers = Import_store.batch_transfers db batch_id in
      let categorisations =
        List.concat_map
          (fun id ->
            match Import_store.find_item db id with
            | None -> []
            | Some item ->
                List.filter_map
                  (fun revision ->
                    let key = Result.get_ok (Idempotency_key.of_string (Printf.sprintf "categorize:%s:%d" (Transfer_id.to_string id) revision)) in
                    Option.map (fun (t : Transfer.t) -> t.id) (view.transfer_by_key scope key))
                  (List.init item.revision (fun i -> i + 1)))
          transfers
      in
      let events =
        List.map
          (fun id ->
            { Ledger_event.key = Result.get_ok (Idempotency_key.of_string ("undo:" ^ Transfer_id.to_string id)); linked = false;
              body = Reverse { transfer_id = id; value_date = None; memo = None } })
          (List.rev categorisations @ transfers)
      in
      try
        Sql.transaction db (fun () ->
            (match events with
            | [] -> ()
            | _ -> (
                match Service.submit ctx.service ctx.access ~scope events with
                | Ok results ->
                    List.iter
                      (function
                        | Ok _ | Error `Transfer_already_reversed -> ()
                        | Error e -> raise (Abort (`Invalid ("undo failed: " ^ Ledger_error.transfer_to_string e))))
                      results
                | Error e -> raise (Abort (e :> error))));
            let now = ctx.service.now () in
            List.iter
              (fun id -> Option.iter (fun item -> Import_store.save_item db ~now { item with status = "dismissed" }) (Import_store.find_item db id))
              transfers;
            Import_store.undo_batch db ~now batch_id;
            Ok (Option.get (Import_store.find_batch db batch_id)))
      with Abort e -> Error e)

(* The suggester for an access: the owner's rules first, then categorisation history, then an
   optional extra (the LLM). *)
let default_suggester ?(extra = Suggest.none) (service : Service.t) access : Suggest.t =
  let rules = List.concat_map (Import_store.rules service.db) (Access.owners access) in
  let compiled = Suggest.of_rules rules in
  let history =
    Suggest.of_history (fun merchant ->
        List.find_map (fun owner -> Import_store.history service.db owner merchant) (Access.owners access))
  in
  Suggest.(compiled <|> history <|> extra)

let context ?extra service access = { service; access; suggester = default_suggester ?extra service access }
