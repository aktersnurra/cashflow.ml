(* Importing a bank statement, as a pipeline of stages composed with Kleisli composition:

     decode >=> fingerprint >=> classify >=> suggest >=> suggest_with_llm >=> promote >=> check_balance

   Each stage takes the previous one's result and either refines it or stops with an error, so a
   stage can be tested on its own, and a bank sync can reuse everything after [decode]. Running
   the pipeline is the preview; [confirm] runs it again inside a transaction and posts the result. *)

module Access = Cashflow_core.Access
module Account = Cashflow_core.Account
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Amount = Cashflow_core.Amount
module Chart = Cashflow_core.Chart
module Csv = Cashflow_core.Csv
module Currency = Cashflow_core.Currency
module Date = Cashflow_core.Date
module Idempotency_key = Cashflow_core.Idempotency_key
module Ledger_core = Cashflow_core.Ledger_core
module Ledger_error = Cashflow_core.Ledger_error
module Ledger_event = Cashflow_core.Ledger_event
module Owner = Cashflow_core.Owner
module Statement_format = Cashflow_core.Statement_format
module Suggest = Cashflow_core.Suggest
module Transfer = Cashflow_core.Transfer
module Transfer_id = Cashflow_core.Transfer_id
module Xlsx = Cashflow_core.Xlsx

let ( let* ) = Result.bind
let ( >=> ) f g x = Result.bind (f x) g

type error = [ Service.error | `Unreadable of string | `Logo of string * string ]

let error_to_string : [< error ] -> string = function
  | `Unreadable reason -> "the file couldn't be read: " ^ reason
  | `Logo (_, reason) -> reason
  | #Service.error as e -> Service.error_to_string e

(* What the user said about a row the data can't decide (§6.3). *)
type decision = Is_duplicate | Is_new

(* A bank account or card the import opens when the statement is for one that isn't here yet. Nothing
   is created by a preview; [confirm] opens it and posts the rows in one transaction. *)
type new_account = { name : string; kind : Account.kind; owner : Owner.t; institution : string option; number_last4 : string option }

type target = Existing of Account_id.t | Create of new_account

type request = {
  target : target;
  file_name : string option;
  content : string;
  decisions : (int * decision) list;  (** by row index, for ambiguous rows *)
}

type ctx = { service : Service.t; access : Access.t; suggester : Suggest.t }

type decoded = {
  ctx : ctx;
  request : request;
  account : Account_info.t;  (** for a [Create] target, a draft that isn't in the ledger *)
  format : string;
  identity : Statement_format.identity;
  sha256 : string;
  rows : Statement_format.row list;
}

type keyed_row = {
  index : int;
  row : Statement_format.row;
  base : string;  (** everything that identifies the row except its occurrence *)
  occurrence : int;  (** among identical rows that day in this file *)
  positional : bool;  (** no running balance or reference, so only the occurrence tells identical rows apart *)
  complete_day : bool;  (** strictly inside the file's dates, so the file holds all of that day's rows *)
}

type keyed = { decoded : decoded; keyed_rows : keyed_row list }

type status =
  | New
  | Duplicate of Transfer_id.t
  | Matched of Transfer.t  (** confirms a pending manual entry with the same date and amount *)
  | Ambiguous of Transfer_id.t list
      (** on an edge day, identical to rows already imported: the data can't say whether it is one of them *)
  | Decided_duplicate of Transfer_id.t  (** an ambiguous row the user said is already imported *)
  | Confirms_leg of Transfer.t
      (** the other side of a manual move between two statement-backed accounts, whose first
          statement already posted it *)

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
  opening : (Date.t * int64) option;
      (** for an account with nothing before the file: the balance the bank's running balances say
          it had the day before the first row, to set as its opening balance *)
}

let max_file_bytes = 5 * 1024 * 1024

let read_table content = if Xlsx.is_xlsx content then Result.map_error (fun e -> `Unreadable e) (Xlsx.table content) else Ok (Csv.parse content)

(* The statement itself, out of the zip a bank may have put it in. *)
let unwrap content =
  if String.length content > max_file_bytes then Error (`Invalid "the file is larger than 5 MB")
  else Result.map_error (fun e -> `Unreadable e) (Xlsx.unzip_statement ~max_bytes:max_file_bytes content)

(* A format cashflow knows, or else the layout the assistant reads from the file, if there is one. *)
let read_format ctx content =
  let* content = unwrap content in
  if String.length content > max_file_bytes then Error (`Invalid "the file is larger than 5 MB")
  else
    let* table = read_table content in
    match Statement_format.detect table with
    | Some format -> Ok (table, format)
    | None -> (
        match Llm_features.statement_layout ctx.service ctx.access table with
        | Ok layout -> Ok (table, Statement_format.of_layout layout)
        | Error None -> Error (`Unreadable "no header row with a date and an amount column was found")
        | Error (Some reason) -> Error (`Unreadable ("it isn't a format cashflow knows, and the assistant couldn't read it either: " ^ reason)))

(* The account a [Create] target will be, for the preview: an id that matches nothing, so the
   preview finds no duplicates and no pending entries, as is right for an account that is new. *)
let draft_account ctx (n : new_account) =
  let* () = Service.may_open ctx.access n.owner n.kind in
  if n.kind <> Asset && n.kind <> Liability then Error (`Invalid "statements import into bank accounts or cards")
  else if String.trim n.name = "" then Error (`Invalid "the new account needs a name")
  else
    let* currency = Service.currency ctx.service (Access.user ctx.access).default_currency in
    let* number_last4 = Account_info.validate_last4 n.number_last4 in
    let account : Account.t =
      { id = Id_generator.account (); key = Result.get_ok (Idempotency_key.of_string "import:draft"); owner = n.owner; kind = n.kind; currency;
        name = String.trim n.name; flags = Account.no_flags; closed = false; balances = Account.zero_balances; created_at = ctx.service.now () }
    in
    Ok { Account_info.account; details = { Account_info.no_details with institution = n.institution; number_last4; statement_backed = true } }

(* Stage 1: read the file and find its format. *)
let decode ctx (request : request) =
  let* table, (module F : Statement_format.S) = read_format ctx request.content in
  let* account =
    match request.target with
    | Existing id ->
        let* account = Service.find_account ctx.service ctx.access id in
        if account.account.kind <> Asset && account.account.kind <> Liability then Error (`Invalid "statements import into bank accounts or cards") else Ok account
    | Create n -> draft_account ctx n
  in
  let identity = F.identity table in
  match identity.last4, account.details.number_last4 with
  | Some a, Some b when a <> b ->
      Error (`Invalid (Printf.sprintf "this statement is for account ****%s, but %s is ****%s" a account.account.name b))
  | _ -> (
      match F.parse ~scale:(Currency.scale account.account.currency) table with
      | Error reason -> Error (`Unreadable reason)
      | Ok [] -> Error (`Unreadable "the file has no transactions")
      | Ok rows ->
          (* Of the statement, not its zip, so the same statement downloaded twice is recognised. *)
          let* statement = unwrap request.content in
          Ok { ctx; request; account; format = F.name; identity; sha256 = Digestif.SHA256.(to_hex (digest_string statement)); rows })

(* What a file says about its account, and which of the user's bank accounts and cards it could go
   into: [matches] are those whose last four digits are the file's. *)
type identified = { format : string; identity : Statement_format.identity; matches : Account_info.t list; candidates : Account_info.t list }

let identify ctx content =
  let* table, (module F : Statement_format.S) = read_format ctx content in
  let* () = match F.parse ~scale:2 table with Error reason -> Error (`Unreadable reason) | Ok [] -> Error (`Unreadable "the file has no transactions") | Ok _ -> Ok () in
  let identity = F.identity table in
  let candidates =
    List.filter
      (fun (i : Account_info.t) -> (not i.account.closed) && (i.account.kind = Asset || i.account.kind = Liability) && Option.is_none i.details.role)
      (Service.accounts ctx.service ctx.access)
  in
  let matches =
    match identity.last4 with
    | None -> []
    | Some last4 -> List.filter (fun (i : Account_info.t) -> i.details.number_last4 = Some last4) candidates
  in
  Ok { format = F.name; identity; matches; candidates }

let normalize_text s = String.lowercase_ascii s |> String.split_on_char ' ' |> List.filter (( <> ) "") |> String.concat " "

(* Stage 2: what identifies each row (§6.3): the bank's reference or the running balance when the
   format has one, otherwise only the row's position among identical rows that day, which holds
   across files only on days a file covers in full. *)
let key_of base occurrence = Result.get_ok (Idempotency_key.of_string ("import:" ^ Digestif.SHA256.(to_hex (digest_string (base ^ "\x00" ^ string_of_int occurrence)))))

let fingerprint (d : decoded) =
  let dates = List.map (fun (r : Statement_format.row) -> r.booking_date) d.rows in
  let first = List.fold_left Date.min (List.hd dates) dates and last = List.fold_left Date.max (List.hd dates) dates in
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
        { index; row; base; occurrence = n; positional = Option.is_none row.balance && Option.is_none row.reference;
          complete_day = Date.compare first row.booking_date < 0 && Date.compare row.booking_date last < 0 })
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

(* Stage 3: duplicates, pending manual entries the bank now confirms (§6.7: the same account,
   direction, purchase date and amount), and rows only the user can place. *)
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
  (* The transfers already imported for a positional identity: occurrences 0, 1, ... in use. *)
  let imported = Hashtbl.create 8 in
  let already_imported base =
    match Hashtbl.find_opt imported base with
    | Some ids -> ids
    | None ->
        let rec go n acc = match usable_key view account.owner (key_of base n) 0 with `Exists (_, (t : Transfer.t)) -> go (n + 1) (t.id :: acc) | `Free _ -> List.rev acc in
        let ids = go 0 [] in
        Hashtbl.replace imported base ids;
        ids
  in
  let decided = Hashtbl.create 8 in
  let next_decided base decision =
    let n = Option.value (Hashtbl.find_opt decided (base, decision)) ~default:0 in
    Hashtbl.replace decided (base, decision) (n + 1);
    n
  in
  let posts = Store_sqlite.unconfirmed_posts_on d.ctx.service.db account.id in
  let new_or_matched (row : Statement_format.row) key =
    let purchase_date = Option.value row.transaction_date ~default:row.booking_date in
    let same_day (t : Transfer.t) = Date.equal t.value_date purchase_date && direction_matches account.id row t && not (Hashtbl.mem claimed t.id) in
    let same_amount (t : Transfer.t) = Int64.equal (Amount.to_int64 t.amount) (Int64.abs row.amount) in
    (* A manual entry already posted by the statement of the account at its other end: entered
       on the same day, or posted on it. *)
    let entered_same_day (post : Transfer.t) =
      same_day post
      || (match post.kind with Post_pending { pending_id } -> Option.fold ~none:false ~some:(fun (m : Transfer.t) -> same_day { post with value_date = m.value_date }) (view.transfer pending_id) | _ -> false)
    in
    match List.filter (fun t -> same_day t && same_amount t) pendings with
    | [ pending ] -> Hashtbl.replace claimed pending.id (); (key, Matched pending)
    | _ -> (
        match List.filter (fun t -> entered_same_day t && same_amount t) posts with
        | [ post ] -> Hashtbl.replace claimed post.id (); (key, Confirms_leg post)
        | _ -> (key, New))
  in
  let free key = match usable_key view account.owner key 0 with `Exists (key, (t : Transfer.t)) -> (key, Duplicate t.id) | `Free key -> (key, New) in
  let rows =
    List.map
      (fun (r : keyed_row) ->
        let key, status =
          match r.positional && not r.complete_day, already_imported r.base with
          | true, (_ :: _ as existing) -> (
              (* An edge day may be cut off, so the occurrence doesn't say which of these it is. *)
              match List.assoc_opt r.index d.request.decisions with
              | None -> (key_of r.base r.occurrence, Ambiguous existing)
              | Some Is_duplicate ->
                  let j = next_decided r.base Is_duplicate in
                  (key_of r.base r.occurrence, Decided_duplicate (List.nth existing (min j (List.length existing - 1))))
              | Some Is_new -> (
                  (* The next free occurrence, so a later complete-day export lines up. *)
                  match free (key_of r.base (List.length existing + next_decided r.base Is_new)) with
                  | key, New -> new_or_matched r.row key
                  | other -> other))
          | _ -> ( match free (key_of r.base r.occurrence) with key, New -> new_or_matched r.row key | other -> other)
        in
        let near_pending =
          let purchase_date = Option.value r.row.transaction_date ~default:r.row.booking_date in
          match status with
          | New ->
              List.find_opt
                (fun (t : Transfer.t) ->
                  Date.equal t.value_date purchase_date && direction_matches account.id r.row t && not (Hashtbl.mem claimed t.id)
                  && not (Int64.equal (Amount.to_int64 t.amount) (Int64.abs r.row.amount)))
                pendings
          | Duplicate _ | Matched _ | Ambiguous _ | Decided_duplicate _ | Confirms_leg _ -> None
        in
        { index = r.index; row = r.row; key; status; near_pending; suggestion = None })
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
        | Duplicate _ | Matched _ | Ambiguous _ | Decided_duplicate _ | Confirms_leg _ -> planned)
      rows
  in
  Ok (d, rows)

(* Stage 4b: rows nobody's rules or history recognised go to the LLM in one batch, if one is
   configured; its suggestions are only ever suggestions. [confirm] asks before it takes the
   database and passes the answer in, so no transaction waits on the network. *)
let suggest_with_llm ?answered (d, rows) =
  let unknown = List.filter_map (fun p -> match p.status, p.suggestion with New, None -> Some (p.index, p.row) | _ -> None) rows in
  let suggestions =
    match answered with Some suggestions -> suggestions | None -> Llm_features.suggest_categories d.ctx.service d.ctx.access ~bank_account:d.account unknown
  in
  Ok (d, List.map (fun p -> match List.assoc_opt p.index suggestions with Some s when p.suggestion = None && p.status = New -> { p with suggestion = Some s } | _ -> p) rows)

(* Stage 4c: the bank account may have opted in to applying confident history and LLM suggestions. Read
   inside [confirm]'s transaction like the rest of the plan, so a setting changed meanwhile is honoured. *)
let promote (d, rows) =
  let bank = d.account.account in
  let threshold = Import_store.auto_apply_threshold d.ctx.service.db bank.id in
  (* History and the LLM look across all of an owner's categories, so a suggestion may belong to another owner or
     currency than the bank account; the inbox refuses such a category, and so must a row filed without asking. *)
  let fits (s : Suggest.suggestion) =
    match Service.find_account d.ctx.service d.ctx.access s.category_id with
    | Ok category -> Cashflow_core.Owner.equal category.account.owner bank.owner && String.equal (Currency.code category.account.currency) (Currency.code bank.currency)
    | Error _ -> false
  in
  let promote (s : Suggest.suggestion) = Suggest.promote ~threshold:(if fits s then threshold else None) s in
  Ok (d, List.map (fun p -> { p with suggestion = Option.map promote p.suggestion }) rows)

(* The balance after the file's last row. Banks list rows newest or oldest first; the running
   balances say which (each one is the previous plus the row's amount, in time order), and when
   they can't, the dates do. *)
let rec in_time_order = function
  | (a : Statement_format.row) :: (b :: _ as rest) -> (
      match a.balance, b.balance with Some x, Some y -> Int64.equal y (Int64.add x b.amount) && in_time_order rest | _ -> false)
  | [ _ ] | [] -> true

(* The first row in time, when the running balances say which end that is. *)
let oldest_row (rows : Statement_format.row list) =
  match rows, List.rev rows with
  | first :: _ :: _, last :: _ -> if in_time_order rows then Some first else if in_time_order (List.rev rows) then Some last else None
  | [ only ], _ -> Some only
  | _ -> None

let closing_balance (rows : Statement_format.row list) =
  match rows, List.rev rows with
  | first :: _, last :: _ ->
      let closing =
        if List.length rows > 1 && in_time_order rows then last
        else if List.length rows > 1 && in_time_order (List.rev rows) then first
        else if Date.compare first.booking_date last.booking_date >= 0 then first
        else last
      in
      Option.map (fun balance -> (closing.booking_date, balance)) closing.balance
  | _ -> None

(* Stage 5: will the ledger agree with the bank once the new rows are posted? *)
(* Stage 5: will the ledger agree with the bank once the new rows are posted? The ledger is
   compared at the end of the statement's closing day, so when it will hold more of that day than
   the file does (a later file already brought in the rest of the day), there is no fair point to
   compare and no check is made. *)
let check_balance (d, rows) =
  let db = d.ctx.service.db and account = d.account.account in
  let already_imported = Import_store.find_active_batch db ~account_id:account.id ~sha:d.sha256 in
  let posting p = match p.status with New | Matched _ -> true | Duplicate _ | Decided_duplicate _ | Ambiguous _ | Confirms_leg _ -> false in
  let balance =
    Option.bind (closing_balance d.rows) (fun (on_date, statement_balance) ->
        let on_day p = Date.equal p.row.booking_date on_date in
        let ledger_rows = Import_store.posted_count_on db account on_date + List.length (List.filter (fun p -> on_day p && posting p) rows) in
        if ledger_rows > List.length (List.filter on_day rows) then None
        else
          let existing = Import_store.posted_balance_on db account on_date in
          let sign = if Account.debit_normal account.kind then 1L else -1L in
          let added =
            List.fold_left
              (fun sum p -> if posting p && Date.compare p.row.booking_date on_date <= 0 then Int64.add sum (Int64.mul sign p.row.amount) else sum)
              0L rows
          in
          Some { on_date; statement_balance = Int64.mul sign statement_balance; ledger_balance = Int64.add existing added })
  in
  let opening_of_rows =
    match oldest_row d.rows with
    | Some ({ balance = Some after; _ } as first) ->
        let day_before = Date.add_days first.booking_date (-1) in
        (* The bank writes a card's debt as negative; the ledger counts it in the account's own
           direction, where a liability's debt is positive. *)
        let sign = if Account.debit_normal account.kind then 1L else -1L in
        let opening = Int64.mul sign (Int64.sub after first.amount) in
        if Int64.equal opening 0L || not (Int64.equal (Import_store.posted_balance_on db account day_before) 0L) then None else Some (day_before, opening)
    | _ -> None
  in
  let opening = opening_of_rows in
  (* A new account is opened with that balance, so the check counts it already. *)
  let balance =
    match d.request.target, opening, balance with
    | Create _, Some (_, b), Some check -> Some { check with ledger_balance = Int64.add check.ledger_balance b }
    | _ -> balance
  in
  Ok { decoded_file = d; rows; already_imported; balance; opening }

let pipeline ?answered ctx = decode ctx >=> fingerprint >=> classify >=> suggest >=> suggest_with_llm ?answered >=> promote >=> check_balance
let preview ctx request = pipeline ctx request

type summary = { account : Account_info.t; batch : Import_store.batch; inbox_count : int; balance : balance_check option }

exception Abort of error

(* The account an imported row is posted against until someone categorises it. *)
let uncategorized ctx (account : Account.t) kind =
  let currency = Currency.code account.currency in
  match Service.role_account ctx.service account.owner Uncategorized kind ~currency with
  | Some info -> Ok info
  | None -> (
      let key = Result.get_ok (Idempotency_key.of_string ("seed:uncategorized-" ^ Account.kind_to_string kind ^ "-" ^ currency)) in
      let slug = if kind = Account.Expense then "uncategorized-expense" else "uncategorized-income" in
      let name = Starter.name ~locale:"en" { entry = List.find (fun (e : Chart.entry) -> e.slug = slug) Chart.entries; currency = Some currency } in
      match
        Service.open_account ctx.service ctx.access ~key ~owner:account.owner ~kind ~name ~currency
          ~flags:Account.no_flags ~details:{ Account_info.no_details with role = Some Uncategorized }
      with
      | Ok (`Created info | `Exists info) -> Ok info
      | Error e -> Error e)

let event_for ctx (account : Account.t) (p : planned) =
  match p.status with
  | Duplicate _ | Decided_duplicate _ | Ambiguous _ | Confirms_leg _ -> Ok None
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

let entry_of (post : Transfer.t) = match post.kind with Post_pending { pending_id } -> Some pending_id | _ -> None

(* The LLM is asked first, outside the transaction; inside it the plan is worked out again
   against the database as it is then, reusing those suggestions. *)
let confirm ctx request =
  let db = ctx.service.db in
  let now = ctx.service.now () in
  let* outside = preview ctx request in
  let answered = List.filter_map (fun p -> match p.suggestion with Some ({ Suggest.source = Llm; _ } as s) -> Some (p.index, s) | _ -> None) outside.rows in
  try
    let result = Sql.transaction db (fun () ->
        let fail e = raise (Abort e) in
        let request =
          match request.target with
          | Existing _ -> request
          | Create n ->
              let key = Result.get_ok (Idempotency_key.of_string ("import-account:" ^ outside.decoded_file.sha256)) in
              let details = outside.decoded_file.account.details in
              let info =
                match
                  Service.open_account ctx.service ctx.access ~key ~owner:n.owner ~kind:n.kind ~name:(String.trim n.name)
                    ~currency:(Currency.code outside.decoded_file.account.account.currency) ~flags:Account.no_flags ~details
                with
                | Ok (`Created info | `Exists info) -> info
                | Error e -> fail (e :> error)
              in
              (match outside.opening with
              | Some (value_date, balance) -> (
                  match Service.set_opening_balance ctx.service ctx.access ~account_id:info.account.id ~balance ~value_date with
                  | Ok (Ok _) -> ()
                  | Ok (Error e) -> fail (`Invalid ("opening balance: " ^ Ledger_error.transfer_to_string e))
                  | Error e -> fail (e :> error))
              | None -> ());
              { request with target = Existing info.account.id }
        in
        let p = match pipeline ~answered ctx request with Ok p -> p | Error e -> fail e in
        match p.already_imported with
        | Some batch -> Ok (`Already_imported batch)
        | None ->
            (match List.filter (fun r -> match r.status with Ambiguous _ -> true | _ -> false) p.rows with
            | [] -> ()
            | undecided ->
                fail
                  (`Invalid
                    (Printf.sprintf "%s %s %s already imported (same day, text and amount); decide whether %s already imported or new"
                       (match undecided with [ _ ] -> "row" | _ -> "rows")
                       (String.concat ", " (List.map (fun r -> string_of_int (r.index + 1)) undecided))
                       (match undecided with [ _ ] -> "looks like one" | _ -> "look like ones")
                       (match undecided with [ _ ] -> "it is" | _ -> "each is"))));
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
                new_count = count (fun r -> r.status = New); duplicate_count = count (fun r -> match r.status with Duplicate _ | Decided_duplicate _ -> true | _ -> false);
                matched_count = count (fun r -> match r.status with Matched _ | Confirms_leg _ -> true | _ -> false);
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
                  | Decided_duplicate id -> (Some id, "decided_duplicate")
                  | Confirms_leg post ->
                      Option.iter (fun entry -> Import_store.confirm_leg db ~entry ~account:account.id ~batch_id:batch.batch_id) (entry_of post);
                      (Some post.id, "confirmed_leg")
                  | Ambiguous _ -> assert false (* refused before anything is posted *)
                  | Matched pending ->
                      Import_store.confirm_leg db ~entry:pending.id ~account:account.id ~batch_id:batch.batch_id;
                      ( Option.map (fun (_, id, _) -> id) (List.find_opt (fun (q, _, _) -> q.index = planned.index) posted), "matched" )
                  | New ->
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
            (* The file teaches the account what it is, so the next one finds it by itself. *)
            let details = d.account.details in
            let learned =
              { details with
                statement_backed = true;
                number_last4 = (match details.number_last4 with Some _ as known -> known | None -> d.identity.last4);
                institution = (match details.institution with Some _ as known -> known | None -> d.identity.institution) }
            in
            if learned <> details then Account_store.save_details db account.id learned;
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
            Ok (`Imported { account = { d.account with details = learned }; batch; inbox_count = !inbox_count; balance })) in
    (match result with
     | Ok (`Imported _) when Option.is_some ctx.service.assistant && Option.is_some ctx.service.logo_network ->
         let owner = outside.decoded_file.account.account.owner in
         let names = List.map (fun (p : planned) ->
           let name = match p.row.counterparty with Some s when String.trim s <> "" -> s | _ -> p.row.description in
           Suggest.merchant_key p.row, name) outside.rows in
         (try ctx.service.schedule (fun () ->
           try Merchant_logos.resolve ctx.service ctx.access ~owner names
           with exn -> Logs.err (fun m -> m "logo resolution failed: %s" (Printexc.to_string exn)))
          with exn -> Logs.err (fun m -> m "logo scheduling failed: %s" (Printexc.to_string exn)))
     | _ -> ());
    result
  with Abort e -> Error e

(* Undoing an import reverses everything it posted, and everything triage posted about its rows
   since (categorisations, both sides of a shared cost, pairings), each in the scope it was
   posted in. A row paired with one from another import goes back to that inbox. *)
let undo ctx ~batch_id =
  let db = ctx.service.db in
  match Import_store.find_batch db batch_id with
  | None -> Error (`Not_found "import")
  | Some batch when batch.undone -> Ok batch
  | Some batch when Option.is_some (Import_store.dependent db batch_id) ->
      (* Undoing it would take rows out of the ledger that a later import still counts as there,
         and re-uploading that file would be refused as already imported. *)
      let later = Option.get (Import_store.dependent db batch_id) in
      Error
        (`Invalid
          (Printf.sprintf "a later import (%s) overlaps this one; undo that first"
             (Option.value later.file_name ~default:(Ptime.to_rfc3339 ~tz_offset_s:0 later.created_at))))
  | Some batch -> (
      let* _ = Service.find_account ctx.service ctx.access batch.account_id in
      let view = Store_sqlite.view db in
      let transfers = Import_store.batch_transfers db batch_id in
      let triage = List.sort_uniq Transfer_id.compare (List.concat_map (Import_store.triage_transfers db) transfers) in
      let partners =
        List.concat_map (Import_store.triaged_items db) triage |> List.filter (fun item -> not (List.exists (Transfer_id.equal item) transfers))
      in
      (* Latest first, so a categorisation is undone before the row it moved. *)
      let to_reverse = List.filter_map view.transfer (List.rev triage @ transfers) in
      let reverse (t : Transfer.t) =
        { Ledger_event.key = Result.get_ok (Idempotency_key.of_string ("undo:" ^ Transfer_id.to_string t.id)); linked = false;
          body = Reverse { transfer_id = t.id; value_date = None; memo = None } }
      in
      try
        Sql.transaction db (fun () ->
            List.iter
              (fun (t : Transfer.t) ->
                match Service.submit ctx.service ctx.access ~scope:t.scope [ reverse t ] with
                | Ok [ (Ok _ | Error `Transfer_already_reversed) ] -> ()
                | Ok [ Error e ] -> raise (Abort (`Invalid ("undo failed: " ^ Ledger_error.transfer_to_string e)))
                | Ok _ -> raise (Abort (`Invalid "undo failed"))
                | Error e -> raise (Abort (e :> error)))
              to_reverse;
            let now = ctx.service.now () in
            (* Only a partner still paired goes back to the inbox: one whose own import was undone
               already is dismissed and stays so. *)
            List.iter
              (fun id ->
                Option.iter
                  (fun (item : Import_store.item) -> if item.status = "paired" then Import_store.save_item db ~now { item with status = "open" })
                  (Import_store.find_item db id))
              partners;
            List.iter
              (fun id -> Option.iter (fun item -> Import_store.save_item db ~now { item with status = "dismissed" }) (Import_store.find_item db id))
              transfers;
            Import_store.forget_legs db ~batch_id;
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
