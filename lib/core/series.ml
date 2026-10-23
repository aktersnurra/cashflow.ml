type kind = Bill | Income | Transfer

type t = {
  id : string;
  kind : kind;
  name : string;
  cadence : Analytics.cadence;
  account : Account_id.t;
  counter : Account_id.t;
  amount : int64;
  last_amount : int64;
  category : Account_info.t option;
  first_date : Date.t;
  last_date : Date.t;
  next_expected : Date.t;
  occurrences : int;
}

let kind_to_string = function Bill -> "bill" | Income -> "income" | Transfer -> "transfer"

let name_of (t : Transfer.t) = String.trim (match t.memo.counterparty with Some c when String.trim c <> "" -> c | _ -> t.memo.description)
let last payments = List.nth payments (List.length payments - 1)

let group key transfers =
  let groups = Hashtbl.create 16 in
  List.iter (fun t -> match key t with Some k -> Hashtbl.replace groups k (t :: Option.value (Hashtbl.find_opt groups k) ~default:[]) | None -> ()) transfers;
  Hashtbl.fold (fun k payments acc -> (k, payments) :: acc) groups []

(* A bill is expected at its usual amount, except after a price change: once the last two
   payments agree, that is the price now. *)
let bills book ~cash transfers =
  Analytics.recurring_payments book transfers
  |> List.filter (fun (r : Analytics.recurring) -> cash r.paid_from)
  |> List.map (fun (r : Analytics.recurring) ->
         { id = "bill:" ^ r.merchant; kind = Bill; name = r.merchant_name; cadence = r.cadence; account = r.paid_from;
           counter = (match r.category with Some c -> c.account.id | None -> r.paid_from);
           amount = Int64.neg (if Int64.equal r.last_amount r.previous_amount then r.last_amount else r.typical_amount);
           last_amount = Int64.neg r.last_amount; category = r.category; first_date = r.first_date; last_date = r.last_date;
           next_expected = r.next_expected; occurrences = r.occurrences })

(* Salary is paid on payday, which follows the pay cycle's banking-day rule rather than the same
   date each month: a monthly income that last came within 3 days of a cycle start comes next on
   the following cycle start. *)
let next_income ~setting cadence date =
  let near first = abs (Date.days_between ~first ~last:date) <= 3 in
  let cycle = Cycle.cycle_of setting date in
  let following = Cycle.next setting cycle in
  match cadence with
  | Analytics.Monthly when near cycle.first -> following.first
  | Monthly when near following.first -> (Cycle.next setting following).first
  | _ -> Analytics.next_after cadence date

let incomes book ~cash ~setting transfers =
  transfers
  |> List.filter (fun (t : Transfer.t) -> cash t.debit_account_id && Analytics.is Income (book.Analytics.inside t.credit_account_id))
  |> group (fun t -> match Analytics.merchant_key t with "" -> None | key -> Some key)
  |> List.filter_map (fun (merchant, payments) ->
         Option.map
           (fun (r : Analytics.regular) ->
             let latest = last r.payments in
             { id = "income:" ^ merchant; kind = Income; name = name_of latest; cadence = r.regular_cadence; account = latest.debit_account_id;
               counter = latest.credit_account_id; amount = r.typical; last_amount = Analytics.amount latest;
               category = book.Analytics.inside latest.credit_account_id; first_date = (List.hd r.payments).value_date; last_date = latest.value_date;
               next_expected = next_income ~setting r.regular_cadence latest.value_date; occurrences = List.length r.payments })
           (Analytics.regular ~steady:true payments))

(* Money between a cash account and an account that is neither a category nor itself paid for
   by a category: another of the scope's accounts, or one outside it (the household's). A card
   bill or a loan payment follows what was spent or owed, so only its timing must be regular,
   and the next one is expected to be the average of the last three. *)
let transfers_between book ~cash transfers =
  let category id = Analytics.is Expense (book.Analytics.inside id) || Analytics.is Income (book.Analytics.inside id) in
  transfers
  |> List.filter (fun (t : Transfer.t) ->
         (cash t.debit_account_id || cash t.credit_account_id) && not (category t.debit_account_id || category t.credit_account_id))
  |> group (fun (t : Transfer.t) -> Some (t.credit_account_id, t.debit_account_id))
  |> List.filter_map (fun ((from, to_), payments) ->
         let to_debt = Analytics.is Liability (book.Analytics.inside to_) in
         Option.map
           (fun (r : Analytics.regular) ->
             let latest = last r.payments in
             let leaves = cash from in
             let sign value = if leaves then Int64.neg value else value in
             let expected =
               if not to_debt then r.typical
               else
                 let recent = List.filteri (fun i _ -> i >= List.length r.payments - 3) r.payments in
                 Int64.div (List.fold_left (fun sum t -> Int64.add sum (Analytics.amount t)) 0L recent) (Int64.of_int (List.length recent))
             in
             { id = Printf.sprintf "transfer:%s:%s" (Account_id.to_string from) (Account_id.to_string to_); kind = Transfer; name = name_of latest;
               cadence = r.regular_cadence; account = (if leaves then from else to_); counter = (if leaves then to_ else from);
               amount = sign expected; last_amount = sign (Analytics.amount latest); category = None;
               first_date = (List.hd r.payments).value_date; last_date = latest.value_date; next_expected = r.next;
               occurrences = List.length r.payments })
           (Analytics.regular ~steady:(not to_debt) payments))

let detect book ~cash ~setting transfers =
  let transfers = List.filter (fun (t : Transfer.t) -> Analytics.posted t && t.code <> Reclassify && t.code <> Reversal) transfers in
  bills book ~cash transfers @ incomes book ~cash ~setting transfers @ transfers_between book ~cash transfers
  |> List.sort (fun a b -> String.compare a.id b.id)
