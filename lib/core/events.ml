(* Things in the statements that need a person, rather than figures about them: a salary or bill that
   should have arrived by now and hasn't, a charge that looks taken twice, the biggest purchase in half a
   year, a yearly bill coming up. Each says what, how much and when, so the card can say so in kronor
   and dates and offer what to do; anything less certain stays quiet.

   Only what the statements cover is judged: a payment expected after [covered_through] is not late,
   it just isn't imported yet. *)

type t =
  | Missing of { series : Series.t; expected : Date.t }
      (** a regular income or bill expected (with a few days' grace) before the statements end, not there *)
  | Double_charge of { merchant : string; amount : int64; first : Transfer.t; second : Transfer.t }
  | Large_purchase of { purchase : Transfer.t; amount : int64; largest_before : int64 }
      (** at least twice the largest single purchase of the half year before it *)
  | Yearly_bill of { name : string; amount : int64; due : Date.t; last_paid : Date.t }
      (** a bill paid about a year apart at least twice, due again within a month *)

(* How late a payment may be before it counts as missing: banks book a few days either way. *)
let grace = function Analytics.Weekly -> 2 | Monthly -> 4 | Quarterly -> 7 | Yearly -> 14

let interval = function Analytics.Weekly -> 7 | Monthly -> 30 | Quarterly -> 91 | Yearly -> 365

(* Late but within one interval: a series more than an interval overdue has stopped (the forecast leaves
   it out), which is not news to act on today. Only series seen at least three times are trusted. *)
let missing ~covered_through (series : Series.t list) =
  List.filter_map
    (fun (s : Series.t) ->
      match s.kind with
      | Transfer -> None
      | Bill | Income ->
          let due = Date.add_days s.next_expected (grace s.cadence) in
          if s.occurrences >= 3 && Date.compare due covered_through < 0
             && Date.days_between ~first:s.next_expected ~last:covered_through <= interval s.cadence
          then Some (Missing { series = s; expected = s.next_expected })
          else None)
    series

(* Money out of the scope to a category. *)
let purchase book (t : Transfer.t) =
  Analytics.posted t && t.code <> Reclassify && t.code <> Reversal && Analytics.is Expense (book.Analytics.inside t.debit_account_id)

(* The same merchant and amount a day or less apart, at least [floor]: twice where it usually happens once.
   A merchant with three or more such charges in the window is a habit (lunch, a fare), not a mistake. *)
let double_charges book ~since ~floor transfers =
  let charges = List.filter (fun (t : Transfer.t) -> purchase book t && Date.compare t.value_date since >= 0) transfers in
  let groups = Hashtbl.create 32 in
  List.iter
    (fun (t : Transfer.t) ->
      let key = (Analytics.merchant_key t, Analytics.amount t) in
      if fst key <> "" && Int64.compare (snd key) floor >= 0 then Hashtbl.replace groups key (t :: Option.value (Hashtbl.find_opt groups key) ~default:[]))
    charges;
  Hashtbl.fold
    (fun (merchant, amount) ts acc ->
      match List.sort (fun (a : Transfer.t) (b : Transfer.t) -> Date.compare a.value_date b.value_date) ts with
      | [ a; b ] when Date.days_between ~first:a.value_date ~last:b.value_date <= 1 -> Double_charge { merchant; amount; first = a; second = b } :: acc
      | _ -> acc)
    groups []

(* The single biggest purchase since [since], when it is at least twice anything bought in the half year
   before it and at least [floor]: a regular bill (rent) is never news, so merchants with a series are left
   out, and so is a charge the same merchant took about a year before for about the same (a yearly bill).
   Nothing else bought in that half year is nothing to compare with, not a reason. One at most. *)
let large_purchase book ~since ~floor ~(series : Series.t list) transfers =
  let all = List.filter (purchase book) transfers in
  let yearly (t : Transfer.t) =
    let key = Analytics.merchant_key t and a = Analytics.amount t in
    List.exists
      (fun (p : Transfer.t) ->
        let b = Analytics.amount p in
        Analytics.merchant_key p = key && abs (Date.days_between ~first:p.value_date ~last:t.value_date - 365) <= 21
        && Int64.compare (Int64.mul (Int64.abs (Int64.sub a b)) 4L) (Int64.max a b) <= 0)
      all
  in
  let regular t = let key = Analytics.merchant_key t in List.exists (fun (s : Series.t) -> s.id = "bill:" ^ key) series || yearly t in
  let purchases = List.filter (fun t -> not (regular t)) all in
  let recent = List.filter (fun (t : Transfer.t) -> Date.compare t.value_date since >= 0) purchases in
  match List.sort (fun a b -> Int64.compare (Analytics.amount b) (Analytics.amount a)) recent with
  | [] -> []
  | biggest :: _ ->
      let window = Date.add_days biggest.value_date (-182) in
      let within (t : Transfer.t) = Date.compare t.value_date window >= 0 && Date.compare t.value_date biggest.value_date < 0 in
      let largest_before = List.fold_left (fun m t -> if within t then Int64.max m (Analytics.amount t) else m) 0L purchases in
      let amount = Analytics.amount biggest in
      (* "Biggest in half a year" needs most of half a year of spending before it, bills included. *)
      let earliest = List.fold_left (fun d (t : Transfer.t) -> if within t then Date.min d t.value_date else d) biggest.value_date all in
      let history = Date.days_between ~first:earliest ~last:biggest.value_date >= 120 in
      if history && Int64.compare largest_before 0L > 0 && Int64.compare amount floor >= 0 && Int64.compare amount (Int64.mul 2L largest_before) >= 0 then
        [ Large_purchase { purchase = biggest; amount; largest_before } ]
      else []

(* Yearly bills (insurance, car tax, a membership) come too rarely for the three occurrences a series
   needs, so two are enough here: the same merchant about a year apart (within ten days) for about the same
   amount (within a quarter), next due a year after the last, within the coming month. *)
let yearly_bills book ~today transfers =
  let groups = Hashtbl.create 32 in
  List.iter
    (fun (t : Transfer.t) ->
      if purchase book t then
        match Analytics.merchant_key t with "" -> () | key -> Hashtbl.replace groups key (t :: Option.value (Hashtbl.find_opt groups key) ~default:[]))
    transfers;
  Hashtbl.fold
    (fun _ charges acc ->
      match List.sort (fun (a : Transfer.t) (b : Transfer.t) -> Date.compare b.value_date a.value_date) charges with
      | last :: before :: _ ->
          let apart = Date.days_between ~first:before.value_date ~last:last.value_date in
          let a = Analytics.amount last and b = Analytics.amount before in
          let similar = Int64.compare (Int64.mul (Int64.abs (Int64.sub a b)) 4L) (Int64.max a b) <= 0 in
          let due = Analytics.add_months last.value_date 12 in
          (* Only these two: a merchant charged in between is not a yearly bill. *)
          let only_these = List.for_all (fun (t : Transfer.t) -> Date.compare t.value_date before.value_date <= 0 || t == last) charges in
          if abs (apart - 365) <= 10 && similar && only_these && Date.compare due today >= 0 && Date.days_between ~first:today ~last:due <= 31 then
            Yearly_bill { name = last.memo.description; amount = a; due; last_paid = last.value_date } :: acc
          else acc
      | _ -> acc)
    groups []

(* The amount at stake, for ranking: what a person would lose or need to find. *)
let stake = function
  | Missing { series; _ } -> Int64.abs series.amount
  | Double_charge { amount; _ } -> amount
  | Large_purchase { amount; _ } -> amount
  | Yearly_bill { amount; _ } -> amount

(* [floor] is the smallest amount worth a card (100 in the currency, at its scale). *)
let detect book ~today ~covered_through ~floor ~(series : Series.t list) transfers =
  let covered = Option.value covered_through ~default:today in
  let since = Date.add_days covered (-30) in
  List.concat
    [ missing ~covered_through:covered series;
      double_charges book ~since ~floor transfers;
      large_purchase book ~since ~floor:(Int64.mul floor 10L) ~series transfers;
      yearly_bills book ~today transfers ]
