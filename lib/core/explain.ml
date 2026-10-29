(* Why a finished cycle came out as it did, set beside the cycles before it: "normal", or the few things
   that made the difference, each with its kroner and what kind of thing it was. A yearly bill (insurance,
   car tax) is expected and says so; a single big purchase is a one-off, not a change in habits; what is
   left over, spread across a category's purchases, is a habit, and how many cycles running it has been up
   says whether it is a blip or a drift. That split, not the total, is what tells a person whether to do
   anything.

   Only complete cycles are explained (the caller sees to it); the comparison is with the average of up to
   six earlier cycles that had income, and needs at least two of them. *)

let ( +: ) = Int64.add
let ( -: ) = Int64.sub

type driver =
  | Yearly of { category : Account_info.t; description : string; amount : int64; date : Date.t }
      (** charged about a year before too, for about the same: expected *)
  | One_off of { category : Account_info.t; description : string; amount : int64; date : Date.t }
      (** a single purchase from a merchant the earlier cycles didn't have *)
  | Habit of { category : Account_info.t; difference : int64; cycles_up : int }
      (** more than usual across the category's purchases; [cycles_up] cycles running, this one included *)
  | Less of { category : Account_info.t; difference : int64 }  (** less than usual; [difference] is positive *)
  | Income_change of { difference : int64 }  (** income above (positive) or below usual *)

type t = {
  cycle : Cycle.t;
  spending : int64;
  usual_spending : int64;
  income : int64;
  usual_income : int64;
  normal : bool;  (** spending within the threshold of usual, and income too *)
  drivers : driver list;  (** the largest few, largest first *)
  compared_with : int;  (** how many earlier cycles *)
}

let amount_of = function
  | Yearly { amount; _ } | One_off { amount; _ } -> amount
  | Habit { difference; _ } | Less { difference; _ } -> difference
  | Income_change { difference } -> Int64.abs difference

let purchases book ~first ~last transfers =
  List.filter
    (fun (t : Transfer.t) ->
      Analytics.posted t && t.code <> Reclassify && t.code <> Reversal
      && Date.compare t.value_date first >= 0 && Date.compare t.value_date last <= 0
      && Analytics.is Expense (book.Analytics.inside t.debit_account_id))
    transfers

(* The purchases that ended up in category [id]: charged to it directly, or filed into it from the inbox
   (a reclassification from where the import put it), less those filed out of it again. *)
let in_category ~(id : Account_id.t) ~first ~last transfers =
  let within (t : Transfer.t) = Analytics.posted t && t.code <> Reversal && Date.compare t.value_date first >= 0 && Date.compare t.value_date last <= 0 in
  let key (t : Transfer.t) = (t.value_date, Analytics.amount t, t.memo.description) in
  let moved_out = Hashtbl.create 8 in
  List.iter
    (fun (t : Transfer.t) -> if within t && t.code = Reclassify && Account_id.equal t.credit_account_id id then Hashtbl.add moved_out (key t) ())
    transfers;
  List.filter
    (fun (t : Transfer.t) ->
      within t && Account_id.equal t.debit_account_id id
      && (match Hashtbl.find_opt moved_out (key t) with Some () -> Hashtbl.remove moved_out (key t); false | None -> true))
    transfers

let total_by_category book ~first ~last transfers =
  Analytics.totals_by_account book Expense ~first ~last transfers |> List.map (fun ((i : Account_info.t), n) -> (i.account.id, (i, n)))

(* [floor]: the smallest difference worth naming (300 in the currency, at its scale). [transfers] must reach
   back about thirteen months before the cycle, so a yearly bill can be recognised. *)
let explain book ~setting ~floor ~(cycle : Cycle.t) transfers =
  let rec earlier (c : Cycle.t) k acc = if k = 0 then List.rev acc else let p = Cycle.previous setting c in earlier p (k - 1) (p :: acc) in
  let flows (c : Cycle.t) = Analytics.flows_between book ~first:c.first ~last:c.last transfers in
  let before = List.filter (fun c -> Int64.compare (flows c).income 0L > 0) (earlier cycle 6 []) in
  match before with
  | [] | [ _ ] -> None
  | _ ->
      let n = Int64.of_int (List.length before) in
      let avg f = Int64.div (List.fold_left (fun sum c -> sum +: f c) 0L before) n in
      let now = flows cycle in
      let usual_spending = avg (fun c -> (flows c).spending) and usual_income = avg (fun c -> (flows c).income) in
      let threshold = Int64.max floor (Int64.div usual_spending 20L) in
      let current = total_by_category book ~first:cycle.first ~last:cycle.last transfers in
      let earlier_totals = List.map (fun (c : Cycle.t) -> (c, total_by_category book ~first:c.first ~last:c.last transfers)) before in
      let total_in totals id = match List.assoc_opt id totals with Some (_, n) -> n | None -> 0L in
      let usual_of id = Int64.div (List.fold_left (fun sum (_, totals) -> sum +: total_in totals id) 0L earlier_totals) n in
      let categories =
        List.sort_uniq (fun (a : Account_info.t) (b : Account_info.t) -> Account_id.compare a.account.id b.account.id)
          (List.map (fun (_, (i, _)) -> i) current @ List.concat_map (fun (_, totals) -> List.map (fun (_, (i, _)) -> i) totals) earlier_totals)
      in
      let earliest = (List.nth before (List.length before - 1)).first in
      let seen_before = purchases book ~first:earliest ~last:(Date.add_days cycle.first (-1)) transfers in
      let known merchant = List.exists (fun t -> Analytics.merchant_key t = merchant) seen_before in
      (* About a year before (within three weeks), for about the same (within a quarter). *)
      let a_year_before (t : Transfer.t) =
        let merchant = Analytics.merchant_key t and a = Analytics.amount t in
        List.exists
          (fun (p : Transfer.t) ->
            let apart = Date.days_between ~first:p.value_date ~last:t.value_date in
            let b = Analytics.amount p in
            Analytics.merchant_key p = merchant && abs (apart - 365) <= 21
            && Int64.compare (Int64.mul (Int64.abs (a -: b)) 4L) (Int64.max a b) <= 0)
          (purchases book ~first:(Date.add_days cycle.first (-400)) ~last:(Date.add_days cycle.first (-1)) transfers)
      in
      (* How many cycles running, ending with this one, the category was above its usual. *)
      let cycles_up (id : Account_id.t) usual =
        let rec go = function
          | (_, totals) :: rest when Int64.compare (total_in totals id) usual > 0 -> 1 + go rest
          | _ -> 0
        in
        1 + go earlier_totals
      in
      let drivers =
        List.concat_map
          (fun (category : Account_info.t) ->
            let id = category.account.id in
            let difference = total_in current id -: usual_of id in
            if Int64.compare difference threshold >= 0 then
              let mine = in_category ~id ~first:cycle.first ~last:cycle.last transfers in
              let specific =
                List.filter_map
                  (fun (t : Transfer.t) ->
                    let amount = Analytics.amount t and description = t.memo.description and date = t.value_date in
                    if Int64.compare amount (Int64.div threshold 2L) < 0 then None
                    else if a_year_before t then Some (Yearly { category; description; amount; date })
                    else if not (known (Analytics.merchant_key t)) then Some (One_off { category; description; amount; date })
                    else None)
                  mine
              in
              let explained = List.fold_left (fun sum d -> sum +: amount_of d) 0L specific in
              let rest = difference -: explained in
              specific @ if Int64.compare rest threshold >= 0 then [ Habit { category; difference = rest; cycles_up = cycles_up id (usual_of id) } ] else []
            else if Int64.compare (Int64.neg difference) threshold >= 0 then [ Less { category; difference = Int64.neg difference } ]
            else [])
          categories
      in
      let income_difference = now.income -: usual_income in
      let drivers = if Int64.compare (Int64.abs income_difference) threshold >= 0 then Income_change { difference = income_difference } :: drivers else drivers in
      let drivers = List.sort (fun a b -> Int64.compare (amount_of b) (amount_of a)) drivers |> List.filteri (fun i _ -> i < 3) in
      Some
        { cycle; spending = now.spending; usual_spending; income = now.income; usual_income;
          normal = Int64.compare (Int64.abs (now.spending -: usual_spending)) threshold < 0 && Int64.compare (Int64.abs income_difference) threshold < 0;
          drivers; compared_with = List.length before }
