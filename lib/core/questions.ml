(* The questions an accountant would answer before being asked (design/superpowers/specs/
   2026-10-29-accountant-briefing-design.md). Each family turns the books into an answer with exact
   figures and four of the five factors that say whether it is worth a person's attention:

   - material: the money at stake in a year against the person's own yearly income (full at 5%);
   - surprising: beyond their usual swing, or new;
   - reliable: enough history behind it;
   - actionable: something they could change, or that changes a plan.

   The fifth, new, is what the person said about it before, which the caller knows. The score is the
   product, so a question near zero on any factor is not worth asking. Only complete cycles (the
   statements reach their end) are read, so an irregular upload never makes a month look cheap. *)

let ( +: ) = Int64.add
let ( -: ) = Int64.sub

type family = Last_month | Where_it_goes | Regular_bills | Drift | Leak | Fees | Normal_month | Real_savings | Season_ahead | Payday_week

let family_name = function
  | Last_month -> "last_month"
  | Where_it_goes -> "where_it_goes"
  | Regular_bills -> "regular_bills"
  | Drift -> "drift"
  | Leak -> "leak"
  | Fees -> "fees"
  | Normal_month -> "normal_month"
  | Real_savings -> "real_savings"
  | Season_ahead -> "season_ahead"
  | Payday_week -> "payday_week"

(* The first part of every key of the family. *)
let key_prefix = function
  | Last_month -> "month"
  | Where_it_goes -> "where"
  | Regular_bills -> "bills"
  | Drift -> "drift"
  | Leak -> "leak"
  | Fees -> "fees"
  | Normal_month -> "normal"
  | Real_savings -> "savings"
  | Season_ahead -> "season"
  | Payday_week -> "payday"

let families = [ Last_month; Where_it_goes; Regular_bills; Drift; Leak; Fees; Normal_month; Real_savings; Season_ahead; Payday_week ]
let family_of_name name = List.find_opt (fun f -> family_name f = name) families
let family_of_key key = match String.split_on_char ':' key with prefix :: _ -> List.find_opt (fun f -> key_prefix f = prefix) families | [] -> None

type bill = { bill_name : string; yearly : int64 }

(* Amounts are per cycle unless the name says otherwise; all positive except where signed. *)
type answer =
  | Last_month of Explain.t  (** why the last complete cycle differed from the ones before it *)
  | Where_it_goes of { fixed : int64; flexible : int64; one_off : int64; top : (Account_info.t * int64) list; cycles : int }
      (** per cycle: regular bills, everything else, single large purchases; the biggest flexible categories *)
  | Regular_bills of { yearly : int64; count : int; biggest : bill list; fresh : bill list; raised : (bill * int64) list }
      (** every regular bill as a yearly total; new ones; price rises (with the yearly increase) *)
  | Drift of { category : Account_info.t; usual : int64; recent : int64; cycles : int }
      (** above its earlier level [cycles] cycles running *)
  | Leak of { name : string; count : int; total : int64; months : int; category : Account_info.t option }
      (** many small purchases at one merchant over the last [months] months *)
  | Fees of { total : int64; months : int }  (** paid in fees and interest over [months] months *)
  | Normal_month of { baseline : int64; liquid : int64; months_covered : float }
      (** a cycle's spending without one-offs, and how many such cycles the cash would cover *)
  | Real_savings of { usual : int64; with_one_offs : int64; one_offs : int64; cycles : int }
      (** savings in a cycle without one-offs, what the totals show, and the one-offs between them *)
  | Season_ahead of { cycle : Cycle.t; last_year : int64; usual : int64 }
      (** the coming cycle cost this much last year, against a usual one *)
  | Payday_week of { first_week : int64; rest : int64; cycles : int }  (** flexible spending per day: the payday week, the rest *)

type question = {
  answer : answer;
  family : family;
  key : string;  (** stable while the answer means the same; a material change makes a new key *)
  stake : int64;  (** a year's money behind it *)
  material : float;
  surprising : float;
  reliable : float;
  actionable : float;
}

let score q = q.material *. q.surprising *. q.reliable *. q.actionable

type input = {
  book : Analytics.book;
  setting : Pay_cycle_setting.t;
  today : Date.t;
  covered_through : Date.t option;  (** where the statements end; [None] reads as today *)
  transfers : Transfer.t list;  (** about thirteen months, for the season *)
  series : Series.t list;
  liquid : int64;  (** cash on hand *)
  fee_categories : Account_id.t list;
  floor : int64;  (** the smallest amount worth naming (100 in the currency) *)
}

let clamp x = Float.max 0. (Float.min 1. x)
let fdiv a b = if Int64.equal b 0L then 0. else Int64.to_float a /. Int64.to_float b
let avg = function [] -> 0L | xs -> Int64.div (List.fold_left ( +: ) 0L xs) (Int64.of_int (List.length xs))

(* Buckets a quarter apart, so a key changes when an amount moves materially, not on every upload. *)
let bucket x = if Int64.compare x 0L <= 0 then 0 else int_of_float (Float.log (Int64.to_float x) /. Float.log 1.25)

(* A purchase: posted money to a category, as it ended up — filed from the inbox (a reclassification
   from where the import put it) counts where it was filed. *)
type purchase = { t : Transfer.t; amount : int64; category : Account_info.t option; merchant : string }

let purchases (input : input) =
  let book = input.book in
  let key (t : Transfer.t) = (t.value_date, Analytics.amount t, t.memo.description) in
  let moves = Hashtbl.create 64 in
  List.iter
    (fun (t : Transfer.t) ->
      if Analytics.posted t && t.code = Reclassify && Analytics.is Expense (book.inside t.debit_account_id) then
        Hashtbl.replace moves (key t) (t.debit_account_id :: Option.value (Hashtbl.find_opt moves (key t)) ~default:[]))
    input.transfers;
  List.filter_map
    (fun (t : Transfer.t) ->
      if Analytics.posted t && t.code <> Reclassify && t.code <> Reversal && Analytics.is Expense (book.inside t.debit_account_id) then
        let final = match Hashtbl.find_opt moves (key t) with Some (latest :: _) -> latest | _ -> t.debit_account_id in
        let category = match book.inside final with Some i when i.details.role = None -> Some i | _ -> None in
        Some { t; amount = Analytics.amount t; category; merchant = Analytics.merchant_key t }
      else None)
    input.transfers

let within ~first ~last (p : purchase) = Date.compare p.t.value_date first >= 0 && Date.compare p.t.value_date last <= 0

(* Up to [n] complete cycles, newest first: a cycle is complete when the statements reach its end. *)
let complete_cycles (input : input) n =
  let covered = Date.min input.today (Option.value input.covered_through ~default:input.today) in
  let rec go (c : Cycle.t) k acc = if k = 0 then List.rev acc else go (Cycle.previous input.setting c) (k - 1) (c :: acc) in
  let current = Cycle.cycle_of input.setting covered in
  let last_complete = if Date.equal current.last covered then current else Cycle.previous input.setting current in
  go last_complete n []

let has_income (input : input) (c : Cycle.t) = Int64.compare (Analytics.flows_between input.book ~first:c.first ~last:c.last input.transfers).income 0L > 0

let per_year = function Analytics.Weekly -> 52L | Monthly -> 12L | Quarterly -> 4L | Yearly -> 1L

(* Bills: regular payments a month or more apart. A weekly shop at the same supermarket is regular too, but
   it is how a person spends, not a bill. *)
let bill (s : Series.t) = s.kind = Bill && s.cadence <> Weekly

let regular_merchants (input : input) =
  List.filter_map (fun (s : Series.t) -> if bill s then Some (String.sub s.id 5 (String.length s.id - 5)) else None) input.series

(* A single purchase large against a cycle's spending, at a merchant not bought from in the half year
   before: not how the person usually spends. *)
let one_off ~(all : purchase list) ~regular ~threshold (p : purchase) =
  Int64.compare p.amount threshold >= 0
  && (not (List.mem p.merchant regular))
  && not
       (List.exists
          (fun (q : purchase) ->
            q.merchant = p.merchant && q.t != p.t && Date.compare q.t.value_date p.t.value_date < 0
            && Date.days_between ~first:q.t.value_date ~last:p.t.value_date <= 182)
          all)

let ask (input : input) =
  let cycles = List.filter (has_income input) (complete_cycles input 12) in
  let recent = List.filteri (fun i _ -> i < 6) cycles in
  let all = purchases input in
  let regular = regular_merchants input in
  match recent with
  | [] -> []
  | _ ->
      let flows c = Analytics.flows_between input.book ~first:c.Cycle.first ~last:c.last input.transfers in
      let income = avg (List.map (fun c -> (flows c).income) recent) in
      let spending = avg (List.map (fun c -> (flows c).spending) recent) in
      (* What a year's money is measured against: income, or spending when there is little income. *)
      let yearly = Int64.mul 12L (Int64.max income spending) in
      let material stake = clamp (fdiv stake (Int64.div yearly 20L)) in
      let one_off_threshold = Int64.max (Int64.mul input.floor 10L) (Int64.div spending 10L) in
      let is_one_off = one_off ~all ~regular ~threshold:one_off_threshold in
      let in_cycle (c : Cycle.t) = List.filter (within ~first:c.first ~last:c.last) all in
      let n = List.length recent in
      let nf = Int64.of_int n in
      let questions = ref [] in
      let add q = questions := q :: !questions in
      (* 0. Why the last cycle came out as it did, while it is recent: what made the difference, as
         the drivers say (a yearly bill is expected; a one-off is not a habit). An ordinary month is not news. *)
      (let last = List.hd recent in
       if Date.days_between ~first:last.last ~last:input.today <= 40 then
         match Explain.explain input.book ~setting:input.setting ~floor:(Int64.mul input.floor 3L) ~cycle:last input.transfers with
         | Some e when not e.normal ->
             let stake = Int64.mul (Int64.abs (e.spending -: e.usual_spending)) 12L in
             let habits = List.exists (function Explain.Habit _ | Less _ -> true | _ -> false) e.drivers in
             add
               { answer = Last_month e; family = Last_month; key = "month:" ^ Cycle.label last; stake; material = material stake;
                 surprising = 0.9; reliable = clamp (float_of_int e.compared_with /. 4.); actionable = (if habits then 0.8 else 0.6) }
         | _ -> ());
      (* 1. Where it goes *)
      (let fixed, one_offs, flexible =
         List.fold_left
           (fun (f, o, x) c ->
             List.fold_left
               (fun (f, o, x) p -> if List.mem p.merchant regular then (f +: p.amount, o, x) else if is_one_off p then (f, o +: p.amount, x) else (f, o, x +: p.amount))
               (f, o, x) (in_cycle c))
           (0L, 0L, 0L) recent
       in
       let by_category = Hashtbl.create 16 in
       List.iter
         (fun c ->
           List.iter
             (fun p ->
               match p.category with
               | Some cat when not (List.mem p.merchant regular || is_one_off p) ->
                   let id = cat.account.id in
                   Hashtbl.replace by_category id (cat, p.amount +: Option.fold ~none:0L ~some:snd (Hashtbl.find_opt by_category id))
               | _ -> ())
             (in_cycle c))
         recent;
       let top =
         Hashtbl.fold (fun _ (cat, total) acc -> (cat, Int64.div total nf) :: acc) by_category []
         |> List.sort (fun (_, a) (_, b) -> Int64.compare b a)
         |> List.filteri (fun i _ -> i < 3)
       in
       let flexible = Int64.div flexible nf in
       if n >= 2 && Int64.compare flexible 0L > 0 then
         add
           { answer = Where_it_goes { fixed = Int64.div fixed nf; flexible; one_off = Int64.div one_offs nf; top; cycles = n };
             family = Where_it_goes;
             key = Printf.sprintf "where:%s:%d" (String.concat "," (List.map (fun ((c : Account_info.t), _) -> Account_id.to_string c.account.id) top)) (bucket flexible);
             stake = Int64.mul flexible 12L; material = material (Int64.mul flexible 12L); surprising = 0.8; reliable = clamp (float_of_int n /. 3.);
             actionable = 0.7 });
      (* 2. Regular bills *)
      (let bills =
         List.filter_map
           (fun (s : Series.t) ->
             if bill s then Some (s, { bill_name = s.name; yearly = Int64.mul (Int64.abs s.amount) (per_year s.cadence) }) else None)
           input.series
       in
       let yearly_total = List.fold_left (fun sum (_, b) -> sum +: b.yearly) 0L bills in
       let fresh = List.filter_map (fun ((s : Series.t), b) -> if Date.days_between ~first:s.first_date ~last:input.today <= 100 then Some b else None) bills in
       (* A price rise: the latest payment against the median of those before the last two months (a series
          takes the latest price as its amount, so it can't say). *)
       let raised =
         List.filter_map
           (fun ((s : Series.t), b) ->
             let merchant = String.sub s.id 5 (String.length s.id - 5) in
             let earlier = List.filter (fun p -> p.merchant = merchant && Date.days_between ~first:p.t.value_date ~last:s.last_date > 45) all in
             let typical = match earlier with [] -> Int64.abs s.last_amount | _ -> Analytics.median Int64.compare (List.map (fun p -> p.amount) earlier) in
             let last = Int64.abs s.last_amount in
             let rise = last -: typical in
             if Int64.compare (Int64.mul rise 20L) typical >= 0 && Int64.compare rise 0L > 0 then Some (b, Int64.mul rise (per_year s.cadence)) else None)
           bills
       in
       (* Small bills are the ones a person can cancel; rent is not. *)
       let small = List.filter (fun (_, b) -> Int64.compare (Int64.mul b.yearly 33L) yearly <= 0) bills in
       let biggest = List.map snd bills |> List.sort (fun a b -> Int64.compare b.yearly a.yearly) |> List.filteri (fun i _ -> i < 3) in
       let changed = fresh <> [] || raised <> [] in
       (* What could change: the small bills (cancellable), and what is new or dearer. *)
       let stake =
         List.fold_left (fun s (_, b) -> s +: b.yearly) 0L small
         +: List.fold_left (fun s b -> s +: b.yearly) 0L fresh
         +: List.fold_left (fun s (_, r) -> s +: r) 0L raised
       in
       if bills <> [] then
         add
           { answer = Regular_bills { yearly = yearly_total; count = List.length bills; biggest; fresh; raised };
             family = Regular_bills;
             key =
               Printf.sprintf "bills:%d:%d:%s" (List.length bills) (bucket yearly_total)
                 (String.concat "," (List.map (fun b -> b.bill_name) fresh @ List.map (fun (b, _) -> b.bill_name) raised));
             stake; material = material stake; surprising = (if changed then 0.9 else 0.6); reliable = 0.9;
             actionable = (if changed || small <> [] then 0.8 else 0.3) });
      (* 3. Drift: per category, cycles oldest first *)
      (let chronological = List.rev cycles in
       let categories =
         List.sort_uniq (fun (a : Account_info.t) (b : Account_info.t) -> Account_id.compare a.account.id b.account.id)
           (List.filter_map (fun p -> p.category) all)
       in
       List.iter
         (fun (cat : Account_info.t) ->
           let totals =
             List.map
               (fun c -> List.fold_left (fun s p -> match p.category with Some k when Account_id.equal k.account.id cat.account.id && not (is_one_off p) -> s +: p.amount | _ -> s) 0L (in_cycle c))
               chronological
           in
           (* The run: the latest cycles above the level before them. *)
           let rec run k =
             if k >= List.length totals - 3 then k
             else
               let before = List.filteri (fun i _ -> i < List.length totals - k - 1) totals in
               let level = avg before in
               let x = List.nth totals (List.length totals - k - 1) in
               if Int64.compare (Int64.mul x 100L) (Int64.mul level 115L) > 0 && Int64.compare (x -: level) input.floor > 0 then run (k + 1) else k
           in
           let k = run 0 in
           if k >= 3 then
             let before = List.filteri (fun i _ -> i < List.length totals - k) totals in
             let latest = List.filteri (fun i _ -> i >= List.length totals - k) totals in
             let usual = avg before and now = avg latest in
             let spread =
               let m = Int64.to_float usual in
               Float.sqrt (List.fold_left (fun s x -> let d = Int64.to_float x -. m in s +. (d *. d)) 0. before /. float_of_int (List.length before))
             in
             let stake = Int64.mul (now -: usual) 12L in
             let mostly_bills =
               let in_cat = List.filter (fun p -> match p.category with Some c -> Account_id.equal c.account.id cat.account.id | None -> false) all in
               let billed = List.filter (fun p -> List.mem p.merchant regular) in_cat in
               Int64.compare (Int64.mul (List.fold_left (fun s p -> s +: p.amount) 0L billed) 2L) (List.fold_left (fun s p -> s +: p.amount) 0L in_cat) > 0
             in
             add
               { answer = Drift { category = cat; usual; recent = now; cycles = k };
                 family = Drift;
                 key = Printf.sprintf "drift:%s:%s" (Account_id.to_string cat.account.id) (Cycle.label (List.nth cycles (k - 1)));
                 stake; material = material stake;
                 surprising = clamp (Int64.to_float (now -: usual) /. Float.max spread (0.1 *. Int64.to_float usual) /. 3.);
                 reliable = clamp (float_of_int (k + List.length before) /. 8.);
                 actionable = (if mostly_bills then 0.4 else 0.9) })
         categories);
      (* 4. Leaks: the last half year the statements cover *)
      (let last = (List.hd recent).last in
       let first = Date.add_days last (-182) in
       let window = List.filter (within ~first ~last) all in
       let small = Int64.max (Int64.mul input.floor 3L) (Int64.div income 100L) in
       let groups = Hashtbl.create 32 in
       List.iter
         (fun p -> if p.merchant <> "" && not (List.mem p.merchant regular) then Hashtbl.replace groups p.merchant (p :: Option.value (Hashtbl.find_opt groups p.merchant) ~default:[]))
         window;
       let months = max 1 (Date.days_between ~first ~last / 30) in
       Hashtbl.iter
         (fun merchant ps ->
           let count = List.length ps in
           let median = Analytics.median Int64.compare (List.map (fun p -> p.amount) ps) in
           let total = List.fold_left (fun s p -> s +: p.amount) 0L ps in
           if count >= 8 && Int64.compare median small <= 0 then
             let latest = List.fold_left (fun (a : purchase) (b : purchase) -> if Date.compare b.t.value_date a.t.value_date > 0 then b else a) (List.hd ps) ps in
             let stake = Int64.div (Int64.mul total 12L) (Int64.of_int months) in
             add
               { answer = Leak { name = latest.t.memo.description; count; total; months; category = latest.category };
                 family = Leak;
                 key = Printf.sprintf "leak:%s:%d" merchant (bucket total);
                 stake; material = material stake; surprising = clamp (float_of_int count /. 20.); reliable = clamp (float_of_int months /. 4.);
                 actionable = 0.9 })
         groups);
      (* 5. Fees and interest over the cycles read *)
      (let paid = List.filter (fun p -> match p.category with Some c -> List.exists (Account_id.equal c.account.id) input.fee_categories | None -> false) all in
       let oldest = List.nth cycles (List.length cycles - 1) in
       let paid = List.filter (within ~first:oldest.first ~last:(List.hd cycles).last) paid in
       let total = List.fold_left (fun s p -> s +: p.amount) 0L paid in
       let months = List.length cycles in
       if Int64.compare total input.floor >= 0 then
         let stake = Int64.div (Int64.mul total 12L) (Int64.of_int months) in
         add
           { answer = Fees { total; months }; family = Fees; key = Printf.sprintf "fees:%d" (bucket stake); stake; material = material stake;
             surprising = 0.7; reliable = clamp (float_of_int months /. 6.); actionable = 0.6 });
      (* 6 and 7. A normal cycle, and what is really saved *)
      (let one_offs = Int64.div (List.fold_left (fun s c -> List.fold_left (fun s p -> if is_one_off p then s +: p.amount else s) s (in_cycle c)) 0L recent) nf in
       let baseline = spending -: one_offs in
       if n >= 2 && Int64.compare baseline 0L > 0 then (
         let months_covered = fdiv input.liquid baseline in
         add
           { answer = Normal_month { baseline; liquid = input.liquid; months_covered };
             family = Normal_month;
             key = Printf.sprintf "normal:%d:%d" (bucket baseline) (int_of_float months_covered);
             stake = Int64.mul baseline 12L;
             material = (if months_covered < 1. then 1. else material (Int64.mul baseline 12L) *. 0.5);
             surprising = (if months_covered < 1. then 1. else if months_covered < 3. then 0.6 else 0.3);
             reliable = clamp (float_of_int n /. 4.); actionable = 0.5 };
         let with_one_offs = avg (List.map (fun c -> Analytics.savings (flows c)) recent) in
         let stake = Int64.mul one_offs 12L in
         add
           { answer = Real_savings { usual = with_one_offs +: one_offs; with_one_offs; one_offs; cycles = n };
             family = Real_savings;
             key = Printf.sprintf "savings:%d:%d" (bucket (with_one_offs +: one_offs)) (bucket one_offs);
             stake; material = material stake;
             surprising = clamp (fdiv one_offs (Int64.max (Int64.abs with_one_offs) (Int64.div income 10L)));
             reliable = clamp (float_of_int n /. 4.); actionable = 0.6 }));
      (* 8. The coming cycle, last year *)
      (let next = Cycle.next input.setting (Cycle.cycle_of input.setting input.today) in
       let rec back c k = if k = 0 then c else back (Cycle.previous input.setting c) (k - 1) in
       let last_year = back next 12 in
       let known = List.exists (fun (p : purchase) -> Date.compare p.t.value_date (Date.add_days last_year.first (-30)) <= 0) all in
       if known && List.length cycles >= 6 then
         let spent = (Analytics.flows_between input.book ~first:last_year.first ~last:last_year.last input.transfers).spending in
         let usual = avg (List.map (fun c -> (flows c).spending) cycles) in
         let extra = spent -: usual in
         if Int64.compare (Int64.mul spent 100L) (Int64.mul usual 125L) >= 0 && Int64.compare extra (Int64.mul input.floor 10L) >= 0 then
           add
             { answer = Season_ahead { cycle = next; last_year = spent; usual }; family = Season_ahead; key = "season:" ^ Cycle.label next;
               stake = extra; material = material extra; surprising = clamp ((fdiv spent usual -. 1.) /. 0.5); reliable = 0.6; actionable = 0.7 });
      (* 9. The week after payday *)
      (if n >= 4 then
         let flexible p = not (List.mem p.merchant regular || is_one_off p) in
         let week, rest, rest_days =
           List.fold_left
             (fun (w, r, d) (c : Cycle.t) ->
               let edge = Date.add_days c.first 6 in
               let ps = List.filter flexible (in_cycle c) in
               let w' = List.fold_left (fun s p -> if Date.compare p.t.value_date edge <= 0 then s +: p.amount else s) 0L ps in
               let r' = List.fold_left (fun s p -> if Date.compare p.t.value_date edge > 0 then s +: p.amount else s) 0L ps in
               (w +: w', r +: r', d + Cycle.day_count c - 7))
             (0L, 0L, 0) recent
         in
         let first_week = Int64.div week (Int64.of_int (7 * n)) and rest = Int64.div rest (Int64.of_int (max 1 rest_days)) in
         let ratio = fdiv first_week rest in
         if Int64.compare rest 0L > 0 && ratio >= 1.6 then
           let stake = Int64.mul (Int64.mul (first_week -: rest) 7L) 12L in
           add
             { answer = Payday_week { first_week; rest; cycles = n }; family = Payday_week;
               key = Printf.sprintf "payday:%d" (int_of_float (ratio *. 2.)); stake; material = material stake;
               surprising = clamp ((ratio -. 1.) /. 1.5); reliable = clamp (float_of_int n /. 6.); actionable = 0.8 });
      List.rev !questions
