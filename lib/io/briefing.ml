(* The accountant's briefing on the Overview: the few questions about a person's money worth answering before
   they ask, answered with the engine's figures (Questions), ranked by signal. What the person said decides the
   fifth factor, new: "got it" acknowledges an answer (a materially different one has another key and comes
   back), "not useful" mutes its family. Both are kept with the attention feed's card states. *)

module Access = Cashflow_core.Access
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Date = Cashflow_core.Date
module Q = Cashflow_core.Questions

let question_key (q : Q.question) = "question:" ^ q.key
let mute_key family = "question-mute:" ^ Q.family_name family

(* Below this a question isn't worth a person's attention; at most [limit] are, and at most two of a family. *)
let floor = 0.12
let limit = 5

let input (ctx : Insights.ctx) =
  let book = Insights.book ctx in
  let today = ctx.today in
  let transfers = Insights.transfers ctx ~first:(Date.add_days today (-420)) ~last:today in
  let cash id = match book.inside id with Some info -> info.account.kind = Asset && not (Analytics.is_investment info) | None -> false in
  let accounts = Insights.accounts ctx in
  let fee_categories =
    List.filter_map
      (fun (i : Account_info.t) -> match Starter.untouched i with Some s when s.entry.slug = "fees" -> Some i.account.id | _ -> None)
      accounts
  in
  { Q.book; setting = ctx.setting; today; covered_through = Insights.covered_through ctx; transfers;
    series = Cashflow_core.Series.detect book ~cash ~setting:ctx.setting transfers; liquid = Analytics.liquid_assets book accounts; fee_categories;
    floor = Int64.mul 100L (Int64.of_float (10. ** float_of_int (Currency.scale ctx.currency))) }

let questions (service : Service.t) access (ctx : Insights.ctx) =
  let user = Access.user access in
  let said key = match Attention.current_state service.db user.id key ~today:ctx.today with Attention.State Shown -> false | _ -> true in
  let muted = List.filter (fun f -> said (mute_key f)) Q.families in
  Q.ask (input ctx)
  |> List.filter (fun (q : Q.question) -> (not (List.mem q.family muted)) && (not (said (question_key q))) && Q.score q >= floor)
  |> List.stable_sort (fun a b -> Float.compare (Q.score b) (Q.score a))
  |> List.fold_left
       (fun (kept, per_family) (q : Q.question) ->
         let seen = Option.value (List.assoc_opt q.family per_family) ~default:0 in
         if List.length kept >= limit || seen >= 2 then (kept, per_family)
         else (q :: kept, (q.family, seen + 1) :: List.remove_assoc q.family per_family))
       ([], [])
  |> fst |> List.rev

(* JSON: the figures, in the scope's currency; the wording is the client's (or, later, the model's). *)
let answer_json currency (a : Q.answer) =
  let m = Json.money currency in
  let bill (b : Q.bill) = `Assoc [ ("name", `String b.bill_name); ("yearly", m b.yearly) ] in
  match a with
  | Last_month e -> Json.explanation currency e
  | Where_it_goes { fixed; flexible; one_off; top; cycles } ->
      `Assoc
        [ ("fixed", m fixed); ("flexible", m flexible); ("one_off", m one_off); ("cycles", `Int cycles);
          ("top", `List (List.map (fun (c, total) -> `Assoc [ ("category", Json.account_ref c); ("total", m total) ]) top)) ]
  | Regular_bills { yearly; count; biggest; fresh; raised } ->
      `Assoc
        [ ("yearly", m yearly); ("count", `Int count); ("biggest", `List (List.map bill biggest)); ("fresh", `List (List.map bill fresh));
          ("raised", `List (List.map (fun (b, rise) -> `Assoc [ ("name", `String b.Q.bill_name); ("yearly", m b.yearly); ("rise", m rise) ]) raised)) ]
  | Drift { category; usual; recent; cycles } ->
      `Assoc [ ("category", Json.account_ref category); ("usual", m usual); ("recent", m recent); ("cycles", `Int cycles) ]
  | Leak { name; count; total; months; category } ->
      `Assoc
        [ ("name", `String name); ("count", `Int count); ("total", m total); ("months", `Int months);
          ("category", Json.option Json.account_ref category) ]
  | Fees { total; months } -> `Assoc [ ("total", m total); ("months", `Int months) ]
  | Normal_month { baseline; liquid; months_covered } ->
      `Assoc [ ("baseline", m baseline); ("liquid", m liquid); ("months_covered", `Float (Float.round (months_covered *. 10.) /. 10.)) ]
  | Real_savings { usual; with_one_offs; one_offs; cycles } ->
      `Assoc [ ("usual", m usual); ("with_one_offs", m with_one_offs); ("one_offs", m one_offs); ("cycles", `Int cycles) ]
  | Season_ahead { cycle; last_year; usual } -> `Assoc [ ("cycle", Json.cycle cycle); ("last_year", m last_year); ("usual", m usual) ]
  | Payday_week { first_week; rest; cycles } -> `Assoc [ ("first_week", m first_week); ("rest", m rest); ("cycles", `Int cycles) ]

let question_json currency (q : Q.question) =
  let r x = `Float (Float.round (x *. 100.) /. 100.) in
  `Assoc
    [ ("key", `String (question_key q)); ("family", `String (Q.family_name q.family)); ("answer", answer_json currency q.answer);
      ("stake", Json.money currency q.stake); ("score", r (Q.score q));
      ("factors", `Assoc [ ("material", r q.material); ("surprising", r q.surprising); ("reliable", r q.reliable); ("actionable", r q.actionable) ]) ]

let briefing_json service access ~scope =
  let ( let* ) = Result.bind in
  let* ctx = Insights.context service access ~scope () in
  Ok
    (`Assoc
      [ ("currency", `String (Currency.code ctx.currency)); ("questions", `List (List.map (question_json ctx.currency) (questions service access ctx))) ])

let commands =
  let open Commands in
  [ command "get_briefing" Read
      "The questions about your money worth answering before you ask, already answered with exact figures and ranked by \
       signal: where the money goes, regular bills, spending drifting up, small purchases that add up, fees, what a normal \
       month costs and how long the cash covers it, what you really save without one-offs, a costly month coming up, and \
       spending in the week after payday. Each has its factors (material, surprising, reliable, actionable)."
      [ ("scope", `Assoc [ ("type", `String "string"); ("description", `String "me (default) or household.") ]) ]
      (fun service access fields ->
        let scope = match List.assoc_opt "scope" fields with Some (`String "household") -> Analytics.Household | _ -> Analytics.Me in
        lift (briefing_json service access ~scope));
    command ~mcp:false "act_on_question" Write "Got it (acknowledge this answer) or not useful (stop asking this kind of question)."
      ~required:[ "key"; "action" ]
      [ string_prop "key" "The question's key."; string_prop "action" "got_it or not_useful." ]
      (fun service access fields ->
        let ( let* ) = Result.bind in
        let* key = required_string "key" fields in
        let* action = required_string "action" fields in
        let* family =
          match String.split_on_char ':' key with
          | "question" :: rest -> (match Q.family_of_key (String.concat ":" rest) with Some f -> Ok f | None -> invalid "unknown question")
          | _ -> invalid "unknown question"
        in
        let* () =
          match action with
          | "got_it" -> lift (Attention.act service access ~key ~action:(`Resolve "got_it") ~until:None)
          | "not_useful" ->
              let* () = lift (Attention.act service access ~key ~action:(`Resolve "not_useful") ~until:None) in
              lift (Attention.act service access ~key:(mute_key family) ~action:(`Resolve "muted") ~until:None)
          | _ -> invalid "action must be got_it or not_useful"
        in
        lift (briefing_json service access ~scope:Analytics.Me)) ]
