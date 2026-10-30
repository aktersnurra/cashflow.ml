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
module User_id = Cashflow_core.User_id

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

(* The model's part: it reads a digest of the books and the questions already there, and proposes a few more,
   each tied to a check (Questions.check) the engine runs. Its reply is cached by a digest of what it was shown,
   so it is asked again only when the books change; with [write] false only a reply already there is used. A
   failed attempt is remembered for the same digest. It is shown merchants, categories and amounts, never
   account numbers or names of people, through the same zero-retention provider as the other model features. *)

let money_text currency n = Json.decimal ~scale:(Currency.scale currency) n

let digest_json currency (d : Q.digest) ~(standard : Q.question list) =
  let m = money_text currency in
  `Assoc
    [ ("currency", `String (Currency.code currency));
      ( "cycles",
        `List (List.map (fun ((c : Cashflow_core.Cycle.t), (f : Analytics.flows)) -> `Assoc [ ("cycle", `String (Cashflow_core.Cycle.label c)); ("income", `String (m f.income)); ("spending", `String (m f.spending)) ]) d.cycles) );
      ( "categories",
        `List
          (List.map
             (fun (l : Q.category_line) ->
               `Assoc [ ("category_id", `String (Account_id.to_string l.category.account.id)); ("name", `String l.category.account.name); ("per_cycle", `List (List.map (fun n -> `String (m n)) l.per_cycle)) ])
             d.categories) );
      ( "merchants",
        `List
          (List.map
             (fun (l : Q.merchant_line) ->
               `Assoc
                 [ ("merchant", `String l.merchant); ("name", `String l.name); ("category", `String (match l.category with Some c -> c.account.name | None -> "uncategorised"));
                   ("purchases", `Int l.count); ("total", `String (m l.total)); ("first", `String (Date.to_string l.first)); ("last", `String (Date.to_string l.last));
                   ("regular_bill", `Bool l.regular) ])
             d.merchants) );
      ("already_answered", `List (List.map (fun (q : Q.question) -> `String q.key) standard)) ]

let proposal_schema =
  let str = `Assoc [ ("type", `String "string") ] in
  `Assoc
    [ ("type", `String "object");
      ( "properties",
        `Assoc
          [ ( "questions",
              `Assoc
                [ ("type", `String "array");
                  ( "items",
                    `Assoc
                      [ ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [ ("title", str); ("check", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "merchant_spend"; `String "category_change"; `String "new_merchant" ]) ]);
                              ("merchant", str); ("category_id", str); ("months", `Assoc [ ("type", `String "integer") ]) ] );
                        ("required", `List [ `String "title"; `String "check"; `String "merchant"; `String "category_id"; `String "months" ]);
                        ("additionalProperties", `Bool false) ] ) ] ) ] );
      ("required", `List [ `String "questions" ]);
      ("additionalProperties", `Bool false) ]

let system ~language =
  Printf.sprintf
    "You are a careful personal accountant reviewing a client's bank data: their last complete pay cycles, spending per \
     category per cycle, and the merchants that cost the most. already_answered lists the questions the app has \
     answered already. Propose at most four more questions the client would want answered: what they would be \
     surprised by or could act on. Each must be answerable by exactly one check: merchant_spend (what one merchant \
     cost; give its merchant key and months, 3, 6 or 12), category_change (a category's last three cycles against the \
     three before; give its category_id), or new_merchant (a merchant first bought from recently; give its key). The \
     title is a short question to the client in %s, with no numbers or amounts in it: the app adds the figures. Leave \
     fields the check doesn't use empty (\"\" or 0). Return an empty list when nothing is worth asking."
    language

let proposals_of_json json =
  let open Yojson.Safe.Util in
  try
    json |> member "questions" |> to_list
    |> List.filter_map (fun q ->
           let s name = match member name q with `String v -> v | _ -> "" in
           let title = s "title" in
           match s "check" with
           | "merchant_spend" -> Some { Q.title; check = Merchant_spend { merchant = s "merchant"; months = (match member "months" q with `Int n -> n | _ -> 6) } }
           | "category_change" -> (
               match Account_id.of_string (s "category_id") with Some category -> Some { Q.title; check = Category_change { category } } | None -> None)
           | "new_merchant" -> Some { Q.title; check = New_merchant { merchant = s "merchant" } }
           | _ -> None)
    |> List.filteri (fun i _ -> i < 4)
  with Type_error _ -> []

let propose (service : Service.t) access (ctx : Insights.ctx) input ~standard ~write =
  match service.assistant with
  | None -> `Off
  | Some llm -> (
      let user = Access.user access in
      let digest = Q.digest input in
      match digest.cycles with
      | [] | [ _ ] -> `Off
      | _ -> (
          let shown = Yojson.Safe.to_string (digest_json ctx.currency digest ~standard) in
          let key = Printf.sprintf "brief-ask:%s:%s:%s" (User_id.to_string user.id) (String.sub (Digest.to_hex (Digest.string shown)) 0 16) user.locale in
          let failed_key = key ^ ":failed" in
          match Sql.run service.db Llm_features.cache_get key with
          | Some reply -> `Proposals (Option.fold ~none:[] ~some:proposals_of_json (Llm.json_of_reply reply))
          | None when Option.is_some (Sql.run service.db Llm_features.cache_get failed_key) -> `Failed
          | None when not write -> `Unwritten
          | None -> (
              let language = if String.starts_with ~prefix:"sv" user.locale then "Swedish" else "English" in
              let result = llm.complete ~schema:("briefing_questions", proposal_schema) ~system:(system ~language) ~user:shown () in
              match Result.map Llm.json_of_reply result with
              | Ok (Some json) ->
                  Service.log_llm_call service access ~purpose:"briefing_questions" ~items:1 ~ok:true;
                  Sql.run service.db Llm_features.cache_put key (Yojson.Safe.to_string json) llm.model (service.now ());
                  `Proposals (proposals_of_json json)
              | _ ->
                  Service.log_llm_call service access ~purpose:"briefing_questions" ~items:1 ~ok:false;
                  Sql.run service.db Llm_features.cache_put failed_key "failed" llm.model (service.now ());
                  `Failed)))

(* The briefing: the families' answers and the model's checked questions together, without what the person put
   away or muted, ranked, at most [limit] and two of a family. [can_propose]: the model hasn't been asked about
   these books yet. *)
let questions ?(write = false) (service : Service.t) access (ctx : Insights.ctx) =
  let user = Access.user access in
  let said key = match Attention.current_state service.db user.id key ~today:ctx.today with Attention.State Shown -> false | _ -> true in
  let muted = List.filter (fun f -> said (mute_key f)) Q.families in
  let input = input ctx in
  let standard = Q.ask input in
  let proposed, can_propose =
    match propose service access ctx input ~standard ~write with
    | `Proposals ps -> (List.filter_map (Q.verify input ~existing:standard) ps, false)
    | `Unwritten -> ([], true)
    | `Off | `Failed -> ([], false)
  in
  let kept =
    standard @ proposed
    |> List.filter (fun (q : Q.question) -> (not (List.mem q.family muted)) && (not (said (question_key q))) && Q.score q >= floor)
    |> List.stable_sort (fun a b -> Float.compare (Q.score b) (Q.score a))
    |> List.fold_left
         (fun (kept, per_family) (q : Q.question) ->
           let seen = Option.value (List.assoc_opt q.family per_family) ~default:0 in
           if List.length kept >= limit || seen >= 2 then (kept, per_family)
           else (q :: kept, (q.family, seen + 1) :: List.remove_assoc q.family per_family))
         ([], [])
    |> fst |> List.rev
  in
  (kept, can_propose)

(* JSON: the figures, in the scope's currency; the wording is the client's (or, later, the model's). *)
let answer_json currency (a : Q.answer) =
  let m = Json.money currency in
  let bill (b : Q.bill) = `Assoc [ ("name", `String b.bill_name); ("yearly", m b.yearly) ] in
  match a with
  | Proposed { title; figures } ->
      let figures =
        match figures with
        | Merchant_figures { name; count; total; months } ->
            `Assoc [ ("check", `String "merchant_spend"); ("name", `String name); ("count", `Int count); ("total", m total); ("months", `Int months) ]
        | Category_figures { category; recent; earlier } ->
            `Assoc [ ("check", `String "category_change"); ("category", Json.account_ref category); ("recent", m recent); ("earlier", m earlier) ]
        | New_merchant_figures { name; first; count; total } ->
            `Assoc [ ("check", `String "new_merchant"); ("name", `String name); ("first", Json.date first); ("count", `Int count); ("total", m total) ]
      in
      `Assoc [ ("title", `String title); ("figures", figures) ]
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

(* The question and its answer in plain words with the engine's figures, for the advisor when the person asks
   about a card: what the server states, not what the client sent. *)
let describe currency (q : Q.question) =
  let m n = money_text currency n ^ " " ^ Currency.code currency in
  let name (c : Account_info.t) = c.account.name in
  let bills bs = String.concat ", " (List.map (fun (b : Q.bill) -> b.bill_name ^ " " ^ m b.yearly ^ " a year") bs) in
  match q.answer with
  | Proposed { title; figures = Merchant_figures { name = n; count; total; months } } ->
      (title, Printf.sprintf "%s: %s over the last %d months, %d purchases." n (m total) months count)
  | Proposed { title; figures = Category_figures { category; recent; earlier } } ->
      (title, Printf.sprintf "%s: %s a cycle over the last three complete cycles, %s over the three before." (name category) (m recent) (m earlier))
  | Proposed { title; figures = New_merchant_figures { name = n; first; count; total } } ->
      (title, Printf.sprintf "%s: first bought from on %s; %d purchases, %s in all." n (Date.to_string first) count (m total))
  | Last_month e ->
      ( "Why the last month cost what it did",
        Printf.sprintf "Cycle %s: spent %s against a usual %s (average of %d cycles before). Drivers: %s." (Cashflow_core.Cycle.label e.cycle) (m e.spending)
          (m e.usual_spending) e.compared_with
          (String.concat "; "
             (List.map
                (function
                  | Cashflow_core.Explain.Yearly { description; amount; _ } -> Printf.sprintf "yearly bill %s %s" description (m amount)
                  | One_off { description; amount; _ } -> Printf.sprintf "one-off %s %s" description (m amount)
                  | Habit { category; difference; cycles_up } -> Printf.sprintf "%s up %s (%d cycles running)" (name category) (m difference) cycles_up
                  | Less { category; difference } -> Printf.sprintf "%s down %s" (name category) (m difference)
                  | Income_change { difference } -> Printf.sprintf "income %s %s" (if Int64.compare difference 0L >= 0 then "up" else "down") (m (Int64.abs difference)))
                e.drivers)) )
  | Where_it_goes { fixed; flexible; one_off; top; cycles } ->
      ( "Where the money goes",
        Printf.sprintf "Per cycle over %d complete cycles: regular bills %s, flexible spending %s, one-offs %s. Biggest flexible categories: %s." cycles (m fixed)
          (m flexible) (m one_off) (String.concat ", " (List.map (fun (c, t) -> name c ^ " " ^ m t) top)) )
  | Regular_bills { yearly; count; biggest; fresh; raised } ->
      ( "Regular bills",
        Printf.sprintf "%d regular bills, %s a year. Biggest: %s.%s%s" count (m yearly) (bills biggest)
          (if fresh = [] then "" else " New: " ^ bills fresh ^ ".")
          (if raised = [] then "" else " Price rises: " ^ String.concat ", " (List.map (fun ((b : Q.bill), r) -> b.bill_name ^ " up " ^ m r ^ " a year") raised) ^ ".") )
  | Drift { category; usual; recent; cycles } ->
      (name category ^ " has crept up", Printf.sprintf "%s: %s a cycle for the last %d cycles, against %s before." (name category) (m recent) cycles (m usual))
  | Leak { name = n; count; total; months; _ } -> (n ^ " adds up", Printf.sprintf "%d purchases at %s in %d months, %s in all." count n months (m total))
  | Fees { total; months } -> ("Fees and interest", Printf.sprintf "%s in fees and interest over %d cycles." (m total) months)
  | Normal_month { baseline; liquid; months_covered } ->
      ("What a normal month costs", Printf.sprintf "%s a cycle without one-offs; cash on hand %s covers %.1f such cycles." (m baseline) (m liquid) months_covered)
  | Real_savings { usual; with_one_offs; one_offs; cycles } ->
      ( "What is really saved",
        Printf.sprintf "Over %d cycles: %s saved a cycle on average, %s without one-offs (one-offs %s a cycle)." cycles (m with_one_offs) (m usual) (m one_offs) )
  | Season_ahead { cycle; last_year; usual } ->
      ( "A costly month ahead",
        Printf.sprintf "The cycle %s cost %s last year, against a usual %s." (Cashflow_core.Cycle.label cycle) (m last_year) (m usual) )
  | Payday_week { first_week; rest; cycles } ->
      ( "The week after payday",
        Printf.sprintf "Over %d cycles: %s a day of flexible spending in the first week after payday, %s a day the rest of the cycle." cycles (m first_week) (m rest) )

let find service access ~scope ~key =
  match Insights.context service access ~scope () with
  | Error _ -> None
  | Ok ctx -> List.find_map (fun q -> if question_key q = key then Some (describe ctx.currency q) else None) (fst (questions service access ctx))

let briefing_json ?write service access ~scope =
  let ( let* ) = Result.bind in
  let* ctx = Insights.context service access ~scope () in
  let questions, can_propose = questions ?write service access ctx in
  Ok
    (`Assoc
      [ ("currency", `String (Currency.code ctx.currency)); ("questions", `List (List.map (question_json ctx.currency) questions));
        ("can_propose", `Bool can_propose) ])

let commands =
  let open Commands in
  [ command "get_briefing" Read
      "The questions about your money worth answering before you ask, already answered with exact figures and ranked by \
       signal: where the money goes, regular bills, spending drifting up, small purchases that add up, fees, what a normal \
       month costs and how long the cash covers it, what you really save without one-offs, a costly month coming up, and \
       spending in the week after payday, and the questions a model proposed and the app checked. Each has its factors \
       (material, surprising, reliable, actionable). can_propose: the model hasn't been asked about these figures yet; \
       ask again with propose true to have it (slower)."
      [ ("scope", `Assoc [ ("type", `String "string"); ("description", `String "me (default) or household.") ]);
        ("propose", `Assoc [ ("type", `String "boolean"); ("description", `String "Ask the model for more questions if it hasn't been asked about these figures.") ]) ]
      (fun service access fields ->
        let scope = match List.assoc_opt "scope" fields with Some (`String "household") -> Analytics.Household | _ -> Analytics.Me in
        let write = match List.assoc_opt "propose" fields with Some (`Bool b) -> b | _ -> false in
        lift (briefing_json ~write service access ~scope));
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
