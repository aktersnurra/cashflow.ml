(* The analytics queries, as a GADT indexed by each query's result type. One descriptor per query
   gives its name, description, argument schema, parser and encoder; the MCP tools and the web
   API's /api/query routes are both generated from [all]. Adding a query is one constructor, one
   case in [run] (the compiler insists) and one descriptor. *)
module Access = Cashflow_core.Access
module Account_id = Cashflow_core.Account_id
module Account_info = Cashflow_core.Account_info
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Cycle = Cashflow_core.Cycle
module Date = Cashflow_core.Date
module Household = Cashflow_core.Household
module User_id = Cashflow_core.User_id

let ( let* ) = Result.bind

type common = { scope : Analytics.scope; currency : string option }

type _ t =
  | Cashflow_summary : common * int -> Analytics.cycle_summary list t
  | Spending_by_category : common * Insights.period * bool -> Analytics.category_total list t
  | Category_trend : common * Account_id.t * int -> (Account_info.t * (Cycle.t * int64) list) t
  | Net_worth : common * int -> (Date.t * int64) list t
  | Largest_expenses : common * Insights.period * int -> Service.transfer_view list t
  | Runway : common -> Analytics.runway t
  | Recurring : common -> Analytics.recurring list t
  | Budget_status : common * int -> Insights.budget_report t
  | Contribution_plan : common * string option * (string * string) list -> Insights.plan t
  | Settle_up : common -> Insights.owed list t
  | Goals : common -> Insights.goal_progress list t

let amount_arg (ctx : Insights.ctx) name text =
  Option.to_result ~none:(`Invalid (Printf.sprintf "%s must be a decimal amount like 1240.50" name))
    (Json.parse_decimal ~scale:(Currency.scale ctx.currency) text)

let run : type a. Service.t -> Access.t -> a t -> (a * Insights.ctx, [> Service.error ]) result =
 fun service access query ->
  let with_legacy_ctx common f =
    let* ctx = Insights.context service access ~scope:common.scope ?currency:common.currency () in
    let* value = f ctx in
    Ok (value, ctx)
  in
  match query with
  | Cashflow_summary (common, cycles) ->
      let module W = Cashflow_workflow in
      let result = Reporting_live.run ~db:service.db ~access ~now:service.now (fun () ->
        W.Reporting.cashflow_summary ~access ~scope:common.scope ?currency:common.currency ~cycles ()) in
      let* report = Result.map_error (function
        | W.Reporting_error.Invalid_currency reason -> `Invalid reason
        | W.Reporting_error.Unknown_currency code -> `Invalid ("unknown currency " ^ code)
        | Household_required -> `Invalid "you don't belong to a household, so use scope \"me\""
        | Access_denied -> `Forbidden "access denied"
        | Invalid_range _ -> `Invalid "from_date is after to_date"
        | Storage_unavailable operation -> `Storage_unavailable operation) result in
      let context = report.W.Reporting.context in
      let ctx : Insights.ctx = { service; access = W.Reporting_context.access context;
        scope = W.Reporting_context.scope context; owners = W.Reporting_context.owners context;
        currency = W.Reporting_context.currency context; setting = W.Reporting_context.setting context;
        today = W.Reporting_context.today context } in
      Ok (report.cycles, ctx)
  | Spending_by_category (common, period, compare_previous) ->
      with_legacy_ctx common (fun ctx -> Ok (Insights.spending_by_category ctx ~period ~compare_previous))
  | Category_trend (common, category, cycles) ->
      with_legacy_ctx common (fun ctx -> Insights.category_trend ctx ~category ~cycles)
  | Net_worth (common, cycles) -> with_legacy_ctx common (fun ctx -> Ok (Insights.net_worth ctx ~cycles))
  | Largest_expenses (common, period, limit) ->
      with_legacy_ctx common (fun ctx -> Ok (Insights.largest_expenses ctx ~period ~limit))
  | Runway common -> with_legacy_ctx common (fun ctx -> Ok (Insights.runway ctx))
  | Recurring common -> with_legacy_ctx common (fun ctx -> Ok (Insights.recurring ctx))
  | Budget_status (common, cycle_offset) ->
      with_legacy_ctx common (fun ctx -> Ok (Insights.budget_status ctx ~cycle_offset))
  | Contribution_plan (common, target, incomes) ->
      with_legacy_ctx common (fun ctx ->
        let* target = match target with None -> Ok None | Some t -> Result.map Option.some (amount_arg ctx "target" t) in
        let* incomes =
          List.fold_right
            (fun (member, text) acc -> let* acc = acc in let* v = amount_arg ctx "income" text in Ok ((member, v) :: acc))
            incomes (Ok [])
        in
        Insights.contribution_plan ctx ~target ~incomes)
  | Settle_up common -> with_legacy_ctx common Insights.settle_up
  | Goals common -> with_legacy_ctx common (fun ctx -> Ok (Insights.goals ctx))

(* Arguments *)

module Args = struct
  type fields = (string * Yojson.Safe.t) list

  let string name (fields : fields) =
    match List.assoc_opt name fields with
    | None | Some `Null -> Ok None
    | Some (`String s) -> Ok (Some s)
    | Some _ -> Error (name ^ " must be a string")

  let int name ~default ~min ~max (fields : fields) =
    match List.assoc_opt name fields with
    | None | Some `Null -> Ok default
    | Some (`Int n) when n >= min && n <= max -> Ok n
    | Some _ -> Error (Printf.sprintf "%s must be an integer from %d to %d" name min max)

  let bool name ~default (fields : fields) =
    match List.assoc_opt name fields with None | Some `Null -> Ok default | Some (`Bool b) -> Ok b | Some _ -> Error (name ^ " must be a boolean")

  let common fields =
    let* scope = string "scope" fields in
    let* scope =
      match scope with
      | None -> Ok Analytics.Me
      | Some s -> Option.to_result ~none:"scope must be me, household or family" (Analytics.scope_of_string s)
    in
    let* currency = string "currency" fields in
    Ok { scope; currency }

  let date name fields =
    let* text = string name fields in
    match text with
    | None -> Ok None
    | Some text -> Result.map Option.some (Result.map_error (fun _ -> name ^ " must be a date as YYYY-MM-DD") (Date.of_string text))

  (* Either a cycle offset (0 is the current cycle, -1 the one before) or an explicit date range. *)
  let period fields =
    let* first = date "from_date" fields in
    let* last = date "to_date" fields in
    match first, last with
    | Some first, Some last ->
        if Date.compare first last > 0 then Error "from_date is after to_date"
        else if Date.days_between ~first ~last > 731 then Error "a date range may cover at most two years"
        else Ok (Insights.Range (first, last))
    | Some _, None | None, Some _ -> Error "pass both from_date and to_date, or neither"
    | None, None ->
        let* offset = int "cycle_offset" ~default:0 ~min:(-36) ~max:0 fields in
        Ok (Insights.Cycle_offset offset)

  let account_id name fields =
    let* text = string name fields in
    match Option.bind text Account_id.of_string with Some id -> Ok id | None -> Error (name ^ " must be an account id")
end

(* Schemas *)

let prop name kind description = (name, `Assoc [ ("type", `String kind); ("description", `String description) ])

let common_props =
  [ prop "scope" "string"
      "Whose books: \"me\" (your personal accounts), \"household\" (the shared accounts) or \"family\" (both, with \
       contributions between them cancelling out). Defaults to me.";
    prop "currency" "string" "ISO 4217 code of the accounts to include. Defaults to the scope's default currency." ]

let period_props =
  [ prop "cycle_offset" "integer" "Which pay cycle: 0 is the current one, -1 the previous one, and so on. Ignored when from_date and to_date are given.";
    prop "from_date" "string" "Start of an explicit range, YYYY-MM-DD, inclusive.";
    prop "to_date" "string" "End of an explicit range, YYYY-MM-DD, inclusive." ]

let cycles_prop = prop "cycles" "integer" "How many pay cycles, ending with the current one (1-36, default 6)."

(* Encoders *)

let money (ctx : Insights.ctx) = Json.money ctx.currency
let envelope (ctx : Insights.ctx) fields =
  `Assoc (("scope", `String (Analytics.scope_to_string ctx.scope)) :: ("currency", `String (Currency.code ctx.currency)) :: fields)

let flows ctx (f : Analytics.flows) =
  [ ("income", money ctx f.income); ("spending", money ctx f.spending); ("transfers_in", money ctx f.transfers_in);
    ("transfers_out", money ctx f.transfers_out) ]

let rate = function None -> `Null | Some bp -> `String (Json.decimal ~scale:2 (Int64.of_int bp) ^ "%")

type 'a descriptor = {
  name : string;
  description : string;
  properties : (string * Yojson.Safe.t) list;
  parse : Args.fields -> ('a t, string) result;
  encode : Insights.ctx -> 'a -> Yojson.Safe.t;
}

type any = Any : 'a descriptor -> any

let cashflow_summary =
  { name = "cashflow_summary";
    description =
      "Income, spending, money moved to or from other books (contributions, settlements), savings and savings rate per pay \
       cycle. reconciled is false for cycles not yet confirmed against a bank statement, so treat those figures as provisional.";
    properties = common_props @ [ cycles_prop ];
    parse = (fun f -> let* c = Args.common f in let* n = Args.int "cycles" ~default:6 ~min:1 ~max:36 f in Ok (Cashflow_summary (c, n)));
    encode =
      (fun ctx rows ->
        envelope ctx
          [ ( "cycles",
              `List
                (List.map
                   (fun (r : Analytics.cycle_summary) ->
                     `Assoc (Json.(match cycle r.cycle with `Assoc f -> f | _ -> []) @ flows ctx r.flows
                             @ [ ("savings", money ctx r.savings); ("savings_rate", rate r.savings_rate_bp); ("reconciled", `Bool r.reconciled) ]))
                   rows) ) ]) }

let spending_by_category =
  { name = "spending_by_category";
    description = "Net spending per expense category in a pay cycle or date range, largest first, optionally with the previous period for comparison.";
    properties = common_props @ period_props @ [ prop "compare_previous" "boolean" "Also return the previous period's total per category." ];
    parse =
      (fun f ->
        let* c = Args.common f in
        let* p = Args.period f in
        let* compare = Args.bool "compare_previous" ~default:false f in
        Ok (Spending_by_category (c, p, compare)));
    encode =
      (fun ctx rows ->
        envelope ctx
          [ ( "categories",
              `List
                (List.map
                   (fun (r : Analytics.category_total) ->
                     `Assoc
                       [ ("category", Json.account_ref r.category); ("total", money ctx r.total);
                         ("previous", Json.option (money ctx) r.previous) ])
                   rows) ) ]) }

let category_trend =
  { name = "category_trend";
    description = "One category's net total per pay cycle, for spotting trends.";
    properties = common_props @ [ prop "category_id" "string" "The expense or income account id, from list_accounts."; cycles_prop ];
    parse =
      (fun f ->
        let* c = Args.common f in
        let* id = Args.account_id "category_id" f in
        let* n = Args.int "cycles" ~default:6 ~min:1 ~max:36 f in
        Ok (Category_trend (c, id, n)));
    encode =
      (fun ctx (info, rows) ->
        envelope ctx
          [ ("category", Json.account_ref info);
            ("cycles", `List (List.map (fun (c, total) -> `Assoc [ ("cycle", `String (Cycle.label c)); ("total", money ctx total) ]) rows)) ]) }

let net_worth =
  { name = "net_worth_series";
    description = "Assets minus liabilities at the end of each pay cycle (or today, for the current one), including revaluations of investment accounts.";
    properties = common_props @ [ cycles_prop ];
    parse = (fun f -> let* c = Args.common f in let* n = Args.int "cycles" ~default:12 ~min:1 ~max:36 f in Ok (Net_worth (c, n)));
    encode =
      (fun ctx rows ->
        envelope ctx [ ("points", `List (List.map (fun (d, worth) -> `Assoc [ ("date", Json.date d); ("net_worth", money ctx worth) ]) rows)) ]) }

let largest_expenses =
  { name = "largest_transactions";
    description = "The largest spending transactions in a pay cycle or date range.";
    properties = common_props @ period_props @ [ prop "limit" "integer" "How many (1-50, default 10)." ];
    parse =
      (fun f ->
        let* c = Args.common f in
        let* p = Args.period f in
        let* n = Args.int "limit" ~default:10 ~min:1 ~max:50 f in
        Ok (Largest_expenses (c, p, n)));
    encode = (fun ctx rows -> envelope ctx [ ("transactions", `List (List.map Json.transfer rows)) ]) }

let runway =
  { name = "runway";
    description = "Liquid assets (asset accounts not tagged \"investment\") divided by average spending over the last three complete pay cycles.";
    properties = common_props;
    parse = (fun f -> let* c = Args.common f in Ok (Runway c));
    encode =
      (fun ctx (r : Analytics.runway) ->
        envelope ctx
          [ ("liquid_assets", money ctx r.liquid); ("average_monthly_spending", money ctx r.average_monthly_spending);
            ("months", match r.months with None -> `Null | Some m -> `Float (Float.round (m *. 10.) /. 10.)) ]) }

let recurring =
  { name = "recurring_payments";
    description =
      "Subscriptions and fixed bills detected from the last 400 days: merchant, cadence, typical and latest amount, and the next \
       expected date. price_changed marks a latest charge that differs from the one before by more than 2%.";
    properties = common_props;
    parse = (fun f -> let* c = Args.common f in Ok (Recurring c));
    encode =
      (fun ctx rows ->
        envelope ctx
          [ ( "payments",
              `List
                (List.map
                   (fun (r : Analytics.recurring) ->
                     `Assoc
                       [ ("merchant", `String r.merchant_name); ("cadence", `String (Analytics.cadence_to_string r.cadence));
                         ("typical_amount", money ctx r.typical_amount); ("last_amount", money ctx r.last_amount);
                         ("price_changed", `Bool (Analytics.price_changed r)); ("last_date", Json.date r.last_date);
                         ("next_expected", Json.date r.next_expected); ("occurrences", `Int r.occurrences);
                         ("category", Json.option Json.account_ref r.category) ])
                   rows) ) ]) }

let status_string = function `On_pace -> "on_pace" | `Ahead_of_pace -> "ahead_of_pace" | `Over_budget -> "over_budget"

let budget_status =
  { name = "budget_status";
    description =
      "Each budgeted category in a pay cycle: budget, whether rollover is enabled, amount rolled over from earlier cycles, \
       spent so far, what pace would expect by today, and a status (on_pace, ahead_of_pace when more than 15% ahead of pace, over_budget).";
    properties = common_props @ [ prop "cycle_offset" "integer" "0 for the current pay cycle, -1 for the previous one." ];
    parse = (fun f -> let* c = Args.common f in let* o = Args.int "cycle_offset" ~default:0 ~min:(-36) ~max:0 f in Ok (Budget_status (c, o)));
    encode =
      (fun ctx (r : Insights.budget_report) ->
        envelope ctx
          [ ("cycle", Json.cycle r.report_cycle);
            ( "lines",
              `List
                (List.map
                   (fun (l : Analytics.budget_line) ->
                     `Assoc
                       [ ("category", Json.account_ref l.line_category); ("budget", money ctx l.budgeted); ("carried", money ctx l.carried);
                         ("rollover", `Bool l.rollover); ("spent", money ctx l.spent); ("expected_by_now", money ctx l.expected_by_now); ("status", `String (status_string l.status)) ])
                   r.lines) ) ]) }

let contribution_plan =
  { name = "contribution_plan";
    description =
      "How much each household member should transfer to the household this pay cycle, split in proportion to income, and \
       net contributions so far after money returned to each member. The target defaults to the household's budgets plus \
       its savings goals' monthly amounts. Incomes default to each member's income last cycle; only your own is shown.";
    properties =
      [ prop "currency" "string" "ISO 4217 code; defaults to the household's.";
        prop "target" "string" "Total to collect this cycle, as a decimal amount, e.g. \"24000\".";
        ("incomes", `Assoc [ ("type", `String "object"); ("description", `String "Optional incomes to use instead, keyed by member user id, as decimal strings."); ("additionalProperties", `Assoc [ ("type", `String "string") ]) ]) ];
    parse =
      (fun f ->
        let* currency = Args.string "currency" f in
        let* target = Args.string "target" f in
        let* incomes =
          match List.assoc_opt "incomes" f with
          | None | Some `Null -> Ok []
          | Some (`Assoc pairs) ->
              List.fold_right
                (fun (k, v) acc -> let* acc = acc in match v with `String s -> Ok ((k, s) :: acc) | _ -> Error "incomes must map member ids to decimal strings")
                pairs (Ok [])
          | Some _ -> Error "incomes must be an object"
        in
        Ok (Contribution_plan ({ scope = Household; currency }, target, incomes)));
    encode =
      (fun ctx (p : Insights.plan) ->
        envelope ctx
          [ ("cycle", Json.cycle p.plan_cycle); ("target", money ctx p.target); ("target_basis", `String p.target_basis);
            ( "members",
              `List
                (List.map
                   (fun (m : Insights.member_plan) ->
                     `Assoc
                       [ ("user_id", `String (User_id.to_string m.member)); ("name", `String m.member_name);
                         ("income", Json.option (money ctx) m.income); ("contribution", money ctx m.contribution);
                         ("transferred", money ctx m.contributed);
                         ("remaining", money ctx (Int64.max 0L (Int64.sub m.contribution m.contributed))) ])
                   p.members) ) ]) }

let settle_up =
  { name = "settle_up_status";
    description = "What the household owes each member for shared costs they paid from personal accounts.";
    properties = [ prop "currency" "string" "ISO 4217 code; defaults to the household's." ];
    parse = (fun f -> let* currency = Args.string "currency" f in Ok (Settle_up { scope = Household; currency }));
    encode =
      (fun ctx rows ->
        envelope ctx
          [ ( "owed",
              `List
                (List.map
                   (fun (o : Insights.owed) -> `Assoc [ ("user_id", `String (User_id.to_string o.owed_member)); ("name", `String o.owed_name); ("amount", money ctx o.owed) ])
                   rows) ) ]) }

let goals =
  { name = "savings_goals";
    description = "Savings goals with money earmarked so far, what remains, and the monthly amount needed to reach the target date.";
    properties = common_props;
    parse = (fun f -> let* c = Args.common f in Ok (Goals c));
    encode =
      (fun ctx rows ->
        envelope ctx
          [ ( "goals",
              `List
                (List.map
                   (fun (g : Insights.goal_progress) ->
                     `Assoc
                       [ ("id", `String g.goal.goal_id); ("name", `String g.goal.name); ("target", money ctx g.goal.target_amount);
                         ("target_date", Json.option Json.date g.goal.target_date); ("saved", money ctx g.saved);
                         ("remaining", money ctx g.remaining); ("monthly_amount", Json.option (money ctx) g.goal.monthly_amount);
                         ("required_monthly", Json.option (money ctx) g.required_monthly); ("on_track", `Bool g.on_track) ])
                   rows) ) ]) }

let all =
  [ Any cashflow_summary; Any spending_by_category; Any category_trend; Any net_worth; Any largest_expenses; Any runway;
    Any recurring; Any budget_status; Any contribution_plan; Any settle_up; Any goals ]

let find name = List.find_opt (fun (Any d) -> d.name = name) all

let schema (Any d) =
  `Assoc [ ("type", `String "object"); ("properties", `Assoc d.properties); ("required", `List []); ("additionalProperties", `Bool false) ]

let execute service access (Any d) (fields : Args.fields) =
  match List.find_opt (fun (name, _) -> not (List.mem_assoc name d.properties)) fields with
  | Some (name, _) -> Error (`Invalid ("unknown argument " ^ name))
  | None -> (
      match d.parse fields with
      | Error reason -> Error (`Invalid reason)
      | Ok query ->
          let* value, ctx = run service access query in
          Ok (d.encode ctx value))
