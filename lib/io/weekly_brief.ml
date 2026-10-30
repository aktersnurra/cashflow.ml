(* The weekly brief on Home: the last full week (Monday to Sunday) in figures that code computes, with a few sentences a
   model writes from those figures alone. Whether there is a brief at all, and every number in it, is deterministic; the
   text is cached by a digest of the figures, so it is written again only when they change, never per transaction. It
   reaches no one outside the app. *)
module Access = Cashflow_core.Access
module Analytics = Cashflow_core.Analytics
module Currency = Cashflow_core.Currency
module Cycle = Cashflow_core.Cycle
module Date = Cashflow_core.Date

type t = {
  first : Date.t;
  last : Date.t;
  currency : Currency.t;
  income : int64;
  spending : int64;
  previous_spending : int64;
  top : (string * int64 * int64 option) list;  (** category name, spent, spent the week before *)
  upcoming : int * int64;  (** recurring payments expected within the next seven days (or up to five days late, as the bank lags): how many, and their
      typical total. Shown beside the text, not given to the model: the window moves every day and would rewrite the text daily. *)
  text : string option;
  can_write : bool;  (** a model is configured, no text has been written for these figures yet and an attempt has not failed *)
}

let last_full_week today =
  let since_monday = match Date.weekday today with `Mon -> 0 | `Tue -> 1 | `Wed -> 2 | `Thu -> 3 | `Fri -> 4 | `Sat -> 5 | `Sun -> 6 in
  let last = Date.add_days today (-(since_monday + 1)) in
  (Date.add_days last (-6), last)

let scope_name = function Analytics.Me -> "me" | Household -> "household" | Family -> "family"

let figures ~scope (b : t) =
  let m = Json.money b.currency in
  `Assoc
    [ ("scope", `String (scope_name scope));
      ("week", `String (Date.to_string b.first ^ " to " ^ Date.to_string b.last)); ("currency", `String (Currency.code b.currency));
      ("income", m b.income); ("spending", m b.spending); ("spending_the_week_before", m b.previous_spending);
      ( "top_categories",
        `List (List.map (fun (name, total, previous) -> `Assoc [ ("name", `String name); ("total", m total); ("previous", Json.option m previous) ]) b.top) ) ]

let compute ?(scope = Analytics.Me) ?currency (service : Service.t) access ~write =
  match Insights.context service access ~scope ?currency () with
  | Error _ -> None
  | Ok ctx ->
      (* The last week the statements cover in full: one they stop partway through would look quiet, and
         say nothing set beside the week before. *)
      let reference = match Insights.covered_through ctx with Some d -> Date.min ctx.today (Date.add_days d 1) | None -> ctx.today in
      let first, last = last_full_week reference in
      let before = Date.add_days first (-7) in
      let week from upto = { Cycle.first = from; last = upto; year = 0; month = 0 } in
      let summaries =
        Analytics.cashflow_summary (Insights.book ctx) ~cycles:[ week first last; week before (Date.add_days first (-1)) ] ~reconciled_through:None
          (Insights.transfers ctx ~first:before ~last)
      in
      (match summaries with
      | [ now; prior ] when Int64.compare now.flows.income 0L > 0 || Int64.compare now.flows.spending 0L > 0 ->
          let locale = (Access.user access).locale in
          let top =
            List.filter (fun (c : Analytics.category_total) -> Int64.compare c.total 0L > 0) (Insights.spending_by_category ctx ~period:(Range (first, last)) ~compare_previous:true)
            |> List.filteri (fun i _ -> i < 3)
            |> List.map (fun (c : Analytics.category_total) -> (Starter.display_name ~locale c.category, c.total, c.previous))
          in
          let upcoming =
            List.fold_left
              (fun (count, total) (r : Analytics.recurring) ->
                if Date.compare r.next_expected (Date.add_days ctx.today (-5)) >= 0 && Date.compare r.next_expected (Date.add_days ctx.today 6) <= 0 then (count + 1, Int64.add total r.typical_amount)
                else (count, total))
              (0, 0L) (Insights.recurring ctx)
          in
          let brief =
            { first; last; currency = ctx.currency; income = now.flows.income; spending = now.flows.spending; previous_spending = prior.flows.spending; top; upcoming;
              text = None; can_write = false }
          in
          let text, can_write =
            match Llm_features.week_summary service access ~write ~last ~figures:(figures ~scope brief) with
            | `Text t -> (Some t, false)
            | `Unwritten -> (None, true)
            | `Failed | `Off -> (None, false)
          in
          Some { brief with text; can_write }
      | _ -> None)

let json = function
  | None -> `Assoc [ ("brief", `Null) ]
  | Some b ->
      let m = Json.money b.currency in
      `Assoc
        [ ( "brief",
            `Assoc
              [ ("first", Json.date b.first); ("last", Json.date b.last); ("currency", `String (Currency.code b.currency)); ("income", m b.income);
                ("spending", m b.spending); ("previous_spending", m b.previous_spending);
                ( "top",
                  `List (List.map (fun (name, total, previous) -> `Assoc [ ("name", `String name); ("total", m total); ("previous", Json.option m previous) ]) b.top) );
                ("upcoming_count", `Int (fst b.upcoming)); ("upcoming_total", m (snd b.upcoming));
                ("text", Json.option (fun t -> `String t) b.text); ("can_write", `Bool b.can_write) ] ) ]

let commands =
  let open Commands in
  [ command ~mcp:false "get_weekly_brief" Read
      "The last full week in figures, with a few generated sentences when a model is configured. Returns null when the week had no income or spending. Takes scope me, household or family."
      (Query.common_props @ [ ("write", `Assoc [ ("type", `String "boolean"); ("description", `String "Write the sentences if they have not been written for these figures; defaults to false.") ]) ])
      (fun service access fields ->
        let ( let* ) = Result.bind in
        let* write = arg (Query.Args.bool "write" ~default:false fields) in
        let* common = arg (Query.Args.common fields) in
        Ok (json (compute ~scope:common.scope ?currency:common.currency service access ~write))) ]
