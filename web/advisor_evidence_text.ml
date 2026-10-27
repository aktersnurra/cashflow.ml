open! Core

(* Closed presentation mapping for the existing stored projection vocabulary. Unknown
   strings stay literal; neither model prose nor financial values are translated. *)
let title locale = function
  | "cycle_summary" -> Msg.advisor_evidence_cycle_summary locale
  | "category_spending" -> Msg.advisor_evidence_category_spending locale
  | "category_trend" -> Msg.advisor_evidence_category_trend locale
  | "relevant_transactions" -> Msg.advisor_evidence_relevant_transactions locale
  | "recurring" -> Msg.advisor_evidence_recurring locale
  | "budget_status" -> Msg.advisor_evidence_budget_status locale
  | "goals" -> Msg.advisor_evidence_goals locale
  | "forecast" -> Msg.advisor_evidence_forecast locale
  | "propose_set_budget" -> Msg.advisor_evidence_propose_budget locale
  | other -> other

let label locale = function
  | "Returned rows" -> Msg.advisor_evidence_returned locale
  | "Total rows" -> Msg.advisor_evidence_total locale
  | "Truncated" -> Msg.advisor_evidence_truncated locale
  | "Income (first returned cycle)" -> Msg.advisor_evidence_income locale
  | "Spending (first returned cycle)" -> Msg.advisor_evidence_spending locale
  | "Largest returned category" -> Msg.advisor_evidence_category locale
  | "First returned cycle" -> Msg.advisor_evidence_cycle locale
  | "Largest returned expense" -> Msg.advisor_evidence_expense locale
  | "Largest returned typical payment" -> Msg.advisor_evidence_payment locale
  | "First returned budget: spent" -> Msg.advisor_evidence_budget locale
  | "First returned goal: remaining" -> Msg.advisor_evidence_goal locale
  | "Forecast lowest cash" -> Msg.advisor_evidence_lowest locale
  | "Forecast lowest date" -> Msg.advisor_evidence_lowest_date locale
  | "Category" -> Msg.advisor_evidence_label_category locale
  | "Current budget" -> Msg.advisor_evidence_label_current_budget locale
  | "Proposed budget" -> Msg.advisor_evidence_label_proposed_budget locale
  | "From cycle" -> Msg.advisor_evidence_label_from_cycle locale
  | "Carry over" -> Msg.advisor_evidence_label_carry_over locale
  | other -> other

let value locale (f : Cashflow_advisor_protocol.Wire.fact) =
  let module W = Cashflow_advisor_protocol.Wire in
  match f.label, f.value with
  | "Truncated", W.Text "true" -> Msg.advisor_evidence_yes locale
  | "Truncated", W.Text "false" -> Msg.advisor_evidence_no locale
  | "Carry over", W.Text "true" -> Msg.advisor_evidence_yes locale
  | "Carry over", W.Text "false" -> Msg.advisor_evidence_no locale
  | "Current budget", W.Text "none" -> Msg.advisor_evidence_none locale
  | _, (W.Text x | Date x) -> x
  | _, Money m -> m.decimal ^ " " ^ m.currency.code

let digits s = not (String.is_empty s) && String.for_all s ~f:Char.is_digit
let integer s = digits s || (String.is_prefix s ~prefix:"-" && digits (String.drop_prefix s 1))
let date s = match String.split s ~on:'-' with
  | [y; m; d] -> String.length y = 4 && String.length m = 2 && String.length d = 2 && List.for_all [y;m;d] ~f:digits
  | _ -> false

(* Match only the exact application-owned caveat formats, preserving the original
   numeric/date tokens (no float conversion, currency math or free-text parsing). *)
let assumption locale original =
  match String.split original ~on:' ' with
  | ["stopped_series_excluded"] -> Msg.advisor_evidence_stopped locale
  | ["other_currency_accounts_excluded"] -> Msg.advisor_evidence_other_currency locale
  | ["no_income_detected"] -> Msg.advisor_evidence_no_income locale
  | ["no_range_yet"] -> Msg.advisor_evidence_no_range locale
  | ["stale_import:"; d] when date d -> Msg.advisor_evidence_stale_import locale ~date:d
  | ["short_history:"; d] when digits d -> Msg.advisor_evidence_short_history locale ~days:d
  | ["range_from:"; n; "forecasts"; "since"; d] when digits n && date d ->
      Msg.advisor_evidence_range locale ~count:n ~date:d
  | ["seasonal:"; month; percent; "percent"] ->
      (match String.split month ~on:'-' with
       | [y; m] when String.length y = 4 && String.length m = 2 && digits y && digits m && integer percent ->
           Msg.advisor_evidence_seasonal locale ~month ~percent
       | _ -> original)
  | ["usual_spending:"; amount; "minor"; "units;"; first; "to"; last; days; "days"]
    when integer amount && date first && String.is_suffix last ~suffix:";"
         && date (String.drop_suffix last 1) && digits days ->
      Msg.advisor_evidence_usual locale ~amount ~first ~last:(String.drop_suffix last 1) ~days
  | _ -> original
