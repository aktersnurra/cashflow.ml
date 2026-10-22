module Access = Cashflow_core.Access
module Analytics = Cashflow_core.Analytics
module Owner = Cashflow_core.Owner
module Time = Cashflow_core.Time

type t = {
  access : Access.t;
  scope : Analytics.scope;
  owners : Owner.t list;
  currency : Cashflow_core.Currency.t;
  setting : Cashflow_core.Pay_cycle_setting.t;
  today : Cashflow_core.Date.t;
}

let select_scope ~access ~scope =
  let user = Access.user access in
  match scope, Access.household access with
  | Analytics.Me, _ -> Ok ([Access.personal access], user.pay_cycle, user.default_currency)
  | (Household | Family), None -> Error Reporting_error.Household_required
  | Household, Some h -> Ok ([Owner.Household h.id], h.pay_cycle, h.default_currency)
  | Family, Some h -> Ok ([Access.personal access; Owner.Household h.id], h.pay_cycle, h.default_currency)

let default_currency ~access ~scope =
  Result.map (fun (_, _, currency) -> currency) (select_scope ~access ~scope)

let make ~access ~scope ~currency ~now =
  Result.map (fun (owners, setting, _) ->
    let user = Access.user access in
    let zone = Result.value (Time.parse_zone user.timezone) ~default:Time.utc in
    {access; scope; owners; currency; setting; today=Time.local_date zone now})
    (select_scope ~access ~scope)

let access t = t.access
let scope t = t.scope
let owners t = t.owners
let currency t = t.currency
let setting t = t.setting
let today t = t.today
