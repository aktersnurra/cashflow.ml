(* The forecast query's response as records, so the page never reads raw fields. Money stays as
   the server's exact decimal string (J.t) for formatting; [value] gives a float for drawing. *)
open! Core

type point = { date : string; cash : J.t; earmarked : J.t }
type basis = { cadence : string; typical : J.t; last : J.t; occurrences : int; since : string }

type item = {
  id : string;
  date : string;
  kind : string;  (** bill, income, transfer, contribution or one_off *)
  name : string;
  amount : J.t;
  account_id : string;
  account_name : string;
  category_id : string option;
  basis : basis option;
}

type outlook = { account_id : string; account_name : string; lowest_date : string; lowest : J.t; overdraw : string option }

type t = {
  currency : string;
  today : string;
  imported_through : string option;
  horizon_end : string;
  history : point list;
  baseline : point list;
  scenario : point list option;
  lowest_date : string;
  lowest : J.t;
  range : (string * J.t * J.t) list;  (** date, low, high, from today *)
  chance_positive : int option;  (** percent *)
  accounts : outlook list;
  items : item list;
  assumptions : J.t list;  (** each { kind, ...figures }; the page words them *)
}

let value json = Fmt.to_float json
let point json = { date = J.str (J.field "date" json); cash = J.field "cash" json; earmarked = J.field "earmarked" json }

let item json =
  { id = J.str (J.field "id" json); date = J.str (J.field "date" json); kind = J.str (J.field "kind" json); name = J.str (J.field "name" json);
    amount = J.field "amount" json; account_id = J.str (J.path [ "account"; "id" ] json); account_name = J.str (J.path [ "account"; "name" ] json);
    category_id = J.str_opt (J.path [ "category"; "id" ] json);
    basis =
      (match J.field "basis" json with
      | `Null -> None
      | b ->
          Some
            { cadence = J.str (J.field "cadence" b); typical = J.field "typical" b; last = J.field "last" b; occurrences = J.int (J.field "occurrences" b);
              since = J.str (J.field "since" b) }) }

let outlook json : outlook =
  { account_id = J.str (J.path [ "account"; "id" ] json); account_name = J.str (J.path [ "account"; "name" ] json);
    lowest_date = J.str (J.path [ "lowest"; "date" ] json); lowest = J.path [ "lowest"; "balance" ] json; overdraw = J.str_opt (J.field "overdraw" json) }

let of_json json =
  { currency = J.str ~default:"SEK" (J.field "currency" json); today = J.str (J.field "today" json); imported_through = J.str_opt (J.field "imported_through" json);
    horizon_end = J.str (J.field "horizon_end" json); history = List.map (J.list (J.field "history" json)) ~f:point;
    baseline = List.map (J.list (J.field "baseline" json)) ~f:point;
    scenario = (match J.field "scenario" json with `Null -> None | s -> Some (List.map (J.list s) ~f:point));
    lowest_date = J.str (J.path [ "lowest"; "date" ] json); lowest = J.path [ "lowest"; "cash" ] json;
    range = List.map (J.list (J.field "range" json)) ~f:(fun r -> (J.str (J.field "date" r), J.field "low" r, J.field "high" r));
    chance_positive = (match J.field "chance_positive" json with `Int p -> Some p | _ -> None);
    accounts = List.map (J.list (J.field "accounts" json)) ~f:outlook; items = List.map (J.list (J.field "items" json)) ~f:item;
    assumptions = J.list (J.field "assumptions" json) }
