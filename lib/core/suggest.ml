(* Category suggestions for imported rows. A suggester is a function; suggesters combine with
   [<|>], which takes the first that has an answer. Rules are compiled into closures once per
   import rather than interpreted again for every row. *)
type source = Rule of string | History of int | Llm

type suggestion = {
  category_id : Account_id.t;
  tags : string list;
  shared : bool;
  source : source;
  confidence : int;  (** 0 to 100 *)
  auto_apply : bool;  (** only rules the user marked so are applied without asking *)
}

type input = { bank_account : Account_info.t; row : Statement_format.row; merchant : string }
type t = input -> suggestion option

let none : t = fun _ -> None
let ( <|> ) (a : t) (b : t) : t = fun input -> match a input with Some _ as found -> found | None -> b input
let first suggesters = List.fold_right ( <|> ) suggesters none

(* Rules carry their own [auto_apply], and so does history: a merchant a person always filed under the same
   category is filed there again. Otherwise history and LLM suggestions are applied without asking only when
   the bank account opted in and the suggestion is at least as confident as it asked for. A suggestion whose
   category doesn't [fit] the bank account (another owner or currency) is never applied. *)
let promote ?(fits = true) ~threshold (s : suggestion) =
  match s.source with
  | _ when not fits -> { s with auto_apply = false }
  | History _ when s.auto_apply -> s
  | History _ | Llm ->
      { s with auto_apply = (match threshold with Some minimum -> (not s.shared) && s.confidence >= minimum | None -> false) }
  | Rule _ -> s

let source_to_string = function Rule _ -> "rule" | History _ -> "history" | Llm -> "llm"

let merchant_key (row : Statement_format.row) =
  Merchant.key ~description:row.description ~counterparty:row.counterparty

type rule = {
  rule_id : string;
  owner : Owner.t;
  pattern : string;  (** matched case-insensitively against description and counterparty *)
  direction : [ `In | `Out ] option;
  min_amount : int64 option;  (** absolute amounts, minor units *)
  max_amount : int64 option;
  bank_account_id : Account_id.t option;
  category_id : Account_id.t;
  tags : string list;
  shared : bool;
  auto_apply : bool;
}

let compile (rule : rule) : t =
  let pattern = String.lowercase_ascii (String.trim rule.pattern) in
  let within value = function None -> true | Some bound -> bound value in
  fun input ->
    let amount = Int64.abs input.row.amount in
    let text = String.lowercase_ascii (input.row.description ^ " " ^ Option.value input.row.counterparty ~default:"") in
    let direction = if Int64.compare input.row.amount 0L < 0 then `Out else `In in
    if
      pattern <> "" && Str_compat.contains ~sub:pattern text
      && within direction (Option.map (fun d x -> d = x) rule.direction)
      && within amount (Option.map (fun m x -> Int64.compare x m >= 0) rule.min_amount)
      && within amount (Option.map (fun m x -> Int64.compare x m <= 0) rule.max_amount)
      && within input.bank_account.account.id (Option.map Account_id.equal rule.bank_account_id)
      && Owner.equal rule.owner input.bank_account.account.owner
    then
      Some { category_id = rule.category_id; tags = rule.tags; shared = rule.shared; source = Rule rule.rule_id; confidence = 95; auto_apply = rule.auto_apply }
    else None

let of_rules rules = first (List.map compile rules)

(* What this merchant was categorised as before, with more confidence the more often it agreed. When every
   choice was the same category, from the first one on, it is applied without asking. *)
let of_history (lookup : string -> (Account_id.t * int * bool) option) : t =
 fun input ->
  match lookup input.merchant with
  | Some (category_id, times, consistent) ->
      Some
        { category_id; tags = []; shared = false; source = History times; confidence = (if consistent then 95 else min 90 (50 + (times * 10)));
          auto_apply = consistent }
  | None -> None
