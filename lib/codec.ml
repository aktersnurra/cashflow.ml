(* Column encodings for domain types, shared by every SQLite module. *)
open Sql

let id (type a) (module I : Id.S with type t = a) name = map ~name text ~to_db:I.to_string ~of_db:I.of_string
let account_id = id (module Account_id) "account id"
let transfer_id = id (module Transfer_id) "transfer id"
let user_id = id (module User_id) "user id"
let household_id = id (module Household_id) "household id"
let date = map ~name:"date" text ~to_db:Date.to_string ~of_db:(fun value -> Result.to_option (Date.of_string value))

let time =
  map ~name:"time" text
    ~to_db:(fun time -> Ptime.to_rfc3339 ~frac_s:3 ~tz_offset_s:0 time)
    ~of_db:(fun value -> match Ptime.of_rfc3339 value with Ok (time, _, _) -> Some time | Error _ -> None)

let amount = map ~name:"amount" int64 ~to_db:Amount.to_int64 ~of_db:Amount.of_int64
let key = map ~name:"idempotency key" text ~to_db:Idempotency_key.to_string ~of_db:(fun v -> Result.to_option (Idempotency_key.of_string v))

let enum name cases =
  map ~name text
    ~to_db:(fun value -> fst (List.find (fun (_, case) -> case = value) cases))
    ~of_db:(fun value -> List.assoc_opt value cases)

let account_kind =
  enum "account kind" [ ("asset", Account.Asset); ("liability", Liability); ("equity", Equity); ("income", Income); ("expense", Expense) ]

let transfer_code =
  enum "transfer code"
    (List.map (fun code -> (Transfer.code_to_string code, code))
       Transfer.[ Purchase; Income; Internal; Contribution; Reclassify; Reversal; Revaluation; Opening; Settlement; Adjustment ])

(* An owner is stored as two columns, kind and id. *)
let owner_kind = function Owner.User _ -> "user" | Household _ -> "household"
let owner_id = function Owner.User id -> User_id.to_string id | Household id -> Household_id.to_string id

let owner kind id =
  match kind, User_id.of_string id, Household_id.of_string id with
  | "user", Some id, _ -> Owner.User id
  | "household", _, Some id -> Household id
  | _ -> raise (Sql_error (Printf.sprintf "invalid owner %s:%s" kind id))

let tag_separator = "\x1f"
let tags = function None -> [] | Some joined -> List.sort String.compare (String.split_on_char tag_separator.[0] joined)
