(* An account with the display details that live outside the ledger. *)
type role =
  | Uncategorized  (** suspense account imports post against until they are categorised *)
  | Untracked_cash
  | Opening
  | Unrealized
  | Receivable  (** what the household owes a member *)
  | Payable  (** what the household owes [member_id] *)
  | Cash

type details = {
  institution : string option;
  number_last4 : string option;
  role : role option;
  member_id : User_id.t option;
  statement_backed : bool;  (** a bank statement confirms this account's transfers (§6.7) *)
  tags : string list;
}

let no_details = { institution = None; number_last4 = None; role = None; member_id = None; statement_backed = false; tags = [] }

type t = { account : Account.t; details : details }

let roles =
  [ ("uncategorized", Uncategorized); ("untracked_cash", Untracked_cash); ("opening", Opening); ("unrealized", Unrealized);
    ("receivable", Receivable); ("payable", Payable); ("cash", Cash) ]

let role_to_string role = fst (List.find (fun (_, r) -> r = role) roles)
let role_of_string value = List.assoc_opt value roles

let validate_last4 = function
  | None -> Ok None
  | Some digits when String.length digits = 4 && String.for_all (fun c -> c >= '0' && c <= '9') digits -> Ok (Some digits)
  | Some _ -> Error (`Invalid "number_last4 must be exactly four digits")

(* Shown in place of an account number, which is never stored. *)
let masked_number info = Option.map (fun digits -> "****" ^ digits) info.details.number_last4
