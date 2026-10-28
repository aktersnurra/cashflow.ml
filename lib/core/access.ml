type t = { user : User.t; household : Household.t option }

let make ~user ~household = { user; household }
let user access = access.user
let household access = access.household
let personal access = Owner.User access.user.id
let owners access = personal access :: Option.to_list (Option.map (fun (h : Household.t) -> Owner.Household h.id) access.household)
let can_use access owner = List.exists (Owner.equal owner) (owners access)
(* The household's creator runs it: invites, renames, adds and closes its bank accounts. *)
let is_owner access = match access.household with Some h -> User_id.equal h.created_by access.user.id | None -> false

let restrict access (view : Ledger_core.view) =
  let visible (account : Account.t) = can_use access account.owner in
  { view with
    account = (fun id -> Option.bind (view.account id) (fun account -> if visible account then Some account else None));
    account_by_key = (fun owner key -> if can_use access owner then view.account_by_key owner key else None);
    transfer_by_key = (fun owner key -> if can_use access owner then view.transfer_by_key owner key else None) }
