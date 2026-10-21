(* Who an account belongs to, and the scope within which idempotency keys are unique. *)
type t = User of User_id.t | Household of Household_id.t

let equal a b =
  match a, b with
  | User a, User b -> User_id.equal a b
  | Household a, Household b -> Household_id.equal a b
  | User _, Household _ | Household _, User _ -> false

let to_string = function
  | User id -> "user:" ^ User_id.to_string id
  | Household id -> "household:" ^ Household_id.to_string id
