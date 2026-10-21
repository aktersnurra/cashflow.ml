type t = {
  id : Household_id.t;
  name : string;
  default_currency : string;
  report_split_bp : int;  (** a member's share in basis points for "my share" reports; 5000 is 50/50 *)
  pay_cycle : Pay_cycle_setting.t;
  created_by : User_id.t;
  created_at : Ptime.t;
}

type status = Invited | Active | Declined | Left

type member = { user_id : User_id.t; display_name : string; status : status; invited_by : User_id.t }

let status_to_string = function Invited -> "invited" | Active -> "active" | Declined -> "declined" | Left -> "left"

let status_of_string = function
  | "invited" -> Some Invited
  | "active" -> Some Active
  | "declined" -> Some Declined
  | "left" -> Some Left
  | _ -> None
