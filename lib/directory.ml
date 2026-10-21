(* Users, households and memberships. *)
open Sql

let pay_cycle = map ~name:"pay cycle" text ~to_db:Pay_cycle_setting.to_string ~of_db:Pay_cycle_setting.of_string

let user_columns =
  "id, oidc_issuer, oidc_subject, display_name, email, locale, timezone, default_currency, pay_cycle, is_admin, created_at"

let user_row =
  row
    Codec.(user_id @-> text @-> text @-> text @-> nullable text @-> text @-> text @-> text @-> pay_cycle @-> bool @-> time @-> nil)
    (fun id oidc_issuer oidc_subject display_name email locale timezone default_currency pay_cycle is_admin created_at : User.t ->
      { id; oidc_issuer; oidc_subject; display_name; email; locale; timezone; default_currency; pay_cycle; is_admin; created_at })

let user_by_identity =
  stmt ("SELECT " ^ user_columns ^ " FROM users WHERE oidc_issuer = ? AND oidc_subject = ?") (text @-> text @-> nil) (opt user_row)

let user_by_id = stmt ("SELECT " ^ user_columns ^ " FROM users WHERE id = ?") Codec.(user_id @-> nil) (opt user_row)
let user_by_email = stmt ("SELECT " ^ user_columns ^ " FROM users WHERE lower(email) = lower(?)") (text @-> nil) (opt user_row)
let all_users = stmt ("SELECT " ^ user_columns ^ " FROM users ORDER BY display_name") nil (many user_row)

let insert_user =
  stmt
    "INSERT INTO users (id, oidc_issuer, oidc_subject, display_name, email, is_admin, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
    Codec.(user_id @-> text @-> text @-> text @-> nullable text @-> bool @-> time @-> nil)
    exec

let refresh_user =
  stmt "UPDATE users SET display_name = ?, email = ?, is_admin = ? WHERE id = ?"
    Codec.(text @-> nullable text @-> bool @-> user_id @-> nil)
    exec

let update_settings =
  stmt "UPDATE users SET locale = ?, timezone = ?, default_currency = ?, pay_cycle = ? WHERE id = ?"
    Codec.(text @-> text @-> text @-> pay_cycle @-> user_id @-> nil)
    exec

type claims = { issuer : string; subject : string; display_name : string; email : string option; is_admin : bool }

(* Users are created on first sign-in. Name, email and admin rights follow the identity
   provider, so they are refreshed on every sign-in. *)
let resolve_user db ~now (claims : claims) =
  Sql.transaction db (fun () ->
      match run db user_by_identity claims.issuer claims.subject with
      | Some user ->
          if user.display_name <> claims.display_name || user.email <> claims.email || user.is_admin <> claims.is_admin then
            run db refresh_user claims.display_name claims.email claims.is_admin user.id;
          (Option.get (run db user_by_id user.id), `Existing)
      | None ->
          let id = User_id.fresh () in
          run db insert_user id claims.issuer claims.subject claims.display_name claims.email claims.is_admin now;
          (Option.get (run db user_by_id id), `Created))

let find_user db id = run db user_by_id id
let find_user_by_email db email = run db user_by_email email
let users db = run db all_users

let save_settings db (user : User.t) (settings : User.settings) =
  run db update_settings settings.locale settings.timezone settings.default_currency settings.pay_cycle user.id

let household_row =
  row
    Codec.(household_id @-> text @-> text @-> int @-> pay_cycle @-> user_id @-> time @-> nil)
    (fun id name default_currency report_split_bp pay_cycle created_by created_at : Household.t ->
      { id; name; default_currency; report_split_bp; pay_cycle; created_by; created_at })

let household_columns = "h.id, h.name, h.default_currency, h.report_split_bp, h.pay_cycle, h.created_by, h.created_at"
let household_by_id = stmt ("SELECT " ^ household_columns ^ " FROM households h WHERE h.id = ?") Codec.(household_id @-> nil) (opt household_row)

let active_household_of =
  stmt
    ("SELECT " ^ household_columns ^ " FROM households h JOIN memberships m ON m.household_id = h.id \
      WHERE m.user_id = ? AND m.status = 'active'")
    Codec.(user_id @-> nil) (opt household_row)

let insert_household =
  stmt "INSERT INTO households (id, name, default_currency, created_by, created_at) VALUES (?, ?, ?, ?, ?)"
    Codec.(household_id @-> text @-> text @-> user_id @-> time @-> nil)
    exec

let update_household =
  stmt "UPDATE households SET name = ?, default_currency = ?, report_split_bp = ?, pay_cycle = ? WHERE id = ?"
    Codec.(text @-> text @-> int @-> pay_cycle @-> household_id @-> nil)
    exec

let status = map ~name:"membership status" text ~to_db:Household.status_to_string ~of_db:Household.status_of_string

let upsert_membership =
  stmt
    "INSERT INTO memberships (household_id, user_id, status, invited_by, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?) \
     ON CONFLICT (household_id, user_id) DO UPDATE SET status = excluded.status, invited_by = excluded.invited_by, \
     updated_at = excluded.updated_at"
    Codec.(household_id @-> user_id @-> status @-> user_id @-> time @-> time @-> nil)
    exec

let set_status =
  stmt "UPDATE memberships SET status = ?, updated_at = ? WHERE household_id = ? AND user_id = ?"
    Codec.(status @-> time @-> household_id @-> user_id @-> nil)
    exec

let membership_row =
  row Codec.(user_id @-> text @-> status @-> user_id @-> nil) (fun user_id display_name status invited_by : Household.member ->
      { user_id; display_name; status; invited_by })

let members_of =
  stmt
    "SELECT m.user_id, u.display_name, m.status, m.invited_by FROM memberships m JOIN users u ON u.id = m.user_id \
     WHERE m.household_id = ? ORDER BY m.created_at"
    Codec.(household_id @-> nil) (many membership_row)

let invitations_for =
  stmt
    ("SELECT " ^ household_columns ^ " FROM households h JOIN memberships m ON m.household_id = h.id \
      WHERE m.user_id = ? AND m.status = 'invited'")
    Codec.(user_id @-> nil) (many household_row)

let find_household db id = run db household_by_id id
let active_household db user_id = run db active_household_of user_id
let members db household_id = run db members_of household_id
let invitations db user_id = run db invitations_for user_id

let create_household db ~now ~(creator : User.t) ~name ~default_currency =
  let id = Household_id.fresh () in
  run db insert_household id name default_currency creator.id now;
  run db upsert_membership id creator.id Active creator.id now now;
  Option.get (find_household db id)

let save_household db (household : Household.t) =
  run db update_household household.name household.default_currency household.report_split_bp household.pay_cycle household.id

let invite db ~now ~household_id ~(invited_by : User.t) ~user_id = run db upsert_membership household_id user_id Invited invited_by.id now now
let set_membership db ~now ~household_id ~user_id status = run db set_status status now household_id user_id
