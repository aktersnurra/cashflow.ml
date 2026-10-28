(* What every page gets: the session, the user's language and currency, the theme, and two
   effects: [navigate], and [changed], which every write calls so that everything showing data
   reloads. *)
open! Core
open! Bonsai_web.Cont

type t = {
  session : J.t Bonsai.t;
  locale : string Bonsai.t;
  currency : string Bonsai.t;
  version : int Bonsai.t;
  changed : unit Effect.t Bonsai.t;
  navigate : (Route.t -> unit Effect.t) Bonsai.t;
  notify : (string -> unit Effect.t) Bonsai.t;  (** a short message at the bottom of the screen *)
  theme : Theme.t Bonsai.t;
  set_theme : (Theme.t -> unit Effect.t) Bonsai.t;  (** applies and remembers it on this device *)
}

(* Whoever created the household runs it: invites, renames, adds its bank accounts. *)
let is_owner session =
  match J.str_opt (J.path [ "household"; "created_by" ] session) with Some creator -> String.equal creator (J.str (J.path [ "user"; "id" ] session)) | None -> false
let has_household session = not (J.is_null (J.field "household" session))
