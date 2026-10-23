(* The accounts list, decoded once for the pages that pick accounts. *)
open! Core

type account = {
  id : string;
  name : string;
  kind : string;
  owner : string;  (** "personal" or "household" *)
  currency : string;
  balance : J.t;
  pending : J.t;
  institution : string option;
  role : string option;
  statement_backed : bool;
  number : string option;
  tags : string list;
  closed : bool;
}

let of_json json =
  { id = J.str (J.field "id" json); name = J.str (J.field "name" json); kind = J.str (J.field "kind" json); owner = J.str (J.field "owner" json);
    currency = J.str (J.field "currency" json); balance = J.field "balance" json;
    pending = J.field "pending" json; institution = J.str_opt (J.field "institution" json); role = J.str_opt (J.field "role" json);
    statement_backed = J.bool (J.field "statement_backed" json); number = J.str_opt (J.field "number" json);
    tags = List.map (J.list (J.field "tags" json)) ~f:(fun t -> J.str t); closed = J.bool (J.field "closed" json) }

let of_list json = List.map (J.list (J.field "accounts" json)) ~f:of_json
let find accounts id = List.find accounts ~f:(fun a -> String.equal a.id id)
let is_bank a = String.equal a.kind "asset" || String.equal a.kind "liability"
let is_category a = (String.equal a.kind "expense" || String.equal a.kind "income") && Option.is_none a.role

let matches query a =
  let query = String.lowercase (String.strip query) in
  String.is_empty query || String.is_substring (String.lowercase a.name) ~substring:query
