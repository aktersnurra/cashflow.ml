(* Reading the server's JSON without ceremony: a missing field reads as a neutral value. *)
open! Core

type t = Yojson.Safe.t

let field name (json : t) = match json with `Assoc fields -> Option.value (List.Assoc.find fields ~equal:String.equal name) ~default:`Null | _ -> `Null
let path names json = List.fold names ~init:json ~f:(fun json name -> field name json)
let str ?(default = "") json = match json with `String s -> s | `Int n -> Int.to_string n | _ -> default
let str_opt json = match json with `String s -> Some s | _ -> None
let int ?(default = 0) json = match json with `Int n -> n | _ -> default
let bool json = match json with `Bool b -> b | _ -> false
let list json = match json with `List items -> items | _ -> []
let is_null = function `Null -> true | _ -> false
let ( $ ) json name = field name json
