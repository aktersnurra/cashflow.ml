(* Light or dark, or whatever the system says. The choice is per device: it lives in
   localStorage and is applied as data-theme on <html>, which style.css reads. *)
open! Core
open Js_of_ocaml

type t = System | Light | Dark [@@deriving equal]

let key = Js.string "cashflow.theme"
let to_string = function System -> "system" | Light -> "light" | Dark -> "dark"
let of_string = function "light" -> Light | "dark" -> Dark | _ -> System

(* Storage can be missing or throw (private windows, blocked site data); then it is System. *)
let load () =
  try
    Js.Optdef.case Dom_html.window##.localStorage (fun () -> System) (fun storage ->
        Js.Opt.case (storage##getItem key) (fun () -> System) (fun v -> of_string (Js.to_string v)))
  with _ -> System

let apply t =
  let root = Dom_html.document##.documentElement in
  match t with System -> root##removeAttribute (Js.string "data-theme") | t -> root##setAttribute (Js.string "data-theme") (Js.string (to_string t))

let save t =
  (try Js.Optdef.iter Dom_html.window##.localStorage (fun storage -> storage##setItem key (Js.string (to_string t))) with _ -> ());
  apply t

let system_is_dark () =
  let query : < matches : bool Js.t Js.readonly_prop > Js.t =
    Js.Unsafe.meth_call Dom_html.window "matchMedia" [| Js.Unsafe.inject (Js.string "(prefers-color-scheme: dark)") |]
  in
  Js.to_bool query##.matches

(* The sidebar button flips what is on screen, so from System it picks the opposite of the system. *)
let flipped = function Light -> Dark | Dark -> Light | System -> if system_is_dark () then Light else Dark
