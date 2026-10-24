module Access = Cashflow_core.Access
module Owner = Cashflow_core.Owner

type view = { key : string; logo : string option; can_retry : bool }
type error = [ `Forbidden of string | `Invalid of string | `Fetch of Logo_fetch.error ]
val error_code : error -> string
val error_message : error -> string
val set : Service.t -> Access.t -> owner:Owner.t -> merchant_key:string -> link:string -> (view, error) result
type clear_mode = None_selected | Automatic
val clear : Service.t -> Access.t -> owner:Owner.t -> merchant_key:string -> mode:clear_mode -> (view, error) result
val resolve : Service.t -> Access.t -> owner:Owner.t -> (string * string) list -> unit
val view : Service.t -> Access.t -> owner:Owner.t -> merchant_key:string -> view
val blob : Service.t -> Access.t -> owner:Owner.t -> id:int -> Merchant_logo_store.blob option
