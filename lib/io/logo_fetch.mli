type error = Invalid_link | Unsafe_address | Unreachable_site | Redirect_limit | Oversized_body | Unsupported_image | Too_small_image
val error_code : error -> string
val error_message : error -> string

type response = { status : int; headers : (string * string) list; body : string }
type network = {
  with_request : 'a. (unit -> ('a, error) result) -> ('a, error) result;
  resolve : host:string -> (string list, error) result;
  get : uri:Uri.t -> address:string -> max_bytes:int -> (response, error) result;
}
type image
val bytes : image -> string
val content_type : image -> string
val width : image -> int
val height : image -> int
val source_url : image -> string
val dimensions : string -> (string * int * int) option
val hostname : string -> bool
val fetch : network -> string -> (image, error) result
