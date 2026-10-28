type error = Invalid_link | Unsafe_address | Unreachable_site | Redirect_limit | Oversized_body | Unsupported_image | Too_small_image
let error_code = function
  | Invalid_link -> "invalid_link" | Unsafe_address -> "unsafe_address"
  | Unreachable_site -> "unreachable_site" | Redirect_limit -> "redirect_limit"
  | Oversized_body -> "oversized_body" | Unsupported_image -> "unsupported_image"
  | Too_small_image -> "too_small_image"
let error_message = function
  | Invalid_link -> "That link isn't valid" | Unsafe_address -> "That address isn't safe"
  | Unreachable_site -> "That site couldn't be reached" | Redirect_limit -> "Too many redirects"
  | Oversized_body -> "That image is too large" | Unsupported_image -> "That link isn't an image"
  | Too_small_image -> "That image is too small"

type response = { status : int; headers : (string * string) list; body : string }
type network = { with_request : 'a. (unit -> ('a, error) result) -> ('a, error) result;
                 resolve : host:string -> (string list, error) result;
                 get : uri:Uri.t -> address:string -> max_bytes:int -> (response, error) result }
type image = { bytes : string; content_type : string; width : int; height : int; source_url : string }
let bytes x = x.bytes
let content_type x = x.content_type
let width x = x.width
let height x = x.height
let source_url x = x.source_url

let public_address address =
  match Ipaddr.of_string address with
  | Error _ -> false
  | Ok ip ->
      let raw = Ipaddr.to_octets ip in
      let b i = Char.code raw.[i] in
      let v4 s offset =
        let a = Char.code s.[offset] and c = Char.code s.[offset + 1] in
        not (a = 0 || a = 10 || a = 127 || a >= 224 ||
             (a = 169 && c = 254) || (a = 172 && c >= 16 && c <= 31) ||
             (a = 192 && (c = 168 || c = 0)) ||
             (a = 100 && c >= 64 && c <= 127) ||
             (a = 198 && (c = 18 || c = 19)) ||
             (a = 192 && c = 0) || (a = 198 && c = 51) ||
             (a = 203 && c = 0) || a = 255)
      in
      if String.length raw = 4 then Ipaddr.is_global ip && v4 raw 0
      else if String.length raw <> 16 then false
      else if String.sub raw 0 12 = String.make 10 '\000' ^ "\255\255" then v4 raw 12
      else Ipaddr.is_global ip && b 0 >= 0x20 && b 0 < 0x40 &&
        not (b 0 = 0x20 && ((b 1 = 0x01 && (b 2 = 0x0d && b 3 = 0xb8 || b 2 = 0)) || b 1 = 0x02))

let hostname host =
  let labels = String.split_on_char '.' host in
  String.length host <= 253 && List.length labels >= 2 &&
  not (String.ends_with ~suffix:"." host) &&
  (match Ipaddr.of_string host with Ok _ -> false | Error _ -> true) &&
  List.for_all (fun label ->
    let n = String.length label in
    n > 0 && n <= 63 && label.[0] <> '-' && label.[n-1] <> '-' &&
    String.for_all (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true | _ -> false) label) labels &&
  let last = List.hd (List.rev labels) in
  String.length last >= 2 && String.exists (function 'a'..'z' | 'A'..'Z' -> true | _ -> false) last &&
  not (List.mem (String.lowercase_ascii last) [ "local"; "localhost"; "internal"; "test"; "invalid"; "example" ])

let parse link =
  let uri = Uri.of_string link in
  let uri = if Uri.scheme uri = None && not (String.starts_with ~prefix:"//" link)
    then Uri.of_string ("https://" ^ link) else uri in
  match Uri.scheme uri, Uri.host uri with
  | Some "https", Some host when hostname host &&
      (Uri.port uri = None || Uri.port uri = Some 443) &&
      Uri.userinfo uri = None && Uri.fragment uri = None -> Ok uri
  | _ -> Error Invalid_link

let header name headers =
  List.find_map (fun (key, value) -> if String.equal (String.lowercase_ascii key) name then Some value else None) headers
let u8 s i = Char.code s.[i]
let be16 s i = u8 s i * 256 + u8 s (i+1)
let be32 s i = (be16 s i * 65536) + be16 s (i+2)
let le16 s i = u8 s i + u8 s (i+1) * 256
let le24 s i = u8 s i + u8 s (i+1) * 256 + u8 s (i+2) * 65536
let le32 s i = le16 s i + le16 s (i+2) * 65536
let dimensions s =
  let n = String.length s in
  if n >= 45 && String.starts_with ~prefix:"\137PNG\r\n\026\n\000\000\000\rIHDR" s && String.sub s (n-8) 4 = "IEND" then
    Some ("image/png", be32 s 16, be32 s 20)
  else if n >= 6 && String.starts_with ~prefix:"\000\000\001\000" s then
    let count = le16 s 4 in
    if count = 0 || count > (n - 6) / 16 then None else
    let directory_end = 6 + count * 16 in
    let rec entries index best =
      if index = count then Option.map (fun (w, h) -> "image/x-icon", w, h) best else
      let entry = 6 + index * 16 in
      let size = le32 s (entry + 8) and offset = le32 s (entry + 12) in
      if size = 0 || offset < directory_end || offset > n || size > n - offset then None else
      let width = if u8 s entry = 0 then 256 else u8 s entry in
      let height = if u8 s (entry + 1) = 0 then 256 else u8 s (entry + 1) in
      (* A usable entry must beat every entry that is too small on either axis. *)
      let best = match best with
        | Some (w, h) when min w h >= min width height -> best
        | _ -> Some (width, height) in
      entries (index + 1) best
    in entries 0 None
  else if n >= 30 && String.starts_with ~prefix:"RIFF" s && le32 s 4 = n - 8 && String.sub s 8 4 = "WEBP" then
    (match String.sub s 12 4 with
     | "VP8X" -> Some ("image/webp", 1 + le24 s 24, 1 + le24 s 27)
     | "VP8L" -> Some ("image/webp", 1 + (u8 s 21 lor ((u8 s 22 land 63) lsl 8)),
                                1 + ((u8 s 22 lsr 6) lor (u8 s 23 lsl 2) lor ((u8 s 24 land 15) lsl 10)))
     | "VP8 " when n >= 30 && String.sub s 23 3 = "\157\001\042" ->
         Some ("image/webp", le16 s 26 land 0x3fff, le16 s 28 land 0x3fff)
     | _ -> None)
  else if n >= 4 && String.starts_with ~prefix:"\255\216" s && String.ends_with ~suffix:"\255\217" s then
    let rec jpeg i =
      if i + 4 > n then None
      else if u8 s i <> 255 then jpeg (i+1)
      else let marker = u8 s (i+1) in
        if marker = 0xd9 || marker = 0xda then None
        else if marker = 0xd8 || marker = 0x01 then jpeg (i+2)
        else let size = be16 s (i+2) in
          if size < 2 || i + 2 + size > n then None
          else if List.mem marker [ 0xc0; 0xc1; 0xc2; 0xc3; 0xc5; 0xc6; 0xc7; 0xc9; 0xca; 0xcb; 0xcd; 0xce; 0xcf ] && size >= 7
          then Some ("image/jpeg", be16 s (i+7), be16 s (i+5))
          else jpeg (i+2+size)
    in jpeg 2
  else None

let svg = "image/svg+xml"

(* A raster image as it is, or an SVG in its cleaned form. *)
let identify body =
  match dimensions body with
  | Some (kind, width, height) -> Some (body, kind, width, height)
  | None -> Option.map (fun (bytes, width, height) -> (bytes, svg, width, height)) (Svg_logo.clean body)

(* What stored bytes are, if they are still fit to serve: a raster image, or an SVG already in its cleaned form. *)
let stored bytes =
  match dimensions bytes with
  | Some _ as raster -> raster
  | None -> (
      match Svg_logo.clean bytes with
      | Some (clean, width, height) when String.equal clean bytes -> Some (svg, width, height)
      | _ -> None)

let attribute name tag =
  let pattern = Str.regexp_case_fold
    ("[ \t\r\n]" ^ name ^ "[ \t\r\n]*=[ \t\r\n]*\\(\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\|\\([^ \t\r\n>]+\\)\\)") in
  try
    ignore (Str.search_forward pattern tag 0);
    let rec first = function
      | [] -> None
      | group :: rest -> (try Some (Str.matched_group group tag) with Not_found -> first rest)
    in first [ 2; 3; 4 ]
  with Not_found -> None

let html_words s = Str.split (Str.regexp "[ \t\r\n]+") s

let icons uri html =
  let pattern = Str.regexp_case_fold "<link[ \t\r\n][^>]*>" in
  let rec collect pos acc =
    try
      ignore (Str.search_forward pattern html pos);
      let tag = Str.matched_string html and next = Str.match_end () in
      let acc = match attribute "rel" tag, attribute "href" tag with
        | Some rel, Some href ->
            let words = html_words (String.lowercase_ascii rel) in
            let apple = List.mem "apple-touch-icon" words in
            let icon = List.mem "icon" words in
            if not (apple || icon) then acc else
            let size = match attribute "sizes" tag with
              | None -> 0
              | Some s -> List.fold_left (fun largest token ->
                  let square = try Scanf.sscanf token "%dx%d" (fun w h -> if w = h then w else 0)
                    with _ -> 0 in
                  max largest square) 0 (html_words s) in
            (* An icon without a usable size is still a candidate, after the sized ones; its bytes are checked. *)
            if not apple && size > 0 && size < 64 then acc
            else ((if apple then 0 else 1), -size, Uri.resolve "https" uri (Uri.of_string href)) :: acc
        | _ -> acc in
      collect next acc
    with Not_found -> acc
  in
  let ranked = List.stable_sort (fun (a,b,_) (c,d,_) -> compare (a,b) (c,d)) (List.rev (collect 0 [])) in
  let seen = Hashtbl.create 8 in
  let rec select remaining acc = function
    | [] -> List.rev acc
    | _ when remaining = 0 -> List.rev acc
    | (_, _, url) :: rest ->
        (match parse (Uri.to_string url) with
         | Error _ -> select remaining acc rest
         | Ok url ->
             let key = Uri.to_string url in
             if Hashtbl.mem seen key then select remaining acc rest else (
               Hashtbl.add seen key ();
               select (remaining - 1) (url :: acc) rest)) in
  let selected = select 4 [] ranked in
  let fallback = Uri.with_path uri "/favicon.ico" in
  if Hashtbl.mem seen (Uri.to_string fallback) then selected else selected @ [fallback]

let fetch network link =
  let ( let* ) result f = Result.bind result f in
  let* initial = parse link in
  (* Share six bounded request scopes across the page, candidates and every redirect. *)
  let requests_remaining = ref 6 in
  (* [prefix_ok]: a web page is only read for its <head>, so one bigger than [max_bytes] is cut off there instead of
     refused; an image bigger than its limit is still refused. *)
  let rec request ?(prefix_ok = false) redirects max_bytes uri =
    if !requests_remaining = 0 then Error Unreachable_site else
    let () = decr requests_remaining in
    let* uri, response = network.with_request (fun () ->
      let* uri = parse (Uri.to_string uri) in
      let host = Option.get (Uri.host uri) in
      let* addresses = network.resolve ~host in
      if addresses = [] then Error Unreachable_site
      else if not (List.for_all public_address addresses) then Error Unsafe_address
      else
        (* Every address was checked public above. IPv4 goes first and a failed connection falls through to the next
           address, as a browser or curl would: a host whose first record is IPv6 must not fail where IPv4 works.
           Only "couldn't connect" moves on; any answer, even an error status, is final. *)
        let v6 address = String.contains address ':' in
        let ordered = List.filter (fun a -> not (v6 a)) addresses @ List.filter v6 addresses in
        let rec connect = function
          | [] -> Error Unreachable_site
          | [ address ] -> network.get ~uri ~address ~max_bytes
          | address :: rest -> (
              match network.get ~uri ~address ~max_bytes with Error Unreachable_site -> connect rest | result -> result)
        in
        let* response = connect ordered in
        if String.length response.body <= max_bytes then Ok (uri, response)
        else if prefix_ok then Ok (uri, { response with body = String.sub response.body 0 max_bytes })
        else Error Oversized_body) in
    if List.mem response.status [ 301; 302; 303; 307; 308 ] then
      if redirects = 3 then Error Redirect_limit
      else (match header "location" response.headers with
        | None -> Error Unreachable_site
        | Some target -> request ~prefix_ok (redirects+1) max_bytes (Uri.resolve "https" uri (Uri.of_string target)))
    else if response.status < 200 || response.status >= 300 then Error Unreachable_site
    else Ok (uri, response)
  in
  let verified_image final response =
    if String.length response.body > 262_144 then Error Oversized_body
    else match identify response.body with
    | None -> Error Unsupported_image
    | Some (bytes, kind, width, height) ->
        (* An SVG scales, so only a raster image has a minimum size. *)
        if kind <> svg && (width < 32 || height < 32) then Error Too_small_image
        else Ok { bytes; content_type = kind; width; height; source_url = Uri.to_string final }
  in
  let image uri =
    let* final, response = request 0 262_144 uri in
    verified_image final response
  in
  let path = Uri.path initial in
  let direct = List.exists (fun suffix -> String.ends_with ~suffix (String.lowercase_ascii path))
    [ ".png"; ".jpg"; ".jpeg"; ".webp"; ".ico"; ".svg" ] in
  if direct then image initial
  else
    let* page_uri, page = request ~prefix_ok:true 0 524_288 initial in
    match identify page.body with
    | Some _ -> verified_image page_uri page
    | None ->
        let rec candidates last = function
          | [] -> Error last
          | uri :: rest -> (match image uri with Ok _ as success -> success | Error error -> candidates error rest)
        in candidates Unsupported_image (icons page_uri page.body)
