(* An SVG logo, cleaned before it is stored. Shown through <img> an SVG can run nothing, but opened at its own URL it
   is a document of this origin, so only drawing survives: allowed elements, no event handlers, no reference to
   anything outside the file. Anything it can't parse as such is refused, never passed through. *)

let svg_ns = "http://www.w3.org/2000/svg"
let xlink_ns = "http://www.w3.org/1999/xlink"

let elements =
  [ "svg"; "g"; "defs"; "symbol"; "use"; "title"; "desc"; "style";
    "path"; "rect"; "circle"; "ellipse"; "line"; "polyline"; "polygon"; "text"; "tspan";
    "linearGradient"; "radialGradient"; "stop"; "clipPath"; "mask"; "pattern"; "marker";
    "filter"; "feBlend"; "feColorMatrix"; "feComponentTransfer"; "feComposite"; "feDropShadow"; "feFlood";
    "feFuncA"; "feFuncB"; "feFuncG"; "feFuncR"; "feGaussianBlur"; "feMerge"; "feMergeNode"; "feOffset" ]

(* Every url(...) in a value must point inside the file (url(#id)), and nothing else in CSS may load an image. *)
let local_references value =
  let value = String.lowercase_ascii value in
  let url = Str.regexp "url([ '\"]*" in
  let rec from i =
    match Str.search_forward url value i with
    | exception Not_found -> true
    | _ -> let j = Str.match_end () in j < String.length value && value.[j] = '#' && from j
  in
  from 0 && not (Cashflow_core.Str_compat.contains ~sub:"image(" value || Cashflow_core.Str_compat.contains ~sub:"image-set(" value)

let local_href value = String.starts_with ~prefix:"#" (String.trim value)

(* The attribute as stored, or None to drop it. xlink:href becomes href, which every current browser reads. *)
let attribute ((uri, name), value) =
  let plain = uri = "" in
  if (plain && name = "href") || (uri = xlink_ns && name = "href") then
    if local_href value then Some (("", "href"), value) else None
  else if not plain then None
  else if String.starts_with ~prefix:"on" (String.lowercase_ascii name) then None
  else if not (local_references value) then None
  else Some (("", name), value)

let number text =
  let text = String.trim text in
  let text = if String.ends_with ~suffix:"px" text then String.sub text 0 (String.length text - 2) else text in
  match float_of_string_opt text with Some f when Float.is_finite f && f > 0. -> Some f | _ -> None

(* Its nominal size, from viewBox or else width and height; a logo with neither has no shape to show. *)
let size attributes =
  let find name = List.find_map (fun ((uri, n), v) -> if uri = "" && n = name then Some v else None) attributes in
  let whole f = int_of_float (Float.ceil f) in
  let from_view_box =
    Option.bind (find "viewBox") (fun box ->
      match List.filter (( <> ) "") (String.split_on_char ' ' (String.map (function ',' | '\t' | '\n' | '\r' -> ' ' | c -> c) box)) with
      | [ _; _; w; h ] -> (match number w, number h with Some w, Some h -> Some (whole w, whole h) | _ -> None)
      | _ -> None)
  in
  match from_view_box with
  | Some _ as size -> size
  | None -> (
      match Option.bind (find "width") number, Option.bind (find "height") number with
      | Some w, Some h -> Some (whole w, whole h)
      | _ -> None)

exception Refused

let clean bytes =
  try
    (* An entity is never expanded: the document is refused. An unbound prefix gets a namespace nothing allows. *)
    let input = Xmlm.make_input ~strip:false ~entity:(fun _ -> raise Refused) ~ns:(fun _ -> Some "urn:unbound")
        (`String (0, bytes)) in
    let buffer = Buffer.create (String.length bytes) in
    let output = Xmlm.make_output ~decl:false (`Buffer buffer) in
    Xmlm.output output (`Dtd None);
    (* [skipping] counts open elements inside a dropped one; [style] holds a <style>'s text until it can be checked. *)
    let rec next ~depth ~skipping ~style ~size_so_far =
      match Xmlm.input input with
      | `Dtd _ -> next ~depth ~skipping ~style ~size_so_far
      | `El_start ((uri, name), attributes) ->
          if depth = 0 && not ((uri = svg_ns || uri = "") && name = "svg") then raise Refused;
          if skipping > 0 || Option.is_some style || not ((uri = svg_ns || uri = "") && List.mem name elements) then
            next ~depth:(depth + 1) ~skipping:(skipping + 1) ~style ~size_so_far
          else
            let kept = List.filter_map attribute attributes in
            let kept = if depth = 0 then kept @ [ ((Xmlm.ns_xmlns, "xmlns"), svg_ns) ] else kept in
            let size_so_far = if depth = 0 then size attributes else size_so_far in
            if name = "style" then next ~depth:(depth + 1) ~skipping ~style:(Some (kept, Buffer.create 256)) ~size_so_far
            else (
              Xmlm.output output (`El_start ((svg_ns, name), kept));
              next ~depth:(depth + 1) ~skipping ~style ~size_so_far)
      | `El_end ->
          let depth = depth - 1 in
          if skipping > 0 then next ~depth ~skipping:(skipping - 1) ~style ~size_so_far
          else (
            (match style with
             | Some (kept, text) ->
                 let text = Buffer.contents text in
                 let lower = String.lowercase_ascii text in
                 if local_references text && not (Cashflow_core.Str_compat.contains ~sub:"@import" lower) then (
                   Xmlm.output output (`El_start ((svg_ns, "style"), kept));
                   if text <> "" then Xmlm.output output (`Data text);
                   Xmlm.output output `El_end)
             | None -> Xmlm.output output `El_end);
            if depth = 0 then size_so_far else next ~depth ~skipping ~style:None ~size_so_far)
      | `Data text ->
          (match skipping, style with
           | 0, Some (_, buffer) -> Buffer.add_string buffer text
           | 0, None -> Xmlm.output output (`Data text)
           | _ -> ());
          next ~depth ~skipping ~style ~size_so_far
    in
    match next ~depth:0 ~skipping:0 ~style:None ~size_so_far:None with
    | None -> None
    | Some (width, height) -> if Xmlm.eoi input then Some (Buffer.contents buffer, width, height) else None
  with Refused | Xmlm.Error _ | Invalid_argument _ -> None
