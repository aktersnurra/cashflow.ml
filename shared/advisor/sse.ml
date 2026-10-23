type frame = { id : string option; event : string option; data : string }
type error = Frame_too_large | Incomplete_frame
type t = { max : int; line : Buffer.t; mutable bytes : int;
  mutable cr : bool; mutable id : string option; mutable event : string option;
  mutable data : string list; mutable failed : bool }
let create ~max_frame_bytes =
  if max_frame_bytes <= 0 then invalid_arg "max_frame_bytes";
  { max = max_frame_bytes; line = Buffer.create 128; bytes = 0; cr = false;
    id = None; event = None; data = []; failed = false }
let feed t chunk =
  let frames = ref [] in
  let line () =
    let s = Buffer.contents t.line in
    Buffer.clear t.line;
    if String.length s = 0 then (
      (match t.data with [] -> () | data ->
        frames := { id = t.id; event = t.event; data = String.concat "\n" (List.rev data) } :: !frames);
      t.bytes <- 0; t.id <- None; t.event <- None; t.data <- [])
    else if s.[0] <> ':' then (
      let key, value = match String.index_opt s ':' with
        | None -> s, ""
        | Some n -> let start = if n + 1 < String.length s && s.[n+1] = ' ' then n+2 else n+1 in
          String.sub s 0 n, String.sub s start (String.length s - start) in
      match key with
      | "data" -> t.data <- value :: t.data
      | "id" when not (String.contains value '\000') -> t.id <- Some value
      | "event" -> t.event <- Some value
      | _ -> ())
  in
  String.iter (fun c ->
    if not t.failed then (
      if t.cr && c = '\n' then (
        t.cr <- false;
        if t.bytes > 0 then (t.bytes <- t.bytes + 1; if t.bytes > t.max then t.failed <- true))
      else (
        t.cr <- false; t.bytes <- t.bytes + 1;
        if t.bytes > t.max then t.failed <- true
        else if c = '\r' then (line (); t.cr <- true)
        else if c = '\n' then line ()
        else Buffer.add_char t.line c))) chunk;
  if t.failed then Error Frame_too_large else Ok (List.rev !frames)
let finish t =
  if t.failed then Error Frame_too_large
  else if t.bytes <> 0 then Error Incomplete_frame else Ok ()
let encode (f : frame) =
  let meta key = function None -> "" | Some s ->
    if String.contains s '\r' || String.contains s '\n' || String.contains s '\000' then invalid_arg "SSE metadata";
    key ^ ": " ^ s ^ "\n" in
  if String.contains f.data '\r' then invalid_arg "SSE data CR";
  meta "id" f.id ^ meta "event" f.event ^
  String.concat "" (List.map (fun s -> "data: " ^ s ^ "\n") (String.split_on_char '\n' f.data)) ^ "\n"
