(* A small CSV reader: quoted fields with doubled quotes, CRLF or LF line ends, a UTF-8 byte order
   mark, and the delimiter guessed from the first line. Bank exports that aren't valid UTF-8 are
   taken to be Latin-1 (Windows-1252 in practice) and converted. *)
type table = string list list

let is_valid_utf8 s = String.is_valid_utf_8 s

let latin1_to_utf8 s =
  let buffer = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_utf_8_uchar buffer (Uchar.of_int (Char.code c))) s;
  Buffer.contents buffer

let normalize_encoding s =
  let s = if String.starts_with ~prefix:"\xEF\xBB\xBF" s then String.sub s 3 (String.length s - 3) else s in
  if is_valid_utf8 s then s else latin1_to_utf8 s

let guess_delimiter s =
  let first_line = match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s in
  let count c = String.fold_left (fun n x -> if x = c then n + 1 else n) 0 first_line in
  List.fold_left (fun (best, n) c -> let m = count c in if m > n then (c, m) else (best, n)) (',', 0) [ ';'; '\t'; ',' ] |> fst

let parse ?delimiter text =
  let text = normalize_encoding text in
  let delimiter = match delimiter with Some d -> d | None -> guess_delimiter text in
  let rows = ref [] and row = ref [] and field = Buffer.create 64 in
  let end_field () = row := Buffer.contents field :: !row; Buffer.clear field in
  let end_row () =
    end_field ();
    let fields = List.rev !row in
    if not (List.for_all (fun f -> String.trim f = "") fields) then rows := fields :: !rows;
    row := []
  in
  let length = String.length text in
  let rec plain i =
    if i >= length then (if Buffer.length field > 0 || !row <> [] then end_row ())
    else
      match text.[i] with
      | '"' when Buffer.length field = 0 -> quoted (i + 1)
      | c when c = delimiter -> end_field (); plain (i + 1)
      | '\r' when i + 1 < length && text.[i + 1] = '\n' -> end_row (); plain (i + 2)
      | '\n' | '\r' -> end_row (); plain (i + 1)
      | c -> Buffer.add_char field c; plain (i + 1)
  and quoted i =
    if i >= length then end_row ()
    else
      match text.[i] with
      | '"' when i + 1 < length && text.[i + 1] = '"' -> Buffer.add_char field '"'; quoted (i + 2)
      | '"' -> plain (i + 1)
      | c -> Buffer.add_char field c; quoted (i + 1)
  in
  plain 0;
  List.rev !rows
