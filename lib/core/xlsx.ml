(* The cells of a spreadsheet's first sheet, from an .xlsx file (a zip of XML parts), as the same
   table of strings a CSV gives, so every statement format reads both. Numbers come out as the
   decimal they stand for at Excel's own precision of 15 significant digits, so 12345.67 stays
   12345.67 however the file stored it. Dates stored as day numbers stay numbers; the formats
   read those (Statement_format.parse_date). *)

(* A zip member is refused above this, so a small file can't unpack into a huge one. *)
let max_part_bytes = 20 * 1024 * 1024

let is_xlsx content = Zipc.string_has_magic content

type tree = Element of string * (string * string) list * tree list | Text of string

let parse_xml text =
  try
    let input = Xmlm.make_input ~strip:false (`String (0, text)) in
    let el ((_, name), attributes) children = Element (name, List.map (fun ((_, key), value) -> (key, value)) attributes, children) in
    Ok (snd (Xmlm.input_doc_tree ~el ~data:(fun d -> Text d) input))
  with Xmlm.Error ((line, column), e) -> Error (Printf.sprintf "XML error at %d:%d: %s" line column (Xmlm.error_message e))

let children name = function Element (_, _, kids) -> List.filter (function Element (n, _, _) -> n = name | Text _ -> false) kids | Text _ -> []
let child name tree = List.nth_opt (children name tree) 0
let attribute key = function Element (_, attributes, _) -> List.assoc_opt key attributes | Text _ -> None
let rec text = function Text t -> t | Element (_, _, kids) -> String.concat "" (List.map text kids)

let ( let* ) = Result.bind

let part zip path =
  match Zipc.find path zip with
  | None -> Error ("the workbook has no " ^ path)
  | Some member -> (
      match Zipc.Member.kind member with
      | Zipc.Member.Dir -> Error (path ^ " is a directory")
      | Zipc.Member.File file ->
          if Zipc.File.decompressed_size file > max_part_bytes then Error (path ^ " is too large")
          else Result.map_error (fun e -> path ^ ": " ^ e) (Zipc.File.to_binary_string file))

(* The first sheet's part, through the workbook's relationships. *)
let first_sheet zip =
  let* workbook = Result.bind (part zip "xl/workbook.xml") parse_xml in
  let* id =
    match Option.bind (child "sheets" workbook) (child "sheet") with
    | Some sheet -> Option.to_result ~none:"the first sheet has no relationship id" (attribute "id" sheet)
    | None -> Error "the workbook has no sheets"
  in
  let* rels = Result.bind (part zip "xl/_rels/workbook.xml.rels") parse_xml in
  match List.find_opt (fun r -> attribute "Id" r = Some id) (children "Relationship" rels) |> Fun.flip Option.bind (attribute "Target") with
  | None -> Error "the first sheet's part isn't listed"
  | Some target -> if String.starts_with ~prefix:"/" target then Ok (String.sub target 1 (String.length target - 1)) else Ok ("xl/" ^ target)

let shared_strings zip =
  match Zipc.find "xl/sharedStrings.xml" zip with
  | None -> Ok [||]
  | Some _ ->
      let* tree = Result.bind (part zip "xl/sharedStrings.xml") parse_xml in
      (* A rich-text string is several runs; phonetic hints (rPh) are not part of the text. *)
      let string si = String.concat "" (List.map text (children "t" si @ List.concat_map (children "t") (children "r" si))) in
      Ok (Array.of_list (List.map string (children "si" tree)))

let max_rows = 100_000
(* Excel's column limit, plus a budget for the dense strings table used by statement readers. *)
let max_columns = 16_384
let max_cells = 1_000_000

let row_index reference =
  if reference = "" || not (String.for_all (fun c -> c >= '0' && c <= '9') reference) then Error "invalid row reference"
  else
    match int_of_string_opt reference with
    | Some n when n > 0 && n <= max_rows -> Ok (n - 1)
    | _ -> Error "row reference is outside the supported sheet"

(* "AB12" -> column 27 (0-based). Bound each step before multiplication can overflow. *)
let column reference =
  let rec letters i n =
    if i < String.length reference && reference.[i] >= 'A' && reference.[i] <= 'Z' then
      let digit = Char.code reference.[i] - Char.code 'A' + 1 in
      if n > (max_columns - digit) / 26 then Error "the sheet has too many columns"
      else letters (i + 1) ((n * 26) + digit)
    else if i = 0 then Error "invalid cell reference"
    else
      let* _ = row_index (String.sub reference i (String.length reference - i)) in
      Ok (n - 1)
  in
  letters 0 0

let number text =
  match float_of_string_opt (String.trim text) with
  | Some f when Float.is_integer f && Float.abs f < 1e15 -> Printf.sprintf "%.0f" f
  | Some f -> Printf.sprintf "%.15g" f
  | None -> text

let cell strings c =
  let value = Option.map text (child "v" c) in
  match attribute "t" c, value with
  | Some "s", Some index -> (match int_of_string_opt (String.trim index) with Some i when i >= 0 && i < Array.length strings -> strings.(i) | _ -> "")
  | Some "inlineStr", _ -> Option.fold ~none:"" ~some:text (child "is" c)
  | Some "e", _ -> ""
  | Some "str", Some v -> v
  | Some "b", Some v -> if String.trim v = "1" then "TRUE" else "FALSE"
  | _, Some v -> number v
  | _, None -> ""

let table content =
  let* zip = Zipc.of_binary_string content in
  let* sheet_path = first_sheet zip in
  let* strings = shared_strings zip in
  let* sheet = Result.bind (part zip sheet_path) parse_xml in
  let rows = match child "sheetData" sheet with Some data -> children "row" data | None -> [] in
  (* Rows and cells can be missing. Validate positions and the total expansion before allocating
     each dense row; XML size alone doesn't bound the space occupied by absent cells. *)
  let rec read_cells j width acc = function
    | [] -> Ok (width, acc)
    | c :: rest ->
        if j >= max_columns then Error "the sheet has too many columns"
        else
          let* col = match attribute "r" c with Some r -> column r | None -> Ok j in
          read_cells (j + 1) (max width (col + 1)) ((col, cell strings c) :: acc) rest
  in
  let rec read_rows i expanded height acc = function
    | [] -> Ok (height, acc)
    | row :: rest ->
        if i >= max_rows then Error "the sheet has more than 100 000 rows"
        else
          let* index = match attribute "r" row with Some r -> row_index r | None -> Ok i in
          let* width, cells = read_cells 0 0 [] (children "c" row) in
          if width > max_cells - expanded then Error "the sheet expands to too many cells"
          else
            let values = Array.make width "" in
            List.iter (fun (col, value) -> values.(col) <- value) cells;
            read_rows (i + 1) (expanded + width) (max height (index + 1)) ((index, Array.to_list values) :: acc) rest
  in
  let* height, numbered = read_rows 0 0 0 [] rows in
  let table = Array.make height [] in
  List.iter (fun (index, values) -> table.(index) <- values) numbered;
  Ok (Array.to_list table)
