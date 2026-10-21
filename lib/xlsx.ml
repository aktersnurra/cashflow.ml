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

(* "AB12" -> column 27 (0-based). *)
let column reference =
  String.fold_left (fun (n, letters) c -> if letters && c >= 'A' && c <= 'Z' then ((n * 26) + Char.code c - 64, true) else (n, false)) (0, true) reference |> fst |> fun n -> n - 1

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
  (* Rows and cells can be missing (empty ones are left out), so place them by their references. *)
  let numbered =
    List.mapi
      (fun i row ->
        let index = match Option.bind (attribute "r" row) int_of_string_opt with Some r -> r - 1 | None -> i in
        let cells =
          List.mapi (fun j c -> ((match attribute "r" c with Some r -> column r | None -> j), cell strings c)) (children "c" row)
        in
        let width = List.fold_left (fun w (col, _) -> max w (col + 1)) 0 cells in
        (index, List.init width (fun col -> Option.value (List.assoc_opt col cells) ~default:"")))
      rows
  in
  let height = List.fold_left (fun h (r, _) -> max h (r + 1)) 0 numbered in
  if height > 100_000 then Error "the sheet has more than 100 000 rows" else
  Ok (List.init height (fun r -> Option.value (List.assoc_opt r numbered) ~default:[]))
