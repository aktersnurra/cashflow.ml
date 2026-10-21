(* A compiler for the subset of Unicode MessageFormat 2 the UI needs: text with placeholders
   ({$name}, {$name :integer}), declarations (.input {$n :integer}), and selection
   (.match $n with variants such as `one {{...}}` and `* {{...}}`).

   Messages live in one file per locale, `id = message`, continued on indented lines. The output is
   an OCaml module with one function per message whose labelled arguments are the message's
   variables, typed by their function: :integer is int, :number float, :currency money, :date date,
   and anything else a string. *)

type part = Text of string | Var of { name : string; fn : string option }
type key = Exact of string | Category of string | Star
type variant = { keys : key list; pattern : part list }
type message = Simple of part list | Select of { declarations : (string * string) list; selectors : string list; variants : variant list }

exception Error of string

let fail fmt = Printf.ksprintf (fun m -> raise (Error m)) fmt

(* Parsing *)

let is_name_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '-' || c = '.'

let parse_name s i =
  let j = ref i in
  while !j < String.length s && is_name_char s.[!j] do incr j done;
  if !j = i then fail "expected a name at %d in %S" i s;
  (String.sub s i (!j - i), !j)

let skip_spaces s i =
  let j = ref i in
  while !j < String.length s && (s.[!j] = ' ' || s.[!j] = '\n' || s.[!j] = '\t' || s.[!j] = '\r') do incr j done;
  !j

(* {$name} or {$name :fn ...options} *)
let parse_expression s i =
  let i = skip_spaces s (i + 1) in
  if i >= String.length s || s.[i] <> '$' then fail "expected $variable in placeholder in %S" s;
  let name, i = parse_name s (i + 1) in
  let i = skip_spaces s i in
  let fn, i =
    if i < String.length s && s.[i] = ':' then
      let fn, i = parse_name s (i + 1) in
      (Some fn, i)
    else (None, i)
  in
  match String.index_from_opt s i '}' with Some close -> ((name, fn), close + 1) | None -> fail "unclosed placeholder in %S" s

let parse_pattern s =
  let parts = ref [] and text = Buffer.create 32 in
  let flush () = if Buffer.length text > 0 then (parts := Text (Buffer.contents text) :: !parts; Buffer.clear text) in
  let rec go i =
    if i >= String.length s then ()
    else
      match s.[i] with
      | '\\' when i + 1 < String.length s -> Buffer.add_char text s.[i + 1]; go (i + 2)
      | '{' ->
          flush ();
          let (name, fn), next = parse_expression s i in
          parts := Var { name; fn } :: !parts;
          go next
      | '}' -> fail "unmatched } in %S" s
      | c -> Buffer.add_char text c; go (i + 1)
  in
  go 0;
  flush ();
  List.rev !parts

(* A quoted pattern {{...}} starting at i; returns its contents and the index after it. *)
let quoted s i =
  if not (i + 1 < String.length s && s.[i] = '{' && s.[i + 1] = '{') then fail "expected {{ at %d" i;
  let rec find j depth =
    if j + 1 >= String.length s then fail "unclosed {{ in %S" s
    else if s.[j] = '\\' then find (j + 2) depth
    else if s.[j] = '{' then find (j + 1) (depth + 1)
    else if s.[j] = '}' && s.[j + 1] = '}' && depth = 0 then j
    else if s.[j] = '}' then find (j + 1) (depth - 1)
    else find (j + 1) depth
  in
  let close = find (i + 2) 0 in
  (String.sub s (i + 2) (close - i - 2), close + 2)

let plural_categories = [ "zero"; "one"; "two"; "few"; "many"; "other" ]

let parse_message text =
  let s = String.trim text in
  if not (String.length s > 0 && s.[0] = '.') then Simple (parse_pattern s)
  else
    let rec declarations i acc =
      let i = skip_spaces s i in
      if i + 6 <= String.length s && String.sub s i 6 = ".input" then
        let i = skip_spaces s (i + 6) in
        let (name, fn), i = parse_expression s i in
        declarations i ((name, Option.value fn ~default:"string") :: acc)
      else (List.rev acc, i)
    in
    let declarations, i = declarations 0 [] in
    if not (i + 6 <= String.length s && String.sub s i 6 = ".match") then fail "expected .match after the declarations in %S" s;
    let rec selectors i acc =
      let i = skip_spaces s i in
      if i < String.length s && s.[i] = '$' then
        let name, i = parse_name s (i + 1) in
        selectors i (name :: acc)
      else (List.rev acc, i)
    in
    let selectors, i = selectors (i + 6) [] in
    if selectors = [] then fail ".match needs at least one selector in %S" s;
    let rec variants i acc =
      let i = skip_spaces s i in
      if i >= String.length s then List.rev acc
      else
        let rec keys i acc =
          let i = skip_spaces s i in
          if i < String.length s && s.[i] = '{' then (List.rev acc, i)
          else if s.[i] = '*' then keys (i + 1) (Star :: acc)
          else
            let name, i = parse_name s i in
            keys i ((if List.mem name plural_categories then Category name else Exact name) :: acc)
        in
        let keys, i = keys i [] in
        if List.length keys <> List.length selectors then fail "a variant has %d keys but there are %d selectors in %S" (List.length keys) (List.length selectors) s;
        let body, i = quoted s i in
        variants i ({ keys; pattern = parse_pattern body } :: acc)
    in
    Select { declarations; selectors; variants = variants i [] }

(* A locale file: `id = message`, with indented continuation lines and # comments. *)
let parse_file ~file text =
  let lines = String.split_on_char '\n' text in
  let entries = ref [] and current = ref None in
  let close () = Option.iter (fun (id, buf, line) -> entries := (id, Buffer.contents buf, line) :: !entries) !current; current := None in
  List.iteri
    (fun n line ->
      let trimmed = String.trim line in
      if trimmed = "" || trimmed.[0] = '#' then ()
      else if line.[0] = ' ' || line.[0] = '\t' then
        match !current with
        | Some (_, buf, _) -> Buffer.add_char buf '\n'; Buffer.add_string buf trimmed
        | None -> fail "%s:%d: continuation line without a message" file (n + 1)
      else
        match String.index_opt line '=' with
        | Some eq ->
            close ();
            let id = String.trim (String.sub line 0 eq) in
            let buf = Buffer.create 64 in
            Buffer.add_string buf (String.trim (String.sub line (eq + 1) (String.length line - eq - 1)));
            current := Some (id, buf, n + 1)
        | None -> fail "%s:%d: expected `id = message`" file (n + 1))
    lines;
  close ();
  List.rev_map
    (fun (id, body, line) ->
      match parse_message body with
      | message -> (id, message)
      | exception Error reason -> fail "%s:%d: %s: %s" file line id reason)
    !entries

(* Checking *)

let variables message =
  let of_parts parts = List.filter_map (function Var { name; fn } -> Some (name, fn) | Text _ -> None) parts in
  let raw =
    match message with
    | Simple parts -> of_parts parts
    | Select { declarations; variants; _ } ->
        List.map (fun (n, f) -> (n, Some f)) declarations @ List.concat_map (fun v -> of_parts v.pattern) variants
  in
  (* A variable's type comes from its first annotation; it must not change. *)
  let table = Hashtbl.create 8 in
  List.iter
    (fun (name, fn) ->
      match Hashtbl.find_opt table name, fn with
      | None, fn -> Hashtbl.replace table name fn
      | Some None, Some fn -> Hashtbl.replace table name (Some fn)
      | Some (Some a), Some b when a <> b -> fail "variable $%s is used as both :%s and :%s" name a b
      | Some _, _ -> ())
    raw;
  Hashtbl.fold (fun name fn acc -> (name, Option.value fn ~default:"string") :: acc) table [] |> List.sort compare

let check_message id message =
  match message with
  | Simple _ -> ()
  | Select { declarations; selectors; variants } ->
      List.iter (fun s -> if not (List.mem_assoc s declarations) then fail "%s: selector $%s has no .input declaration" id s) selectors;
      if not (List.exists (fun v -> List.for_all (( = ) Star) v.keys) variants) then fail "%s: a .match needs a catch-all variant (*)" id;
      List.iteri
        (fun i selector ->
          if List.assoc selector declarations = "integer" then
            if not (List.exists (fun v -> match List.nth v.keys i with Category "one" | Exact "1" -> true | _ -> false) variants) then
              fail "%s: selector $%s needs a variant for the plural category one" id selector)
        selectors

type locale_file = { locale : string; messages : (string * message) list }

let check files =
  match files with
  | [] -> fail "no locale files"
  | reference :: others ->
      List.iter (fun f -> List.iter (fun (id, m) -> check_message (f.locale ^ ":" ^ id) m) f.messages) files;
      let ids f = List.sort compare (List.map fst f.messages) in
      List.iter
        (fun f ->
          let missing = List.filter (fun id -> not (List.mem id (ids f))) (ids reference) in
          let extra = List.filter (fun id -> not (List.mem id (ids reference))) (ids f) in
          if missing <> [] then fail "%s is missing %s" f.locale (String.concat ", " missing);
          if extra <> [] then fail "%s has messages %s is missing: %s" f.locale reference.locale (String.concat ", " extra);
          List.iter
            (fun (id, m) ->
              let mine = variables m and theirs = variables (List.assoc id reference.messages) in
              if mine <> theirs then
                fail "%s: %s and %s disagree on its variables (%s vs %s)" id f.locale reference.locale
                  (String.concat ", " (List.map (fun (n, t) -> n ^ ":" ^ t) mine))
                  (String.concat ", " (List.map (fun (n, t) -> n ^ ":" ^ t) theirs)))
            f.messages)
        others

(* Code generation *)

let ocaml_name id =
  String.map (fun c -> if c = '.' || c = '-' then '_' else c) id |> fun s ->
  if List.mem s [ "and"; "or"; "to"; "type"; "open"; "end"; "in"; "of"; "do"; "done"; "new"; "method"; "class"; "val" ] then s ^ "_" else s

let ocaml_type = function
  | "integer" -> "int"
  | "number" -> "float"
  | "currency" -> "Mf2_runtime.money"
  | "date" -> "Mf2_runtime.date"
  | _ -> "string"

let format_call fn var =
  match fn with
  | "integer" -> Printf.sprintf "Mf2_runtime.format_integer locale %s" var
  | "number" -> Printf.sprintf "Mf2_runtime.format_number locale %s" var
  | "currency" -> Printf.sprintf "Mf2_runtime.format_money locale %s" var
  | "date" -> Printf.sprintf "Mf2_runtime.format_date locale %s" var
  | _ -> var

let pattern_expression types parts =
  match parts with
  | [] -> "\"\""
  | parts ->
      "String.concat \"\" ["
      ^ String.concat "; "
          (List.map
             (function
               | Text t -> Printf.sprintf "%S" t
               | Var { name; _ } -> format_call (List.assoc name types) (ocaml_name name))
             parts)
      ^ "]"

let key_condition types selector key =
  let var = ocaml_name selector in
  match key, List.assoc selector types with
  | Star, _ -> None
  | Exact value, "integer" -> Some (Printf.sprintf "%s = %s" var value)
  | Exact value, _ -> Some (Printf.sprintf "%s = %S" var value)
  | Category c, "integer" -> Some (Printf.sprintf "Mf2_runtime.plural_category locale %s = %S" var c)
  | Category c, _ -> Some (Printf.sprintf "%s = %S" var c)

(* Variants are tried most specific first, as MF2 orders them: exact values, then categories, then *. *)
let specificity v = List.fold_left (fun acc k -> acc + match k with Exact _ -> 100 | Category _ -> 10 | Star -> 0) 0 v.keys

let body_expression types = function
  | Simple parts -> pattern_expression types parts
  | Select { selectors; variants; _ } ->
      let ordered = List.stable_sort (fun a b -> compare (specificity b) (specificity a)) variants in
      let branches =
        List.map
          (fun v ->
            let conditions = List.filter_map (fun (s, k) -> key_condition types s k) (List.combine selectors v.keys) in
            (conditions, pattern_expression types v.pattern))
          ordered
      in
      let rec chain = function
        | [] -> "assert false"
        | ([], body) :: _ -> body
        | (conditions, body) :: rest -> Printf.sprintf "if %s then %s else %s" (String.concat " && " conditions) body (chain rest)
      in
      chain branches

let generate files =
  check files;
  let buffer = Buffer.create 4096 in
  let add fmt = Printf.bprintf buffer fmt in
  add "(* Generated by mf2c from locales/*.mf2. Do not edit. *)\n\n";
  add "let locales = [ %s ]\n\n" (String.concat "; " (List.map (fun f -> Printf.sprintf "%S" f.locale) files));
  let reference = List.hd files in
  List.iter
    (fun (id, reference_message) ->
      let types = variables reference_message in
      let labels = String.concat " " (List.map (fun (n, t) -> Printf.sprintf "~(%s : %s)" (ocaml_name n) (ocaml_type t)) types) in
      add "let %s %s locale =\n  ignore locale;\n  match locale with\n" (ocaml_name id) labels;
      List.iter
        (fun f -> if f != reference then add "  | %S -> %s\n" f.locale (body_expression types (List.assoc id f.messages)))
        files;
      add "  | _ -> %s\n\n" (body_expression types reference_message))
    reference.messages;
  Buffer.contents buffer
