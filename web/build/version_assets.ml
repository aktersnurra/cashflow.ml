(* Cache keys follow asset bytes, so existing immutable browser caches cannot
   retain an older interface after the non-cacheable entry document changes. *)
let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let replace_reference source reference replacement =
  let reference_length = String.length reference in
  let rec find offset =
    if offset + reference_length > String.length source then
      invalid_arg ("Missing entry-document asset reference: " ^ reference)
    else if String.sub source offset reference_length = reference then
      String.sub source 0 offset ^ replacement
      ^ String.sub source (offset + reference_length) (String.length source - offset - reference_length)
    else find (offset + 1)
  in
  find 0

let () =
  if Array.length Sys.argv <> 4 then invalid_arg "Expected template, stylesheet and script paths";
  let source = read_file Sys.argv.(1) in
  let version asset = Digest.to_hex (Digest.file asset) in
  let source = replace_reference source "\"/assets/style.css\""
      ("\"/assets/style.css?v=" ^ version Sys.argv.(2) ^ "\"") in
  let source = replace_reference source "\"/assets/main.bc.js\""
      ("\"/assets/main.bc.js?v=" ^ version Sys.argv.(3) ^ "\"") in
  print_string source
