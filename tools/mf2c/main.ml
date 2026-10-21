(* mf2c en.mf2 sv.mf2 ... > msg.ml — the first file is the reference locale and the fallback. *)
let () =
  let files = List.tl (Array.to_list Sys.argv) in
  let read path = In_channel.with_open_bin path In_channel.input_all in
  match
    List.map (fun path -> { Mf2.locale = Filename.remove_extension (Filename.basename path); messages = Mf2.parse_file ~file:path (read path) }) files
  with
  | files -> print_string (Mf2.generate files)
  | exception Mf2.Error reason -> prerr_endline ("mf2c: " ^ reason); exit 1
