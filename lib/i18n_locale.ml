(* Locales the UI is translated into. *)
let supported = [ "en"; "sv" ]
let default = "en"
let negotiate requested = List.find_opt (fun locale -> List.mem locale supported) requested |> Option.value ~default
