(* Every command the outside world can call: the core use cases and the attention feed. *)
let all () = Commands.base @ Attention.commands
let find name = List.find_opt (fun (c : Commands.command) -> c.name = name) (all ())
