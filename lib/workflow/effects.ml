type handler = { handle : 'a. 'a Effect_ops.t -> 'a }
type _ Effect.t += Operation : 'a Effect_ops.t -> 'a Effect.t
let perform op = Effect.perform (Operation op)
let run handler f =
  Effect.Deep.try_with f () {
    effc = (fun (type a) (eff : a Effect.t) -> match eff with
      | Operation op -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
          (* Only interpreter failures discontinue the suspended workflow. Exceptions
             from the resumed computation must never resume this continuation twice. *)
          match handler.handle op with
          | value -> Effect.Deep.continue k value
          | exception exn ->
              let backtrace = Printexc.get_raw_backtrace () in
              Effect.Deep.discontinue_with_backtrace k exn backtrace)
      | _ -> None)
  }
