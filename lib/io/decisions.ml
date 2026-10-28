(* OpenRouter's decisions API (alpha), for a model such as TypeSafe's Jev that answers typed
   questions with calibrated probabilities instead of writing text: choosing a category for a bank
   row. A record of functions, like [Llm.t], so tests can pass a fake. Every request asks for
   zero-data-retention routing and no data collection; the OpenRouter account's own ZDR guardrail
   enforces the same whatever the alpha endpoint does with these fields. *)

type question = {
  id : string;  (** the caller's key; not shown to the model *)
  instructions : string;
  options : (string * string) list;  (** option name, and what it means *)
}

type answer = { choice : string; probability : float  (** of [choice], 0 to 1 *) }

type t = { model : string; choose : state:string -> question list -> ((string * answer) list, string) result }

let request_body ~model ~state questions =
  `Assoc
    [ ("model", `String model); ("state", `String state);
      ( "questions",
        `Assoc
          (List.map
             (fun q ->
               ( q.id,
                 `Assoc
                   [ ("type", `String "choice"); ("instructions", `String q.instructions);
                     ("criteria", `Assoc (List.map (fun (name, meaning) -> (name, `String meaning)) q.options)) ] ))
             questions) );
      ("provider", `Assoc [ ("zdr", `Bool true); ("data_collection", `String "deny") ]) ]

(* Each answer's choice and the probability given to it, for the questions asked; anything else
   is dropped. *)
let answers_of_response questions text =
  match Yojson.Safe.from_string text with
  | exception Yojson.Json_error e -> Error e
  | json -> (
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "answers" fields with
          | Some (`Assoc answers) ->
              Ok
                (List.filter_map
                   (fun q ->
                     match List.assoc_opt q.id answers with
                     | Some (`Assoc a) -> (
                         match List.assoc_opt "choice" a with
                         | Some (`String choice) when List.mem_assoc choice q.options ->
                             let probability =
                               match List.assoc_opt "probabilities" a with
                               | Some (`Assoc ps) -> (
                                   match List.assoc_opt choice ps with Some (`Float p) -> p | Some (`Int p) -> float_of_int p | _ -> 0.)
                               | _ -> 0.
                             in
                             Some (q.id, { choice; probability = Float.min 1. (Float.max 0. probability) })
                         | _ -> None)
                     | _ -> None)
                   questions)
          | _ -> Error (match List.assoc_opt "error" fields with Some e -> Yojson.Safe.to_string e | None -> "no answers"))
      | _ -> Error "unexpected response")

let openrouter ?app_url ~(post : Https.post) ~api_key ~model () =
  let choose ~state questions =
    match questions with
    | [] -> Ok []
    | _ -> (
        match
          post ~uri:"https://openrouter.ai/api/alpha/decisions"
            ~headers:([ ("authorization", "Bearer " ^ api_key); ("content-type", "application/json") ] @ Llm.attribution_headers ?app_url ())
            ~body:(Yojson.Safe.to_string (request_body ~model ~state questions))
        with
        | Error e -> Error e
        | Ok text -> answers_of_response questions text)
  in
  { model; choose }
