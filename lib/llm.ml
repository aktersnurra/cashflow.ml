(* A chat-completion client. It's a record of functions so tests can pass a fake; the real one
   talks to OpenRouter, asking providers not to retain or train on the data. *)
type t = { model : string; complete : system:string -> user:string -> (string, string) result }

let openrouter ~(post : Https.post) ~api_key ~model =
  let complete ~system ~user =
    let body =
      `Assoc
        [ ("model", `String model);
          ("messages", `List [ `Assoc [ ("role", `String "system"); ("content", `String system) ]; `Assoc [ ("role", `String "user"); ("content", `String user) ] ]);
          ("temperature", `Float 0.2); ("provider", `Assoc [ ("data_collection", `String "deny") ]) ]
    in
    match
      post ~uri:"https://openrouter.ai/api/v1/chat/completions"
        ~headers:[ ("authorization", "Bearer " ^ api_key); ("content-type", "application/json"); ("x-title", "cashflow") ]
        ~body:(Yojson.Safe.to_string body)
    with
    | Error e -> Error e
    | Ok text -> (
        match Yojson.Safe.from_string text with
        | `Assoc fields -> (
            match List.assoc_opt "choices" fields with
            | Some (`List (`Assoc choice :: _)) -> (
                match List.assoc_opt "message" choice with
                | Some (`Assoc message) -> (match List.assoc_opt "content" message with Some (`String content) -> Ok content | _ -> Error "no content")
                | _ -> Error "no message")
            | _ -> Error "no choices")
        | _ -> Error "unexpected response"
        | exception Yojson.Json_error e -> Error e)
  in
  { model; complete }

(* The JSON in a reply, which models sometimes wrap in a code fence. *)
let json_of_reply reply =
  let text = String.trim reply in
  let text =
    if String.starts_with ~prefix:"```" text then
      let lines = String.split_on_char '\n' text in
      String.concat "\n" (List.filter (fun l -> not (String.starts_with ~prefix:"```" (String.trim l))) lines)
    else text
  in
  try Some (Yojson.Safe.from_string text) with Yojson.Json_error _ -> None
