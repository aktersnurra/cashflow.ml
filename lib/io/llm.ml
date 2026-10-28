(* A chat-completion client. It's a record of functions so tests can pass a fake; the real one
   talks to OpenRouter, restricting routing to allowed zero-data-retention endpoints. With a
   [schema] (a name and a JSON Schema) the reply is structured output: JSON that the provider
   guarantees matches it. *)
type t = { model : string; complete : ?schema:string * Yojson.Safe.t -> system:string -> user:string -> unit -> (string, string) result }

(* Tells OpenRouter which app is calling, so its dashboard can group the usage. [app_url] is where the app
   is served; without one only the name is sent. *)
let attribution_headers ?app_url () = ("x-title", "cashflow") :: (match app_url with Some url -> [ ("http-referer", url) ] | None -> [])

let openrouter ?app_url ~(post : Https.post) ~api_key ~model ~provider_only () =
  let complete ?schema ~system ~user () =
    if provider_only = [] then Error "OpenRouter provider allowlist must not be empty"
    else
    let body =
      `Assoc
        ([ ("model", `String model);
          ("messages", `List [ `Assoc [ ("role", `String "system"); ("content", `String system) ]; `Assoc [ ("role", `String "user"); ("content", `String user) ] ]);
          ("temperature", `Float 0.2);
          (* Financial data must never fall back to providers with weaker retention policies; with a
             schema, only to endpoints that honour it. *)
          ("provider", `Assoc ([ ("only", `List (List.map (fun provider -> `String provider) provider_only));
                                 ("zdr", `Bool true); ("data_collection", `String "deny") ]
                               @ if Option.is_some schema then [ ("require_parameters", `Bool true) ] else [])) ]
        @
        match schema with
        | None -> []
        | Some (name, schema) ->
            [ ("response_format", `Assoc [ ("type", `String "json_schema"); ("json_schema", `Assoc [ ("name", `String name); ("strict", `Bool true); ("schema", schema) ]) ]) ])
    in
    match
      post ~uri:"https://openrouter.ai/api/v1/chat/completions"
        ~headers:([ ("authorization", "Bearer " ^ api_key); ("content-type", "application/json") ] @ attribution_headers ?app_url ())
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
