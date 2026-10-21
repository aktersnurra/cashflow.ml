(* MCP over JSON-RPC: the protocol handling follows kcal's lib/mcp.ml; the tools are generated
   from the Commands table. Mistakes the model can fix come back as tool results with isError,
   which clients show to the model, rather than as protocol errors. *)
module Analytics = Cashflow_core.Analytics

let log_src = Logs.Src.create "cashflow.mcp" ~doc:"MCP JSON-RPC handling"
module Log = (val Logs.src_log log_src : Logs.LOG)

let response ?(id = `Null) result = `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]
let rpc_error ?(id = `Null) code message = `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]) ]

let stateless_protocol_version = "2026-07-28"
let session_protocol_versions = [ "2025-06-18"; "2025-03-26" ]
let server_info = `Assoc [ ("name", `String "cashflow"); ("version", `String "dev") ]

let complete fields = `Assoc (("resultType", `String "complete") :: fields)
let cacheable fields = complete (("ttlMs", `Int 300_000) :: ("cacheScope", `String "public") :: fields)

let text_result value =
  complete [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String (Yojson.Safe.to_string value)) ] ]); ("structuredContent", value) ]

let tool_error message = complete [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String message) ] ]); ("isError", `Bool true) ]

let annotations (c : Commands.command) =
  `Assoc [ ("readOnlyHint", `Bool (c.scope = Read)); ("destructiveHint", `Bool (List.mem c.name [ "reverse_transaction"; "correct_transaction"; "undo_import"; "close_account" ])); ("idempotentHint", `Bool true) ]

let tool (c : Commands.command) =
  `Assoc [ ("name", `String c.name); ("description", `String c.description); ("inputSchema", Commands.schema c); ("annotations", annotations c) ]

let tools () = List.filter (fun (c : Commands.command) -> c.mcp) (Api.all ())

let discover_result =
  cacheable
    [ ("supportedVersions", `List (List.map (fun v -> `String v) (stateless_protocol_version :: session_protocol_versions)));
      ("capabilities", `Assoc [ ("tools", `Assoc []) ]); ("_meta", `Assoc [ ("io.modelcontextprotocol/serverInfo", server_info) ]) ]

let call service access ~scopes ~request_id name arguments =
  match List.find_opt (fun (c : Commands.command) -> c.name = name) (tools ()) with
  | None -> Ok (response ~id:request_id (tool_error ("unknown tool " ^ name)))
  | Some c when not (Commands.allowed ~scopes access c) -> Error `Forbidden
  | Some c -> (
      match Commands.execute service access ~scopes c arguments with
      | Ok value -> Ok (response ~id:request_id (text_result value))
      | Error (`Forbidden reason) -> Ok (response ~id:request_id (tool_error ("not allowed: " ^ reason)))
      | Error error -> Ok (response ~id:request_id (tool_error (Import.error_to_string error)))
      | exception Sql.Sql_error message ->
          Log.err (fun m -> m "tool=%s storage error: %s" name message);
          Ok (rpc_error ~id:request_id (-32603) "Internal error"))

let handle service access ~scopes request =
  match request with
  | `Assoc fields -> (
      let request_id = Option.value (List.assoc_opt "id" fields) ~default:`Null in
      let valid = List.assoc_opt "jsonrpc" fields = Some (`String "2.0") in
      let params = match List.assoc_opt "params" fields with Some (`Assoc p) -> Some p | _ -> None in
      match List.assoc_opt "method" fields with
      | Some (`String "server/discover") when valid -> Ok (response ~id:request_id discover_result)
      | Some (`String "initialize") when valid ->
          let requested = Option.bind params (fun p -> match List.assoc_opt "protocolVersion" p with Some (`String v) -> Some v | _ -> None) in
          let version = match requested with Some v when List.mem v session_protocol_versions -> v | _ -> List.hd session_protocol_versions in
          Ok
            (response ~id:request_id
               (`Assoc
                  [ ("protocolVersion", `String version); ("capabilities", `Assoc [ ("tools", `Assoc []) ]); ("serverInfo", server_info);
                    ( "instructions",
                      `String
                        "A double-entry ledger of personal and household finances. Amounts are decimal strings in the account's \
                         currency. Writes need an idempotency_key you choose; retrying with the same key is safe. Transactions are never \
                         edited: confirm with the user before reversing or correcting one. Analytics tools take a scope: me, household or \
                         family." ) ]))
      | Some (`String "ping") when valid -> Ok (response ~id:request_id (`Assoc []))
      | Some (`String "tools/list") when valid -> Ok (response ~id:request_id (cacheable [ ("tools", `List (List.map tool (tools ()))) ]))
      | Some (`String "tools/call") when valid -> (
          match params with
          | Some p -> (
              match List.assoc_opt "name" p with
              | Some (`String name) -> call service access ~scopes ~request_id name (Option.value (List.assoc_opt "arguments" p) ~default:(`Assoc []))
              | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
          | None -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String m) when valid && String.starts_with ~prefix:"notifications/" m -> Ok `Null
      | Some (`String _) when valid -> Ok (rpc_error ~id:request_id (-32601) "Method not found")
      | _ -> Ok (rpc_error ~id:request_id (-32600) "Invalid Request"))
  | _ -> Ok (rpc_error (-32700) "Parse error")
