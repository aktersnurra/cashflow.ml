module Api = Cashflow_io.Api
module Commands = Cashflow_io.Commands
module Llm = Cashflow_io.Llm
module Logo_fetch = Cashflow_io.Logo_fetch
module Merchant_logo_store = Cashflow_io.Merchant_logo_store
module Merchant_logos = Cashflow_io.Merchant_logos
module Sql = Cashflow_io.Sql
module Owner = Cashflow_core.Owner
module Str_compat = Cashflow_core.Str_compat

open Test_service_support

let png =
  let path = if Sys.file_exists "test/fixtures/logo_64x48.png" then "test/fixtures/logo_64x48.png" else "fixtures/logo_64x48.png" in
  let ch = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ch) (fun () -> really_input_string ch (in_channel_length ch))

let network calls =
  Logo_fetch.{ with_request = (fun f -> f ()); resolve = (fun ~host:_ -> Ok ["8.8.8.8"]);
    get = (fun ~uri ~address:_ ~max_bytes:_ -> incr calls;
      if Uri.path uri = "/" then Ok {status=200; headers=[]; body=""}
      else Ok {status=200; headers=[]; body=png}) }

let manual () =
  let calls = ref 0 in
  let db = Store_sqlite.open_memory () in
  let service = Service.make ~logo_network:(network calls) db in
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
  let owner = Access.personal alice in
  let save link = Result.get_ok (Merchant_logos.set service alice ~owner ~merchant_key:"shop" ~link) in
  let first = save "https://shop.com/image.png" in
  Alcotest.(check bool) "logo" true (Option.is_some first.logo);
  let before = Merchant_logo_store.get db owner "shop" in
  let blobs () = Sql.(run db (stmt "SELECT COUNT(*) FROM logos" nil (one (row (int @-> nil) Fun.id)))) in
  let associations () = Sql.(run db (stmt "SELECT COUNT(*) FROM merchant_logos" nil (one (row (int @-> nil) Fun.id)))) in
  let blob_count = blobs () and association_count = associations () in
  let refused service link =
    Alcotest.(check bool) "refused" true (Result.is_error (Merchant_logos.set service alice ~owner ~merchant_key:"shop" ~link));
    Alcotest.(check bool) "complete persisted state unchanged" true (before = Merchant_logo_store.get db owner "shop");
    Alcotest.(check int) "no extra blob" blob_count (blobs ());
    Alcotest.(check int) "no extra association" association_count (associations ()) in
  refused service "http://shop.com";
  let failed_calls = ref 0 in
  let failing = Logo_fetch.{ (network failed_calls) with get = (fun ~uri:_ ~address:_ ~max_bytes:_ -> incr failed_calls; Error Unreachable_site) } in
  refused (Service.make ~logo_network:failing db) "https://shop.com/different.png";
  Alcotest.(check int) "network failure reached injected transport" 1 !failed_calls;
  ignore (save "shop.com");
  Alcotest.(check int) "dedup" 1 (Sql.(run db (stmt "SELECT COUNT(*) FROM logos" nil (one (row (int @-> nil) Fun.id)))));
  Alcotest.(check string) "true blob storage" "blob" (Sql.(run db (stmt "SELECT typeof(bytes) FROM logos LIMIT 1" nil (one (row (text @-> nil) Fun.id)))));
  Alcotest.(check bool) "isolated" true ((Merchant_logos.view service bob ~owner:(Access.personal bob) ~merchant_key:"shop").logo = None);
  Alcotest.(check bool) "forbidden" true (Result.is_error (Merchant_logos.set service bob ~owner ~merchant_key:"shop" ~link:"shop.com"))

let chain () =
  let calls = ref 0 and prompts = ref [] and clock = ref now in
  let db = Store_sqlite.open_memory () in
  let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system ~user () ->
    let prefix = "Reply JSON only: " in
    Alcotest.(check bool) "prompt introduces JSON example" true (Str_compat.contains ~sub:prefix system);
    let at = String.index system ':' + 2 in
    let example = String.sub system at (String.length system - at) in
    let example = String.trim example |> fun s -> if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1) else s in
    let expected = `Assoc ["merchants", `List [`Assoc ["name", `String "name"; "domain", `String "domain.tld"; "confidence", `Int 70]]] in
    Alcotest.(check bool) "outgoing prompt example parses as merchant schema" true (Yojson.Safe.from_string example = expected);
    prompts := user :: !prompts;
    Ok "{\"merchants\":[{\"name\":\"ICA\",\"domain\":\"shop.com\",\"confidence\":80}]}" )} in
  let service = Service.make ~now:(fun () -> !clock) ~assistant ~logo_network:(network calls) db in
  let access = sign_in service "alice" in
  let owner = Access.personal access in
  Merchant_logos.resolve service access ~owner ["ica", "ICA"; "ica", "ICA"];
  Alcotest.(check int) "one prompt" 1 (List.length !prompts);
  Alcotest.(check bool) "image resolved" true (Option.is_some (Merchant_logos.view service access ~owner ~merchant_key:"ica").logo);
  Merchant_logos.resolve service access ~owner ["ica", "ICA"];
  Alcotest.(check int) "cache hit" 1 (List.length !prompts);
  ignore (Merchant_logos.clear service access ~owner ~merchant_key:"ica" ~mode:Merchant_logos.None_selected);
  Merchant_logos.resolve service access ~owner ["ica", "ICA"];
  Alcotest.(check bool) "none wins" true ((Merchant_logos.view service access ~owner ~merchant_key:"ica").logo = None);
  Alcotest.(check int) "no prompt" 1 (List.length !prompts);
  ignore (Result.get_ok (Merchant_logos.set service access ~owner ~merchant_key:"ica" ~link:"shop.com/pic.png"));
  Merchant_logos.resolve service access ~owner ["ica", "ICA"];
  Alcotest.(check int) "user beats model" 1 (List.length !prompts);
  ignore (Result.get_ok (Merchant_logos.clear service access ~owner ~merchant_key:"ica" ~mode:Merchant_logos.Automatic));
  Alcotest.(check int) "no name available, no new prompt" 1 (List.length !prompts)

let automatic_uses_original_display_name () =
  let db = Store_sqlite.open_memory () in
  let prompts = ref [] in
  let assistant = Llm.{ model="fake"; complete=(fun ?schema:_ ~system:_ ~user () ->
    prompts := user :: !prompts;
    Ok {|{"merchants":[{"name":"7-Eleven","domain":"seven-eleven.com","confidence":90}]}|}) } in
  let service = Service.make ~assistant ~logo_network:(network (ref 0)) db in
  let access = sign_in service "alice" in
  let owner = Access.personal access in
  let checking = open_checking service access "Checking" in
  let groceries = account_named service access "Groceries" in
  let transfer = { (entry "7-Eleven purchase" ~from_account:checking ~to_account:groceries 100) with
    memo = { Transfer.empty_memo with description="7-Eleven purchase"; counterparty=Some "7-Eleven" } } in
  ignore (ok (Service.add_entry service access transfer));
  let merchant_key = "-eleven" in
  ignore (Result.get_ok (Merchant_logos.set service access ~owner ~merchant_key ~link:"shop.com/pic.png"));
  let result = Result.get_ok (Merchant_logos.clear service access ~owner ~merchant_key ~mode:Merchant_logos.Automatic) in
  Alcotest.(check (list string)) "original name only" [{|{"merchants":["7-Eleven"]}|}] !prompts;
  Alcotest.(check bool) "logo resolves after automatic clear" true (Option.is_some result.logo)

let automatic_without_display_name () =
  let db = Store_sqlite.open_memory () in
  let calls = ref 0 in
  let assistant = Llm.{ model="fake"; complete=(fun ?schema:_ ~system:_ ~user:_ () -> incr calls; Ok {|{"merchants":[]}|}) } in
  let service = Service.make ~assistant ~logo_network:(network (ref 0)) db in
  let access = sign_in service "alice" in
  let owner = Access.personal access in
  let bob = sign_in service "bob" in
  let checking = open_checking service bob "Bob checking" in
  let groceries = account_named service bob "Groceries" in
  ignore (ok (Service.add_entry service bob { (entry "unknown" ~from_account:checking ~to_account:groceries 100) with
    memo = {Transfer.empty_memo with counterparty=Some "UNKNOWN"} }));
  ignore (Result.get_ok (Merchant_logos.set service access ~owner ~merchant_key:"unknown" ~link:"shop.com/pic.png"));
  ignore (Result.get_ok (Merchant_logos.clear service access ~owner ~merchant_key:"unknown" ~mode:Merchant_logos.Automatic));
  Alcotest.(check int) "no invented name prompt" 0 !calls;
  Alcotest.(check bool) "override removed without caching unknown" true (Merchant_logo_store.get db owner "unknown" = None)

let resolver_cases () =
  let clock = ref now and calls = ref 0 and prompts = ref [] in
  let response = ref "" and during = ref (fun () -> ()) in
  let db = Store_sqlite.open_memory () in
  let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system:_ ~user () ->
    Alcotest.(check bool) "LLM outside SQL transaction" false (Sql.in_transaction db);
    prompts := user :: !prompts; !during (); Ok !response)} in
  let service = Service.make ~now:(fun () -> !clock) ~assistant ~logo_network:(network calls) db in
  let access = sign_in service "alice" in
  let owner = Access.personal access in
  let resolve key name = Merchant_logos.resolve service access ~owner [key, name] in
  response := {|{"merchants":[{"name":"BAD","domain":"127.0.0.1","confidence":90},{"name":"BAD","domain":"shop.com/path","confidence":90}]}|};
  resolve "bad" "BAD";
  Alcotest.(check int) "unsafe proposal not fetched" 0 !calls;
  resolve "bad" "BAD";
  Alcotest.(check int) "unknown cached" 1 (List.length !prompts);
  response := {|{"merchants":[{"name":"FAIL","domain":"bad.com","confidence":69}]}|};
  resolve "low" "FAIL";
  Alcotest.(check int) "low confidence" 0 !calls;
  response := "not json";
  resolve "malformed" "MALFORMED";
  Alcotest.(check int) "malformed" 0 !calls;
  response := {|{"merchants":[{"name":"RETRY","domain":"retry.com","confidence":70}]}|};
  let failing = Logo_fetch.{ (network calls) with get = (fun ~uri:_ ~address:_ ~max_bytes:_ -> incr calls; Error Unreachable_site) } in
  let service_fail = Service.make ~now:(fun () -> !clock) ~assistant ~logo_network:failing db in
  Merchant_logos.resolve service_fail access ~owner ["retry", "RETRY"];
  let count = List.length !prompts in
  clock := Option.get (Ptime.add_span now (Ptime.Span.of_int_s (30*86400-1)));
  Merchant_logos.resolve service_fail access ~owner ["retry", "RETRY"];
  Alcotest.(check int) "before thirty days" count (List.length !prompts);
  clock := Option.get (Ptime.add_span now (Ptime.Span.of_int_s (30*86400)));
  Merchant_logos.resolve service_fail access ~owner ["retry", "RETRY"];
  Alcotest.(check int) "at thirty days" (count+1) (List.length !prompts);
  Alcotest.(check string) "names only" {|{"merchants":["RETRY"]}|} (List.hd !prompts);
  Alcotest.(check int) "logged calls" (List.length !prompts)
    (Sql.(run db (stmt "SELECT COUNT(*) FROM llm_calls WHERE purpose='logos'" nil (one (row (int @-> nil) Fun.id)))));
  let off = Service.make ~logo_network:(network calls) db in
  let before = !calls in
  Merchant_logos.resolve off access ~owner ["new", "NEW"];
  Alcotest.(check int) "off does not fetch" before !calls;
  response := {|{"merchants":[{"name":"EDIT","domain":"shop.com","confidence":80}]}|};
  during := (fun () -> during := (fun () -> ()); ignore (Merchant_logos.clear service access ~owner ~merchant_key:"edit" ~mode:Merchant_logos.None_selected));
  resolve "edit" "EDIT";
  Alcotest.(check bool) "edit not overwritten" true ((Merchant_logos.view service access ~owner ~merchant_key:"edit").logo = None);
  Alcotest.(check bool) "user choice retained" true ((Option.get (Merchant_logo_store.get db owner "edit")).source = Merchant_logo_store.None_selected)

let batching () =
 let db = Store_sqlite.open_memory () and sizes = ref [] in
 let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system:_ ~user () ->
   sizes := (match Yojson.Safe.from_string user with `Assoc [_, `List names] -> List.length names | _ -> 0) :: !sizes;
   Ok {|{"merchants":[]}|})} in
 let service = Service.make ~assistant ~logo_network:(network (ref 0)) db in
 let access = sign_in service "batch" in
 Merchant_logos.resolve service access ~owner:(Access.personal access)
   (List.init 121 (fun i -> string_of_int i, "NAME" ^ string_of_int i));
 Alcotest.(check (list int)) "60 bounded" [1;60;60] !sizes

let association () =
  let db = Store_sqlite.open_memory () in
  let service = Service.make ~logo_network:(network (ref 0)) db in
  let alice = sign_in service "alice" in
  let bob = sign_in service "bob" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let owner = Owner.Household household.id in
  let alice = Service.access_for service (Access.user alice) in
  ignore (Result.get_ok (Merchant_logos.set service alice ~owner ~merchant_key:"shop" ~link:"shop.com/pic.png"));
  let id = Option.get (Merchant_logo_store.get db owner "shop") |> fun state -> Option.get state.logo_id in
  Alcotest.(check bool) "associated blob" true (Option.is_some (Merchant_logos.blob service alice ~owner ~id));
  Alcotest.(check bool) "other owner cannot read deduplicated blob" true (Merchant_logos.blob service bob ~owner:(Access.personal bob) ~id = None);
  Alcotest.(check bool) "nonmember" true (Merchant_logos.blob service bob ~owner ~id = None);
  ignore (Service.leave_household service alice);
  Alcotest.(check bool) "stale access revoked" true (Merchant_logos.blob service alice ~owner ~id = None)

let commands () =
  let db = Store_sqlite.open_memory () in
  let service = Service.make ~logo_network:(network (ref 0)) db in
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
  let args = `Assoc ["merchant_key", `String "shop"; "link", `String "shop.com/pic.png"] in
  let run access scopes name args = Commands.execute service access ~scopes (Option.get (Api.find name)) args in
  let schema = Commands.schema (Option.get (Api.find "set_merchant_logo")) in
  let required = Yojson.Safe.Util.member "required" schema in
  Alcotest.(check bool) "schema requires link and key" true
    (required = `List [`String "merchant_key"; `String "link"]);
  let clear_schema = Commands.schema (Option.get (Api.find "clear_merchant_logo")) in
  Alcotest.(check bool) "clear mode schema" true
    (Yojson.Safe.Util.member "enum" (Yojson.Safe.Util.member "mode" (Yojson.Safe.Util.member "properties" clear_schema))
     = `List [`String "none"; `String "automatic"]);
  Alcotest.(check bool) "read only" true (Result.is_error (run alice ["ledger:read"] "set_merchant_logo" args));
  let saved = Result.get_ok (run alice ["ledger:write"] "set_merchant_logo" args) in
  Alcotest.(check bool) "same origin" true (String.starts_with ~prefix:"/api/logos/" (Yojson.Safe.Util.to_string (Yojson.Safe.Util.member "logo" saved)));
  let bad_link = run alice ["ledger:write"] "set_merchant_logo" (`Assoc ["merchant_key", `String "shop"; "link", `String "http://bad.com"]) in
  Alcotest.(check bool) "stable link error" true (match bad_link with Error (`Logo ("invalid_link", _)) -> true | _ -> false);
  Alcotest.(check bool) "invalid owner" true (Result.is_error (run bob ["ledger:write"] "set_merchant_logo" (`Assoc ["merchant_key", `String "shop"; "owner", `String "household"; "link", `String "shop.com"])));
  Alcotest.(check bool) "stable mode error" true (match run alice ["ledger:write"] "clear_merchant_logo" (`Assoc ["merchant_key", `String "shop"; "mode", `String "bogus"]) with Error (`Logo ("invalid_mode", _)) -> true | _ -> false);
  ignore (Result.get_ok (run alice ["ledger:write"] "clear_merchant_logo" (`Assoc ["merchant_key", `String "shop"; "mode", `String "none"])));
  Alcotest.(check bool) "removed" true ((Merchant_logos.view service alice ~owner:(Access.personal alice) ~merchant_key:"shop").logo = None)

let retry_projection () =
  let db = Store_sqlite.open_memory () in
  let prompts = ref 0 and calls = ref 0 in
  let reply = ref {|{"merchants":[]}|} in
  let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system:_ ~user:_ () -> incr prompts; Ok !reply)} in
  let service = Service.make ~assistant ~logo_network:(network calls) db in
  let alice = sign_in service "alice" and bob = sign_in service "bob" in
  let owner = Access.personal alice in
  let checking = open_checking service alice "Checking" in
  let groceries = account_named service alice "Groceries" in
  ignore (ok (Service.add_entry service alice { (entry "ICA" ~from_account:checking ~to_account:groceries 100) with
    memo = {Transfer.empty_memo with description="ICA"} }));
  let retry service access fold =
    let json = Result.get_ok (Commands.execute service access ~scopes:["ledger:read"]
      (Option.get (Api.find "query_transactions")) (`Assoc ["fold_triage", `Bool fold])) in
    let rows = Yojson.Safe.Util.(member "transactions" json |> to_list) in
    List.exists (fun row -> Yojson.Safe.Util.(member "merchant" row |> member "can_retry") = `Bool true) rows in
  Alcotest.(check bool) "no retry before automatic lookup" false (retry service alice false);
  Merchant_logos.resolve service alice ~owner ["ica", "ICA"];
  List.iter (fun fold -> Alcotest.(check bool) "unknown automatic logo can retry" true (retry service alice fold)) [false; true];
  let before = !prompts, !calls in
  ignore (retry service alice true);
  Alcotest.(check bool) "query does not trigger discovery" true (before = (!prompts, !calls));
  Alcotest.(check bool) "assistant off hides retry" false (retry (Service.make ~logo_network:(network calls) db) alice true);
  Alcotest.(check bool) "missing fetch transport hides retry" false (retry (Service.make ~assistant db) alice true);
  Alcotest.(check bool) "other owner does not inherit retry" false (retry service bob true);
  ignore (Result.get_ok (Merchant_logos.set service alice ~owner ~merchant_key:"ica" ~link:"shop.com/icon.png"));
  Alcotest.(check bool) "resolved user logo hides retry" false (retry service alice true);
  ignore (Result.get_ok (Merchant_logos.clear service alice ~owner ~merchant_key:"ica" ~mode:Merchant_logos.None_selected));
  Alcotest.(check bool) "explicitly removed logo can opt back in" true (retry service alice true);
  reply := {|{"merchants":[{"name":"ICA","domain":"shop.com","confidence":90}]}|};
  let failing = Logo_fetch.{(network calls) with get=(fun ~uri:_ ~address:_ ~max_bytes:_ -> Error Unreachable_site)} in
  let failing_service = Service.make ~assistant ~logo_network:failing db in
  ignore (Result.get_ok (Merchant_logos.clear failing_service alice ~owner ~merchant_key:"ica" ~mode:Merchant_logos.Automatic));
  Alcotest.(check bool) "failed fetch can retry" true (retry failing_service alice true)

let failed_provider_replies =
  ["provider error", Error "rate limited"; "malformed JSON", Ok "not JSON";
   "malformed schema", Ok {|{"merchants":false}|}]

let provider_failures_retry () =
  List.iter (fun (label, reply) ->
    let db = Store_sqlite.open_memory () and calls = ref 0 in
    let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system:_ ~user:_ () -> reply)} in
    let service = Service.make ~now:(fun () -> now) ~assistant ~logo_network:(network calls) db in
    let access = sign_in service "alice" in
    let owner = Access.personal access in
    let checking = open_checking service access "Checking" in
    let groceries = account_named service access "Groceries" in
    ignore (ok (Service.add_entry service access { (entry "ICA" ~from_account:checking ~to_account:groceries 100) with
      memo={Transfer.empty_memo with description="ICA"} }));
    Merchant_logos.resolve service access ~owner ["ica", "ICA"; "other", "OTHER"];
    List.iter (fun key ->
      Alcotest.(check bool) (label ^ " persists every failed key") true
        (match Merchant_logo_store.get db owner key with
         | Some {source=Llm; status=Failed; logo_id=None; website=None; confidence=None; _} -> true
         | _ -> false);
      Alcotest.(check bool) (label ^ " enables retry") true
        (Merchant_logos.view service access ~owner ~merchant_key:key).can_retry) ["ica"; "other"];
    List.iter (fun fold ->
      let json = Result.get_ok (Commands.execute service access ~scopes:["ledger:read"] Commands.query_transactions
        (`Assoc ["fold_triage", `Bool fold])) in
      let rows = Yojson.Safe.Util.(member "transactions" json |> to_list) in
      Alcotest.(check bool) (label ^ " query exposes retry") true
        (List.exists (fun row -> Yojson.Safe.Util.(member "merchant" row |> member "can_retry") = `Bool true) rows)) [false; true];
    Alcotest.(check int) (label ^ " does not fetch an image") 0 !calls;
    ignore (Result.get_ok (Merchant_logos.set service access ~owner ~merchant_key:"ica" ~link:"shop.com/icon.png"));
    let reset = Result.get_ok (Merchant_logos.clear service access ~owner ~merchant_key:"ica" ~mode:Merchant_logos.Automatic) in
    Alcotest.(check bool) (label ^ " automatic reset can retry") true reset.can_retry)
    failed_provider_replies

let provider_failures_preserve_edits () =
  List.iter (fun (label, reply) ->
    let db = Store_sqlite.open_memory () and during = ref (fun () -> ()) in
    let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system:_ ~user:_ () -> !during (); reply)} in
    let service = Service.make ~assistant ~logo_network:(network (ref 0)) db in
    let access = sign_in service "alice" in
    let owner = Access.personal access in
    during := (fun () -> ignore (Result.get_ok (Merchant_logos.set service access ~owner ~merchant_key:"edit" ~link:"shop.com/icon.png")));
    Merchant_logos.resolve service access ~owner ["edit", "EDIT"];
    Alcotest.(check bool) (label ^ " preserves manual logo") true
      (match Merchant_logo_store.get db owner "edit" with Some {source=User; status=Resolved; logo_id=Some _; _} -> true | _ -> false);
    during := (fun () -> ignore (Result.get_ok (Merchant_logos.clear service access ~owner ~merchant_key:"none" ~mode:Merchant_logos.None_selected)));
    Merchant_logos.resolve service access ~owner ["none", "NONE"];
    Alcotest.(check bool) (label ^ " preserves explicit removal") true
      (match Merchant_logo_store.get db owner "none" with Some {source=None_selected; status=Unknown; logo_id=None; _} -> true | _ -> false))
    failed_provider_replies

let provider_failures_check_membership () =
  List.iter (fun (label, reply) ->
    let db = Store_sqlite.open_memory () and during = ref (fun () -> ()) in
    let assistant = Llm.{model="fake"; complete=(fun ?schema:_ ~system:_ ~user:_ () -> !during (); reply)} in
    let service = Service.make ~assistant ~logo_network:(network (ref 0)) db in
    let access = sign_in service "alice" in
    let household = ok (Service.create_household service access ~name:"Home" ~default_currency:"SEK") in
    let access = Service.access_for service (Access.user access) in
    let owner = Owner.Household household.id in
    during := (fun () -> ignore (Service.leave_household service access));
    Merchant_logos.resolve service access ~owner ["ica", "ICA"];
    Alcotest.(check bool) (label ^ " cannot write after membership ends") true
      (Option.is_none (Merchant_logo_store.get db owner "ica"))) failed_provider_replies

let () = Alcotest.run "merchant logos" ["store", [Alcotest.test_case "retry is an exception-only action" `Quick retry_projection; Alcotest.test_case "commands" `Quick commands; Alcotest.test_case "manual and isolation" `Quick manual;
 Alcotest.test_case "chain" `Quick chain;
 Alcotest.test_case "automatic uses original display name" `Quick automatic_uses_original_display_name;
 Alcotest.test_case "automatic without a display name" `Quick automatic_without_display_name;
 Alcotest.test_case "resolver cases" `Quick resolver_cases;
 Alcotest.test_case "provider failures are retryable" `Quick provider_failures_retry;
 Alcotest.test_case "provider failures preserve edits" `Quick provider_failures_preserve_edits;
 Alcotest.test_case "provider failures check membership" `Quick provider_failures_check_membership;
 Alcotest.test_case "bounded batches" `Quick batching;
 Alcotest.test_case "owner blob association and membership loss" `Quick association]]
