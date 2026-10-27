module Logo_fetch = Cashflow_io.Logo_fetch
open Logo_fetch

let fixture ?(name = "logo_64x48") file =
  let path = "test/fixtures/" ^ name ^ "." ^ file in
  let path = if Sys.file_exists path then path else "fixtures/" ^ name ^ "." ^ file in
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () -> really_input_string channel (in_channel_length channel))

let png = fixture "png"

let network ?(page = "") ?(addresses = [ "8.8.8.8" ]) calls =
  { with_request = (fun f -> f ());
    resolve = (fun ~host:_ -> Ok addresses);
    get = (fun ~uri ~address ~max_bytes ->
      calls := (Uri.to_string uri, address, max_bytes) :: !calls;
      if Uri.path uri = "/" || Uri.path uri = "" then
        Ok { status = 200; headers = [ "content-type", "text/html" ]; body = page }
      else Ok { status = 200; headers = [ "content-type", "image/png" ]; body = png }) }

let rejects_address () =
  List.iter (fun address ->
    let calls = ref [] in
    let result = fetch (network ~addresses:[ "8.8.8.8"; address ] calls) "example.com" in
    Alcotest.(check bool) address true (result = Error Unsafe_address);
    Alcotest.(check int) "no connection" 0 (List.length !calls))
    [ "127.0.0.1"; "10.0.0.1"; "172.16.0.1"; "192.168.1.1";
      "169.254.169.254"; "100.64.0.1"; "0.0.0.0"; "224.0.0.1";
      "::1"; "::"; "fc00::1"; "fe80::1"; "ff02::1";
      "2001:db8::1"; "2002:c0a8:0101::";
      "::ffff:127.0.0.1"; "::ffff:169.254.169.254" ]

let rejects_links () =
  List.iter (fun link ->
    let calls = ref [] in
    let result = fetch (network calls) link in
    Alcotest.(check bool) link true (result = Error Invalid_link);
    Alcotest.(check int) "no connection" 0 (List.length !calls))
    [ "http://example.com"; "https://user@example.com"; "https://example.com:444/";
      "https://127.0.0.1"; "localhost"; "example.internal"; "https://example.com/#fragment";
      "https://[2606:4700:4700::1111]/"; "https://evil.example@safe.example/" ]

let icon_order () =
  let calls = ref [] in
  let page = "<link rel='icon' sizes='128x128' href='/ordinary.png'>" ^
    "<link rel='apple-touch-icon' sizes='64x64' href='/small.png'>" ^
    "<link href='icons/apple.png' sizes='180x180' rel='apple-touch-icon'>" in
  let result = fetch (network ~page calls) "example.com" in
  Alcotest.(check bool) "success" true (Result.is_ok result);
  Alcotest.(check string) "preferred relative icon" "https://example.com/icons/apple.png"
    (let uri, _, _ = List.hd !calls in uri)

let bare_image_path () =
  let calls = ref [] in
  match fetch (network calls) "example.com/picture.png" with
  | Error _ -> Alcotest.fail "bare hostname and image path rejected"
  | Ok image -> Alcotest.(check string) "default https" "https://example.com/picture.png" (source_url image)

let extensionless_image_and_redirect () =
  let calls = ref [] in
  let net = network calls in
  let net = { net with get = (fun ~uri ~address ~max_bytes ->
    if Uri.path uri = "/landing" then
      Ok { status = 302; headers = [ "location", "/opaque?id=1" ]; body = "" }
    else if Uri.path uri = "/opaque" then
      Ok { status = 200; headers = [ "content-type", "application/octet-stream" ]; body = png }
    else net.get ~uri ~address ~max_bytes) } in
  List.iter (fun link ->
    calls := [];
    match fetch net link with
    | Error _ -> Alcotest.fail ("image at " ^ link ^ " rejected")
    | Ok image ->
        Alcotest.(check string) "redirect final source" "https://example.com/opaque?id=1" (source_url image);
        Alcotest.(check string) "detected bytes" "image/png" (content_type image);
        Alcotest.(check bool) "no favicon GET" true
          (not (List.exists (fun (url, _, _) -> String.ends_with ~suffix:"favicon.ico" url) !calls)))
    [ "https://example.com/opaque?id=1"; "https://example.com/landing" ];
  let too_large = String.sub png 0 (String.length png - 8) ^
    String.make (262_145 - String.length png) '\000' ^
    String.sub png (String.length png - 8) 8 in
  let net = { net with get = (fun ~uri:_ ~address:_ ~max_bytes:_ ->
    Ok { status = 200; headers = []; body = too_large }) } in
  Alcotest.(check bool) "image cap despite HTML allowance" true
    (fetch net "https://example.com/opaque?id=1" = Error Oversized_body)

let quoted_icon_attributes () =
  List.iter (fun (name, page) ->
    let calls = ref [] in
    match fetch (network ~page calls) "example.com" with
    | Error _ -> Alcotest.fail (name ^ " icon rejected")
    | Ok image -> Alcotest.(check string) name "https://example.com/large.png" (source_url image))
    [ "multi-token rel", "<link rel=\"shortcut icon\" sizes=\"128x128\" href=\"/large.png\">";
      "largest square size", "<link rel='icon' sizes=\"32x32 128x128\" href='/large.png'>" ^
        "<link rel='icon' sizes='96x96' href='/smaller.png'>" ]

let failed_icon_falls_back () = 
  let calls = ref [] in
  let page = "<link rel='apple-touch-icon' sizes='180x180' href='/broken.png'>" ^
    "<link rel='icon' sizes='128x128' href='/square.png'>" in
  let net = network ~page calls in
  let net = { net with get = (fun ~uri ~address ~max_bytes ->
    if Uri.path uri = "/broken.png" then
      Ok { status = 200; headers = []; body = "not an image" }
    else net.get ~uri ~address ~max_bytes) } in
  match fetch net "example.com" with
  | Error _ -> Alcotest.fail "square icon rejected after broken apple"
  | Ok image ->
      Alcotest.(check string) "next safe candidate" "https://example.com/square.png" (source_url image);
      Alcotest.(check int) "HTML 512k, images 256k" 262_144
        (let _, _, size = List.hd !calls in size)

let redirect_boundary () =
  let calls = ref [] in
  let network = { with_request = (fun f -> f ());
    resolve = (fun ~host -> Ok [ if host = "private.example.org" then "10.1.2.3" else "8.8.8.8" ]);
    get = (fun ~uri ~address ~max_bytes:_ ->
      calls := (Uri.to_string uri, address) :: !calls;
      Ok { status = 302; headers = [ "Location", "https://private.example.org/icon.png" ]; body = "" }) } in
  Alcotest.(check bool) "redirect blocked" true (fetch network "example.org" = Error Unsafe_address);
  Alcotest.(check int) "private address never used" 1 (List.length !calls);
  let calls = ref 0 in
  let network = { with_request = (fun f -> f ());
    resolve = (fun ~host:_ -> Ok [ "8.8.8.8" ]);
    get = (fun ~uri:_ ~address:_ ~max_bytes:_ ->
      incr calls;
      Ok { status = 302; headers = [ "location", "/next" ]; body = "" }) } in
  Alcotest.(check bool) "fourth redirect" true (fetch network "example.org" = Error Redirect_limit);
  Alcotest.(check int) "four requests only" 4 !calls

let request_scope () =
  let active = ref false and count = ref 0 in
  let net = { with_request = (fun f ->
      Alcotest.(check bool) "no inherited redirect deadline" false !active;
      active := true; incr count;
      Fun.protect ~finally:(fun () -> active := false) f);
    resolve = (fun ~host:_ ->
      Alcotest.(check bool) "DNS inside scope" true !active;
      Ok [ "8.8.8.8" ]);
    get = (fun ~uri ~address ~max_bytes:_ ->
      Alcotest.(check bool) "pinned GET inside same scope" true !active;
      Alcotest.(check string) "resolved address" "8.8.8.8" address;
      Alcotest.(check string) "TLS/Host name remains original" "example.org" (Option.get (Uri.host uri));
      Ok { status = 302; headers = [ "location", "/next" ]; body = "" }) } in
  Alcotest.(check bool) "redirect cap" true (fetch net "example.org" = Error Redirect_limit);
  Alcotest.(check int) "independent budget for each hop" 4 !count;
  Alcotest.(check bool) "scope released" false !active

(* A host listing IPv6 first (or an address that will not connect) must still work where another address does. *)
let tries_next_address () =
  let tried = ref [] in
  let net failing =
    { with_request = (fun f -> f ());
      resolve = (fun ~host:_ -> Ok [ "2606:4700:4700::1111"; "8.8.4.4"; "8.8.8.8" ]);
      get = (fun ~uri:_ ~address ~max_bytes:_ ->
        tried := address :: !tried;
        if List.mem address failing then Error Unreachable_site
        else Ok { status = 200; headers = [ "content-type", "image/png" ]; body = png }) } in
  Alcotest.(check bool) "works" true (Result.is_ok (fetch (net []) "https://example.org/logo.png"));
  Alcotest.(check (list string)) "IPv4 is tried before IPv6" [ "8.8.4.4" ] (List.rev !tried);
  tried := [];
  Alcotest.(check bool) "falls back to the next address" true (Result.is_ok (fetch (net [ "8.8.4.4" ]) "https://example.org/logo.png"));
  Alcotest.(check (list string)) "in order" [ "8.8.4.4"; "8.8.8.8" ] (List.rev !tried);
  tried := [];
  Alcotest.(check bool) "IPv6 is the last resort" true (Result.is_ok (fetch (net [ "8.8.4.4"; "8.8.8.8" ]) "https://example.org/logo.png"));
  Alcotest.(check (list string)) "all tried" [ "8.8.4.4"; "8.8.8.8"; "2606:4700:4700::1111" ] (List.rev !tried);
  tried := [];
  Alcotest.(check bool) "nothing connects" true (fetch (net [ "8.8.4.4"; "8.8.8.8"; "2606:4700:4700::1111" ]) "https://example.org/logo.png" = Error Unreachable_site);
  tried := [];
  let answers = { with_request = (fun f -> f ()); resolve = (fun ~host:_ -> Ok [ "8.8.4.4"; "8.8.8.8" ]);
    get = (fun ~uri:_ ~address ~max_bytes:_ -> tried := address :: !tried; Ok { status = 404; headers = []; body = "" }) } in
  Alcotest.(check bool) "an error status is an answer, not a reason to retry" true (fetch answers "https://example.org/logo.png" = Error Unreachable_site);
  Alcotest.(check int) "one connection" 1 (List.length !tried)

(* Real home pages are far bigger than the limit; only their <head> matters, so the start of the page is enough. *)
let big_page_is_read_by_its_head () =
  let head = "<html><head><link rel=\"apple-touch-icon\" href=\"/logo.png\"></head><body>" in
  let page = head ^ String.make 700_000 'x' in
  let net =
    { with_request = (fun f -> f ()); resolve = (fun ~host:_ -> Ok [ "8.8.8.8" ]);
      get = (fun ~uri ~address:_ ~max_bytes ->
        (* What the real network layer hands back: at most one byte more than the limit. *)
        if Uri.path uri = "/logo.png" then Ok { status = 200; headers = []; body = png }
        else Ok { status = 200; headers = [ "content-type", "text/html" ]; body = String.sub page 0 (min (String.length page) (max_bytes + 1)) }) } in
  Alcotest.(check bool) "the icon named in the head is found" true (Result.is_ok (fetch net "https://example.org/"));
  let big_image =
    { net with get = (fun ~uri:_ ~address:_ ~max_bytes -> Ok { status = 200; headers = [ "content-type", "image/png" ]; body = String.sub (png ^ String.make 700_000 '\000') 0 (max_bytes + 1) }) } in
  Alcotest.(check bool) "an oversized image is still refused" true (fetch big_image "https://example.org/logo.png" = Error Oversized_body)

let image_boundaries () =
  let run body =
    let calls = ref [] in
    let net = network calls in
    fetch { net with get = (fun ~uri:_ ~address:_ ~max_bytes:_ ->
      Ok { status = 200; headers = [ "content-type", "image/png" ]; body }) } "https://example.org/logo.png" in
  Alcotest.(check bool) "bytes beat claimed type" true (run "<svg width='64' height='64'></svg>" = Error Unsupported_image);
  Alcotest.(check bool) "truncated PNG" true (run (String.sub png 0 24) = Error Unsupported_image);
  Alcotest.(check bool) "body cap" true (run (png ^ String.make 262_145 'x') = Error Oversized_body);
  let small = Bytes.of_string png in
  Bytes.set small 19 '\031';
  Alcotest.(check bool) "32px minimum" true (run (Bytes.to_string small) = Error Too_small_image);
  match run png with
  | Ok image ->
      Alcotest.(check int) "width" 64 (width image);
      Alcotest.(check int) "height" 48 (height image);
      Alcotest.(check string) "detected type" "image/png" (content_type image)
  | Error _ -> Alcotest.fail "valid image refused"

let real_images () =
  List.iter (fun (extension, expected) ->
    let calls = ref [] in
    let net = network calls in
    let net = { net with get = (fun ~uri:_ ~address:_ ~max_bytes:_ ->
      Ok { status = 200; headers = [ "content-type", "text/plain" ]; body = fixture extension }) } in
    match fetch net ("https://example.org/logo." ^ extension) with
    | Error _ -> Alcotest.fail ("real " ^ extension ^ " rejected")
    | Ok image ->
        Alcotest.(check string) "magic, not header" expected (content_type image);
        Alcotest.(check int) "width independently generated" 64 (width image);
        Alcotest.(check int) "height independently generated" 48 (height image))
    [ "png", "image/png"; "webp", "image/webp"; "jpg", "image/jpeg";
      "ico", "image/x-icon" ]

let multi_size_ico () =
  let body = fixture ~name:"logo_multi_size" "ico" in
  Alcotest.(check int) "first directory entry is only 16px" 16 (Char.code body.[6]);
  let run body =
    let net = network (ref []) in
    fetch {net with get=(fun ~uri:_ ~address:_ ~max_bytes:_ ->
      Ok {status=200; headers=["content-type", "image/x-icon"]; body})} "https://example.org/favicon.ico" in
  (match run body with
   | Error error -> Alcotest.fail ("multi-size ICO rejected: " ^ error_message error)
   | Ok image ->
       Alcotest.(check string) "ICO type" "image/x-icon" (content_type image);
       Alcotest.(check int) "largest usable width" 64 (width image);
       Alcotest.(check int) "largest usable height" 64 (height image);
       Alcotest.(check string) "complete ICO retained" body (bytes image));
  let small = Bytes.of_string body in
  Bytes.set small 4 '\001';
  Bytes.set small 5 '\000';
  Alcotest.(check bool) "only declared entries count toward the minimum" true
    (match run (Bytes.to_string small) with Error Too_small_image -> true | _ -> false)

let ico_directory_bounds () =
  let body = fixture ~name:"logo_multi_size" "ico" in
  let corrupt label edit =
    let copy = Bytes.of_string body in
    edit copy;
    Alcotest.(check bool) label true (Option.is_none (dimensions (Bytes.to_string copy))) in
  corrupt "empty directory" (fun b -> Bytes.fill b 4 2 '\000');
  corrupt "directory exceeds body" (fun b -> Bytes.fill b 4 2 '\255');
  corrupt "payload overlaps directory" (fun b -> Bytes.fill b 18 4 '\000');
  corrupt "later entry has no payload" (fun b -> Bytes.fill b (6 + 3 * 16 + 8) 4 '\000');
  corrupt "later entry points outside body" (fun b -> Bytes.fill b (6 + 3 * 16 + 12) 4 '\255');
  Alcotest.(check bool) "truncated later payload" true
    (Option.is_none (dimensions (String.sub body 0 (String.length body - 1))))

let bounded_distinct_candidates () =
  let calls = ref [] in
  let repeated = List.init 1000 (fun i -> Printf.sprintf "<link rel='apple-touch-icon' sizes='512x512' href='%s'>"
    (if i mod 2 = 0 then "/same.png" else "https://example.org/same.png")) in
  let others = List.map (fun (size, name) -> Printf.sprintf "<link rel='apple-touch-icon' sizes='%dx%d' href='/%s.png'>" size size name)
    [256, "next"; 128, "third"; 96, "fourth"; 64, "excluded"] in
  let base = network ~page:(String.concat "" (repeated @ others)) calls in
  let net = {base with get=(fun ~uri ~address ~max_bytes ->
    let response = base.get ~uri ~address ~max_bytes in
    if List.mem (Uri.path uri) [""; "/"; "/favicon.ico"] then response else Error Unreachable_site)} in
  Alcotest.(check bool) "bounded attempts retain favicon fallback" true (Result.is_ok (fetch net "example.org"));
  let paths = List.rev !calls |> List.map (fun (url, _, _) -> Uri.path (Uri.of_string url)) in
  Alcotest.(check (list string)) "only distinct top candidates and fallback" ["/same.png"; "/next.png"; "/third.png"; "/fourth.png"; "/favicon.ico"]
    (List.tl paths)

let many_timed_out_candidates () =
  let scopes = ref 0 and calls = ref [] in
  let page = String.concat "" (List.init 1000 (fun i -> Printf.sprintf "<link rel='apple-touch-icon' href='/icon%d.png'>" i)) in
  let base = network ~page calls in
  let net = {base with with_request=(fun f -> incr scopes; if !scopes = 1 then f () else Error Unreachable_site)} in
  Alcotest.(check bool) "timed-out candidates fail closed" true
    (match fetch net "example.org" with Error Unreachable_site -> true | _ -> false);
  Alcotest.(check int) "page plus at most five candidate timeouts" 6 !scopes

let shared_request_budget () =
  let calls = ref 0 in
  let net = {with_request=(fun f -> f ()); resolve=(fun ~host:_ -> Ok ["8.8.8.8"]);
    get=(fun ~uri ~address:_ ~max_bytes:_ ->
      incr calls;
      match Uri.path uri with
      | "" | "/" -> Ok {status=302; headers=["location", "/landing"]; body=""}
      | "/landing" -> Ok {status=302; headers=["location", "/page"]; body=""}
      | "/page" -> Ok {status=200; headers=[]; body="<link rel='apple-touch-icon' href='/candidate.png'>"}
      | _ -> Ok {status=302; headers=["location", "/loop"]; body=""})} in
  for _ = 1 to 2 do
    let before = !calls in
    Alcotest.(check bool) "overall request budget stops redirecting candidates" true
      (match fetch net "example.org" with Error Unreachable_site -> true | _ -> false);
    Alcotest.(check int) "one budget includes page and candidate redirects, fresh for every fetch" 6 (!calls - before)
  done

let () = Alcotest.run "logo fetch" [
  "boundary", [ Alcotest.test_case "addresses" `Quick rejects_address;
                Alcotest.test_case "links" `Quick rejects_links;
                Alcotest.test_case "redirect" `Quick redirect_boundary;
                Alcotest.test_case "per-request scope" `Quick request_scope ];
  "images", [ Alcotest.test_case "validation" `Quick image_boundaries;
              Alcotest.test_case "real fixture dimensions" `Quick real_images;
              Alcotest.test_case "multi-size ICO" `Quick multi_size_ico;
              Alcotest.test_case "ICO directory bounds" `Quick ico_directory_bounds;
              Alcotest.test_case "extensionless and redirected image" `Quick extensionless_image_and_redirect ];
  "icons", [ Alcotest.test_case "order" `Quick icon_order;
             Alcotest.test_case "quoted tokens" `Quick quoted_icon_attributes;
             Alcotest.test_case "bare image path" `Quick bare_image_path;
             Alcotest.test_case "failed candidate" `Quick failed_icon_falls_back;
             Alcotest.test_case "bounded distinct candidates" `Quick bounded_distinct_candidates;
             Alcotest.test_case "many timed-out candidates" `Quick many_timed_out_candidates;
             Alcotest.test_case "shared request budget" `Quick shared_request_budget;
             Alcotest.test_case "tries the next address" `Quick tries_next_address;
             Alcotest.test_case "big page read by its head" `Quick big_page_is_read_by_its_head ] ]
