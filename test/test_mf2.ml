let money minor = { Mf2_runtime.minor; scale = 2; code = "SEK" }

let test_generated_messages () =
  Alcotest.(check string) "en plural one" "1 transaction to categorize" (Msg.card_inbox_title ~count:1 "en");
  Alcotest.(check string) "en plural other" "1,204 transactions to categorize" (Msg.card_inbox_title ~count:1204 "en");
  Alcotest.(check string) "sv plural" "3 transaktioner att kategorisera" (Msg.card_inbox_title ~count:3 "sv");
  Alcotest.(check string) "exact match beats the category" "Imported. Everything is categorized." (Msg.card_import_done ~count:0 "en");
  Alcotest.(check string) "sv money" "4\xC2\xA0100,00\xC2\xA0kr kvar" (Msg.label_left ~left:(money 4_100_00L) "sv");
  Alcotest.(check string) "en money" "SEK\xC2\xA01,240.50 left" (Msg.label_left ~left:(money 1_240_50L) "en");
  Alcotest.(check string) "dates" "25 okt." (Mf2_runtime.format_date "sv" { year = 2026; month = 10; day = 25 });
  Alcotest.(check string) "unknown locale falls back to English" "Inbox" (Msg.nav_inbox "de")

let compile files = Mf2.generate (List.map (fun (locale, text) -> { Mf2.locale; messages = Mf2.parse_file ~file:locale text }) files)

let fails name files =
  match compile files with
  | _ -> Alcotest.fail (name ^ ": compiled")
  | exception Mf2.Error _ -> ()

let test_compile_errors () =
  fails "a message missing from a locale" [ ("en", "a = A\nb = B\n"); ("sv", "a = A\n") ];
  fails "locales disagree on a variable's type" [ ("en", "a = {$n :integer}\n"); ("sv", "a = {$n}\n") ];
  fails "no catch-all variant" [ ("en", "a =\n  .input {$n :integer}\n  .match $n\n  one {{x}}\n"); ("sv", "a = x\n") ];
  fails "a plural without one" [ ("en", "a =\n  .input {$n :integer}\n  .match $n\n  * {{x}}\n") ];
  fails "an undeclared selector" [ ("en", "a =\n  .match $n\n  * {{x}}\n") ];
  ignore (compile [ ("en", "a = Hello \\{literal\\} {$name}\n") ])

let () =
  Alcotest.run "mf2"
    [ ("messages", [ Alcotest.test_case "generated messages" `Quick test_generated_messages; Alcotest.test_case "compile errors" `Quick test_compile_errors ]) ]
