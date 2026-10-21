let decimal () =
  Alcotest.(check string) "format" "1240.50" (Decimal.format ~scale:2 124050L);
  let check label expected text =
    match Decimal.parse ~scale:2 text, expected with
    | Ok actual, Ok wanted -> Alcotest.(check int64) label wanted actual
    | Error Decimal.Invalid_format, Error Decimal.Invalid_format
    | Error Decimal.Out_of_range, Error Decimal.Out_of_range -> ()
    | Error (Decimal.Excess_precision { scale = 2 }), Error (Decimal.Excess_precision { scale = 2 }) -> ()
    | _ -> Alcotest.fail label
  in
  check "negative" (Ok (-325L)) "-3.25";
  check "precision" (Error (Decimal.Excess_precision { scale = 2 })) "1.234";
  List.iter (check "invalid" (Error Decimal.Invalid_format)) [ ""; "12,50"; "+1"; ".5"; "1.2.3"; "\0111" ];
  check "overflow" (Error Decimal.Out_of_range) "9223372036854775808";
  List.iter (fun scale ->
      match Decimal.parse ~scale "1" with
      | Error (Decimal.Invalid_scale actual) -> Alcotest.(check int) "scale" scale actual
      | _ -> Alcotest.fail "invalid scale") [-1; 5];
  List.iter (fun (scale, text, minor) ->
      Alcotest.(check (result int64 reject)) "accepted" (Ok minor) (Decimal.parse ~scale text);
      Alcotest.(check string) "formatted" text (Decimal.format ~scale minor))
    [0, "12", 12L; 1, "-0.3", -3L; 3, "0.001", 1L; 4, "1.2345", 12345L];
  Alcotest.(check (result int64 reject)) "trailing dot preserved" (Ok 100L) (Decimal.parse ~scale:2 " 1. ")

let merchant () =
  let check expected counterparty =
    Alcotest.(check string) "merchant" expected
      (Merchant.key ~description:"CARD 123  SPOTIFY" ~counterparty)
  in
  check "card spotify" None;
  check "ica" (Some "  ICA 123  ");
  check "card spotify" (Some "   ");
  check "\011" (Some "\011")

let literal_ids () =
  let text = "01890f3e-7c00-7000-8000-000000000001" in
  let check of_string to_string =
    Alcotest.(check (option string)) "round trip" (Some text) (Option.map to_string (of_string text));
    Alcotest.(check bool) "invalid" true (Option.is_none (of_string "not-a-uuid"))
  in
  check Account_id.of_string Account_id.to_string;
  check Transfer_id.of_string Transfer_id.to_string;
  check User_id.of_string User_id.to_string;
  check Household_id.of_string Household_id.to_string

let () = Alcotest.run "Core" ["pure", [
    Alcotest.test_case "Decimal" `Quick decimal;
    Alcotest.test_case "Merchant" `Quick merchant;
    Alcotest.test_case "literal IDs" `Quick literal_ids]]
