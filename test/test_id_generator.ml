module Id_generator = Cashflow_io.Id_generator
module Account_id = Cashflow_core.Account_id
module Transfer_id = Cashflow_core.Transfer_id
module User_id = Cashflow_core.User_id
module Household_id = Cashflow_core.Household_id

let check_generator generate to_string of_string equal () =
  let first = generate () and second = generate () in
  Alcotest.(check bool) "distinct" false (equal first second);
  List.iter (fun id ->
      let text = to_string id in
      Alcotest.(check char) "UUIDv7" '7' text.[14];
      Alcotest.(check bool) "valid UUID" true (Option.is_some (Uuidm.of_string text));
      match of_string text with
      | Some parsed -> Alcotest.(check bool) "round trip" true (equal id parsed)
      | None -> Alcotest.fail "generated ID must parse") [first; second]

let () = Alcotest.run "Id_generator" ["live", [
    Alcotest.test_case "account" `Quick (check_generator Id_generator.account Account_id.to_string Account_id.of_string Account_id.equal);
    Alcotest.test_case "transfer" `Quick (check_generator Id_generator.transfer Transfer_id.to_string Transfer_id.of_string Transfer_id.equal);
    Alcotest.test_case "user" `Quick (check_generator Id_generator.user User_id.to_string User_id.of_string User_id.equal);
    Alcotest.test_case "household" `Quick (check_generator Id_generator.household Household_id.to_string Household_id.of_string Household_id.equal)]]
