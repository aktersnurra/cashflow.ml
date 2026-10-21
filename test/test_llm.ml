open Test_service_support

let prompts = ref []

(* A fake model that picks Groceries for every row, and invents a category for good measure. *)
let fake : Llm.t =
  { model = "fake";
    complete =
      (fun ~system:_ ~user ->
        prompts := user :: !prompts;
        let json = Yojson.Safe.from_string user in
        let open Yojson.Safe.Util in
        let groceries = member "categories" json |> to_list |> List.find (fun c -> member "name" c = `String "Groceries") |> member "id" |> to_string in
        let rows = member "rows" json |> to_list |> List.map (fun r -> member "row" r |> to_int) in
        Ok
          ("```json\n"
          ^ Yojson.Safe.to_string
              (`Assoc
                 [ ( "suggestions",
                     `List
                       (List.concat_map
                          (fun row -> [ `Assoc [ ("row", `Int row); ("category_id", `String groceries); ("confidence", `Int 99) ]; `Assoc [ ("row", `Int row); ("category_id", `String "made-up") ] ])
                          rows) ) ])
          ^ "\n```")) }

let test_suggestions_and_privacy () =
  let service = Service.make ~now:(fun () -> now) ~assistant:fake (Store_sqlite.open_memory ()) in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Alice's secret checking" in
  let csv = "Datum;Text;Belopp;Saldo\n2026-10-20;WILLYS HEMMA;-412,00;87 654,00\n" in
  let preview = ok_import (Import.preview (Import.context service alice) { account_id = checking; file_name = None; content = csv; decisions = [] }) in
  (match (List.hd preview.rows).suggestion with
  | Some { source = Llm; confidence; category_id; _ } ->
      Alcotest.(check bool) "groceries" true (Account_id.equal category_id (account_named service alice "Groceries"));
      Alcotest.(check int) "confidence is capped below rules" 80 confidence
  | _ -> Alcotest.fail "no LLM suggestion");
  let prompt = List.hd !prompts in
  Alcotest.(check bool) "no account name" false (Str_compat.contains ~sub:"secret" prompt);
  Alcotest.(check bool) "no balance" false (Str_compat.contains ~sub:"87654" prompt);
  Alcotest.(check bool) "but the merchant" true (Str_compat.contains ~sub:"WILLYS" prompt)

let () = Alcotest.run "llm" [ ("features", [ Alcotest.test_case "suggestions and privacy" `Quick test_suggestions_and_privacy ]) ]
