let workbook rows =
  let parts =
    [ ("xl/workbook.xml", {|<workbook xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet r:id="rId1"/></sheets></workbook>|});
      ("xl/_rels/workbook.xml.rels", {|<Relationships><Relationship Id="rId1" Target="worksheets/sheet1.xml"/></Relationships>|});
      ("xl/worksheets/sheet1.xml", "<worksheet><sheetData>" ^ rows ^ "</sheetData></worksheet>") ]
  in
  let add zip (path, content) =
    let file = Result.get_ok (Zipc.File.deflate_of_binary_string content) in
    Zipc.add (Result.get_ok (Zipc.Member.make ~path (Zipc.Member.File file))) zip
  in
  Result.get_ok (Zipc.to_binary_string (List.fold_left add Zipc.empty parts))

let assert_rejected rows =
  match Xlsx.table (workbook rows) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "accepted an invalid or oversized worksheet"

let test_column_limit () = assert_rejected {|<row r="1"><c r="XFE1"><v>1</v></c></row>|}

let test_expanded_cell_limit () =
  let rows = List.init 65 (fun i -> Printf.sprintf {|<row r="%d"><c r="XFD%d"><v>1</v></c></row>|} (i + 1) (i + 1)) in
  assert_rejected (String.concat "" rows)

let test_invalid_references () =
  List.iter
    (fun reference -> assert_rejected (Printf.sprintf {|<row r="1"><c r="%s"><v>1</v></c></row>|} reference))
    [ ""; "1"; "A"; "a1"; "A0"; "A-1"; "A1junk"; "A100001"; "ZZZZZZ1";
      String.make 100 'Z' ^ "1"; "A999999999999999999999999999999" ];
  List.iter (fun index -> assert_rejected (Printf.sprintf {|<row r="%s"><c><v>1</v></c></row>|} index))
    [ "0"; "-1"; "junk"; "100001"; "999999999999999999999999999999" ]

let test_sparse_cells () =
  let rows = {|<row r="2"><c r="B2"><v>12.50</v></c><c r="D2" t="inlineStr"><is><t>Salary</t></is></c></row>|} in
  let table = Result.get_ok (Xlsx.table (workbook rows)) in
  Alcotest.(check (list (list string))) "missing rows and cells"
    [ []; [ ""; "12.5"; ""; "Salary" ] ] table;
  let table = Result.get_ok (Xlsx.table (workbook {|<row><c><v>3</v></c><c><v>4</v></c></row>|})) in
  Alcotest.(check (list (list string))) "implicit positions" [ [ "3"; "4" ] ] table;
  let table = Result.get_ok (Xlsx.table (workbook {|<row r="100000"><c r="XFD100000"><v>1</v></c></row>|})) in
  Alcotest.(check int) "last supported row" 100_000 (List.length table);
  let last_row = List.nth table 99_999 in
  Alcotest.(check int) "last supported column" 16_384 (List.length last_row);
  Alcotest.(check string) "sparse value" "1" (List.nth last_row 16_383)

let () =
  Alcotest.run "xlsx"
    [ ("bounds", [ Alcotest.test_case "column limit" `Quick test_column_limit;
                   Alcotest.test_case "expanded cell limit" `Quick test_expanded_cell_limit;
                   Alcotest.test_case "invalid references" `Quick test_invalid_references ]);
      ("parsing", [ Alcotest.test_case "sparse cells" `Quick test_sparse_cells ]) ]
