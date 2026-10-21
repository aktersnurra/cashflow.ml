open Hegel
open Test_service_support

(* Statements are exports of one true account history. A random sequence of exports (each a
   date range of it, so ranges overlap often), re-uploads and undos must leave the ledger holding
   every row covered by an active import exactly once: never a duplicate, never a gap. Undoing an
   import that a later one overlaps is refused, since the later one counts on its rows. Amounts
   and texts come from small pools so that identical rows on the same day are common, and half
   the histories have no running-balance column, so that identical rows can only be told apart
   by their position among the day's identical rows. *)

type row = { day : int; text : string; amount : int; balance : int }

let first_day = date "2026-09-01"
let texts = [| "ICA NARA"; "SPOTIFY"; "SL"; "SWISH"; "LÖN" |]
let amounts = [| -119_00; -250_00; -39_00; 1_000_00; -250_00 |]

let draw_history tc =
  let count = draw tc (integers ~min_value:1 ~max_value:30 ()) in
  let rows = List.init count (fun _ ->
      let i = draw tc (integers ~min_value:0 ~max_value:(Array.length texts - 1) ()) in
      (* Same text, same amount, so identical rows are likely. *)
      (draw tc (integers ~min_value:0 ~max_value:9 ()), texts.(i), amounts.(i)))
  in
  let rows = List.stable_sort (fun (a, _, _) (b, _, _) -> Int.compare a b) rows in
  let _, rows =
    List.fold_left_map (fun balance (day, text, amount) -> let balance = balance + amount in (balance, { day; text; amount; balance })) 0 rows
  in
  Array.of_list rows

let money minor = Printf.sprintf "%s%d,%02d" (if minor < 0 then "-" else "") (abs minor / 100) (abs minor mod 100)

(* Newest first, as banks export. *)
let export ~with_balance rows =
  let line r =
    String.concat ";"
      ([ Date.to_string (Date.add_days first_day r.day); r.text; money r.amount ] @ if with_balance then [ money r.balance ] else [])
  in
  String.concat "\n" (((if with_balance then "Bokföringsdag;Text;Belopp;Saldo" else "Bokföringsdag;Text;Belopp") :: List.rev_map line rows) @ [ "" ])

type batch = { id : string; first : int; last : int }

let test tc =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let ctx = Import.context service alice in
  let history = draw_history tc in
  let with_balance = draw tc (booleans ()) in
  let window first last = List.filter (fun r -> r.day >= first && r.day <= last) (Array.to_list history) in
  (* Active imports, latest first. Undo may refuse when a later import overlaps. *)
  let active = ref [] in
  let covered () =
    Array.to_list history |> List.filter (fun r -> List.exists (fun b -> r.day >= b.first && r.day <= b.last) !active)
  in
  let check () =
    let expected = List.fold_left (fun sum r -> sum + r.amount) 0 (covered ()) in
    if Int64.of_int expected <> balance service alice checking then
      failwith (Printf.sprintf "balance %Ld, but the active imports cover %d" (balance service alice checking) expected)
  in
  for _ = 1 to draw tc (integers ~min_value:1 ~max_value:8 ()) do
    match draw tc (integers ~min_value:0 ~max_value:3 ()) with
    | 0 when !active <> [] ->
        let chosen = List.nth !active (draw tc (integers ~min_value:0 ~max_value:(List.length !active - 1) ())) in
        let others = List.filter (fun b -> b.id <> chosen.id) !active in
        let shares_rows b = List.exists (fun r -> r.day >= b.first && r.day <= b.last) (window chosen.first chosen.last) in
        (match Import.undo ctx ~batch_id:chosen.id with
        | Ok _ -> active := others
        | Error _ when List.exists shares_rows others -> ()
        | Error e -> failwith ("undo refused with no overlapping import: " ^ Import.error_to_string e));
        check ()
    | _ -> (
        let a = draw tc (integers ~min_value:0 ~max_value:9 ()) and b = draw tc (integers ~min_value:0 ~max_value:9 ()) in
        let first, last = (min a b, max a b) in
        match window first last with
        | [] -> ()
        | rows -> (
            match ok_import (Import.confirm ctx { account_id = checking; file_name = None; content = export ~with_balance rows }) with
            | `Already_imported (existing : Import_store.batch) ->
                (* Only an identical file of an active import is refused, and it adds nothing. *)
                if not (List.exists (fun b -> b.id = existing.batch_id) !active) then failwith "refused as a copy of an undone import";
                check ()
            | `Imported (summary : Import.summary) ->
                active := { id = summary.batch.batch_id; first; last } :: !active;
                check ();
                (* With nothing missing up to the statement's closing date, the balance check agrees. *)
                let gapless = List.for_all (fun r -> r.day > last || List.memq r (covered ())) (Array.to_list history) in
                match summary.balance with
                | Some b when gapless && b.statement_balance <> b.ledger_balance ->
                    failwith (Printf.sprintf "balance check: statement %Ld, ledger %Ld" b.statement_balance b.ledger_balance)
                | Some _ when not with_balance -> failwith "a balance check without a balance column"
                | _ -> ()))
  done

let%hegel_test exports_import_exactly_once tc = test tc [@@settings Settings.create ~test_cases:200 ()]

let () =
  Alcotest.run "import properties"
    [ ("exports", [ Alcotest.test_case "overlapping exports, re-uploads and undo import every row exactly once" `Slow exports_import_exactly_once ]) ]
