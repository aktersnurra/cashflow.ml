open Hegel
open Test_service_support

(* Statements are exports of one true account history: a date range of it, whose first and last
   days may be cut off (an export taken mid-day, or one row of a day), while the days in between
   are complete, which is what row identity assumes (§6.3). A random sequence of exports,
   re-uploads and undos, with the user answering ambiguous rows truthfully, must leave the ledger
   holding every row an active import contains exactly once.

   The model knows which real rows the ledger holds. The ledger can only count identical rows
   (same day, text and amount, and no balance column to tell them apart), so the checks are per
   group of them: the balance is the sum of the rows held, and no group holds fewer rows than an
   active import contains. Undoing an import a later one relies on is refused.
   Texts and amounts come from small pools so that identical rows are common, and half the
   histories have no running-balance column. *)

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

type batch = { id : string; file : int list }  (** history indices in the file *)

let test tc =
  let service = make_service () in
  let alice = sign_in service "alice" in
  let checking = open_checking service alice ~statement_backed:true "Checking" in
  let ctx = Import.context service alice in
  let history = draw_history tc in
  let with_balance = draw tc (booleans ()) in
  let group i = let r = history.(i) in (r.day, r.text, r.amount, if with_balance then r.balance else 0) in
  (* Which history rows the ledger holds, and the import that posted each. *)
  let present = Array.make (Array.length history) None in
  let active = ref [] in
  let count f = Array.fold_left (fun n x -> if f x then n + 1 else n) 0 (Array.mapi (fun i x -> (i, x)) present) in
  let held g = count (fun (i, x) -> Option.is_some x && group i = g) in
  let check () =
    let expected = ref 0 in
    Array.iteri (fun i x -> if Option.is_some x then expected := !expected + history.(i).amount) present;
    if Int64.of_int !expected <> balance service alice checking then
      failwith (Printf.sprintf "balance %Ld, but the model holds %d" (balance service alice checking) !expected);
    List.iter
      (fun b ->
        List.iter
          (fun i ->
            let g = group i in
            let in_file = List.length (List.filter (fun j -> group j = g) b.file) in
            if held g < in_file then failwith (Printf.sprintf "an active import has %d identical rows on day %d, the ledger %d" in_file history.(i).day (held g)))
          b.file)
      !active
  in
  (* A date range whose first and last days may be cut off. With balances the cut is at the ends of
     the range, so the file is contiguous in time; without, an edge day can hold any of its rows
     (the case where only the user can tell identical rows apart). *)
  let draw_export () =
    let a = draw tc (integers ~min_value:0 ~max_value:9 ()) and b = draw tc (integers ~min_value:0 ~max_value:9 ()) in
    let first, last = (min a b, max a b) in
    let window = List.filter (fun i -> history.(i).day >= first && history.(i).day <= last) (List.init (Array.length history) Fun.id) in
    if with_balance then
      match window, List.rev window with
      | [], _ | _, [] -> []
      | earliest :: _, latest :: _ ->
          (* Start somewhere on the first day and stop somewhere on the last; history is in time order. *)
          let pick candidates = List.nth candidates (draw tc (integers ~min_value:0 ~max_value:(List.length candidates - 1) ())) in
          let i = pick (List.filter (fun k -> history.(k).day = history.(earliest).day) window) in
          let j = pick (List.filter (fun k -> history.(k).day = history.(latest).day && k >= i) window) in
          List.filter (fun k -> k >= i && k <= j) window
    else List.filter (fun i -> let day = history.(i).day in (day > first && day < last) || draw tc (booleans ~p:0.6 ())) window
  in
  for _ = 1 to draw tc (integers ~min_value:1 ~max_value:8 ()) do
    match draw tc (integers ~min_value:0 ~max_value:3 ()) with
    | 0 when !active <> [] ->
        let chosen = List.nth !active (draw tc (integers ~min_value:0 ~max_value:(List.length !active - 1) ())) in
        let others = List.filter (fun b -> b.id <> chosen.id) !active in
        let posted = List.filter (fun i -> present.(i) = Some chosen.id) (List.init (Array.length history) Fun.id) in
        (* Another import relies on it when it holds a row on the same day with the same amount. *)
        let relied_on b = List.exists (fun i -> List.exists (fun j -> history.(i).day = history.(j).day && history.(i).amount = history.(j).amount) b.file) posted in
        (match Import.undo ctx ~batch_id:chosen.id with
        | Ok _ ->
            active := others;
            List.iter (fun i -> present.(i) <- None) posted
        | Error _ when List.exists relied_on others -> ()
        | Error e -> failwith ("undo refused with nothing relying on it: " ^ Import.error_to_string e));
        check ()
    | _ -> (
        match draw_export () with
        | [] -> ()
        | file -> (
            (* Banks list newest or oldest first. *)
            let file = if draw tc (booleans ()) then List.rev file else file in
            let request decisions : Import.request =
              { account_id = checking; file_name = None; content = export ~with_balance (List.rev_map (Array.get history) file); decisions }
            in
            let preview = ok_import (Import.preview ctx (request [])) in
            (* The user knows which of the file's rows really are new: those the ledger doesn't hold yet. *)
            let decisions =
              let answered = Hashtbl.create 8 in
              List.filter_map
                (fun (p : Import.planned) ->
                  match p.status with
                  | Ambiguous _ ->
                      let g = group (List.nth file p.index) in
                      let rows = List.filter (fun j -> group j = g) file in
                      let missing = List.length (List.filter (fun j -> present.(j) = None) rows) in
                      let n = Option.value (Hashtbl.find_opt answered g) ~default:0 in
                      Hashtbl.replace answered g (n + 1);
                      Some (p.index, if n < missing then Import.Is_new else Is_duplicate)
                  | _ -> None)
                preview.rows
            in
            match ok_import (Import.confirm ctx (request decisions)) with
            | `Already_imported (existing : Import_store.batch) ->
                (* Only an identical file of an active import is refused, and it adds nothing. *)
                if not (List.exists (fun b -> b.id = existing.batch_id) !active) then failwith "refused as a copy of an undone import";
                check ()
            | `Imported (summary : Import.summary) ->
                let id = summary.batch.batch_id in
                List.iter (fun j -> if present.(j) = None then present.(j) <- Some id) file;
                active := { id; file } :: !active;
                if summary.batch.new_count + summary.batch.duplicate_count <> List.length file then failwith "rows neither new nor duplicate";
                check ();
                (* With nothing missing up to the statement's closing date, the balance check agrees. *)
                let last = List.fold_left (fun d i -> max d history.(i).day) 0 file in
                let gapless = Array.for_all Fun.id (Array.mapi (fun i x -> history.(i).day > last || Option.is_some x) present) in
                match summary.balance with
                | Some b when gapless && b.statement_balance <> b.ledger_balance ->
                    failwith
                      (Printf.sprintf "balance check: statement %Ld, ledger %Ld\nhistory: %s\nfile: %s" b.statement_balance b.ledger_balance
                         (String.concat " | " (Array.to_list (Array.map (fun r -> Printf.sprintf "d%d %s %d bal %d" r.day r.text r.amount r.balance) history)))
                         (String.concat "," (List.map string_of_int file)))
                | Some _ when not with_balance -> failwith "a balance check without a balance column"
                | None when with_balance && gapless && Array.for_all Fun.id (Array.mapi (fun i x -> history.(i).day <> last || (Option.is_some x && List.mem i file)) present) ->
                    failwith
                      (Printf.sprintf "no balance check, though the file has the whole closing day\nhistory: %s\nfile: %s"
                         (String.concat " | " (Array.to_list (Array.map (fun r -> Printf.sprintf "d%d %s %d bal %d" r.day r.text r.amount r.balance) history)))
                         (String.concat "," (List.map string_of_int file)))
                | _ -> ()))
  done

let%hegel_test exports_import_exactly_once tc = test tc [@@settings Settings.create ~test_cases:300 ()]

let () =
  Alcotest.run "import properties"
    [ ("exports", [ Alcotest.test_case "overlapping, partial-edge exports, re-uploads and undo import every row exactly once" `Slow exports_import_exactly_once ]) ]
