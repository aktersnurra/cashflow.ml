open Hegel
open Test_service_support

(* The books as a whole, under random use: alice (an admin) and bob share a household; both have
   bank accounts, and so does the household. A random sequence of imports, moves between
   accounts, categorising, sharing, pairing, dismissing, undoing imports and retrying the last
   action must keep these after every step:

   1. Every stored balance matches its transfers.
   2. For alice, bob, the household and each family: what was saved (income minus spending, plus
      money in minus money out) is the change in its bank and liability accounts, and net worth
      says the same. So no flow is counted twice or missed, whatever triage did.
   3. Each bank account holds exactly the rows of its active imports: the bank is the truth.
   4. Undoing an import leaves none of its transfers, or of what triage posted about its rows,
      in effect.
   5. Retrying the last action changes nothing.

   Amounts carry a running counter in their öre, so two statement rows are never identical and
   every row has one identity (identical rows are the import property test's subject). *)

module Ledger_sqlite = Ledger.Make (Store_sqlite)

type bank = { id : Account_id.t; name : string; mutable rows : (string * int) list  (** batch id, amount *) }

let merchants = [| "ICA NARA"; "COOP"; "SL"; "SPOTIFY"; "LÖN"; "SWISH" |]

let test tc =
  let service = make_service () in
  let alice = sign_in service ~admin:true "alice" and bob = sign_in service "bob" in
  let household = ok (Service.create_household service alice ~name:"Home" ~default_currency:"SEK") in
  let alice = Service.access_for service (Access.user alice) in
  ignore (ok (Service.invite_member service alice ~email:"bob@example.com"));
  let bob = ok (Service.respond_to_invitation service bob ~household_id:household.id ~accept:true) in
  let alice = Service.access_for service (Access.user alice) in
  let bank access ?owner name = { id = open_checking service access ?owner ~statement_backed:true name; name; rows = [] } in
  let alice_checking = bank alice "Alice checking" and alice_savings = bank alice "Alice savings" in
  let shared = bank alice ~owner:(Household household.id) "Shared" and bob_checking = bank bob "Bob checking" in
  let banks = [ (alice_checking, alice); (alice_savings, alice); (shared, alice); (bob_checking, bob) ] in
  let household_groceries =
    (List.find (fun (i : Account_info.t) -> i.account.name = "Groceries" && Owner.equal i.account.owner (Household household.id)) (Service.accounts service alice)).account.id
  in
  let pick list = List.nth list (draw tc (integers ~min_value:0 ~max_value:(List.length list - 1) ())) in
  let counter = ref 0 in
  let row_amount () =
    incr counter;
    let kronor = draw tc (integers ~min_value:1 ~max_value:3000 ()) in
    (if draw tc (booleans ~p:0.7 ()) then -1 else 1) * ((kronor * 100) + (!counter mod 100))
  in
  let day () = Printf.sprintf "2026-10-%02d" (draw tc (integers ~min_value:1 ~max_value:20 ())) in
  let money minor = Printf.sprintf "%s%d,%02d" (if minor < 0 then "-" else "") (abs minor / 100) (abs minor mod 100) in
  let import (b, access) rows =
    let csv = "Datum;Text;Belopp\n" ^ String.concat "" (List.map (fun (d, t, a) -> Printf.sprintf "%s;%s;%s\n" d t (money a)) rows) in
    match Import.confirm (Import.context service access) { account_id = b.id; file_name = None; content = csv; decisions = [] } with
    | Ok (`Imported (s : Import.summary)) -> b.rows <- List.map (fun (_, _, a) -> (s.batch.batch_id, a)) rows @ b.rows; true
    | Ok (`Already_imported _) | Error _ -> false
  in
  let open_items access = Inbox.list service access ~limit:500 in
  let owner_of (v : Inbox.item_view) = List.find_map (fun (b, access) -> if Account_id.equal b.id v.item.bank_account_id then Some access else None) banks in
  let personal_categories access kind =
    List.filter
      (fun (i : Account_info.t) -> i.account.kind = kind && i.details.role = None && not i.account.closed && Owner.equal i.account.owner (Access.personal access))
      (Service.accounts service access)
  in
  (* 1, 2 and 3. *)
  let check what =
    let fail fmt = Printf.ksprintf (fun m -> failwith (what ^ ": " ^ m)) fmt in
    if Ledger_sqlite.verify service.db <> [] then fail "stored balances differ from the transfers";
    List.iter
      (fun (label, access, scope) ->
        let ctx = ok (Insights.context service access ~scope ()) in
        let book = Insights.book ctx in
        let first = date "2000-01-01" and last = date "2100-01-01" in
        let transfers = Insights.transfers ctx ~first ~last in
        let saved = Analytics.savings (Analytics.flows_between book ~first ~last transfers) in
        let held =
          List.fold_left
            (fun sum (i : Account_info.t) ->
              match book.inside i.account.id with
              | Some _ when i.account.kind = Asset || i.account.kind = Liability -> Int64.add sum (Int64.sub i.account.balances.debits_posted i.account.balances.credits_posted)
              | _ -> sum)
            0L (Insights.accounts ctx)
        in
        if saved <> held then fail "%s saved %Ld but its accounts changed by %Ld" label saved held;
        match Analytics.net_worth_series book ~dates:[ last ] (Transfer_query.all_before service.db access last) with
        | [ (_, worth) ] when worth = held -> ()
        | [ (_, worth) ] -> fail "%s's net worth is %Ld, its accounts hold %Ld" label worth held
        | _ -> fail "no net worth")
      [ ("alice", alice, Analytics.Me); ("bob", bob, Me); ("the household", alice, Household); ("alice's family", alice, Family); ("bob's family", bob, Family) ];
    List.iter
      (fun (b, _) ->
        let expected = List.fold_left (fun sum (_, a) -> sum + a) 0 b.rows in
        if Int64.of_int expected <> balance service alice b.id then fail "%s holds %Ld, its statements %d" b.name (balance service alice b.id) expected)
      [ List.nth banks 0; List.nth banks 1; List.nth banks 2 ];
    let bob_expected = List.fold_left (fun sum (_, a) -> sum + a) 0 bob_checking.rows in
    if Int64.of_int bob_expected <> balance service bob bob_checking.id then fail "bob's checking differs from its statements"
  in
  let snapshot () = List.map (fun (a : Account.t) -> (a.id, a.balances)) (Store_sqlite.accounts service.db) in
  let last = ref None in
  for step = 1 to draw tc (integers ~min_value:1 ~max_value:14 ()) do
    let action : (string * (unit -> bool)) option =
      match draw tc (integers ~min_value:0 ~max_value:7 ()) with
      | 0 ->
          let target = pick banks in
          let rows = List.init (draw tc (integers ~min_value:1 ~max_value:4 ())) (fun _ -> (day (), pick (Array.to_list merchants), row_amount ())) in
          Some ("import", fun () -> import target rows)
      | 1 ->
          (* A move between two accounts, as each bank reports it. *)
          let from = pick banks in
          let into = pick (List.filter (fun (b, _) -> b.id <> (fst from).id) banks) in
          let amount = abs (row_amount ()) and d = day () in
          Some ("move", fun () -> let a = import from [ (d, "ÖVERFÖRING", -amount) ] in let b = import into [ (d, "ÖVERFÖRING", amount) ] in a || b)
      | 2 -> (
          match open_items alice @ open_items bob with
          | [] -> None
          | items ->
              let v = pick items in
              let access = Option.get (owner_of v) in
              let out = Account_id.equal v.transfer.transfer.credit_account_id v.item.bank_account_id in
              let bank = List.find (fun (b, _) -> Account_id.equal b.id v.item.bank_account_id) banks |> fst in
              let candidates =
                if bank.id = shared.id then
                  List.filter
                    (fun (i : Account_info.t) -> i.account.kind = (if out then Expense else Income) && i.details.role = None && Owner.equal i.account.owner (Household household.id))
                    (Service.accounts service access)
                else personal_categories access (if out then Expense else Income)
              in
              let category = pick candidates in
              Some ("categorize", fun () -> Result.is_ok (Inbox.categorize service access ~transfer_id:v.item.transfer_id ~category_id:category.account.id ())))
      | 3 -> (
          let personal (v : Inbox.item_view) = not (Account_id.equal v.item.bank_account_id shared.id) in
          match List.filter personal (open_items alice) @ List.filter personal (open_items bob) with
          | [] -> None
          | items ->
              let v = pick items in
              let access = Option.get (owner_of v) in
              Some ("share", fun () -> Result.is_ok (Inbox.share service access ~transfer_id:v.item.transfer_id ~household_category_id:household_groceries)))
      | 4 -> (
          let pairs =
            List.concat_map
              (fun access ->
                List.concat_map
                  (fun (v : Inbox.item_view) ->
                    match Inbox.pair_candidates service access ~transfer_id:v.item.transfer_id with
                    | Ok others -> List.map (fun (o : Inbox.item_view) -> (access, v, o)) others
                    | Error _ -> [])
                  (open_items access))
              [ alice; bob ]
          in
          match pairs with
          | [] -> None
          | pairs ->
              let access, v, o = pick pairs in
              let out_v, in_v = if Account_id.equal v.transfer.transfer.credit_account_id v.item.bank_account_id then (v, o) else (o, v) in
              Some ("pair", fun () -> Result.is_ok (Inbox.pair service access ~out_id:out_v.item.transfer_id ~in_id:in_v.item.transfer_id)))
      | 5 -> (
          match List.concat_map (fun (b, access) -> List.map (fun batch -> (b, access, batch)) (Import_store.batches service.db ~account_id:b.id ~limit:50)) banks with
          | [] -> None
          | batches ->
              let b, access, (batch : Import_store.batch) = pick batches in
              Some
                ( "undo",
                  fun () ->
                    match Import.undo (Import.context service access) ~batch_id:batch.batch_id with
                    | Ok _ ->
                        b.rows <- List.filter (fun (id, _) -> id <> batch.batch_id) b.rows;
                        (* 4. *)
                        let view = Store_sqlite.view service.db in
                        let ours = Import_store.batch_transfers service.db batch.batch_id in
                        let triage = List.concat_map (Import_store.triage_transfers service.db) ours in
                        List.iter (fun t -> if Option.is_none (view.reversal t) then failwith ("undo left a transfer in effect: " ^ Transfer_id.to_string t)) (ours @ triage);
                        true
                    | Error _ -> false ))
      | 6 -> (
          match open_items alice @ open_items bob with
          | [] -> None
          | items ->
              let v = pick items in
              Some ("dismiss", fun () -> Result.is_ok (Inbox.dismiss service (Option.get (owner_of v)) ~transfer_id:v.item.transfer_id)))
      | _ -> (
          (* 5. Retrying an import is refused as already imported, and every other action is
             idempotent, so none of them may change a balance. *)
          match !last with
          | Some (name, _) when name = "undo" || name = "import" || name = "move" -> None
          | Some (name, again) ->
              Some
                ( "retry " ^ name,
                  fun () ->
                    let before = snapshot () in
                    ignore (again ());
                    if snapshot () <> before then failwith ("retrying " ^ name ^ " changed the books");
                    false )
          | None -> None)
    in
    match action with
    | None -> ()
    | Some (name, run) ->
        if run () then last := Some (name, run);
        check (Printf.sprintf "step %d (%s)" step name)
  done

let%hegel_test books_stay_consistent tc = test tc [@@settings Settings.create ~test_cases:150 ()]

let () =
  Alcotest.run "service properties"
    [ ("books", [ Alcotest.test_case "imports, triage, undo and retries keep every scope's books consistent" `Slow books_stay_consistent ]) ]
