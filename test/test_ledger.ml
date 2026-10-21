open Test_support

module Suite (S : STORE) = struct
  include Make (S)

let setup () =
  let store = S.create () and env = env () in
  let checking = open_account store env "checking" Asset in
  let salary = open_account store env "salary" Income in
  let groceries = open_account store env "groceries" Expense in
  (store, env, checking, salary, groceries)

let test_posted_transfer_moves_both_balances () =
  let store, env, checking, salary, groceries = setup () in
  expect [ created; created ]
    (submit_ok store env
       [ transfer "pay" ~debit:checking ~credit:salary 30_000_00; transfer "food" ~debit:groceries ~credit:checking 450_00 ]);
  Alcotest.check int64 "checking" 29_550_00L (balance store checking);
  Alcotest.check int64 "salary is a positive income" 30_000_00L (balance store salary);
  Alcotest.check int64 "groceries is a positive expense" 450_00L (balance store groceries);
  Alcotest.(check int) "verify" 0 (List.length (L.verify store))

let test_retry_with_same_key_is_safe () =
  let store, env, checking, salary, _ = setup () in
  let pay = transfer "pay" ~debit:checking ~credit:salary 100_00 in
  expect [ created ] (submit_ok store env [ pay ]);
  expect [ exists ] (submit_ok store env [ pay ]);
  Alcotest.check int64 "counted once" 100_00L (balance store checking);
  expect [ failed `Exists_with_different_fields ] (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 101_00 ])

let test_keys_are_scoped_per_owner () =
  let store, env, checking, salary, _ = setup () in
  let pay = transfer "pay" ~debit:checking ~credit:salary 100_00 in
  expect [ created ] (submit_ok store env [ pay ]);
  expect [ created ] (submit_ok store env ~scope:household [ pay ])

let test_duplicate_key_within_a_batch_exists () =
  let store, env, checking, salary, _ = setup () in
  let pay = transfer "pay" ~debit:checking ~credit:salary 100_00 in
  expect [ created; exists ] (submit_ok store env [ pay; pay ]);
  Alcotest.check int64 "counted once" 100_00L (balance store checking)

let test_account_rules () =
  let store, env, checking, salary, _ = setup () in
  let euros = open_account store env "euros" Asset ~currency:eur in
  let missing = Id_generator.account () in
  expect
    [ failed `Accounts_must_be_different; failed `Debit_account_not_found; failed `Credit_account_not_found;
      failed `Accounts_must_have_same_currency ]
    (submit_ok store env
       [ transfer "a" ~debit:checking ~credit:checking 1; transfer "b" ~debit:missing ~credit:salary 1;
         transfer "c" ~debit:checking ~credit:missing 1; transfer "d" ~debit:euros ~credit:salary 1 ])

let test_balance_limits_include_pending () =
  let store = S.create () and env = env () in
  (* Money leaves an asset as a credit, so an account that can't go below zero never credits more
     than it was debited. *)
  let card = open_account store env "card" Asset ~flags:{ Account.no_flags with credits_must_not_exceed_debits = true } in
  let salary = open_account store env "salary" Income in
  let shop = open_account store env "shop" Expense in
  expect [ created ] (submit_ok store env [ transfer "pay" ~debit:card ~credit:salary 100_00 ]);
  expect [ failed `Exceeds_debits ] (submit_ok store env [ transfer "too much" ~debit:shop ~credit:card 100_01 ]);
  let hold = transfer "hold" ~debit:shop ~credit:card 60_00 ~phase:(Pending { expires_on = None }) in
  expect [ created ] (submit_ok store env [ hold ]);
  expect [ failed `Exceeds_debits ] (submit_ok store env [ transfer "over the hold" ~debit:shop ~credit:card 40_01 ]);
  expect [ created ] (submit_ok store env [ transfer "within" ~debit:shop ~credit:card 40_00 ])

let test_linked_chain_is_atomic () =
  let store, env, checking, salary, groceries = setup () in
  let missing = Id_generator.account () in
  expect [ failed `Linked_event_failed; failed `Credit_account_not_found; created ]
    (submit_ok store env
       [ transfer "first" ~linked:true ~debit:checking ~credit:salary 100_00; transfer "second" ~debit:groceries ~credit:missing 1;
         transfer "unlinked" ~debit:checking ~credit:salary 5_00 ]);
  Alcotest.check int64 "only the unlinked transfer" 5_00L (balance store checking);
  expect [ failed `Linked_event_failed; failed `Linked_event_chain_open ]
    (submit_ok store env
       [ transfer "open-1" ~linked:true ~debit:checking ~credit:salary 1; transfer "open-2" ~linked:true ~debit:checking ~credit:salary 1 ])

let test_retrying_a_linked_chain_reports_it_exists () =
  let store, env, checking, salary, groceries = setup () in
  let chain = [ transfer "a" ~linked:true ~debit:checking ~credit:salary 100_00; transfer "b" ~debit:groceries ~credit:checking 10_00 ] in
  expect [ created; created ] (submit_ok store env chain);
  expect [ exists; exists ] (submit_ok store env chain);
  expect [ exists; failed `Linked_event_failed ]
    (submit_ok store env [ List.hd chain; transfer "c" ~debit:groceries ~credit:checking 1_00 ]);
  Alcotest.check int64 "unchanged" 90_00L (balance store checking)

(* The same key twice in one chain: the second finds the first, which only exists if the chain
   commits, so it must never be reported as existing when the chain doesn't. *)
let test_same_key_twice_in_a_chain () =
  let store, env, checking, salary, groceries = setup () in
  let a = transfer "a" ~linked:true ~debit:checking ~credit:salary 100_00 in
  expect [ created; exists ] (submit_ok store env [ a; { a with linked = false } ]);
  Alcotest.check int64 "posted once" 100_00L (balance store checking);
  let b = transfer "b" ~linked:true ~debit:groceries ~credit:checking 10_00 in
  (* Linked to a transfer that already exists: the chain is a partial match, so nothing is created
     and only what existed before is reported. *)
  let results = submit_ok store env [ b; { b with linked = true }; { a with linked = false } ] in
  expect [ failed `Linked_event_failed; failed `Linked_event_failed; exists ] results;
  Alcotest.check int64 "b rolled back" 100_00L (balance store checking)

let test_pending_post_and_void () =
  let store, env, checking, salary, groceries = setup () in
  ignore (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 1_000_00 ]);
  let hold = transfer "hold" ~debit:groceries ~credit:checking 300_00 ~phase:(Pending { expires_on = None }) in
  let hold_id = created_id (List.hd (submit_ok store env [ hold ])) in
  Alcotest.check int64 "posted untouched" 1_000_00L (balance store checking);
  Alcotest.check int64 "reserved" (-300_00L) (pending store checking);
  let post amount_ key_name =
    { Ledger_event.key = key key_name; linked = false; body = Post_pending { pending_id = hold_id; amount = amount_; value_date = today } }
  in
  expect [ failed `Exceeds_pending_transfer_amount ] (submit_ok store env [ post (Some (amount 300_01)) "too much" ]);
  expect [ created ] (submit_ok store env [ post (Some (amount 250_00)) "capture" ]);
  Alcotest.check int64 "captured" 750_00L (balance store checking);
  Alcotest.check int64 "rest released" 0L (pending store checking);
  expect [ exists ] (submit_ok store env [ post (Some (amount 250_00)) "capture" ]);
  expect [ failed `Pending_transfer_already_posted ] (submit_ok store env [ post None "again" ]);
  let void = { Ledger_event.key = key "void"; linked = false; body = Void_pending { pending_id = hold_id; value_date = today } } in
  expect [ failed `Pending_transfer_already_posted ] (submit_ok store env [ void ]);
  let pay_id = (List.hd (S.transfers store)).id in
  expect [ failed `Pending_transfer_not_pending ]
    (submit_ok store env [ { void with key = key "void-2"; body = Void_pending { pending_id = pay_id; value_date = today } } ]);
  Alcotest.(check int) "verify" 0 (List.length (L.verify store))

let test_void_releases_and_expiry () =
  let store, env, checking, salary, groceries = setup () in
  ignore (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 1_000_00 ]);
  let hold expires_on name = transfer name ~debit:groceries ~credit:checking 100_00 ~phase:(Pending { expires_on }) in
  ignore (submit_ok store env [ hold (Some (date "2026-10-20")) "expired"; hold (Some today) "today"; hold None "open" ]);
  Alcotest.check int64 "three holds" (-300_00L) (pending store checking);
  let voided = L.void_expired store env ~today in
  Alcotest.(check int) "only the expired one" 1 (List.length voided);
  Alcotest.check int64 "released" (-200_00L) (pending store checking);
  Alcotest.(check int) "running it again voids nothing" 0 (List.length (L.void_expired store env ~today))

let test_reversal () =
  let store, env, checking, salary, groceries = setup () in
  ignore (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 1_000_00 ]);
  let food_id = created_id (List.hd (submit_ok store env [ transfer "food" ~debit:groceries ~credit:checking 200_00 ])) in
  let reverse key_name transfer_id =
    { Ledger_event.key = key key_name; linked = false; body = Reverse { transfer_id; value_date = None; memo = None } }
  in
  let results = submit_ok store env [ reverse "undo" food_id ] in
  let reversal_id = created_id (List.hd results) in
  Alcotest.check int64 "money back" 1_000_00L (balance store checking);
  Alcotest.check int64 "expense undone" 0L (balance store groceries);
  expect [ exists ] (submit_ok store env [ reverse "undo" food_id ]);
  expect [ failed `Transfer_already_reversed; failed `Transfer_not_reversible; failed `Transfer_not_found ]
    (submit_ok store env [ reverse "undo-again" food_id; reverse "undo-undo" reversal_id; reverse "nothing" (Id_generator.transfer ()) ])

let test_correction_is_a_linked_reversal_and_repost () =
  let store, env, checking, salary, groceries = setup () in
  ignore (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 1_000_00 ]);
  let food_id = created_id (List.hd (submit_ok store env [ transfer "food" ~debit:groceries ~credit:checking 200_00 ])) in
  expect [ created; created ]
    (submit_ok store env
       [ { Ledger_event.key = key "fix:reverse"; linked = true; body = Reverse { transfer_id = food_id; value_date = None; memo = None } };
         transfer "fix:repost" ~debit:groceries ~credit:checking 120_00 ]);
  Alcotest.check int64 "corrected" 120_00L (balance store groceries)

let test_close_account () =
  let store, env, checking, salary, groceries = setup () in
  ignore (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 100_00 ]);
  let closed result = match result with Ok (account : Account.t) -> string_of_bool account.closed | Error e -> Ledger_error.account_to_string e in
  Alcotest.(check string) "asset with money" (Ledger_error.account_to_string `Account_balance_not_zero) (closed (L.close_account store checking));
  Alcotest.(check string) "income with history" "true" (closed (L.close_account store salary));
  expect [ failed `Credit_account_closed ] (submit_ok store env [ transfer "late pay" ~debit:checking ~credit:salary 1 ]);
  ignore groceries

let test_open_account_is_idempotent () =
  let store = S.create () and env = env () in
  let spec : Account.spec = { owner = me; kind = Asset; currency = sek; name = "  checking "; flags = Account.no_flags } in
  let first = L.open_account store env (key "acc") spec and again = L.open_account store env (key "acc") spec in
  (match first, again with
  | Ok (Created a), Ok (Exists b) -> Alcotest.(check bool) "same account" true (Account_id.equal a.id b.id); Alcotest.(check string) "trimmed" "checking" a.name
  | _ -> Alcotest.fail "expected created, then exists");
  match L.open_account store env (key "acc") { spec with kind = Expense } with
  | Error `Exists_with_different_fields -> ()
  | _ -> Alcotest.fail "expected exists_with_different_fields"

let test_batch_limits () =
  let store, env, checking, salary, _ = setup () in
  (match L.submit store env me [] with Error `Batch_empty -> () | _ -> Alcotest.fail "empty batch accepted");
  let too_many = List.init (Ledger_core.max_batch_size + 1) (fun i -> transfer (string_of_int i) ~debit:checking ~credit:salary 1) in
  match L.submit store env me too_many with Error (`Batch_too_large _) -> () | _ -> Alcotest.fail "oversized batch accepted"

let test_verify_detects_tampering () =
  let store, env, checking, salary, _ = setup () in
  ignore (submit_ok store env [ transfer "pay" ~debit:checking ~credit:salary 100_00 ]);
  let tampered = account store checking in
  S.save_account store { tampered with balances = { tampered.balances with debits_posted = 1L } };
  Alcotest.(check int) "one mismatch" 1 (List.length (L.verify store))

  let suites =
    [ ( "transfers",
        [ Alcotest.test_case "posted transfer moves both balances" `Quick test_posted_transfer_moves_both_balances;
          Alcotest.test_case "retry with the same key is safe" `Quick test_retry_with_same_key_is_safe;
          Alcotest.test_case "keys are scoped per owner" `Quick test_keys_are_scoped_per_owner;
          Alcotest.test_case "duplicate key within a batch" `Quick test_duplicate_key_within_a_batch_exists;
          Alcotest.test_case "account rules" `Quick test_account_rules;
          Alcotest.test_case "balance limits include pending" `Quick test_balance_limits_include_pending;
          Alcotest.test_case "batch limits" `Quick test_batch_limits ] );
      ( "linked",
        [ Alcotest.test_case "chain is atomic" `Quick test_linked_chain_is_atomic;
          Alcotest.test_case "retrying a chain" `Quick test_retrying_a_linked_chain_reports_it_exists;
          Alcotest.test_case "the same key twice in a chain" `Quick test_same_key_twice_in_a_chain;
          Alcotest.test_case "correction" `Quick test_correction_is_a_linked_reversal_and_repost ] );
      ( "pending",
        [ Alcotest.test_case "post and void" `Quick test_pending_post_and_void;
          Alcotest.test_case "void and expiry" `Quick test_void_releases_and_expiry ] );
      ("reversal", [ Alcotest.test_case "reversal" `Quick test_reversal ]);
      ( "accounts",
        [ Alcotest.test_case "close" `Quick test_close_account;
          Alcotest.test_case "open is idempotent" `Quick test_open_account_is_idempotent;
          Alcotest.test_case "verify detects tampering" `Quick test_verify_detects_tampering ] ) ]
end

let () =
  Alcotest.run "ledger"
    (List.concat_map
       (fun (module S : STORE) ->
         let module Suite = Suite (S) in
         List.map (fun (name, cases) -> (S.name ^ " " ^ name, cases)) Suite.suites)
       stores)
