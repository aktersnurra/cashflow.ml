open Cashflow_core
module A = Advisor

let now = Test_service_support.now
let session = Option.get (A.Session_ref.of_hash (String.make 64 'a'))

let fixture () =
  let service = Test_service_support.make_service () in
  let access = Test_service_support.sign_in service "advisor-fixture" in
  let owner = (Access.user access).User.id in
  let clock = ref now in
  let db = service.Cashflow_io.Service.db in
  (db, Cashflow_io.Advisor_store.create ~clock:(fun () -> !clock) db, owner, clock)

let submit ?conversation ?(version = 0L) key : A.submit =
  {
    conversation_id = conversation;
    scope = Personal;
    currency = Result.get_ok (Currency.make ~code:"SEK" ~scale:2);
    locale = "en";
    user_text = "Explain the synthetic budget";
    idempotency_key = Test_service_support.key key;
    expected_version = version;
    source_finding_key = None;
  }

let ok = function Ok x -> x | Error _ -> Alcotest.fail "unexpected advisor error"
let segment n = Option.get (A.Segment_id.of_string (Printf.sprintf "00000000-0000-4000-8000-%012d" n))
