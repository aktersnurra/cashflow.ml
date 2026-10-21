module Account_id = Cashflow_core.Account_id
module Household_id = Cashflow_core.Household_id
module Transfer_id = Cashflow_core.Transfer_id
module User_id = Cashflow_core.User_id

open Base

let now_ms () =
  let days, picoseconds = Ptime.Span.to_d_ps (Ptime.to_span (Ptime_clock.now ())) in
  let open Stdlib.Int64 in
  add (mul (of_int days) 86_400_000L) (div picoseconds 1_000_000_000L)

let make of_string =
  let generator = Uuidm.v7_non_monotonic_gen ~now_ms (Stdlib.Random.State.make_self_init ()) in
  fun () ->
    (* Generated UUIDs parsing back into nominal IDs is an internal invariant. *)
    match of_string (Uuidm.to_string (generator ())) with
    | Some id -> id
    | None -> assert false

let account = make Account_id.of_string
let transfer = make Transfer_id.of_string
let user = make User_id.of_string
let household = make Household_id.of_string
