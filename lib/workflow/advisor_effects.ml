module A = Cashflow_core.Advisor
module T = Advisor_tools

type raw_call = { id : string; name : string; arguments : string; index : int }
type finish_reason = Stop | Tool_calls | Length | Content_filter
type model_event = Text of string | Complete_calls of raw_call list | Round_finished of finish_reason
type message = User of string | Assistant of string * raw_call list | Tool of { call_id : string; content : string }
type context_turn = { turn_id : A.Turn_id.t; attempt_id : A.Attempt_id.t; user_text : string; assistant_text : string }
type selection = { selected_turns : int; omitted_turns : int }
type selected_evidence = { model_content : string; display : A.evidence; serialized_bytes : int }

type context = {
  system_instructions : string;
  history : context_turn list;
  selection : selection;
  user_text : string;
  messages : message list;
  source_finding : A.finding option;
  evidence : selected_evidence list;
  locale : string;
  currency : Cashflow_core.Currency.t;
  model_aliases : string list;
  context_token_budget : int option;
  segment_id : A.Segment_id.t;
}

type context_size = { serialized_bytes : int; conservative_tokens : int; output_reserve_tokens : int }

type _ operation =
  | Check_authority : (unit, A.error) result operation
  | Now : Ptime.t operation
  | Check_cancelled : bool operation
  | Load_context : (context, A.error) result operation
  | Allocate_segment : (A.Segment_id.t, A.error) result operation
  | Measure_context : context -> (context_size, A.error) result operation
  | Begin_model_round : context -> (unit, A.error) result operation
  | Read_model_event : (model_event option, A.error) result operation
  | Close_model_round : unit operation
  | Parse_tool : raw_call -> (T.request, A.error) result operation
  | Read_tool : T.request -> (T.evidence, A.error) result operation
  | Select_evidence : T.evidence -> (selected_evidence, A.error) result operation
  | Append_events : A.event_payload list -> (unit, A.error) result operation
  | Commit_terminal : A.transition -> (A.run_state, A.error) result operation
  | Persist_turn : A.submit -> (A.accepted, A.error) result operation
  | Persist_retry : A.retry -> (A.accepted, A.error) result operation
  | Start_run : A.accepted -> unit operation
  | Request_cancel : A.Run_id.t -> (A.run_state, A.error) result operation
  | Delete_owned_conversation : A.Conversation_id.t -> (unit, A.error) result operation

type handler = { handle : 'a. 'a operation -> 'a }
type _ Effect.t += Operation : 'a operation -> 'a Effect.t

let perform op = Effect.perform (Operation op)

let run handler f =
  Effect.Deep.try_with f ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Operation op ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  (* Only interpreter failures discontinue the suspended workflow. Exceptions
             from the resumed computation must never resume this continuation twice. *)
                  match handler.handle op with
                  | value -> Effect.Deep.continue k value
                  | exception exn ->
                      let backtrace = Printexc.get_raw_backtrace () in
                      Effect.Deep.discontinue_with_backtrace k exn backtrace)
          | _ -> None);
    }
