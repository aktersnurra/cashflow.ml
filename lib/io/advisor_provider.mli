module A = Cashflow_core.Advisor

type raw_call = { id : string; name : string; arguments : string; index : int }
type message = User of string | Assistant of string * raw_call list | Tool of { call_id : string; content : string }
type request = { system : string; messages : message list }
(** The eight application-owned schemas are always attached; callers cannot supply
    schemas, routing/privacy overrides, plugins or server tools. *)
type finish_reason = Stop | Tool_calls | Length | Content_filter
type usage = { prompt_tokens : int; completion_tokens : int; total_tokens : int }
type provider_event = Text of string | Calls_ready of raw_call list | Round_finished of finish_reason
type t
val tool_schemas : Yojson.Safe.t list
val create : post:Https.stream_post -> api_key:string -> model:string -> provider_only:string list -> t
val stream : t -> request -> on_event:(provider_event -> unit) -> (usage, A.error) result
(** One finite round, no execution or retry. Calls and finish are published only
    after DONE and successful transport EOF. Text is delivered incrementally.
    Eio cancellation propagates. Raw activity supervision
    can wrap the injected [post]'s [on_chunk]; deadlines belong to the caller. *)
