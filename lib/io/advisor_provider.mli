module A = Cashflow_core.Advisor

type raw_call = { id : string; name : string; arguments : string; index : int }
type message = User of string | Assistant of string * raw_call list | Tool of { call_id : string; content : string }
type request = { system : string; messages : message list }
(** The eight application-owned schemas are always attached; callers cannot supply
    schemas, routing/privacy overrides, plugins or server tools. *)
type finish_reason = Stop | Tool_calls | Length | Content_filter
type usage = { prompt_tokens : int; completion_tokens : int; total_tokens : int }
type provider_event = Text of string | Calls_ready of raw_call list | Round_finished of finish_reason
type generation_limits = {
  context_tokens : int;
  completion_tokens : int;
}
(** Bounded context/completion policy. Request admission uses a byte-based token
    estimate, not an externally attested upper bound on provider tokenization.
    Upstream context rejection is terminal; no relaxed retry or route fallback. *)
val supported_limits : model:string -> provider_only:string list -> generation_limits option
(** Documented Sonnet 5.5 or 4.6 on Bedrock profile, or None for unsupported configurations. *)
type t
type prepared_request
val model : t -> string
val limits : t -> generation_limits option
val prepare : t -> request -> (prepared_request, A.error) result
val serialized_bytes : prepared_request -> int
(** Estimated request tokens including a framing allowance. *)
val conservative_tokens : prepared_request -> int
val stream_prepared : t -> prepared_request -> on_activity:(unit -> unit) ->
  on_event:(provider_event -> unit) -> (usage, A.error) result
(** Sends the immutable measured bytes; a prepared request is bound to its originating
    provider instance. Activity observes every raw chunk, including ignored fields and
    comments, without exposing content. Exceptions/cancellation propagate. *)
val tool_schemas : Yojson.Safe.t list
val create : ?limits:generation_limits -> ?app_url:string -> post:Https.stream_post -> api_key:string -> model:string -> provider_only:string list -> unit -> t
val stream : t -> request -> on_event:(provider_event -> unit) -> (usage, A.error) result
(** One finite round, no execution or retry. Calls and finish are published only
    after DONE and successful transport EOF. Text is delivered incrementally.
    Eio cancellation propagates. Runtime activity supervision uses [stream_prepared];
    deadlines belong to the caller. *)
