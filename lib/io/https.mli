val certificate_bundle : unit -> (X509.Certificate.t list, unit) result
val tls_config : unit -> (Tls.Config.client, unit) result
val connector : Tls.Config.client -> Uri.t -> [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t -> Tls_eio.t
type post = uri:string -> headers:(string * string) list -> body:string -> (string, string) result
val post : < net : _ Eio.Net.t; .. > -> post
type stream_error = Tls_configuration | Invalid_scheme | Http_status of int | Transport of string
type stream_post = uri:string -> headers:(string * string) list -> body:string -> on_chunk:(string -> unit) -> (unit, stream_error) result
(** The optional trusted TLS configuration is a construction-time seam for local
    fixtures. Production callers omit it to use system CA verification. *)
val stream_post : ?tls:Tls.Config.client -> < net : _ Eio.Net.t; .. > -> stream_post
