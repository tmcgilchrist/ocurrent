type t

val input_webhook : Yojson.Safe.t -> unit
(** Call this when we get a "check_suite" event *)  

(* val v : get_token:(unit -> Api.token Lwt.t) -> string -> t
 * (\** [v ~get_token account] is a configuration that uses [get_token] when it needs 
 *     to get or refresh the API token.
 *     Note: 
 *       [get_token] can return a failed token, in which case the expiry time says when to try again.
 *       If [get_token] instead raises an exception, this is turned into an error token 
 *       with a 1 minute expiry.
 *     @param account This is a string used to label point counters in Prometheus. *\) *)

(**

*)
