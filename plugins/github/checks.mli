
type t

val check_suite_webhook : Yojson.Safe.t -> unit
(** Call this when we get a "check_suite" event *)  

module CheckRunStatus : sig
  type t
  type state = [`Queued | `InProgress | `Completed ]

  (* Construct a CheckRunStatus.t *)
  val v : ?description:string -> ?url:Uri.t -> state -> t
end

module CheckSuite : sig
  type t = {
      id : int;  
      commit_id : Current_git.Commit_id.t;
    }
  val pp : t Fmt.t

  val compare : t -> t -> int
  (** Order by check_suite ID. *)
end

module CheckRun : sig
  type t

  val build : CheckSuite.t -> t
  (** Create CheckRun and set it's status. *)
  val set_status : t Current.t -> string -> CheckRunStatus.t Current.t -> unit Current.t
end


val build: string -> Mirage_crypto_pk.Rsa.priv -> t 

(* Used in pipeline to get changed check_suites. *)
val check_suites : t -> CheckSuite.t list Current.t

(* Setup monitor to update CheckSuite IntMap. *)
val monitor_check_suites : t -> unit -> 'a Lwt.t

val cmdliner : t Cmdliner.Term.t
