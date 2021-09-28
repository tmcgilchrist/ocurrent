(** Integration with GitLab.

https://docs.gitlab.com/ee/user/project/integrations/webhooks.html
*)

val webhook : engine:Current.Engine.t
              -> webhook_secret:string
              -> has_role:(Current_web.User.t option -> Current_web.Role.t -> bool)
              -> Current_web.Resource.t
(** GitLab webhook endpoint.

To trigger events this MUST be added to {! Current_web.routes }. This webhook handles the events:

 - Merge request

*)

module Repo_id : sig
  type t = { owner : string; name : string }

  val pp : t Fmt.t

  val compare : t -> t -> int

  val cmdliner : t Cmdliner.Arg.conv
end

(** Access to the GitLab API. *)
module Api : sig
  type t
  (** Configuration for accessing GitLab. *)

  val webhook_secret : t -> string

  type refs

  module Status : sig
    type t

    type state = [`Cancelled | `Failure | `Running | `Pending | `Success ]

    val v : name:string -> ?description:string -> ?url:Uri.t -> state -> t
  end

  module Commit : sig
    type t

    val id : t -> Current_git.Commit_id.t
    (** The commit ID, which can be used to fetch it. *)

    val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
    (** [set_status commit context status] sets the status of [commit]/[context] to [status]. *)

    val owner_name : t -> string
    (** [owner_name t] is the "owner/name" string identifying the repository. *)

    val repo_id : t -> Repo_id.t
    (** Like [owner_name], but as a [Repo_id.t]. *)

    val hash : t -> string
    (** [hash t] is the Git commit hash of [t]. *)

    val committed_date : t -> string
    (** [committed_date t] is the datetime when [t] was committed *)

    val pp : t Fmt.t
    val compare : t -> t -> int

    val uri : t -> Uri.t
    (** [uri t] is a URI for the GitLab web page showing [t]. *)

  end

  val head_commit : t -> Repo_id.t -> Commit.t Current.t

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitLab configuration. *)
end