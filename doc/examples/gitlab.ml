(* This pipeline monitors a GitLab repository and uses Docker to build the
   latest version of the default branch. *)

let program_name = "gitlab"

open Current.Syntax

module Git = Current_git
module Gitlab = Current_gitlab
module Docker = Current_docker.Default

let () = Prometheus_unix.Logging.init ()

(* Link for GitLab statuses. *)
let url = Uri.of_string "http://localhost:8080"

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  run "opam install . --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam exec -- dune build @install @check @runtest"

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let github_status_of_state = function
  | Ok _              -> Gitlab.Api.Status.v ~url `Success ~description:"Passed" ~name:program_name
  | Error (`Active _) -> Gitlab.Api.Status.v ~url `Pending ~name:program_name
  | Error (`Msg m)    -> Gitlab.Api.Status.v ~url `Failure ~description:m ~name:program_name

let pipeline ~github ~repo () =
  let head = Gitlab.Api.head_commit github repo in
  let src = Git.fetch (Current.map Gitlab.Api.Commit.id head) in
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocaml/opam:alpine-3.13-ocaml-4.12" in
    `Contents (dockerfile ~base)
  in
  Docker.build ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Gitlab.Api.Commit.set_status head "ocurrent"

let main config mode github repo =
  let has_role = Current_web.Site.allow_all in
  let engine = Current.Engine.create ~config (pipeline ~github ~repo) in
  let routes =
    Routes.(s "webhooks" / s "gitlab" /? nil @--> Gitlab.webhook ~engine ~webhook_secret:(Gitlab.Api.webhook_secret github) ~has_role) ::
    Current_web.routes engine
  in
  let site = Current_web.Site.(v ~has_role) ~name:program_name routes in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.pos 0 (Arg.some Gitlab.Repo_id.cmdliner) None @@
  Arg.info
    ~doc:"The GitLab repository (owner/name) to monitor."
    ~docv:"REPO"
    []

let cmd =
  let doc = "Monitor a GitLab repository." in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_gitlab.Api.cmdliner $ repo)),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
