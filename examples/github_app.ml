(* This pipeline is GitHub app.
   It monitors all GitHub repositories the app is asked to handle, and uses
   Docker to build the latest version on all branches and PRs. *)

let program_name = "github_app"

open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

(* Limit to one build at a time. *)
let pool = Current.Pool.create ~label:"docker" 1

let () = Prometheus_unix.Logging.init ()

(* Link for GitHub statuses. *)
let url = Uri.of_string "http://localhost:8080"

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  run "opam install . --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Map from Current.state to CheckRunStatus *)
let github_check_run_status_of_state = function
  | Ok _              -> Github.Api.CheckRunStatus.v ~url (`Completed `Success) ~description:"Passed"
  | Error (`Active _) -> Github.Api.CheckRunStatus.v ~url `Queued
  | Error (`Msg m)    -> Github.Api.CheckRunStatus.v ~url (`Completed (`Failure m)) ~description:m

let pipeline ~app () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocaml/opam:alpine-3.12-ocaml-4.08" in
    `Contents (dockerfile ~base)
  in
  Github.App.installations app |> Current.list_iter (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  Github.Api.Repo.ci_refs ~staleness:(Duration.of_day 90) repo
  |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  Docker.build ~pool ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_check_run_status_of_state 
  |> Github.Api.CheckRun.set_status head program_name

let main config mode app =
  Lwt_main.run begin
    let engine = Current.Engine.create ~config (pipeline ~app) in
    let routes =
      Routes.(s "webhooks" / s "github" /? nil @--> Github.webhook) ::
      Current_web.routes engine
    in
    let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Monitor a GitHub app's repositories." in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.App.cmdliner)),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
