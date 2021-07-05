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

let () = Prometheus_unix.Logging.init ~default_level:Logs.Info ()

(* Link for GitHub statuses. *)
(* 
   This will be a smee server running locally that forwards the 
   webhooks from GitHub.
*)
let url = Uri.of_string "http://127.0.0.1:8080/"

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

let github_status_of_state = function
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let build ~dockerfile src head =
  Docker.build ~pool ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocurrent"

let pipeline ~app () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocaml/opam:debian-10-ocaml-4.08" in
    `Contents (dockerfile ~base)
  in
  Github.App.installations app 
  |> Current.list_iter (module Github.Installation) @@ fun installation ->

  Github.Installation.repositories installation 
  |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->

  Github.Api.Repo.ci_refs ~staleness:(Duration.of_day 90) repo
  |> Current.list_iter (module Github.Api.Commit) @@ 
       fun head ->
       let src = Git.fetch (Current.map Github.Api.Commit.id head) in
       (* TODO Create CheckRun here. *)
       build ~dockerfile src head

(* let build_check_run ~dockerfile src head =
 *   let github_status_of_state = function
 *     | Ok _              -> Github.Checks.CheckRunStatus.v ~url `Completed ~description:"Passed"
 *     | Error (`Active _) -> Github.Checks.CheckRunStatus.v ~url `Queued
 *     | Error (`Msg m)    -> Github.Checks.CheckRunStatus.v ~url `Completed ~description:m
 *   in
 * 
 *   Docker.build ~pool ~pull:false ~dockerfile (`Git src)
 *   |> Current.state
 *   |> Current.map github_status_of_state
 *   |> Github.Checks.CheckRun.set_status head "ocurrent" (\** This should push CheckRun status out*\)
 * 
 * 
 * let pipeline ~app () = 
 *   let dockerfile =
 *     let+ base = Docker.pull ~schedule:weekly "ocaml/opam:debian-10-ocaml-4.08" in
 *     `Contents (dockerfile ~base)
 *   in
 *   Github.Checks.check_suites app
 *   |> Current.list_iter (module Github.Checks.CheckSuite) @@ fun check_suite ->
 *   
 *   let head = Current.map (fun x -> Github.Checks.CheckSuite.(x.commit_id)) check_suite in
 *   let src = Git.fetch head in
 *   let check_run = Current.map Github.Checks.CheckRun.build check_suite in
 *   build_check_run ~dockerfile src check_run *)

  (* 1. For each check_suite triggered return the CheckSuite.t type which includes
        the repo, head_sha *)
  
  (* 2. Create check_run here against the triggering PR? *)
  
  (* 3. Git fetch the latest and run the docker build. *)        

  (* 4. Update the check_run status here with the results of the build. 
     Future we'll want to return the errors from the build or lint step here.
   *)



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
  (* Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.Checks.cmdliner)), *)
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
