let program_name = "build_matrix"

open Current.Syntax

module Git = Current_git
module Docker = Current_docker.Default

let () = Prometheus_unix.Logging.init ()

let dockerfile ~base ~ocaml_version =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  run "opam switch %s" ocaml_version @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  env ["OPAMERRLOGLEN", "0"] @@
  run "opam install . --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let build ocaml_version =
    (** TODO This errors out locally saying 
        Result: Error: Command "docker" "pull" "ocaml/opam:debian-ocaml-4.10" exited with status 1
        Running the command in zsh is successful?
    *)
    let base = Docker.pull ~schedule:weekly ("ocaml/opam:debian-ocaml-" ^ ocaml_version) in
    let dockerfile =
      let+ base = base in
      `Contents (dockerfile ~base ~ocaml_version)
    in
    Docker.build ~label:ocaml_version ~pull:false ~dockerfile (`Git src) |>
    Docker.tag ~tag:(Fmt.str "example-%s" ocaml_version)
  in
  Current.all [
    build "4.10";
    build "4.11"
  ]

let main config mode repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (pipeline ~repo) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.value @@
  Arg.pos 0 Arg.dir (Sys.getcwd ()) @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo)),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
