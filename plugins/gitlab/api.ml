open Lwt.Infix
open Current.Syntax

(* Limit updates to one at a time for now. *)
let pool = Current.Pool.create ~label:"gitlab" 1

(* When we get a webhook event, we fire the condition for that repository `owner/name`, if any. *)
let webhook_cond = Hashtbl.create 10

let input_webhook (body : Gitlab_t.webhook) =
  match body with
  | `MergeRequest mr ->
    let owner_name = mr.merge_request_webhook_project.project_webhook_path_with_namespace in
    (match Hashtbl.find_opt webhook_cond owner_name with
     | Some cond -> Lwt_condition.broadcast cond ()
     | None -> Log.info (fun f -> f "Got webhook event for %S, but we're not interested in that" owner_name))
  | _-> Log.info (fun f -> f "Got webhook event we're not interested in.")


let read_file path =
  let ch = open_in_bin path in
  Fun.protect
    (fun () ->
       let len = in_channel_length ch in
       really_input_string ch len
    )
    ~finally:(fun () -> close_in ch)

type token = {
  token : (string, [`Msg of string]) result;
  expiry : float option;
}

let no_token = {
  token = Error (`Msg "Not fetched yet");
  expiry = Some (-1.0);

}
module Repo_map = Map.Make(Repo_id)

module Status = struct
  type state = [`Cancelled | `Failure | `Running | `Pending | `Success ]

  type t = {
    state: state;
    name: string;
    description: string option;
    url : Uri.t option;
  }

  let v ~name ?description ?url state =
    { state; name; description; url }

  let state_to_string = function
    | `Cancelled -> "cancelled"
    | `Failure -> "failure"
    | `Running -> "running"
    | `Pending -> "pending"
    | `Success -> "success"

  (* TODO JSON version of this might be unnecessary! *)
  let json_items { state; name; description; url } =
    ["state", `String (state_to_string state)
    ; "name", `String name] @
    (match description with None -> [] | Some x -> ["description", `String x]) @
    (match url with None -> [] | Some x -> ["target_url", `String (Uri.to_string x)])

  let digest t = Yojson.Safe.to_string @@ `Assoc (json_items t)

  let pp f t = Fmt.string f (digest t)
end

module Ref = struct
  type t = [ `Ref of string | `PR of int ] [@@deriving to_yojson]

  let compare = Stdlib.compare

  let pp f = function
    | `Ref r -> Fmt.string f r
    | `PR pr -> Fmt.pf f "PR %d" pr

  let to_git = function
    | `Ref head -> head
    | `PR id -> Fmt.str "merge_requests/%d" id
end

module Ref_map = Map.Make(Ref)

module Commit_id = struct
  type t = {
    owner: string;
    repo : string;
    id : Ref.t;
    hash : string;
    committed_date : string;
  } [@@deriving to_yojson]

  let to_git { owner; repo; id; hash; committed_date = _ } =
    let repo = Fmt.str "https://gitlab.com/%s/%s.git" owner repo in
    let gref = Ref.to_git id in
    Current_git.Commit_id.v ~repo ~gref ~hash

  let owner_name { owner; repo; _} = Fmt.str "%s/%s" owner repo

  let uri t =
    Uri.make ~scheme:"https" ~host:"gitlab.com" ~path:(Printf.sprintf "/%s/%s/-/commit/%s" t.owner t.repo t.hash) ()

  let pp_id = Ref.pp

  let compare {owner; repo; id; hash; committed_date = _} b =
    match compare hash b.hash with
    | 0 ->
      begin match Ref.compare id b.id with
        | 0 -> compare (owner, repo) (b.owner, b.repo)
        | x -> x
      end
    | x -> x

  let pp f { owner; repo; id; hash; committed_date } =
    Fmt.pf f "%s/%s@ %a@ %s@ %s" owner repo pp_id id (Astring.String.with_range ~len:8 hash) committed_date

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

type t = {
  account : string;          (* Prometheus label used to report points. *)
  get_token : unit -> token Lwt.t;
  webhook_secret : string; (* Shared secret for validating webhooks from GitLab *)
  token_lock : Lwt_mutex.t;
  mutable token : token;
  mutable head_monitors : commit Current.Monitor.t Repo_map.t;
  mutable refs_monitors : refs Current.Monitor.t Repo_map.t;
}
and commit = t * Commit_id.t
and refs = {
  default_ref : string;
  all_refs : commit Ref_map.t
}

let webhook_secret t = t.webhook_secret

let default_ref t = t.default_ref

let v ~get_token ~account ~webhook_secret () =
  let head_monitors = Repo_map.empty in
  let refs_monitors = Repo_map.empty in
  let token_lock = Lwt_mutex.create () in
  { get_token; token_lock; token = no_token; head_monitors; refs_monitors; account; webhook_secret }

let of_oauth ~token ~webhook_secret =
  let get_token () = Lwt.return { token = Ok token; expiry = None} in
  v ~get_token ~account:"oauth" ~webhook_secret ()

let get_token t =
  Lwt_mutex.with_lock t.token_lock @@ fun () ->
  let now = Unix.gettimeofday () in
  match t.token with
  | { token; expiry = None } -> Lwt.return token
  | { token; expiry = Some expiry } when now < expiry -> Lwt.return token
  | _ ->
    Log.info (fun f -> f "Getting API token");
    Lwt.catch t.get_token
      (fun ex ->
         Log.warn (fun f -> f "Error getting GitLab token: %a" Fmt.exn ex);
         let token = Error (`Msg "Failed to get GitLab token") in
         let expiry = Some (now +. 60.0) in
         Lwt.return {token; expiry}
      )
    >|= fun token ->
    t.token <- token;
    token.token

let await_event ~owner_name =
  let cond =
    match Hashtbl.find_opt webhook_cond owner_name with
    | Some c -> c
    | None ->
      let c = Lwt_condition.create () in
      Hashtbl.add webhook_cond owner_name c;
      c
  in
  Lwt_condition.wait cond

let get_commit repo_owner repo_name =
  let open Gitlab in
  let open Monad in
  Project.by_name ~owner:repo_owner ~name:repo_name () >>~ fun projects ->
  let project = List.hd projects in
  Project.Commit.commits ~project_id:project.Gitlab_t.project_short_id ~ref_name:project.project_short_default_branch ()
  |> Stream.to_list
  >|= fun x -> (List.hd x, project.project_short_default_branch)

(* Get latest Git ref for the default branch in GitLab? *)
(* TODO Handle rate limiting? via get_token t >>= use token *)
let get_default_ref _t { Repo_id.owner = repo_owner; name = repo_name } =
  let prefix = "refs/heads/" in
  Gitlab.Monad.run (get_commit repo_owner repo_name) >>= fun (c, branch_name) ->
  (* Get the first commit, which should be the latest one on that branch. ie the default branch. *)
  Lwt.return { Commit_id.owner = repo_owner
         ; repo = repo_name; id = `Ref (prefix ^ branch_name)
         ; hash = c.commit_id
         ; committed_date = c.commit_created_at }

let make_head_commit_monitor t repo =
  let read () =
    Lwt.catch
      (fun () -> get_default_ref t repo >|= fun c -> Ok (t, c))
      (fun ex -> Lwt_result.fail @@ `Msg (Fmt.str "GitLab query for %a failed: %a" Repo_id.pp repo Fmt.exn ex))
  in
  let watch refresh =
    let owner_name = Printf.sprintf "%s/%s" repo.owner repo.name in
    let rec aux x =
      x >>= fun () ->
      let x = await_event ~owner_name in
      refresh ();
      Lwt_unix.sleep 10.0 >>= fun () ->   (* Limit updates to 1 per 10 seconds *)
      aux x
    in
    let x = await_event ~owner_name in
    let thread =
      Lwt.catch
        (fun () -> aux x)
        (function
          | Lwt.Canceled -> Lwt.return_unit
          | ex -> Log.err (fun f -> f "head_commit thread failed: %a" Fmt.exn ex); Lwt.return_unit
        )
    in
    Lwt.return (fun () -> Lwt.cancel thread; Lwt.return_unit)
  in
  let pp f = Fmt.pf f "Watch %a default ref head" Repo_id.pp repo in
  Current.Monitor.create ~read ~watch ~pp


let head_commit t repo =
  Current.component "%a head" Repo_id.pp repo |>
  let> () = Current.return () in
  let monitor =
    match Repo_map.find_opt repo t.head_monitors with
    | Some i -> i
    | None ->
      let i = make_head_commit_monitor t repo in
      t.head_monitors <- Repo_map.add repo i t.head_monitors;
      i
  in
  Current.Monitor.get monitor

module Commit = struct
  module Set_status = struct
    let id ="gitlab-set-status"

    type nonrec t = t

    module Key = struct
      type t = {
        commit : Commit_id.t;
        context : string;
      }

      let to_json { commit; context } =
        `Assoc [
          "commit", `String (Commit_id.digest commit);
          "context", `String context
        ]

      let digest t = Yojson.Safe.to_string @@ to_json t
    end

    module Value = Status

    module Outcome = Current.Unit

    let auto_cancel = true

    let pp f ({ Key.commit; context }, status) =
      Fmt.pf f "Set %a/%s to@ %a"
        Commit_id.pp commit
        context
        Value.pp status

    (* TODO Store Project_id along with owner/project_name. GitLab is oriented around project_ids *)
    let publish t job key (status : Value.t) =
      let state_to_gitlab = function
        | `Cancelled -> `Cancelled
        | `Failure -> `Failed
        | `Running -> `Running
        | `Pending -> `Pending
        | `Success -> `Success in

      Current.Job.start job ~pool ~level:Current.Level.Above_average >>= fun () ->
      let {Key.commit; context=_} = key in
      get_token t >>= function
      | Error (`Msg m) -> Lwt.fail_with m
      | Ok token ->
        Lwt.try_bind
          (fun () ->
            Gitlab.Monad.run (
              let open Gitlab in
              let open Monad in
              let token = Token.of_string token in
              let sha = commit.Commit_id.hash in
              let* project = Project.by_name ~token ~owner:commit.owner ~name:commit.repo () >>~ fun x -> return (List.hd x) in
              Project.Commit.status ~token ~project_id:project.Gitlab_t.project_short_id ~sha ~state:(state_to_gitlab status.Status.state) ~name:id ()
              >>~ fun resp -> return (resp))
          )
          (fun (_ : Gitlab_t.commit_status) -> Lwt_result.return ())
          (fun ex ->
               Log.err (fun f -> f "@[<v2>%a failed: %a@]"
                            pp (key, status)
                            Fmt.exn ex);
               let s = Fmt.str "%a" Fmt.exn ex in
               Lwt_result.fail (`Msg ("Failed to set GitLab status " ^ s))
          )
  end

  module Set_status_cache = Current_cache.Output(Set_status)

  type t = commit

  let uri (_, commit) = Commit_id.uri commit

  let id (_, commit_id) = Commit_id.to_git commit_id

  let compare (_, a) (_, b) = Commit_id.compare a b

  let owner_name (_, id) = Commit_id.owner_name id

  let repo_id t =
    let full = owner_name t in
    match Astring.String.cut ~sep:"/" full with
    | Some (owner, name) -> { Repo_id.owner; name }
    | None -> Fmt.failwith "Invalid owner_name %S" full

  let hash (_, id) = id.Commit_id.hash

  let committed_date (_, id) = id.Commit_id.committed_date

  let pp = Fmt.using snd Commit_id.pp

  let set_status commit context status =
    Current.component "set_status" |>
    let> (t, commit) = commit
    and> status = status in
    Set_status_cache.set t {Set_status.Key.commit; context} status
end

(* TODO Build this up as a REST Query, then try to optimise to GraphQL. 
*)
let query_branches token =
  let open Gitlab in
  let open Monad in
  let project_id = 29798678 in
  let* merge_requests = Project.merge_requests ~token ~id:project_id ~state:`Opened () in
  and* branches = Project.
  

(* TODO REST or GraphQL to retrieve all git refs for a Repository. *)
let get_refs _t { Repo_id.owner=_; name=_ } =
  
  let add xs map = List.fold_left (fun acc x -> Ref_map.add x.Commit_id.id (t, x) acc) map xs in
  Ref_map.empty
  |> add refs
  |> add prs
  |> fun all_refs -> { default_ref; all_refs }

let make_refs_monitor t repo =
  let read () =
    Lwt.catch
      (fun () -> get_refs t repo >|= Stdlib.Result.ok)
      (fun ex -> Lwt_result.fail @@ `Msg (Fmt.str "GitHub query for %a failed: %a" Repo_id.pp repo Fmt.exn ex))
  in
  let watch refresh =
    let owner_name = Printf.sprintf "%s/%s" repo.owner repo.name in
    let rec aux x =
      x >>= fun () ->
      let x = await_event ~owner_name in
      refresh ();
      Lwt_unix.sleep 10.0 >>= fun () ->   (* Limit updates to 1 per 10 seconds *)
      aux x
    in
    let x = await_event ~owner_name in
    let thread =
      Lwt.catch
        (fun () -> aux x)
        (function
         | Lwt.Canceled -> Lwt.return_unit  (* (could clear metrics here) *)
         | ex -> Log.err (fun f -> f "refs thread failed: %a" Fmt.exn ex); Lwt.return_unit
        )
    in
    Lwt.return (fun () -> Lwt.cancel thread; Lwt.return_unit)
  in
  let pp f = Fmt.pf f "Watch %a CI refs" Repo_id.pp repo in
  Current.Monitor.create ~read ~watch ~pp

let refs t repo =
  Current.Monitor.get (
      match Repo_map.find_opt repo t.refs_monitors with
      | Some i -> i
      | None ->
         let i = make_refs_monitor t repo in
         t.refs_monitors <- Repo_map.add repo i t.refs_monitors;
         i
    )

let to_ptime str =
  Ptime.of_rfc3339 str |> function
  | Ok (t, _, _) -> t
  | Error (`RFC3339 (_, e)) -> Fmt.failwith "%a" Ptime.pp_rfc3339_error e

let remove_stale ?staleness ~default_ref refs =
  match staleness with
  | None -> refs
  | Some staleness ->
     let cutoff = Unix.gettimeofday () -. Duration.to_f staleness in
     let active x =
       let committed = Ptime.to_float_s (to_ptime x.Commit_id.committed_date) in
       committed > cutoff
     in
     let is_default = function
       | { Commit_id.id = `Ref t; _ } -> String.equal default_ref t
       | _ -> false
     in
     List.filter (fun (_, x) -> is_default x || active x) refs


let to_ci_refs ?staleness refs =
  refs.all_refs
  |> Ref_map.bindings
  |> List.map snd
  |> remove_stale ?staleness ~default_ref:refs.default_ref

let ci_refs ?staleness t repo =
  let+ refs =
    Current.component "%a CI refs" Repo_id.pp repo |>
      let> () = Current.return () in
      refs t repo
  in
  to_ci_refs ?staleness refs


module Repo = struct
  type nonrec t = t * Repo_id.t

  let id = snd
  let pp = Fmt.using id Repo_id.pp
  let compare a b = Repo_id.compare (id a) (id b)

  let head_commit t =
    Current.component "head" |>
      let> (api, repo) = t in
      Current.Monitor.get (
          match Repo_map.find_opt repo api.head_monitors with
          | Some i -> i
          | None ->
             let i = make_head_commit_monitor api repo in
             api.head_monitors <- Repo_map.add repo i api.head_monitors;
             i
        )

  let ci_refs ?staleness t =
    let+ refs =
      Current.component "CI refs" |>
        let> (api, repo) = t in
        refs api repo
    in
    to_ci_refs ?staleness refs
end

(* TODO oauth token for api requests. *)

open Cmdliner

let token_file =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"A file containing the GitLab OAuth token."
    ~docv:"PATH"
    ["gitlab-token-file"]

let webhook_secret_file =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"A file containing the GitLab Webhook secret."
    ~docv:"WEBHOOK_SECRET"
    ["gitlab-webhook-secret-file"]

let make_config token_file webhook_secret_file =
  let token = String.trim (read_file token_file) in
  let webhook_secret = String.trim (read_file webhook_secret_file) in
  of_oauth ~token ~webhook_secret

let cmdliner =
  Term.(const make_config $ token_file $ webhook_secret_file)