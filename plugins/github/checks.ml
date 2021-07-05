(** See https://github.com/ocurrent/ocurrent/pull/74/files for some previous context. *)

open Lwt.Infix
open Current.Syntax


(**

 1. Core Current (SAC) type to monitor 
 2. webhook triggers

*)
module CheckSuite = struct
  type t = {
      id : int;
      commit_id : Current_git.Commit_id.t;
    }

  let pp f t = Fmt.int f t.id
  let compare = Stdlib.compare
end

let check_suite_cond = Lwt_condition.create ()

let check_suite_webhook body =
  let action = Yojson.Safe.Util.(body |> member "action" |> to_string) in
  let repo = Yojson.Safe.Util.(body |> member "repository" |> member "git_url" |> to_string ) in
  Log.info (fun f -> f "Got check_suite webhook event action '%s' for %s" action repo);
  (* Note this is git://github.com... is that correct? *)
  let check_suite = Github_j.check_suite_of_string Yojson.Safe.Util.(body |> member "check_suite" |> Yojson.Safe.pretty_to_string) in
  Log.debug (fun f -> f "Got check_suite webhook event action %s payload %s"
                       (Github_j.string_of_check_suite check_suite) action);
  (* Build check_suite type. *)
  let check_suite_id =
    CheckSuite.({ id = check_suite.id
                ; commit_id = Current_git.Commit_id.v ~repo ~gref:check_suite.head_branch ~hash:check_suite.head_sha}) in
  Lwt_condition.broadcast check_suite_cond check_suite_id
  (** Broadcast the check_suite type received. *)
  
module Int_map = Map.Make(Int)

module CheckSuites = Current.Var(struct
    type t = CheckSuite.t Int_map.t
    let equal = Int_map.equal (fun x y -> CheckSuite.(x.id == y.id))
    let pp = Fmt.using (fun t -> Int_map.bindings t |> List.map fst) Fmt.(Dump.list int)
  end)

type t = {
    app_id : string;
    key : Mirage_crypto_pk.Rsa.priv;
    check_suites : CheckSuites.t;
  }

let build app_id key = 
  let check_suites = CheckSuites.create ~name:"check_suites" (Error (`Active `Running)) in
  {app_id; key; check_suites}

module Int_set = Set.Make(Int)

let monitor_check_suites t () =
  let rec aux () =
    Lwt_condition.wait check_suite_cond >>= fun x -> 
    CheckSuites.update t.check_suites (fun old_map ->
        Log.info (fun f -> f "monitor_check_suites id %i" x.id);
        let old_map = match old_map with Ok x -> x | Error _ -> Int_map.empty in
        (* Merge in new check_suites *)
        let new_map = if Int_map.mem x.CheckSuite.id old_map
                      then old_map
                      else Int_map.add x.CheckSuite.id x old_map in
        Stdlib.Result.ok new_map
      );
    Lwt_unix.sleep 60.0 >>= fun () ->   (* Wait at least 1m between updates *)
    aux ()
  in
  aux ()
  (* See App.monitor_installations *)

let check_suites t =
  let+ t = CheckSuites.get t.check_suites in
  t |> Int_map.bindings |> List.map snd

(* TODO In watching LWT thread, trigger refresh of check_suite on???? Not sure here?
 * Unclear about the LWT control flow within a plugin like this
 * See installation.ml for closest analogue.
 * *)


(** Taken from Api.Status, not clear if this needs to merge or "Checks" requires its own
    version of this type.
*)
module CheckRunStatus = struct
  type state = [`Queued | `InProgress | `Completed ]
  type t = {
      state: state;
      description: string option;
      url: Uri.t option
    }

  let v ?description ?url state =
    let description = Option.map (Astring.String.with_range ~len:140) description in (* Max GitHub allows *)
    { state; description; url }

  (** TODO Used to publish CheckRunStatus to Github *)
  (* let state_to_string = function
   *   | `Queued   -> "queued"
   *   | `InProgress   -> "inprogress"
   *   | `Completed   -> "completed"
   * 
   * let json_items { state; description; url } =
   *   ["state", `String (state_to_string state)] @
   *     (match description with None -> [] | Some x -> ["description", `String x]) @
   *       (match url with None -> [] | Some x -> ["target_url", `String (Uri.to_string x)])
   * 
   * let digest t = Yojson.Safe.to_string @@ `Assoc (json_items t)
   * 
   * let pp f t = Fmt.string f (digest t) *)
end

module CheckRun = struct
  type t = {
      id : string option;
      head_sha : string;
      name: string;
    }

  let build check_suite =
    { id = None
    ; head_sha = Current_git.Commit_id.hash check_suite.CheckSuite.commit_id
    ; name = "ocurrent-checks"}

  (* module Set_status = struct
   *   let id = "github-set-check-run-status"
   * 
   *   module Key = struct
   *     type t = {}
   * 
   *     let to_json = {}
   *     let digest t = {}
   *   end
   *   module Value = CheckRunStatus
   *   module Outcome = Current.Unit
   * 
   *   let auto_cancel = true
   * 
   *   let pp f 
   * 
   * 
   * module Set_status_cache = Current_cache.Output(Set_status) *)

  (** TODO What is required to create a check run? *)
  let set_status _t _context _status =
    (* let check_run = Printf.sprintf "{\"name\":\"name\"q,\"head_sha\":\"%s\"}" t in *)
    Current.return ()
end


(* Command-line options *)

let make_config app_id private_key_file=
  let data = Api.read_file private_key_file in
  match X509.Private_key.decode_pem (Cstruct.of_string data) with
    | Error (`Msg msg) -> Fmt.failwith "Failed to parse secret key!@ %s" msg
    | Ok (`RSA key) ->
      let t = build app_id key in
      Lwt.async (monitor_check_suites t);
      t
    | Ok _ -> Fmt.failwith "Unsupported private key type" [@@warning "-11"]

open Cmdliner

let private_key_file =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"A file containing the GitHub app's RSA private key as a PEM file."
    ~docv:"PATH"
    ["github-private-key-file"]

let app_id =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"The GitHub app's (integer) ID"
    ~docv:"ID"
    ["github-app-id"]

let cmdliner =
  Term.(const make_config $ app_id $ private_key_file)
