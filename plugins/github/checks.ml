(** See https://github.com/ocurrent/ocurrent/pull/74/files for some previous context. *)

(* open Lwt.Infix *)
(* open Current.Syntax *)

type t = string

let input_webhook body =
  let action = Yojson.Safe.Util.(body |> member "action" |> to_string) in
  let organisation = Yojson.Safe.Util.(body |> member "organization" |> to_string) in
  let repository = Yojson.Safe.Util.(body |> member "repository" |> to_string) in

  if action == "requested" || action == "rerequested" then
    create_check_run ~token ~owner:organisation ~repo:repository

let create_check_run ?token ~owner ~repo =
  let open Github in
  let open Monad in
  let body = "" in
  run (
    Check.create_check_run ~token ~owner ~repo ~body () 
  )
  
