type repo_visibility = Public | Private [@@deriving to_yojson]

type t = {
  owner : string;
  name : string;
  visibility: repo_visibility;
}

let pp f { owner; name; _ } = Fmt.pf f "%s/%s" owner name

let compare = compare

let to_string = function
  | Public -> "public"
  | Private -> "private"

let to_visibility = function
  | false -> Public
  | true -> Private
    
let cmdliner =
  let open Cmdliner in
  let parse s =
    match Astring.String.cuts ~sep:"/" s with
    | [ owner; name] -> Ok { owner; name; visibility = Public }
    | _ -> Error (`Msg (Fmt.str "%S not in the form 'owner/name'" s))
  in
  Arg.conv ~docv:"REPO" (parse, pp)
