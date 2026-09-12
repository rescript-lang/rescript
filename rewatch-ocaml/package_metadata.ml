let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let last_member name = function
  | `Assoc fields -> List.assoc_opt name (List.rev fields)
  | _ -> None

let package_name package_root =
  let path = Filename.concat package_root "package.json" in
  try
    if not (File_util.exists path) then Ok None
    else
      let json = Yojson.Safe.from_file path in
      match last_member "name" json with
      | Some (`String name) -> Ok (Some name)
      | Some _ | None -> Ok None
  with
  | Sys_error message -> Error ("Could not read package.json: " ^ message)
  | Unix.Unix_error (error, _, _) ->
    Error ("Could not read package.json: " ^ Unix.error_message error)
  | Yojson.Json_error message ->
    Error ("Could not parse package.json: " ^ message)

let url_value = function
  | `String value -> Some value
  | `Assoc fields -> (
    match List.assoc_opt "url" fields with
    | Some (`String value) -> Some value
    | _ -> None)
  | _ -> None

let remove_prefix prefix value =
  if String.starts_with ~prefix value then
    String.sub value (String.length prefix)
      (String.length value - String.length prefix)
  else value

let remove_suffix suffix value =
  if String.ends_with ~suffix value then
    String.sub value 0 (String.length value - String.length suffix)
  else value

let issues_url_from_repository repository =
  let cleaned = repository |> remove_prefix "git+" |> remove_suffix ".git" in
  if
    (not (String.contains cleaned '@'))
    && not (String_util.contains cleaned "://")
  then
    let path = remove_prefix "github:" cleaned in
    "https://github.com/" ^ path ^ "/issues"
  else cleaned ^ "/issues"

let issue_tracker_url package_root =
  let path = Filename.concat package_root "package.json" in
  try
    let json = Yojson.Safe.from_file path in
    match Option.bind (member "bugs" json) url_value with
    | Some url -> Some url
    | None ->
      Option.bind (member "repository" json) url_value
      |> Option.map issues_url_from_repository
  with Sys_error _ | Yojson.Json_error _ -> None
