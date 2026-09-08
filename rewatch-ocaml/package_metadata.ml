let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

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

let contains_substring value substring =
  let value_length = String.length value in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > value_length then false
    else if String.sub value index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let issues_url_from_repository repository =
  let cleaned =
    repository |> remove_prefix "git+" |> remove_suffix ".git"
  in
  if
    not (String.contains cleaned '@')
    && not (contains_substring cleaned "://")
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
