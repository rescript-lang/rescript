exception Error of string
exception Package_error of string

let workspace_lock_root folder =
  let current = Config.load_root folder in
  let rec nearest_parent directory =
    if Config.exists_in_root directory then Some (Config.load_root directory)
    else
      let parent = Filename.dirname directory in
      if parent = directory then None else nearest_parent parent
  in
  match nearest_parent (Filename.dirname folder) with
  | Some parent
    when List.exists
           (fun (dependency : Config.dependency) ->
             dependency.name = current.name)
           (parent.dependencies @ parent.dev_dependencies) ->
    parent.root
  | Some _ | None -> folder

let dependency_candidates root name =
  let rec in_ancestors directory acc =
    let candidate = Filename.concat (Filename.concat directory "node_modules") name in
    let parent = Filename.dirname directory in
    if parent = directory then List.rev (candidate :: acc)
    else in_ancestors parent (candidate :: acc)
  in
  let package_name =
    match List.rev (String.split_on_char '/' name) with
    | last :: _ -> last
    | [] -> name
  in
  let sibling = Filename.concat (Filename.dirname root) name in
  let workspace = Filename.concat (Filename.concat root "packages") package_name in
  in_ancestors root [] @ [sibling; workspace]

let dependency_path root name =
  let existing_realpath path =
    try if Sys.file_exists path then Some (Unix.realpath path) else None
    with Sys_error _ | Unix.Unix_error _ -> None
  in
  dependency_candidates root name |> List.find_map existing_realpath

let require_dependency_directory ~workspace_root package_root
    (dependency : Config.dependency) =
  match dependency_path package_root dependency.name with
  | None ->
    raise
      (Package_error
         (Printf.sprintf
            "Could not build package tree reading dependency '%s' at path '%s'. Error: Could not resolve dependency %s"
            dependency.name workspace_root dependency.name))
  | Some directory when not (Config.exists_in_root directory) ->
    raise
      (Package_error
         (Printf.sprintf
            "Could not build package tree for '%s' at path '%s'. Error: no rescript.json or bsconfig.json in %s"
            dependency.name workspace_root directory))
  | Some directory -> directory

let relative_to root path =
  let prefix = Filename.concat root "" in
  let comparable = Platform.normalize_path_for_comparison in
  if comparable path = comparable root then "."
  else if String.starts_with ~prefix:(comparable prefix) (comparable path) then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else raise (Error (path ^ " is not inside " ^ root))

let path_is_within_canonical ~root path =
  let normalize = Platform.normalize_path_for_comparison in
  let root = normalize root in
  let path = normalize path in
  path = root || String.starts_with ~prefix:(Filename.concat root "") path

(* Graph roots and resolved dependency paths are already canonical. Keeping this
   predicate pure avoids repeating realpath calls throughout package traversal. *)
let is_local_dependency_canonical ~workspace path =
  let equal_component left right =
    Platform.normalize_path_for_comparison left
    = Platform.normalize_path_for_comparison right
  in
  let rec contains_component path component =
    if equal_component (Filename.basename path) component then true
    else
      let parent = Filename.dirname path in
      parent <> path && contains_component parent component
  in
  path_is_within_canonical ~root:workspace path
  && not (contains_component path "node_modules")

let is_local_dependency ~workspace path =
  is_local_dependency_canonical ~workspace:(Unix.realpath workspace)
    (Unix.realpath path)
