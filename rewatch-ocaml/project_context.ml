exception Error of string
exception Package_error of string

type dependency_context = {
  current_root: string;
  workspace_root: string;
  allow_upward_search: bool;
}

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

let workspace_lock_root_for (current : Config.t) =
  let rec nearest_parent directory =
    if Config.exists_in_root directory then Some (Config.load_root directory)
    else
      let parent = Filename.dirname directory in
      if parent = directory then None else nearest_parent parent
  in
  match nearest_parent (Filename.dirname current.root) with
  | Some parent
    when List.exists
           (fun (dependency : Config.dependency) ->
             dependency.name = current.name)
           (parent.dependencies @ parent.dev_dependencies) ->
    parent.root
  | Some _ | None -> current.root

let workspace_lock_root folder =
  workspace_lock_root_for (Config.load_root folder)

let dependency_context (current : Config.t) =
  let workspace_root = workspace_lock_root_for current in
  let has_local_dependency (dependency : Config.dependency) =
    let candidate =
      Filename.concat
        (Filename.concat current.root "node_modules")
        dependency.name
    in
    try
      Sys.file_exists candidate
      && is_local_dependency_canonical ~workspace:current.root
           (Unix.realpath candidate)
    with Sys_error _ | Unix.Unix_error _ -> false
  in
  let is_monorepo_root =
    List.exists has_local_dependency
      (current.dependencies @ current.dev_dependencies)
  in
  {
    current_root = current.root;
    workspace_root;
    allow_upward_search = workspace_root = current.root && not is_monorepo_root;
  }

let dependency_workspace context = context.workspace_root

let dependency_candidates_in context package_root name =
  let candidate root = Filename.concat (Filename.concat root "node_modules") name in
  let rec in_ancestors directory acc =
    let candidate = Filename.concat (Filename.concat directory "node_modules") name in
    let parent = Filename.dirname directory in
    if parent = directory then List.rev (candidate :: acc)
    else in_ancestors parent (candidate :: acc)
  in
  let append_unique values additions =
    List.fold_left
      (fun values value -> if List.mem value values then values else values @ [value])
      values additions
  in
  let direct =
    [
      candidate package_root;
      candidate context.current_root;
      candidate context.workspace_root;
    ]
    |> append_unique []
  in
  if context.allow_upward_search then
    append_unique direct (in_ancestors (Filename.dirname package_root) [])
  else direct

let dependency_path_in context package_root name =
  let existing_realpath path =
    try if Sys.file_exists path then Some (Unix.realpath path) else None
    with Sys_error _ | Unix.Unix_error _ -> None
  in
  dependency_candidates_in context package_root name
  |> List.find_map existing_realpath

let standalone_dependency_context root =
  {
    current_root = root;
    workspace_root = root;
    allow_upward_search = true;
  }

let dependency_candidates root name =
  dependency_candidates_in (standalone_dependency_context root) root name

let dependency_path root name =
  dependency_path_in (standalone_dependency_context root) root name

let require_dependency_directory ~context package_root
    (dependency : Config.dependency) =
  match dependency_path_in context package_root dependency.name with
  | None ->
    raise
      (Package_error
         (Printf.sprintf
            "Could not build package tree reading dependency '%s' at path '%s'. Error: Could not resolve dependency %s"
            dependency.name context.current_root dependency.name))
  | Some directory when not (Config.exists_in_root directory) ->
    raise
      (Package_error
         (Printf.sprintf
            "Could not build package tree for '%s' at path '%s'. Error: no rescript.json or bsconfig.json in %s"
            dependency.name context.current_root directory))
  | Some directory -> directory

let relative_to root path =
  let prefix = Filename.concat root "" in
  let comparable = Platform.normalize_path_for_comparison in
  if comparable path = comparable root then "."
  else if String.starts_with ~prefix:(comparable prefix) (comparable path) then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else raise (Error (path ^ " is not inside " ^ root))

let is_local_dependency ~workspace path =
  is_local_dependency_canonical ~workspace:(Unix.realpath workspace)
    (Unix.realpath path)
