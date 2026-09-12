exception Error of string
exception Package_error of string

type dependency_context = {
  current_root: string;
  workspace_root: string;
  allow_upward_search: bool;
  include_workspace_dependencies: bool;
}

let path_is_within_canonical ~root path =
  let normalize = Platform.normalize_path_for_comparison in
  let root = normalize root in
  let path = normalize path in
  path = root || String.starts_with ~prefix:(Filename.concat root "") path

let rec nearest_config_path directory =
  if Config.exists_in_root directory then Some (Config.path_in_root directory)
  else
    let parent = Filename.dirname directory in
    if parent = directory then None else nearest_config_path parent

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
  match nearest_config_path (Filename.dirname current.root) with
  | Some path -> (
    match Config.load path with
    | parent
      when List.exists
             (fun (dependency : Config.dependency) ->
               dependency.name = current.name)
             (parent.dependencies @ parent.dev_dependencies) ->
      parent.root
    | _ -> current.root)
  | None -> current.root

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
    File_util.exists candidate
    && is_local_dependency_canonical ~workspace:current.root
         (Platform.canonicalize_path candidate)
  in
  let is_monorepo_root =
    List.exists has_local_dependency
      (current.dependencies @ current.dev_dependencies)
  in
  {
    current_root = current.root;
    workspace_root;
    allow_upward_search = workspace_root = current.root && not is_monorepo_root;
    include_workspace_dependencies = workspace_root = current.root;
  }

let dependency_is_local_canonical context path =
  (* A command run from one package listed by a parent workspace owns only that
     package. Sibling packages still resolve through the workspace, but treating
     them as local would also include their development sources, clean their
     outputs, and watch their source trees. *)
  context.include_workspace_dependencies
  && is_local_dependency_canonical ~workspace:context.workspace_root path

let dependency_candidates_in context package_root name =
  let candidate root =
    Filename.concat (Filename.concat root "node_modules") name
  in
  let rec in_ancestors directory acc =
    let candidate =
      Filename.concat (Filename.concat directory "node_modules") name
    in
    let parent = Filename.dirname directory in
    if parent = directory then List.rev (candidate :: acc)
    else in_ancestors parent (candidate :: acc)
  in
  let deduplicate values =
    let seen = Hashtbl.create (List.length values) in
    List.fold_left
      (fun unique value ->
        if Hashtbl.mem seen value then unique
        else (
          Hashtbl.add seen value ();
          value :: unique))
      [] values
    |> List.rev
  in
  let direct =
    [
      candidate package_root;
      candidate context.current_root;
      candidate context.workspace_root;
    ]
    |> deduplicate
  in
  if context.allow_upward_search then
    deduplicate (direct @ in_ancestors (Filename.dirname package_root) [])
  else direct

let dependency_path_in context package_root name =
  let existing_realpath path =
    if File_util.exists path then Some (Platform.canonicalize_path path)
    else None
  in
  dependency_candidates_in context package_root name
  |> List.find_map existing_realpath

let standalone_dependency_context root =
  {
    current_root = root;
    workspace_root = root;
    allow_upward_search = true;
    include_workspace_dependencies = true;
  }

let dependency_path root name =
  dependency_path_in (standalone_dependency_context root) root name

let require_dependency_directory ~context package_root
    (dependency : Config.dependency) =
  match dependency_path_in context package_root dependency.name with
  | None ->
    raise
      (Package_error
         (Printf.sprintf
            "Could not build package tree reading dependency '%s' at path \
             '%s'. Error: Could not resolve dependency %s"
            dependency.name context.current_root dependency.name))
  | Some directory when not (Config.exists_in_root directory) ->
    raise
      (Package_error
         (Printf.sprintf
            "Could not build package tree for '%s' at path '%s'. Error: no \
             rescript.json or bsconfig.json in %s"
            dependency.name context.current_root directory))
  | Some directory -> directory

let relative_to_opt root path =
  let prefix = Filename.concat root "" in
  let comparable = Platform.normalize_path_for_comparison in
  if comparable path = comparable root then Some "."
  else if String.starts_with ~prefix:(comparable prefix) (comparable path) then
    Some
      (String.sub path (String.length prefix)
         (String.length path - String.length prefix))
  else None

let relative_to root path =
  match relative_to_opt root path with
  | Some relative -> relative
  | None -> raise (Error (path ^ " is not inside " ^ root))

let relative_or_absolute ~root path =
  Option.value (relative_to_opt root path) ~default:path

let display_path ~root path =
  match relative_or_absolute ~root path with
  | "." -> "."
  | relative when path_is_within_canonical ~root path -> "./" ^ relative
  | absolute -> absolute
