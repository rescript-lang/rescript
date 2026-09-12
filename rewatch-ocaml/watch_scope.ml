type source_root = {
  directory: string;
  recursive: bool;
  filter: Source_filter.t option;
}

type t = {
  roots: string list;
  paths: Native_watcher.watch_path list;
  sources: source_root list;
  unresolved: string list;
}

let control_file_names = ["rescript.json"; "bsconfig.json"; "package.json"]
let is_control_file_name name = List.mem name control_file_names

let discover ~root ~prod ~features ~filter =
  try
    let root_config = Config.load_root root in
    let resolution =
      Package_resolution.create
        ~diagnostic_mode:Package_resolution.Suppress_diagnostics root_config
    in
    let roots = ref [root] in
    let paths = ref [] in
    let sources = ref [] in
    let unresolved = ref [] in
    let add_path directory recursive =
      paths := Native_watcher.{directory; recursive} :: !paths
    in
    let rec nearest_existing_directory package_root directory =
      if File_util.exists directory then directory
      else
        let parent = Filename.dirname directory in
        if parent = directory || directory = package_root then package_root
        else nearest_existing_directory package_root parent
    in
    let watch_unresolved_dependency package_root name =
      Package_resolution.dependency_candidates resolution ~package_root name
      |> List.iter (fun candidate ->
          let existing = nearest_existing_directory root candidate in
          try
            let canonical_existing = Platform.canonicalize_path existing in
            if Project_context.path_is_within_canonical ~root canonical_existing
            then (
              unresolved := candidate :: !unresolved;
              (* A shallow ancestor watch is sufficient: each directory creation
                 wakes reconciliation, which advances the watch toward the complete
                 candidate without expanding all of node_modules. *)
              add_path canonical_existing false)
          with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ())
    in
    let graph =
      Package_traversal.traverse ~root_config
        ~root_name:(Package_resolution.root_package_name resolution)
        ~prod ~features ~resolve:(fun config request ->
          let dependency = request.Package_traversal.declaration in
          match
            Package_resolution.dependency_path resolution
              ~package_root:config.root dependency.name
          with
          | Some directory when Config.exists_in_root directory -> (
            try
              let resolved =
                Package_traversal.resolve resolution ~package_root:config.root
                  request
              in
              if resolved.dependency.is_local then (
                roots := resolved.dependency.directory :: !roots;
                add_path resolved.dependency.directory false);
              Some resolved
            with Project_context.Package_error _ | Project_context.Error _ ->
              (* A broken dependency configuration must remain watched so fixing
                 that file can recover the long-lived command. *)
              roots := directory :: !roots;
              add_path directory false;
              None)
          | None ->
            watch_unresolved_dependency config.root dependency.name;
            None
          | Some directory when File_util.is_directory directory ->
            (* The parent watch is needed because removing or replacing the
               watched directory itself is not reported consistently by every
               filesystem backend. It also detects a newly installed candidate
               that should take priority over a lower resolution. *)
            roots := directory :: !roots;
            unresolved := directory :: !unresolved;
            add_path (Filename.dirname directory) false;
            add_path directory false;
            None
          | Some path ->
            (* A non-directory candidate may be replaced with an install. Watch
               its parent and retain its type in the snapshot until that happens. *)
            unresolved := path :: !unresolved;
            add_path (Filename.dirname path) false;
            None)
    in
    graph.packages
    |> List.iter (fun (package : Package_traversal.package) ->
        if package.is_local then (
          let package_root = package.config.root in
          let config = package.config in
          let is_local = package.is_local in
          add_path package_root false;
          try
            let requested =
              Package_traversal.find_feature_selection graph package_root
              |> Option.map Package_traversal.feature_selection_to_option
              |> Option.value ~default:None
            in
            Source.active_sources config
              ~prod:(Package_traversal.source_discovery_prod ~prod ~is_local)
              ~features:requested
            |> List.iter (fun source ->
                let directory = Filename.concat config.root source.Config.dir in
                let filter =
                  if package_root = root_config.root then filter else None
                in
                sources :=
                  {directory; recursive = source.recurse; filter} :: !sources;
                let existing =
                  nearest_existing_directory config.root directory
                in
                add_path existing (existing = directory && source.recurse))
          with Source.Error _ -> ()));
    let deduplicated_paths = Hashtbl.create (List.length !paths) in
    List.iter
      (fun (path : Native_watcher.watch_path) ->
        let recursive =
          path.recursive
          || Option.value
               (Hashtbl.find_opt deduplicated_paths path.directory)
               ~default:false
        in
        Hashtbl.replace deduplicated_paths path.directory recursive)
      !paths;
    let paths =
      Hashtbl.to_seq deduplicated_paths
      |> List.of_seq
      |> List.map (fun (directory, recursive) ->
          Native_watcher.{directory; recursive})
    in
    {
      roots = List.sort_uniq String.compare !roots;
      paths;
      sources = !sources;
      unresolved = List.sort_uniq String.compare !unresolved;
    }
  with
  | Config.Error _ | Project_context.Error _ | Project_context.Package_error _
  ->
    {
      roots = [root];
      paths = [Native_watcher.{directory = root; recursive = false}];
      sources = [];
      unresolved = [];
    }

let path_in_scope scope path =
  let name = Filename.basename path in
  let is_control =
    is_control_file_name name && List.mem (Filename.dirname path) scope.roots
  in
  let is_source =
    Option.is_some (Source.source_kind path)
    && List.exists
         (fun source ->
           let in_directory =
             Filename.dirname path = source.directory
             || source.recursive
                && String.starts_with
                     ~prefix:(source.directory ^ Filename.dir_sep)
                     path
           in
           in_directory
           && Option.fold ~none:true
                ~some:(fun filter -> Source_filter.matches_basename filter path)
                source.filter)
         scope.sources
  in
  is_control || is_source || List.mem path scope.unresolved
