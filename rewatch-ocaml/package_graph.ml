exception Error = Project_context.Error

let source_discovery_prod ~prod ~is_local = prod || not is_local

let with_gentype_source_dirs directories (config : Config.t) =
  if config.gentype_args = [] then config
  else
    {
      config with
      gentype_args =
        config.gentype_args
        @ List.concat_map
            (fun directory -> ["-bs-gentype-source-dir"; directory])
            directories;
    }

let dependent_is_allowed allowed_dependents dependent =
  Option.fold ~none:true
    ~some:(fun allowed -> List.mem dependent allowed)
    allowed_dependents

let discover ~(root_config : Config.t) ~prod ~features ~warn_error ~filter
    ~(stats : Build_types.t) =
  Package_diagnostics.validate_metadata root_config;
  let resolution = Package_resolution.create root_config in
  let requested_features = Hashtbl.create 32 in
  let unallowed_dependencies = ref [] in
  let load_config = Package_resolution.load_config resolution in
  let resolve_dependency package_root (dependency : Config.dependency) =
    let resolved =
      Package_resolution.resolve resolution ~package_root dependency
    in
    (resolved.directory, resolved.config)
  in
  let add_feature_request root request =
    match Hashtbl.find_opt requested_features root, request with
    | None, request -> Hashtbl.add requested_features root request
    | Some None, _ | Some _, None ->
      Hashtbl.replace requested_features root None
    | Some (Some current), Some requested ->
      Hashtbl.replace requested_features root
        (Some (List.sort_uniq String.compare (current @ requested)))
  in
  let collected = Hashtbl.create 32 in
  let rec collect ~folder:root ~features ~is_local =
    if root <> root_config.root || not (Hashtbl.mem requested_features root) then
      add_feature_request root features;
    if not (Hashtbl.mem collected root) then (
      Hashtbl.add collected root ();
      let config = load_config root in
      Output.debug ~verbosity:stats.verbosity
        ("Parsing package: " ^ config.name);
      let dependencies =
        List.map (fun dependency -> ("dependencies", dependency))
          config.dependencies
        @ if prod || not is_local then []
          else
            List.map
              (fun dependency -> ("dev-dependencies", dependency))
              config.dev_dependencies
      in
      let resolved_dependencies =
        List.map
          (fun (kind, (dependency : Config.dependency)) ->
            let directory, dependency_config =
              resolve_dependency root dependency
            in
            if
              not
                (dependent_is_allowed dependency_config.allowed_dependents
                   config.name)
            then
              unallowed_dependencies :=
                (config.name, kind, dependency_config.name)
                :: !unallowed_dependencies;
            (dependency, directory))
          dependencies
      in
      List.iter
        (fun ((dependency : Config.dependency), directory) ->
          collect ~folder:directory ~features:dependency.features
            ~is_local:
              (Package_resolution.is_local resolution directory))
        resolved_dependencies)
  in
  collect ~folder:root_config.root ~features ~is_local:true;
  (if !unallowed_dependencies <> [] then
    let details =
      !unallowed_dependencies |> List.sort_uniq compare
      |> List.map (fun (dependent, kind, dependency) ->
           Printf.sprintf "%s %s: %s" dependent kind dependency)
      |> String.concat "\n"
    in
    raise
      (Error
         ("The following packages use dependencies that do not allow them:\n"
         ^ details
         ^ "\nUpdate allowed-dependents in the dependency rescript.json files.")));
  Hashtbl.iter
    (fun root features ->
      Hashtbl.replace stats.retained.active_features root features)
    requested_features;
  let visited = Hashtbl.create 32 in
  let graph_packages = ref [] in
  let rec visit ~folder:root ~features ~warn_error ~filter ~is_local =
    if not (Hashtbl.mem visited root) then (
      Hashtbl.add visited root ();
      let features =
        match Hashtbl.find_opt stats.retained.active_features root with
        | Some features -> features
        | None -> features
      in
      let config = load_config root in
      Package_diagnostics.report_missing_sources
        ~is_root:(root = root_config.root) config;
      let config =
        match warn_error with
        | None -> config
        | Some value ->
          {config with warning_flags = ["-warn-error"; value]}
      in
      let dependencies =
        config.dependencies
        @ if prod || not is_local then [] else config.dev_dependencies
      in
      let dependency_directories =
        List.map
          (fun dependency ->
            let directory, _ = resolve_dependency root dependency in
            (dependency, directory))
          dependencies
      in
      List.iter
        (fun ((dependency : Config.dependency), directory) ->
          visit ~folder:directory ~features:dependency.features
            ~warn_error ~filter:None
            ~is_local:
              (Package_resolution.is_local resolution directory))
        dependency_directories;
      let discovery =
        Output.debug ~verbosity:stats.verbosity
          ("Building source file-tree for package: " ^ config.name);
        Source.discover_with_inventory config
          ~prod:(source_discovery_prod ~prod ~is_local)
          ~features ~filter
          ~on_missing:(Package_diagnostics.report_missing_source_folder config)
          ~on_orphan:(fun path ->
            Printf.eprintf
              "\027[2K\r No implementation file found for interface file (skipping): %s\n%!"
              path)
          ~display_root:root_config.root
      in
      let modules = discovery.modules in
      let owns_outputs =
        root <> root_config.root && Compiler_info.owns_outputs config
      in
      let compile_config =
        let config =
          with_gentype_source_dirs discovery.gentype_dirs config
        in
        let inherited = Build_artifacts.with_root_options config root_config in
        let output_config =
          if owns_outputs then
            {
              inherited with
              package_specs = config.package_specs;
              suffix = config.suffix;
            }
          else inherited
        in
        output_config |> Compiler_args.with_local_warning_policy ~is_local
      in
      let build_dir = Build_artifacts.lib_path root "bs" in
      let ocaml_dir = Build_artifacts.lib_path root "ocaml" in
      File_util.ensure_dir build_dir;
      let source_mtimes = Hashtbl.create (List.length discovery.source_mtimes) in
      List.iter
        (fun (path, modified) -> Hashtbl.replace source_mtimes path modified)
        discovery.source_mtimes;
      let package : Build_types.graph_package =
        {
          graph_root = root;
          graph_build_owner = (if owns_outputs then root else root_config.root);
          graph_is_local = is_local;
          graph_config = config;
          graph_compile_config = compile_config;
          graph_build_dir = build_dir;
          graph_ocaml_dir = ocaml_dir;
          graph_dependencies = dependencies;
          graph_dependency_directories = dependency_directories;
          graph_gentype_dependency_args =
            Compiler_args.gentype_dependency_args_from_paths compile_config
              dependency_directories;
          graph_modules = modules;
          graph_source_mtimes = source_mtimes;
          graph_source_files = discovery.inventory_files;
          graph_present_source_files = discovery.present_files;
        }
      in
      Hashtbl.replace stats.retained.graph_packages root package;
      List.iter
        (fun module_ ->
          (module_.Source.implementation
          :: Option.to_list module_.Source.interface)
          |> List.iter (fun relative_path ->
               let absolute_path = Filename.concat root relative_path in
               Hashtbl.replace stats.retained.source_index
                 (Platform.normalize_path_for_comparison absolute_path)
                 (root, module_, relative_path, absolute_path)))
        modules;
      graph_packages := package :: !graph_packages)
  in
  visit ~folder:root_config.root ~features ~warn_error ~filter ~is_local:true;
  !graph_packages
