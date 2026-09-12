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
    ~(attempt : Build_attempt.t) =
  Package_diagnostics.validate_metadata root_config;
  let resolution = Package_resolution.create root_config in
  let unallowed_dependencies = ref [] in
  let discovered =
    Package_traversal.discover ~root_config ~prod ~features ~resolution
  in
  List.iter
    (fun (package : Package_traversal.package) ->
      Output.debug ~verbosity:attempt.verbosity
        ("Parsing package: " ^ package.config.name);
      List.iter
        (fun (resolved : Package_traversal.resolved) ->
          let dependency = resolved.dependency in
          if
            not
              (dependent_is_allowed dependency.config.allowed_dependents
                 package.config.name)
          then
            unallowed_dependencies :=
              ( package.config.name,
                Package_traversal.dependency_kind_name resolved.request.kind,
                dependency.config.name )
              :: !unallowed_dependencies)
        package.dependencies)
    discovered.packages;
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
         ^ "\nUpdate allowed-dependents in the dependency rescript.json files."
          )));
  let packages_by_root = Hashtbl.create (List.length discovered.packages) in
  List.iter
    (fun (package : Package_traversal.package) ->
      Hashtbl.add packages_by_root package.config.root package)
    discovered.packages;
  let visited = Hashtbl.create 32 in
  let graph_packages = ref [] in
  let rec visit root =
    if not (Hashtbl.mem visited root) then (
      Hashtbl.add visited root ();
      let discovered_package = Hashtbl.find packages_by_root root in
      let is_local = discovered_package.Package_traversal.is_local in
      let features =
        match Feature_requests.find discovered.feature_requests root with
        | Some features -> Feature_requests.to_option features
        | None -> None
      in
      let config = discovered_package.config in
      Package_diagnostics.report_missing_sources
        ~is_root:(root = root_config.root) config;
      let config =
        match warn_error with
        | None -> config
        | Some value -> {config with warning_flags = ["-warn-error"; value]}
      in
      let dependency_directories =
        List.map
          (fun (resolved : Package_traversal.resolved) ->
            let request = resolved.request in
            Build_types.
              {
                declaration = request.declaration;
                directory = resolved.dependency.directory;
                kind = request.kind;
              })
          discovered_package.dependencies
      in
      List.iter
        (fun (dependency : Build_types.graph_dependency) ->
          visit dependency.directory)
        dependency_directories;
      let discovery =
        Output.debug ~verbosity:attempt.verbosity
          ("Building source file-tree for package: " ^ config.name);
        Source.discover_with_inventory config
          ~prod:(source_discovery_prod ~prod ~is_local)
          ~features
          ~filter:(if root = root_config.root then filter else None)
          ~on_missing:(Package_diagnostics.report_missing_source_folder config)
          ~on_orphan:(fun path ->
            Printf.eprintf
              "\027[2K\r No implementation file found for interface file \
               (skipping): %s\n\
               %!"
              path)
          ~display_root:root_config.root
      in
      let modules = discovery.modules in
      let owns_outputs =
        root <> root_config.root && Compiler_info.owns_outputs config
      in
      let compile_config =
        let config = with_gentype_source_dirs discovery.gentype_dirs config in
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
      let source_mtimes =
        Hashtbl.create (List.length discovery.source_mtimes)
      in
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
          graph_dependency_directories = dependency_directories;
          graph_gentype_dependency_args =
            Compiler_args.gentype_dependency_args_from_paths compile_config
              (List.map
                 (fun (dependency : Build_types.graph_dependency) ->
                   (dependency.declaration, dependency.directory))
                 dependency_directories);
          graph_modules = modules;
          graph_source_mtimes = source_mtimes;
          graph_source_files = discovery.inventory_files;
          graph_present_source_files = discovery.present_files;
        }
      in
      Build_session.add_graph_package attempt.session package;
      List.iter
        (fun module_ ->
          module_.Source.implementation
          :: Option.to_list module_.Source.interface
          |> List.iter (fun relative_path ->
              let absolute_path = Filename.concat root relative_path in
              Build_session.add_source_reference attempt.session
                (Platform.normalize_path_for_comparison absolute_path)
                Build_types.
                  {package_root = root; module_; relative_path; absolute_path}))
        modules;
      graph_packages := package :: !graph_packages)
  in
  visit root_config.root;
  !graph_packages
