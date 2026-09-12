exception Error = Project_context.Error

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
  let package_plans = ref [] in
  let rec visit root =
    if not (Hashtbl.mem visited root) then (
      Hashtbl.add visited root ();
      let discovered_package = Hashtbl.find packages_by_root root in
      let is_local = discovered_package.Package_traversal.is_local in
      let features =
        match Package_traversal.find_feature_selection discovered root with
        | Some features ->
          Package_traversal.feature_selection_to_option features
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
      let dependencies =
        List.map
          (fun (resolved : Package_traversal.resolved) ->
            let request = resolved.request in
            Package_plan.
              {
                declaration = request.declaration;
                directory = resolved.dependency.directory;
                kind = request.kind;
              })
          discovered_package.dependencies
      in
      List.iter
        (fun (dependency : Package_plan.dependency) ->
          visit dependency.directory)
        dependencies;
      let discovery =
        Output.debug ~verbosity:attempt.verbosity
          ("Building source file-tree for package: " ^ config.name);
        Source.discover_with_inventory config
          ~prod:(Package_traversal.source_discovery_prod ~prod ~is_local)
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
        let inherited = Config.with_root_options config root_config in
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
      let package : Package_plan.t =
        Package_plan.
          {
            root;
            build_owner = (if owns_outputs then root else root_config.root);
            is_local;
            config;
            compile_config;
            build_dir;
            ocaml_dir;
            dependencies;
            gentype_dependency_args =
              Compiler_args.gentype_dependency_args_from_paths compile_config
                (List.map
                   (fun (dependency : Package_plan.dependency) ->
                     (dependency.declaration, dependency.directory))
                   dependencies);
            modules;
            source_mtimes;
            source_files = discovery.inventory_files;
            present_source_files = discovery.present_files;
          }
      in
      Build_session.add_package_plan attempt.session package;
      List.iter
        (fun module_ ->
          module_.Source.implementation
          :: Option.to_list module_.Source.interface
          |> List.iter (fun relative_path ->
              let absolute_path = Filename.concat root relative_path in
              Build_session.add_source_reference attempt.session
                (Platform.normalize_path_for_comparison absolute_path)
                Build_session.
                  {package_root = root; module_; relative_path; absolute_path}))
        modules;
      package_plans := package :: !package_plans)
  in
  visit root_config.root;
  !package_plans
