let rec run ~(root_config : Config.t) ~dependency_context ~seen ~root ~prod
    ~is_local ~on_clean =
  if not (Hashtbl.mem seen root) then (
    Hashtbl.add seen root ();
    let config_path = Config.path_in_root root in
    let should_clean, package_name =
      if Config.exists_in_root root then (
        let config = Config.load config_path in
        Package_diagnostics.validate_metadata config;
        Package_diagnostics.report_missing_sources
          ~is_root:(root = root_config.root) config;
        (* A consumer clean owns dependencies previously built in this build
           context, but not an independently built package's published tree. *)
        let owns_outputs =
          root <> root_config.root && Compiler_info.owns_outputs config
        in
        if owns_outputs then (false, None)
        else (
          let dependencies =
            config.dependencies
            @ if prod || not is_local then [] else config.dev_dependencies
          in
          List.iter
            (fun (dependency : Config.dependency) ->
              let directory =
                Project_context.require_dependency_directory
                  ~context:dependency_context root dependency
              in
              try
                run ~root_config ~dependency_context ~seen ~root:directory ~prod
                  ~is_local:
                    (Project_context.is_local_dependency_canonical
                       ~workspace:
                         (Project_context.dependency_workspace dependency_context)
                       directory)
                  ~on_clean
              with Config.Error message ->
                raise
                  (Project_context.Package_error
                     (Printf.sprintf
                        "Could not build package tree for '%s' at path '%s'. Error: %s"
                        dependency.name root_config.root message)))
            dependencies;
          let implementation_files, inventory_files =
            Source.discover_for_cleanup config
              ~prod:(prod || not is_local)
              ~on_missing:
                (Package_diagnostics.report_missing_source_folder config)
          in
          let output_config =
            Build_artifacts.with_root_options config root_config
          in
          Build_artifacts.cleanup_watch_output_sidecars
            ~source_files:inventory_files ~root output_config;
          List.iter
            (fun implementation ->
              List.iter
                (fun spec ->
                  let output =
                    Build_artifacts.generated_js_path output_config
                      implementation spec
                  in
                  File_util.remove_file output;
                  File_util.remove_file (output ^ ".map");
                  File_util.remove_file (output ^ ".rewatch-pending");
                  File_util.remove_file (output ^ ".rewatch-backup");
                  File_util.remove_file (output ^ ".map.rewatch-pending");
                  File_util.remove_file (output ^ ".map.rewatch-backup"))
                output_config.package_specs)
            implementation_files;
          (true, Some config.name)))
      else (true, None)
    in
    if should_clean then (
      Option.iter on_clean package_name;
      List.iter
        (fun dir -> File_util.remove_tree (Filename.concat root dir))
        [
          Build_artifacts.lib_path "" "bs";
          Build_artifacts.lib_path "" "ocaml";
        ]))
