exception Error = Project_context.Error

type result = {
  prepared: Build_types.prepared;
  cycle: Module_graph.cycle_info option;
}

let bsc_path () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let runtime_path root =
  try Toolchain.runtime ~find_package:(Project_context.dependency_path root)
  with Toolchain.Error message -> raise (Error message)

let run ~(root_config : Config.t) ~prod ~features ~warn_error ~filter ~watch
    ~(attempt : Build_attempt.t) ~parse_step ~on_cleanup =
  let bsc = bsc_path () in
  let graph_packages =
    Package_graph.discover ~root_config ~prod ~features ~warn_error ~filter
      ~attempt
  in
  Module_graph.validate_visible_namespaces ~root_config graph_packages;
  let runtime = runtime_path root_config.root in
  let source_map_args = Compiler_args.source_map_args root_config ~watch in
  let compiler_context =
    Compiler_info.make_context ~build_root:root_config.root ~bsc_path:bsc
      ~runtime_path:runtime ~source_map_args
      ~inherited_compiler_args:
        (root_config.jsx_args @ root_config.experimental_args)
      ~package_output_specs:(Compiler_info.package_output_specs root_config)
  in
  let cleanup_started = Unix.gettimeofday () in
  List.iter
    (fun (package : Build_types.graph_package) ->
      let package_context =
        Compiler_info.for_package compiler_context
          ~build_root:package.graph_build_owner package.graph_compile_config
      in
      if Compiler_info.needs_clean package_context package.graph_config then (
        Compiler_info.changed_package_output_specs package_context
          package.graph_config
        |> Option.iter (fun previous_specs ->
            let previous_config =
              Compiler_info.config_with_package_output_specs
                package.graph_compile_config previous_specs
            in
            Build_artifacts.remove_public_outputs previous_config
              package.graph_modules);
        let compile_assets = Compile_assets.create [package.graph_ocaml_dir] in
        ignore
          (Build_artifacts.cleanup_stale
             ~ocaml_files:
               (Compile_assets.files compile_assets package.graph_ocaml_dir)
             ~ast_sources:
               (Compile_assets.ast_sources compile_assets
                  package.graph_ocaml_dir)
             ~root:package.graph_root ~ocaml_dir:package.graph_ocaml_dir
             ~source_files:package.graph_source_files
             ~present_source_files:package.graph_present_source_files
             ~is_local:package.graph_is_local package.graph_compile_config
             package.graph_modules);
        Compiler_info.clean_package package.graph_config;
        attempt.compiler_cleaned <- true);
      File_util.ensure_dir package.graph_build_dir;
      File_util.ensure_dir package.graph_ocaml_dir)
    graph_packages;
  let compile_assets =
    graph_packages
    |> List.map (fun (package : Build_types.graph_package) ->
        package.graph_ocaml_dir)
    |> Compile_assets.create
  in
  List.iter
    (fun (package : Build_types.graph_package) ->
      let cleanup =
        Build_artifacts.cleanup_stale
          ~ocaml_files:
            (Compile_assets.files compile_assets package.graph_ocaml_dir)
          ~ast_sources:
            (Compile_assets.ast_sources compile_assets package.graph_ocaml_dir)
          ~root:package.graph_root ~ocaml_dir:package.graph_ocaml_dir
          ~source_files:package.graph_source_files
          ~present_source_files:package.graph_present_source_files
          ~is_local:package.graph_is_local package.graph_compile_config
          package.graph_modules
      in
      Build_attempt.set_cleanup_result attempt package.graph_root cleanup;
      Build_attempt.defer_artifact_cleanup attempt cleanup.deferred_artifacts;
      attempt.cleaned <- attempt.cleaned + List.length cleanup.removed_modules;
      attempt.previous_asts <-
        attempt.previous_asts + cleanup.previous_ast_count;
      List.iter
        (fun module_name ->
          Hashtbl.replace attempt.removed_modules module_name ())
        cleanup.removed_modules)
    graph_packages;
  on_cleanup (Unix.gettimeofday () -. cleanup_started);
  let parse_started = Unix.gettimeofday () in
  let parse_entries =
    graph_packages
    |> List.concat_map (fun (package : Build_types.graph_package) ->
        package.graph_modules
        |> List.concat_map (fun module_ ->
            let paths =
              module_.Source.implementation
              :: Option.to_list module_.Source.interface
            in
            let dirty_paths =
              paths
              |> List.filter (fun path ->
                  Build_freshness.source_is_not_older_than_ast compile_assets
                    ~root:package.graph_root
                    ~source_mtimes:package.graph_source_mtimes path)
            in
            if dirty_paths <> [] then
              Output.debug ~verbosity:attempt.verbosity
                ("Generating AST for module: "
                ^ Source.compiler_basename package.graph_compile_config
                    module_.Source.name);
            let group = package.graph_root ^ "\000" ^ module_.Source.name in
            List.map (fun path -> (package, path, group)) dirty_paths))
  in
  let parse_completed =
    Output.Progress.start_grouped attempt.progress ~step:parse_step ~symbol:"🧱 "
      ~label:"Parsing"
      (List.map (fun (_, _, group) -> group) parse_entries)
  in
  let parse_results =
    Process.run_parallel_map ?poll:attempt.process_poll
      ~on_complete:parse_completed parse_entries
      ~job:(fun ((package : Build_types.graph_package), path, _) ->
        Compiler_process.parse_job ~bsc ~build_dir:package.graph_build_dir
          ~config:package.graph_compile_config path)
  in
  let failed_parse_paths = Hashtbl.create 8 in
  List.iter2
    (fun ((package : Build_types.graph_package), path, _) result ->
      let absolute_path = Filename.concat package.graph_root path in
      let outcome = Build_types.preliminary_parse result in
      Hashtbl.replace attempt.preliminary_parses absolute_path outcome;
      match outcome with
      | Build_types.Parse_failed _ ->
        Hashtbl.replace failed_parse_paths absolute_path ()
      | Build_types.Parsed_successfully _ | Build_types.Use_existing_ast -> ())
    parse_entries parse_results;
  let graph =
    Module_graph.initialize ~root_config ~graph_packages ~compile_assets
      ~attempt ~failed_parse_paths
  in
  let nodes = graph.nodes in
  let namespace_maps = graph.namespace_maps in
  let build_state = graph.build_state in
  let packages = Hashtbl.create (List.length graph_packages) in
  List.iter
    (fun (package : Build_types.graph_package) ->
      let regular_dependency_dirs, development_dependency_dirs =
        List.fold_left
          (fun (regular, development)
               (dependency : Build_types.graph_dependency) ->
            let directory =
              Build_artifacts.lib_path dependency.directory "ocaml"
            in
            if not (File_util.exists directory) then (regular, development)
            else
              match dependency.kind with
              | Package_traversal.Regular -> (directory :: regular, development)
              | Package_traversal.Development ->
                (regular, directory :: development))
          ([], []) package.graph_dependency_directories
        |> fun (regular, development) -> (List.rev regular, List.rev development)
      in
      let common_args dependency_dirs =
        Compiler_args.compiler_common_arguments
          ~config:package.graph_compile_config ~runtime ~dependency_dirs ~watch
          ~gentype_dependency_args:package.graph_gentype_dependency_args
      in
      let parse_paths =
        package.graph_modules
        |> List.concat_map (fun module_ ->
            module_.Source.implementation
            :: Option.to_list module_.Source.interface)
      in
      Hashtbl.add packages package.graph_root
        Build_types.
          {
            regular_common_args = common_args regular_dependency_dirs;
            development_common_args =
              common_args (development_dependency_dirs @ regular_dependency_dirs);
            parse_paths;
          })
    graph_packages;
  let prepared =
    Build_types.{compiler_context; compile_assets; build_state; packages}
  in
  Build_session.install_prepared attempt.session prepared;
  let cycle = Module_graph.find_cycle nodes namespace_maps build_state in
  Build_session.set_graph_has_cycle attempt.session (Option.is_some cycle);
  attempt.parse_seconds <- Unix.gettimeofday () -. parse_started;
  {prepared; cycle}
