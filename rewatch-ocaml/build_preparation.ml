exception Error = Project_context.Error

type result = {
  prepared: Build_session.prepared;
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
  let package_plans =
    Package_graph.discover ~root_config ~prod ~features ~warn_error ~filter
      ~attempt
  in
  Module_graph.validate_visible_namespaces ~root_config package_plans;
  let runtime = runtime_path root_config.root in
  let source_map_args = Compiler_args.source_map_args root_config ~watch in
  let compiler_context =
    Compiler_info.make_context ~build_root:root_config.root ~bsc_path:bsc
      ~runtime_path:runtime ~source_map_args
      ~inherited_compiler_args:
        (root_config.jsx_args @ root_config.experimental_args)
      ~package_output_specs:(Compiler_info.package_output_specs root_config)
  in
  let previous_compile_assets =
    package_plans
    |> List.map (fun (package : Package_plan.t) -> package.ocaml_dir)
    |> Compile_assets.create
  in
  let registered_removed_modules = Hashtbl.create 16 in
  let dependents_by_raw_dependency = Hashtbl.create 64 in
  package_plans
  |> List.concat_map (fun (package : Package_plan.t) ->
      Compile_assets.ast_sources previous_compile_assets package.ocaml_dir)
  |> List.iter (fun (ast_source : Compile_assets.ast_source) ->
      Compile_assets.ast_dependencies previous_compile_assets
        ast_source.ast_path
      |> List.iter (fun dependency ->
          let dependents =
            Hashtbl.find_opt dependents_by_raw_dependency dependency
            |> Option.value ~default:[]
          in
          Hashtbl.replace dependents_by_raw_dependency dependency
            (ast_source.ast_path :: dependents)));
  let invalidate_removed_module module_name =
    if not (Hashtbl.mem registered_removed_modules module_name) then (
      Hashtbl.add registered_removed_modules module_name ();
      Hashtbl.replace attempt.removed_modules module_name ();
      Build_session.mark_module_removed attempt.session module_name;
      Hashtbl.find_opt dependents_by_raw_dependency module_name
      |> Option.value ~default:[]
      |> List.iter File_util.remove_file)
  in
  let cleanup_started = Unix.gettimeofday () in
  List.iter
    (fun (package : Package_plan.t) ->
      let package_context =
        Compiler_info.for_package compiler_context
          ~build_root:package.build_owner package.compile_config
      in
      if Compiler_info.needs_clean package_context package.config then (
        Compiler_info.changed_package_output_specs package_context
          package.config
        |> Option.iter (fun previous_specs ->
            let previous_config =
              Compiler_info.config_with_package_output_specs
                package.compile_config previous_specs
            in
            let previous_implementations =
              List.map
                (fun (module_ : Source.module_) -> module_.implementation)
                package.modules
              @ (Compile_assets.ast_sources previous_compile_assets
                   package.ocaml_dir
                |> List.filter_map (fun (source : Compile_assets.ast_source) ->
                    if Filename.check_suffix source.ast_path ".iast" then None
                    else
                      Project_context.relative_to_opt package.root
                        source.source_path))
              |> List.sort_uniq String.compare
            in
            Build_artifacts.remove_public_outputs previous_config
              previous_implementations);
        ignore
          (Build_artifacts.cleanup_stale
             ~ocaml_files:
               (Compile_assets.files previous_compile_assets package.ocaml_dir)
             ~ast_sources:
               (Compile_assets.ast_sources previous_compile_assets
                  package.ocaml_dir)
             ~root:package.root ~ocaml_dir:package.ocaml_dir
             ~source_files:package.source_files
             ~present_source_files:package.present_source_files
             ~on_removed_module:invalidate_removed_module
             ~on_deferred_artifact:File_util.remove_file
             ~is_local:package.is_local package.compile_config package.modules);
        Compiler_info.clean_package package.config;
        attempt.compiler_cleaned <- true);
      File_util.ensure_dir package.build_dir;
      File_util.ensure_dir package.ocaml_dir)
    package_plans;
  List.iter
    (fun (package : Package_plan.t) ->
      let cleanup =
        Build_artifacts.cleanup_stale
          ~ocaml_files:
            (Compile_assets.files previous_compile_assets package.ocaml_dir)
          ~ast_sources:
            (Compile_assets.ast_sources previous_compile_assets
               package.ocaml_dir)
          ~root:package.root ~ocaml_dir:package.ocaml_dir
          ~source_files:package.source_files
          ~present_source_files:package.present_source_files
          ~on_removed_module:invalidate_removed_module
          ~on_deferred_artifact:(fun path ->
            Build_attempt.defer_artifact_cleanup attempt [path])
          ~is_local:package.is_local package.compile_config package.modules
      in
      Build_attempt.set_cleanup_result attempt package.root cleanup;
      attempt.cleaned <- attempt.cleaned + List.length cleanup.removed_modules;
      attempt.previous_asts <-
        attempt.previous_asts + cleanup.previous_ast_count;
      ())
    package_plans;
  let compile_assets =
    if attempt.compiler_cleaned || Hashtbl.length registered_removed_modules > 0
    then
      package_plans
      |> List.map (fun (package : Package_plan.t) -> package.ocaml_dir)
      |> Compile_assets.create
    else previous_compile_assets
  in
  on_cleanup (Unix.gettimeofday () -. cleanup_started);
  let parse_started = Unix.gettimeofday () in
  let parse_entries =
    package_plans
    |> List.concat_map (fun (package : Package_plan.t) ->
        package.modules
        |> List.concat_map (fun module_ ->
            let paths =
              module_.Source.implementation
              :: Option.to_list module_.Source.interface
            in
            let dirty_paths =
              paths
              |> List.filter (fun path ->
                  Build_freshness.source_is_not_older_than_ast compile_assets
                    ~root:package.root ~source_mtimes:package.source_mtimes path)
            in
            if dirty_paths <> [] then
              Output.debug ~verbosity:attempt.verbosity
                ("Generating AST for module: "
                ^ Source.compiler_basename package.compile_config
                    module_.Source.name);
            let group = package.root ^ "\000" ^ module_.Source.name in
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
      ~job:(fun ((package : Package_plan.t), path, _) ->
        Compiler_process.parse_job ~bsc ~build_dir:package.build_dir
          ~config:package.compile_config path)
  in
  let failed_parse_paths = Hashtbl.create 8 in
  List.iter2
    (fun ((package : Package_plan.t), path, _) result ->
      let absolute_path = Filename.concat package.root path in
      let outcome = Build_attempt.preliminary_parse result in
      Hashtbl.replace attempt.preliminary_parses absolute_path outcome;
      match outcome with
      | Build_attempt.Parse_failed _ ->
        Hashtbl.replace failed_parse_paths absolute_path ()
      | Build_attempt.Parsed_successfully _ | Build_attempt.Use_existing_ast ->
        ())
    parse_entries parse_results;
  let graph =
    Module_graph.initialize ~root_config ~package_plans ~compile_assets
      ~failed_parse_paths
  in
  List.iter
    (fun path ->
      if not (Hashtbl.mem attempt.preliminary_parses path) then
        Hashtbl.replace attempt.preliminary_parses path
          Build_attempt.Use_existing_ast)
    graph.use_existing_ast_paths;
  List.iter
    (fun (node : Module_graph.module_node) ->
      Build_session.add_global_module attempt.session node.key node)
    graph.nodes;
  List.iter
    (Build_session.add_namespace_map attempt.session)
    graph.namespace_maps;
  let nodes = graph.nodes in
  let namespace_maps = graph.namespace_maps in
  let build_state = graph.build_state in
  let packages = Hashtbl.create (List.length package_plans) in
  List.iter
    (fun (package : Package_plan.t) ->
      let regular_dependency_dirs, development_dependency_dirs =
        List.fold_left
          (fun (regular, development) (dependency : Package_plan.dependency) ->
            let directory =
              Build_artifacts.lib_path dependency.directory "ocaml"
            in
            if not (File_util.exists directory) then (regular, development)
            else
              match dependency.kind with
              | Package_traversal.Regular -> (directory :: regular, development)
              | Package_traversal.Development ->
                (regular, directory :: development))
          ([], []) package.dependencies
        |> fun (regular, development) -> (List.rev regular, List.rev development)
      in
      let common_args dependency_dirs =
        Compiler_args.compiler_common_arguments ~config:package.compile_config
          ~runtime ~dependency_dirs ~watch
          ~gentype_dependency_args:package.gentype_dependency_args
      in
      let parse_paths =
        package.modules
        |> List.concat_map (fun module_ ->
            module_.Source.implementation
            :: Option.to_list module_.Source.interface)
      in
      Hashtbl.add packages package.root
        Package_plan.
          {
            regular_common_args = common_args regular_dependency_dirs;
            development_common_args =
              common_args (development_dependency_dirs @ regular_dependency_dirs);
            parse_paths;
          })
    package_plans;
  let prepared =
    Build_session.{compiler_context; compile_assets; build_state; packages}
  in
  Build_session.install_prepared attempt.session prepared;
  let cycle = Module_graph.find_cycle nodes namespace_maps build_state in
  Build_session.set_graph_cycle attempt.session cycle;
  attempt.parse_seconds <- Unix.gettimeofday () -. parse_started;
  {prepared; cycle}
