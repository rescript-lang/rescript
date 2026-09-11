exception Error = Project_context.Error

open Build_types

type cycle_info = {
  cycle: string list;
  blocked: string list;
  modules_by_key: (string, Build_types.global_module) Hashtbl.t;
}

let bsc_path () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let runtime_path root =
  try Toolchain.runtime ~find_package:(Project_context.dependency_path root)
  with Toolchain.Error message -> raise (Error message)

let dependency_head dependency =
  match String.split_on_char '.' dependency with
  | head :: _ -> head
  | [] -> dependency

let compiler_namespace (config : Config.t) =
  match config.namespace, config.namespace_entry with
  | Some namespace, Some _ -> Some ("@" ^ namespace)
  | Some namespace, None -> Some namespace
  | None, _ -> None

let validate_visible_namespaces ~(root_config : Config.t)
    (graph_packages : Build_types.graph_package list) =
  let by_root = Hashtbl.create (List.length graph_packages) in
  List.iter
    (fun (package : Build_types.graph_package) ->
      Hashtbl.replace by_root package.graph_root package)
    graph_packages;
  graph_packages
  |> List.sort (fun first second ->
       String.compare first.Build_types.graph_root second.graph_root)
  |> List.iter (fun consumer ->
       let visible =
         consumer
         :: (consumer.Build_types.graph_dependency_directories
            |> List.filter_map (fun (_, directory) ->
                 Hashtbl.find_opt by_root directory))
       in
       let namespaces = Hashtbl.create (List.length visible) in
       visible
       |> List.sort (fun first second ->
            String.compare first.Build_types.graph_root second.graph_root)
       |> List.iter (fun package ->
            compiler_namespace package.Build_types.graph_compile_config
            |> Option.iter (fun namespace ->
                 match Hashtbl.find_opt namespaces namespace with
                 | None -> Hashtbl.add namespaces namespace package
                 | Some previous
                   when previous.Build_types.graph_root = package.graph_root ->
                   ()
                 | Some previous ->
                   let display package =
                     Printf.sprintf "%s (%s)"
                       package.Build_types.graph_config.name
                       (Project_context.relative_to root_config.root
                          package.graph_root)
                   in
                   raise
                     (Error
                        (Printf.sprintf
                           "Could not initialize build: Namespace %s is provided by both %s and %s while building %s. Give the packages distinct namespaces."
                           namespace (display previous) (display package)
                           (display consumer))))))

let resolve_dependency
    (modules_by_key : (string, Build_types.global_module) Hashtbl.t)
    (node : Build_types.global_module) dependency =
  let raw_name = dependency_head dependency in
  let local_name =
    match node.namespace, String.split_on_char '.' dependency with
    | Some namespace, first :: second :: _ when first = namespace -> second
    | _ -> raw_name
  in
  let local_key =
    match node.namespace with
    | None -> local_name
    | Some namespace -> (
      match node.namespace_entry with
      | Some entry when entry = local_name -> local_name
      | Some _ -> local_name ^ "-@" ^ namespace
      | None -> local_name ^ "-" ^ namespace)
  in
  let is_visible dependency_node =
    dependency_node.Build_types.package_name = node.package_name
    || List.mem dependency_node.package_name node.allowed_dependencies
  in
  match Hashtbl.find_opt modules_by_key local_key with
  | Some dependency_node when is_visible dependency_node -> [local_key]
  | _ when node.namespace = Some raw_name ->
    (* A qualified reference is recorded in the compiler dependency header by
       its leading namespace only. Treating that marker as a module reference
       would make it depend on every module exported by the package. *)
    []
  | _ -> (
    match Hashtbl.find_opt modules_by_key raw_name with
    | Some dependency_node when is_visible dependency_node -> [raw_name]
    | _ ->
      let explicit_namespaced_module =
        match String.split_on_char '.' dependency with
        | namespace :: module_name :: _ ->
          [module_name ^ "-" ^ namespace; module_name ^ "-@" ^ namespace]
          |> List.find_opt (fun key ->
               match Hashtbl.find_opt modules_by_key key with
               | Some dependency_node
                 when dependency_node.Build_types.namespace = Some namespace
                      && is_visible dependency_node ->
                 true
               | Some _ | None -> false)
        | _ -> None
      in
      match explicit_namespaced_module with
      | Some key -> [key]
      | None ->
        Hashtbl.to_seq_values modules_by_key
        |> Seq.filter_map (fun dependency_node ->
             if
               dependency_node.Build_types.namespace = Some raw_name
               && is_visible dependency_node
             then Some dependency_node.key
             else None)
        |> List.of_seq)

let resolved_dependencies
    (modules_by_key : (string, Build_types.global_module) Hashtbl.t)
    (node : Build_types.global_module) =
  node.raw_dependencies
  |> List.concat_map (resolve_dependency modules_by_key node)
  |> List.filter (fun dependency -> dependency <> node.key)
  |> List.sort_uniq String.compare

let find_cycle
    (modules_by_key : (string, Build_types.global_module) Hashtbl.t)
    build_state =
  let graph_nodes =
    Hashtbl.to_seq_values modules_by_key |> List.of_seq
    |> List.sort (fun first second ->
         String.compare first.Build_types.key second.key)
    |> List.map (fun node ->
         ( node,
           (Build_state.find_exn build_state node.Build_types.key).dependencies ))
  in
  try
    ignore
      (Graph.topological_sort graph_nodes
         ~name:(fun (node, _) -> node.Build_types.key)
         ~deps:snd);
    None
  with Graph.Cycle cycle ->
    let blocked =
      Graph.blocked_dependents
        (List.map
           (fun (node, dependencies) ->
             (node.Build_types.key, dependencies))
           graph_nodes)
        cycle
    in
    Some {cycle; blocked; modules_by_key}

let run ~(root_config : Config.t) ~prod ~features ~warn_error ~filter ~watch
    ~(stats : Build_types.t) ~parse_step ~on_cleanup =
  let bsc = bsc_path () in
  let graph_packages =
    Package_graph.discover ~root_config ~prod ~features ~warn_error ~filter ~stats
  in
  validate_visible_namespaces ~root_config graph_packages;
  let runtime = runtime_path root_config.root in
  let source_map_args =
    if root_config.source_map_dev && not watch then
      ["-bs-source-map"; "false"]
    else root_config.source_map_args
  in
  let compiler_context =
    Compiler_info.make_context ~build_root:root_config.root ~bsc_path:bsc
      ~runtime_path:runtime
      ~source_map_args
      ~package_output_specs:(Compiler_info.package_output_specs root_config)
  in
  stats.compiler_context <- Some compiler_context;
  let cleanup_started = Unix.gettimeofday () in
  List.iter
    (fun package ->
      let package_context =
        {
          compiler_context with
          build_root = package.graph_build_owner;
          package_output_specs =
            Compiler_info.package_output_specs package.graph_compile_config;
        }
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
        let compile_assets =
          Compile_assets.create [package.graph_ocaml_dir]
        in
        ignore
          (Build_artifacts.cleanup_stale
             ~ocaml_files:
               (Compile_assets.files compile_assets package.graph_ocaml_dir)
             ~ast_sources:
               (Compile_assets.ast_sources compile_assets
                  package.graph_ocaml_dir)
             ~root:package.graph_root
             ~ocaml_dir:package.graph_ocaml_dir
             ~source_files:package.graph_source_files
             ~is_local:package.graph_is_local
             package.graph_compile_config package.graph_modules);
        Compiler_info.clean_package package.graph_config;
        stats.compiler_cleaned <- true);
      File_util.ensure_dir package.graph_build_dir;
      File_util.ensure_dir package.graph_ocaml_dir)
    graph_packages;
  let compile_assets =
    graph_packages
    |> List.map (fun package -> package.graph_ocaml_dir)
    |> Compile_assets.create
  in
  List.iter
    (fun package ->
      let cleanup =
        Build_artifacts.cleanup_stale
          ~ocaml_files:
            (Compile_assets.files compile_assets package.graph_ocaml_dir)
          ~ast_sources:
            (Compile_assets.ast_sources compile_assets package.graph_ocaml_dir)
          ~root:package.graph_root
          ~ocaml_dir:package.graph_ocaml_dir
          ~source_files:package.graph_source_files
          ~is_local:package.graph_is_local
          package.graph_compile_config package.graph_modules
      in
      Hashtbl.replace stats.cleanup_results package.graph_root
        cleanup;
      stats.deferred_artifact_cleanup :=
        cleanup.deferred_artifacts @ !(stats.deferred_artifact_cleanup);
      stats.cleaned <- stats.cleaned + List.length cleanup.removed_modules;
      stats.previous_asts <-
        stats.previous_asts + cleanup.previous_ast_count;
      List.iter
        (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
        cleanup.removed_modules)
    graph_packages;
  stats.compile_assets <- Some compile_assets;
  on_cleanup (Unix.gettimeofday () -. cleanup_started);
  let parse_started = Unix.gettimeofday () in
  let parse_entries =
    graph_packages
    |> List.concat_map (fun package ->
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
                Output.debug ~verbosity:stats.verbosity
                  ("Generating AST for module: "
                  ^ Source.compiler_basename package.graph_compile_config
                      module_.Source.name);
              let group = package.graph_root ^ "\000" ^ module_.Source.name in
              List.map (fun path -> (package, path, group)) dirty_paths))
  in
  let parse_completed =
    Output.Progress.start_grouped stats.progress ~step:parse_step ~symbol:"🧱 "
      ~label:"Parsing" (List.map (fun (_, _, group) -> group) parse_entries)
  in
  let parse_results =
    parse_entries
    |> List.map (fun (package, path, _) ->
         Compiler_process.parse_job ~bsc ~build_dir:package.graph_build_dir
           ~config:package.graph_compile_config path)
    |> Process.run_parallel ?poll:stats.process_poll ~on_complete:parse_completed
  in
  let failed_parse_paths = Hashtbl.create 8 in
  List.iter2
    (fun (package, path, _) result ->
      let absolute_path = Filename.concat package.graph_root path in
      Hashtbl.replace stats.forced_parse_paths absolute_path ();
      Hashtbl.replace stats.preparse_results absolute_path result;
      if Process.succeeded result then (
        if result.stderr <> "" then
          Hashtbl.replace stats.preparse_stderr absolute_path result.stderr)
      else Hashtbl.replace failed_parse_paths absolute_path ())
    parse_entries parse_results;
  let nodes = ref [] in
  List.iter
    (fun package ->
      List.iter
        (fun module_ ->
          let intf_dependencies =
            match module_.Source.interface with
            | None -> []
            | Some path ->
              if
                Hashtbl.mem failed_parse_paths
                  (Filename.concat package.graph_root path)
              then []
              else
                Compiler_process.ast_dependencies
                  ~build_dir:package.graph_build_dir
                  (Source.ast_path path)
          in
          let raw_dependencies =
            List.sort_uniq String.compare
              ((if
                  Hashtbl.mem failed_parse_paths
                    (Filename.concat package.graph_root
                       module_.Source.implementation)
                then []
                else
                  Compiler_process.ast_dependencies
                    ~build_dir:package.graph_build_dir
                    (Source.ast_path module_.Source.implementation))
              @ intf_dependencies)
          in
          let compiler_base =
            Source.compiler_basename package.graph_compile_config
              module_.Source.name
          in
          if Option.is_none (Compile_assets.cmt compile_assets compiler_base) then
            Hashtbl.replace stats.forced_parse_paths
              (Filename.concat package.graph_root module_.Source.implementation)
              ();
          Hashtbl.replace stats.global_raw_dependencies compiler_base
            raw_dependencies;
          nodes :=
            {
              key = compiler_base;
              package_name = package.graph_config.name;
              package_root = package.graph_root;
              source_path = module_.Source.implementation;
              source = module_;
              namespace = package.graph_compile_config.namespace;
              namespace_entry = package.graph_compile_config.namespace_entry;
              allowed_dependencies =
                List.map
                  (fun (dependency : Config.dependency) -> dependency.name)
                  package.graph_dependencies;
              raw_dependencies;
            }
            :: !nodes)
        package.graph_modules)
    graph_packages;
  let nodes =
    List.sort (fun first second -> String.compare first.key second.key) !nodes
  in
  let by_key = Hashtbl.create (List.length nodes) in
  List.iter
    (fun node ->
      match Hashtbl.find_opt by_key node.key with
      | None -> Hashtbl.add by_key node.key node
      | Some previous ->
        raise
          (Source.duplicate_error ~display_root:root_config.root "" node.key
             (Filename.concat previous.package_root previous.source_path)
             (Filename.concat node.package_root node.source_path)))
    nodes;
  Hashtbl.iter (fun key node -> Hashtbl.add stats.global_modules key node) by_key;
  let graph_nodes =
    List.map
      (fun node -> (node, resolved_dependencies by_key node))
      nodes
  in
  let build_state = Build_state.create (List.length graph_nodes) in
  let modified = Option.map (fun entry -> entry.Compile_assets.modified) in
  List.iter
    (fun (node, _) ->
      Build_state.add build_state ~key:node.key
        ~package_name:node.package_name ~package_root:node.package_root
        ~source:node.source ~raw_dependencies:node.raw_dependencies
        ~last_compiled_cmi:(Compile_assets.cmi compile_assets node.key |> modified)
        ~last_compiled_cmt:(Compile_assets.cmt compile_assets node.key |> modified))
    graph_nodes;
  List.iter
    (fun (node, dependencies) ->
      Build_state.set_dependencies build_state ~key:node.key dependencies)
    graph_nodes;
  stats.build_state <- Some build_state;
  let cycle = find_cycle by_key build_state in
  stats.parse_seconds <- Unix.gettimeofday () -. parse_started;
  cycle
