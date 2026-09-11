exception Error = Project_context.Error

open Build_types

type cycle_info = {
  cycle: string list;
  blocked: string list;
  nodes_by_key: (string, cycle_node) Hashtbl.t;
}

and cycle_node = {
  key: string;
  package_root: string;
  source_path: string option;
  display_name: string;
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
  Config.namespace_compiler_name config.namespace

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
           |> List.filter_map (fun dependency ->
               Hashtbl.find_opt by_root dependency.Build_types.directory))
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
                  Printf.sprintf "%s (%s)" package.Build_types.graph_config.name
                    (Project_context.display_path ~root:root_config.root
                       package.graph_root)
                in
                raise
                  (Error
                     (Printf.sprintf
                        "Could not initialize build: Namespace %s is provided \
                         by both %s and %s while building %s. Give the \
                         packages distinct namespaces."
                        namespace (display previous) (display package)
                        (display consumer))))))

let resolve_dependency
    (modules_by_key : (string, Build_types.global_module) Hashtbl.t)
    namespace_maps_by_name (node : Build_types.global_module) dependency =
  let raw_name = dependency_head dependency in
  let local_name =
    match
      (Config.namespace_name node.namespace, String.split_on_char '.' dependency)
    with
    | Some namespace, first :: second :: _ when first = namespace -> second
    | _ -> raw_name
  in
  let local_key = Config.namespaced_module_name node.namespace local_name in
  let is_visible (dependency_node : Build_types.global_module) =
    dependency_node.Build_types.package_name = node.package_name
    || List.mem dependency_node.package_name node.allowed_dependencies
  in
  match Hashtbl.find_opt modules_by_key local_key with
  | Some dependency_node when is_visible dependency_node -> [local_key]
  | _ when Config.namespace_name node.namespace = Some raw_name ->
    (* A qualified reference is recorded in the compiler dependency header by
       its leading namespace only. Treating that marker as a module reference
       would make it depend on every module exported by the package. *)
    []
  | _ -> (
    match Hashtbl.find_opt modules_by_key raw_name with
    | Some dependency_node when is_visible dependency_node -> [raw_name]
    | _ -> (
      let explicit_namespaced_module =
        match String.split_on_char '.' dependency with
        | namespace :: module_name :: _ ->
          [module_name ^ "-" ^ namespace; module_name ^ "-@" ^ namespace]
          |> List.find_opt (fun key ->
              match Hashtbl.find_opt modules_by_key key with
              | Some dependency_node
                when Config.namespace_name dependency_node.Build_types.namespace
                     = Some namespace
                     && is_visible dependency_node ->
                true
              | Some _ | None -> false)
        | _ -> None
      in
      match explicit_namespaced_module with
      | Some key -> [key]
      | None ->
        Hashtbl.find_opt namespace_maps_by_name raw_name
        |> Option.value ~default:[]
        |> List.filter_map (fun (namespace_map : Build_types.namespace_map) ->
            if
              namespace_map.package_name = node.package_name
              || List.mem namespace_map.package_name node.allowed_dependencies
            then Some namespace_map.key
            else None)))

let resolved_dependencies
    (modules_by_key : (string, Build_types.global_module) Hashtbl.t)
    namespace_maps_by_name (node : Build_types.global_module) =
  let parsed =
    node.raw_dependencies
    |> List.concat_map
         (resolve_dependency modules_by_key namespace_maps_by_name node)
  in
  let implicit_namespace_entry =
    match node.namespace with
    | Config.Namespace_with_entry {name = namespace; entry}
      when Source.module_name node.source_path = entry ->
      Hashtbl.find_opt namespace_maps_by_name namespace
      |> Option.value ~default:[]
      |> List.find_map (fun (namespace_map : Build_types.namespace_map) ->
          if namespace_map.package_root = node.package_root then
            Some namespace_map.key
          else None)
      |> Option.to_list
    | Config.Namespace_with_entry _ | Config.Namespace _ | Config.No_namespace
      ->
      []
  in
  parsed @ implicit_namespace_entry
  |> List.filter (fun dependency -> dependency <> node.key)
  |> List.sort_uniq String.compare

let find_cycle modules_by_key namespace_maps build_state =
  let nodes_by_key =
    Hashtbl.create
      (Hashtbl.length modules_by_key + Hashtbl.length namespace_maps)
  in
  Hashtbl.iter
    (fun key (node : Build_types.global_module) ->
      let module_name = Source.module_name node.source_path in
      let display_name =
        match node.namespace with
        | Config.Namespace_with_entry {name = namespace; entry}
          when entry <> module_name ->
          namespace ^ "." ^ module_name
        | Config.Namespace namespace -> namespace ^ "." ^ module_name
        | Config.Namespace_with_entry _ | Config.No_namespace -> module_name
      in
      Hashtbl.add nodes_by_key key
        {
          key;
          package_root = node.package_root;
          source_path = Some node.source_path;
          display_name;
        })
    modules_by_key;
  Hashtbl.iter
    (fun key (namespace_map : Build_types.namespace_map) ->
      Hashtbl.add nodes_by_key key
        {
          key;
          package_root = namespace_map.package_root;
          source_path = None;
          display_name = namespace_map.compiler_name;
        })
    namespace_maps;
  let graph_nodes =
    Hashtbl.to_seq_values nodes_by_key
    |> List.of_seq
    |> List.sort (fun first second -> String.compare first.key second.key)
    |> List.map (fun node ->
        (node, (Build_state.find_exn build_state node.key).dependencies))
  in
  let name (node, _) = node.key in
  let blocked_nodes = Graph.cycle_blocked_nodes graph_nodes ~name ~deps:snd in
  match blocked_nodes with
  | [] -> None
  | _ ->
    let cycle =
      match Graph.shortest_cycle blocked_nodes ~name ~deps:snd with
      | Some cycle -> cycle
      | None ->
        raise
          (Error "cycle-blocked dependency graph contains no detectable cycle")
    in
    Some {cycle; blocked = List.map name blocked_nodes; nodes_by_key}

let run ~(root_config : Config.t) ~prod ~features ~warn_error ~filter ~watch
    ~(stats : Build_types.t) ~parse_step ~on_cleanup =
  let bsc = bsc_path () in
  let graph_packages =
    Package_graph.discover ~root_config ~prod ~features ~warn_error ~filter
      ~stats
  in
  validate_visible_namespaces ~root_config graph_packages;
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
    (fun package ->
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
          ~root:package.graph_root ~ocaml_dir:package.graph_ocaml_dir
          ~source_files:package.graph_source_files
          ~present_source_files:package.graph_present_source_files
          ~is_local:package.graph_is_local package.graph_compile_config
          package.graph_modules
      in
      Hashtbl.replace stats.retained.cleanup_results package.graph_root cleanup;
      stats.deferred_artifact_cleanup <-
        cleanup.deferred_artifacts @ stats.deferred_artifact_cleanup;
      stats.cleaned <- stats.cleaned + List.length cleanup.removed_modules;
      stats.previous_asts <- stats.previous_asts + cleanup.previous_ast_count;
      List.iter
        (fun module_name ->
          Hashtbl.replace stats.removed_modules module_name ())
        cleanup.removed_modules)
    graph_packages;
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
      ~label:"Parsing"
      (List.map (fun (_, _, group) -> group) parse_entries)
  in
  let parse_results =
    Process.run_parallel_map ?poll:stats.process_poll
      ~on_complete:parse_completed parse_entries ~job:(fun (package, path, _) ->
        Compiler_process.parse_job ~bsc ~build_dir:package.graph_build_dir
          ~config:package.graph_compile_config path)
  in
  let failed_parse_paths = Hashtbl.create 8 in
  List.iter2
    (fun (package, path, _) result ->
      let absolute_path = Filename.concat package.graph_root path in
      let outcome = Build_types.preliminary_parse result in
      Hashtbl.replace stats.preliminary_parses absolute_path outcome;
      match outcome with
      | Build_types.Parse_failed _ ->
        Hashtbl.replace failed_parse_paths absolute_path ()
      | Build_types.Parsed_successfully _ | Build_types.Use_existing_ast -> ())
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
                  ~build_dir:package.graph_build_dir (Source.ast_path path)
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
          (if Option.is_none (Compile_assets.cmt compile_assets compiler_base)
           then
             let implementation =
               Filename.concat package.graph_root module_.Source.implementation
             in
             if not (Hashtbl.mem stats.preliminary_parses implementation) then
               Hashtbl.replace stats.preliminary_parses implementation
                 Build_types.Use_existing_ast);
          nodes :=
            {
              key = compiler_base;
              package_name = package.graph_config.name;
              package_root = package.graph_root;
              source_path = module_.Source.implementation;
              namespace = package.graph_compile_config.namespace;
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
    List.sort
      (fun (first : Build_types.global_module) second ->
        String.compare first.key second.key)
      !nodes
  in
  let by_key : (string, Build_types.global_module) Hashtbl.t =
    Hashtbl.create (List.length nodes)
  in
  List.iter
    (fun (node : Build_types.global_module) ->
      match Hashtbl.find_opt by_key node.key with
      | None -> Hashtbl.add by_key node.key node
      | Some previous ->
        raise
          (Source.duplicate_error ~display_root:root_config.root "" node.key
             (Filename.concat previous.package_root previous.source_path)
             (Filename.concat node.package_root node.source_path)))
    nodes;
  Hashtbl.iter
    (fun key node -> Hashtbl.add stats.retained.global_modules key node)
    by_key;
  let namespace_maps =
    graph_packages
    |> List.filter_map (fun package ->
        let namespace = package.Build_types.graph_compile_config.namespace in
        let namespace_details =
          match namespace with
          | Config.No_namespace -> None
          | Config.Namespace name -> Some (name, name, None)
          | Config.Namespace_with_entry {name; entry} ->
            Some ("@" ^ name, name, Some entry)
        in
        namespace_details
        |> Option.map (fun (compiler_name, name, namespace_entry) ->
            let members =
              package.graph_modules
              |> List.filter (fun module_ ->
                  Some module_.Source.name <> namespace_entry)
              |> List.filter (fun module_ ->
                  Source.is_non_exotic_module_name module_.Source.name)
              |> List.map (fun module_ ->
                  Source.compiler_basename package.graph_compile_config
                    module_.Source.name)
              |> List.sort_uniq String.compare
            in
            Build_types.
              {
                key = Build_types.namespace_map_key package.graph_root;
                compiler_name;
                namespace = name;
                package_name = package.graph_config.name;
                package_root = package.graph_root;
                members;
              }))
  in
  List.iter
    (fun (namespace_map : Build_types.namespace_map) ->
      Hashtbl.add stats.retained.namespace_maps namespace_map.key namespace_map;
      let maps =
        Hashtbl.find_opt stats.retained.namespace_maps_by_name
          namespace_map.namespace
        |> Option.value ~default:[]
      in
      Hashtbl.replace stats.retained.namespace_maps_by_name
        namespace_map.namespace (namespace_map :: maps))
    namespace_maps;
  let source_graph_nodes =
    List.map
      (fun (node : Build_types.global_module) ->
        ( node,
          resolved_dependencies by_key stats.retained.namespace_maps_by_name
            node ))
      nodes
  in
  let build_state =
    Build_state.create
      (List.length source_graph_nodes + List.length namespace_maps)
  in
  let modified = Option.map (fun entry -> entry.Compile_assets.modified) in
  List.iter
    (fun ((node : Build_types.global_module), _) ->
      Build_state.add build_state ~key:node.key ~kind:Build_state.Source_module
        ~last_compiled_cmi:
          (Compile_assets.cmi compile_assets node.key |> modified)
        ~last_compiled_cmt:
          (Compile_assets.cmt compile_assets node.key |> modified))
    source_graph_nodes;
  List.iter
    (fun (namespace_map : Build_types.namespace_map) ->
      Build_state.add build_state ~key:namespace_map.key
        ~kind:Build_state.Namespace_map
        ~last_compiled_cmi:
          (Compile_assets.cmi compile_assets namespace_map.compiler_name
          |> modified)
        ~last_compiled_cmt:
          (Compile_assets.cmt compile_assets namespace_map.compiler_name
          |> modified))
    namespace_maps;
  List.iter
    (fun ((node : Build_types.global_module), dependencies) ->
      Build_state.set_dependencies build_state ~key:node.key dependencies)
    source_graph_nodes;
  List.iter
    (fun (namespace_map : Build_types.namespace_map) ->
      Build_state.set_dependencies build_state ~key:namespace_map.key
        namespace_map.members)
    namespace_maps;
  let packages = Hashtbl.create (List.length graph_packages) in
  List.iter
    (fun (package : Build_types.graph_package) ->
      let regular_dependency_dirs, development_dependency_dirs =
        List.fold_left
          (fun (regular, development) dependency ->
            let directory =
              Build_artifacts.lib_path dependency.directory "ocaml"
            in
            if not (Sys.file_exists directory) then (regular, development)
            else
              match dependency.kind with
              | Build_types.Regular_dependency ->
                (directory :: regular, development)
              | Build_types.Development_dependency ->
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
  Build_types.install_prepared stats
    {compiler_context; compile_assets; build_state; packages};
  let cycle =
    find_cycle stats.retained.global_modules stats.retained.namespace_maps
      build_state
  in
  stats.retained.graph_has_cycle <- Option.is_some cycle;
  stats.parse_seconds <- Unix.gettimeofday () -. parse_started;
  cycle
