exception Error = Project_context.Error
exception Package_error = Project_context.Package_error
exception Stop_watch = Watcher.Stop
exception Build_failure = Compiler_scheduler.Build_failure
exception Parse_failure of string

open Build_artifacts
open Build_types

let bsc_path () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let runtime_path root =
  try Toolchain.runtime ~find_package:(Project_context.dependency_path root)
  with Toolchain.Error message -> raise (Error message)

let run_after_build ~root command =
  let program, args =
    match Str.split (Str.regexp "[ \t\r\n]+") command with
    | program :: args -> (program, args)
    | [] -> raise (Error "--after-build command cannot be empty")
  in
  let result =
    try Process.run ~cwd:root program args with
    | Process.Error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Sys_error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Unix.Unix_error (error, operation, argument) ->
      let target = if argument = "" then program else argument in
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s (%s %s)"
              command (Unix.error_message error) operation target))
  in
  if not (Process.succeeded result) then (
    let output = result.stderr ^ result.stdout in
    raise
      (Error
         (Printf.sprintf "--after-build command failed with %s%s"
            (Process.status_string result.status)
            (if output = "" then "" else ":\n" ^ output))));
  if result.stdout <> "" then print_string result.stdout;
  if result.stderr <> "" then prerr_string result.stderr

let project_root folder =
  if not (Sys.file_exists folder) then
    raise
      (Error
         ("Could not start Rescript build: Could not write lockfile because the specified project folder does not exist: "
         ^ folder));
  Unix.realpath folder

let clean ~seen ~verbosity ~folder ~prod =
  let root = project_root folder in
  let show_plain_progress =
    verbosity >= 0
    && not (Unix.isatty Unix.stdout && Unix.isatty Unix.stderr)
  in
  let on_clean name =
    if show_plain_progress then Printf.printf "Cleaning %s\n%!" name
  in
  let release_build_lock =
    Build_lock.acquire_build (Project_context.workspace_lock_root root)
  in
  Fun.protect ~finally:release_build_lock (fun () ->
    let root_config = Config.load_root root in
    let visited = Hashtbl.create 32 in
    List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
    Clean.run ~root_config ~seen:visited ~root ~prod ~is_local:true ~on_clean)

let compiler_args = Compiler_args_command.run

let dependency_head dependency =
  match String.split_on_char '.' dependency with
  | head :: _ -> head
  | [] -> dependency

let prepare_global_graph ~(root_config : Config.t) ~prod ~features ~warn_error
    ~filter ~watch ~stats ~on_cleanup =
  let bsc = bsc_path () in
  let graph_packages =
    Package_graph.discover ~root_config ~prod ~features ~warn_error ~filter ~stats
  in
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
             ~is_local:
               (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                  package.graph_root)
             package.graph_compile_config package.graph_modules);
        Compiler_info.clean_package package.graph_config;
        stats.compiler_cleaned <- true);
      ensure_dir package.graph_build_dir;
      ensure_dir package.graph_ocaml_dir)
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
          ~is_local:
            (Project_context.is_local_dependency_canonical ~workspace:root_config.root
               package.graph_root)
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
              module_.Source.implementation
              :: Option.to_list module_.Source.interface)
         |> List.filter_map (fun path ->
              if Build_freshness.source_is_not_older_than_ast compile_assets
                   ~root:package.graph_root
                   ~source_mtimes:package.graph_source_mtimes path
              then
                Some (package, path)
              else None))
  in
  let parse_results =
    parse_entries
    |> List.map (fun (package, path) ->
         Compiler_process.parse_job ~bsc ~build_dir:package.graph_build_dir
           ~config:package.graph_compile_config path)
    |> Process.run_parallel ~poll:stats.poll
  in
  let failed_parse_paths = Hashtbl.create 8 in
  List.iter2
    (fun (package, path) result ->
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
  let resolve_dependency node dependency =
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
      dependency_node.package_name = node.package_name
      || List.mem dependency_node.package_name node.allowed_dependencies
    in
    match Hashtbl.find_opt by_key local_key with
    | Some dependency_node
      when dependency_node.package_name = node.package_name ->
      [local_key]
    | _ ->
      (match Hashtbl.find_opt by_key raw_name with
      | Some dependency_node when is_visible dependency_node ->
        [raw_name]
      | _ ->
        let explicit_namespaced_module =
          match String.split_on_char '.' dependency with
        | namespace :: module_name :: _ ->
          [module_name ^ "-" ^ namespace; module_name ^ "-@" ^ namespace]
          |> List.find_opt (fun key ->
               match Hashtbl.find_opt by_key key with
               | Some dependency_node
                 when dependency_node.namespace = Some namespace
                      && is_visible dependency_node ->
                 true
               | Some _ | None -> false)
        | _ -> None
        in
        match explicit_namespaced_module with
        | Some key -> [key]
        | None ->
          nodes
          |> List.filter_map (fun dependency_node ->
               if
                 dependency_node.namespace = Some raw_name
                 && is_visible dependency_node
               then Some dependency_node.key
               else None))
  in
  let graph_nodes =
    List.map
      (fun node ->
        ( node,
          node.raw_dependencies
          |> List.concat_map (resolve_dependency node)
          |> List.filter (fun dependency -> dependency <> node.key)
          |> List.sort_uniq String.compare ))
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
  let cycle =
    try
      ignore
        (Graph.topological_sort graph_nodes
           ~name:(fun (node, _) -> node.key)
           ~deps:snd);
      None
    with Graph.Cycle cycle ->
      let blocked =
        Graph.blocked_dependents
          (List.map
             (fun (node, dependencies) -> (node.key, dependencies))
             graph_nodes)
          cycle
      in
      Some (cycle, blocked, by_key)
  in
  stats.parse_seconds <- Unix.gettimeofday () -. parse_started;
  cycle

let rec run_internal ~(root_config : Config.t) ~seen ~folder:root ~prod ~features
    ~warn_error ~watch ~filter ~is_local ~stats =
  let features =
    match Hashtbl.find_opt stats.active_features root with
    | Some features -> features
    | None -> features
  in
  Hashtbl.replace seen root ();
  let prepared = Hashtbl.find_opt stats.graph_packages root in
  let config =
    match prepared with
    | Some package -> package.graph_config
    | None ->
      let config = Config.load_root root in
      (match warn_error with
      | None -> config
      | Some value -> {config with warning_flags = ["-warn-error"; value]})
  in
  stats.diagnostics <-
    List.rev_append
      (Package_diagnostics.for_package ~is_local config)
      stats.diagnostics;
  let dependency_directories =
    let candidates =
      match prepared with
      | Some package -> package.graph_dependency_directories
      | None ->
        let dependencies : Config.dependency list =
          config.dependencies
          @ if prod || not is_local then [] else config.dev_dependencies
        in
        dependencies
        |> List.map (fun (dependency : Config.dependency) ->
             match Project_context.dependency_path root dependency.name with
             | Some directory -> (dependency, directory)
             | None ->
               raise
                 (Package_error
                    (Printf.sprintf
                       "Could not build package tree reading dependency '%s' at path '%s'. Error: Could not resolve dependency %s"
                       dependency.name root_config.root dependency.name)))
    in
    candidates
    |> List.filter_map (fun ((dependency : Config.dependency), candidate) ->
      let () = match candidate with
        | candidate when Hashtbl.mem seen candidate -> ()
        | candidate when Config.exists_in_root candidate ->
          (try
             run_internal ~root_config ~seen ~folder:candidate ~prod
               ~features:dependency.features ~warn_error:None ~watch
               ~filter:None
               ~is_local:
                 (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                    candidate)
               ~stats
           with Build_failure output ->
             if Option.is_none stats.failure then stats.failure <- Some output)
        | _ -> ()
      in
      let ocaml = lib_path candidate "ocaml" in
      if Sys.file_exists ocaml then Some (dependency, ocaml) else None)
  in
  let dependency_dirs = List.map snd dependency_directories in
  let regular_dependency_names =
    config.dependencies
    |> List.map (fun (dependency : Config.dependency) -> dependency.name)
  in
  let dependency_dirs_for (module_ : Source.module_) =
    if module_.is_dev then dependency_dirs
    else
      dependency_directories
      |> List.filter_map (fun ((dependency : Config.dependency), directory) ->
           if List.mem dependency.name regular_dependency_names then
             Some directory
           else None)
  in
  let bsc, runtime =
    match stats.compiler_context with
    | Some context -> (context.bsc_path, context.runtime_path)
    | None -> raise (Error "Compiler context was not initialized")
  in
  let build_state =
    match stats.build_state with
    | Some state -> state
    | None -> raise (Error "build state was not initialized")
  in
  let compile_assets =
    match stats.compile_assets with
    | Some state -> state
    | None -> raise (Error "compile asset state was not initialized")
  in
  let build_dir =
    match prepared with
    | Some package -> package.graph_build_dir
    | None -> lib_path root "bs"
  in
  let ocaml_dir =
    match prepared with
    | Some package -> package.graph_ocaml_dir
    | None -> lib_path root "ocaml"
  in
  ensure_dir build_dir;
  ensure_dir ocaml_dir;
  Compiler_log.initialize root;
  Hashtbl.replace stats.initialized_logs root ();
  let modules =
    match prepared with
    | Some package -> package.graph_modules
    | None ->
      Source.discover config
        ~prod:(Package_graph.source_discovery_prod ~prod ~is_local)
        ~features ~filter
        ~display_root:root_config.root
        ~on_missing:(Package_diagnostics.report_missing_source_folder config)
        ~on_orphan:(fun path ->
          Printf.eprintf
            "\027[2K\r No implementation file found for interface file (skipping): %s\n%!"
            path)
  in
  let config =
    match prepared with
    | Some package -> package.graph_compile_config
    | None ->
      with_root_options config root_config
      |> Compiler_args.with_local_warning_policy ~is_local
  in
  let cleanup =
    match Hashtbl.find_opt stats.cleanup_results root with
    | Some result -> result
    | None ->
      Build_artifacts.cleanup_stale ~root ~ocaml_dir ~is_local config modules
  in
  let removed_modules = cleanup.removed_modules in
  if not (Hashtbl.mem stats.cleanup_results root) then
    stats.deferred_artifact_cleanup :=
      cleanup.deferred_artifacts @ !(stats.deferred_artifact_cleanup);
  List.iter
    (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
    removed_modules;
  let names = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ -> Hashtbl.replace names module_.Source.name module_)
    modules;
  let parse_paths =
    List.concat_map (fun module_ ->
      module_.Source.implementation :: Option.to_list module_.interface) modules
  in
  let dirty_parse_paths =
    parse_paths
    |> List.filter (fun path ->
         List.mem (Source.module_name path) removed_modules
         || Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)
         ||
         match prepared, stats.compile_assets with
         | Some package, Some compile_assets ->
           Build_freshness.source_is_not_older_than_ast compile_assets ~root
             ~source_mtimes:package.graph_source_mtimes path
         | None, _ | _, None ->
           Build_freshness.source_is_newer ~source:(Filename.concat root path)
             ~artifact:(Build_freshness.published_ast_path ~ocaml_dir path))
  in
  let parse_paths_to_run =
    dirty_parse_paths
    |> List.filter (fun path ->
         not
           (Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)))
  in
  let parsed =
    List.map2 (fun path result -> (path, Some result)) parse_paths_to_run
      (Process.run_parallel ~poll:stats.poll
         (List.map
            (Compiler_process.parse_job ~bsc ~build_dir ~config)
            parse_paths_to_run))
    @ (dirty_parse_paths
      |> List.filter (fun path ->
           Hashtbl.mem stats.forced_parse_paths (Filename.concat root path))
      |> List.map (fun path ->
           ( path,
             Hashtbl.find_opt stats.preparse_results
               (Filename.concat root path) )))
  in
  let warning_asts = ref [] in
  List.iter (fun (path, result) ->
    let absolute_path = Filename.concat root path in
    let stderr =
      match result with
      | Some result -> result.Process.stderr
      | None ->
        Hashtbl.find_opt stats.preparse_stderr absolute_path
        |> Option.value ~default:""
    in
    Option.iter
      (fun result ->
        if not (Process.succeeded result) then
          let output =
            Printf.sprintf "Error in %s:\n%s%s" config.name result.stderr
              result.stdout
          in
          Compiler_log.append root output;
          raise (Parse_failure output))
      result;
    let stderr =
      if is_local then stderr
      else Compiler_process.retain_critical_external_warnings stderr
    in
    if stderr <> "" then stats.had_warnings <- true;
    if stderr <> "" then Compiler_log.append root stderr;
    if stderr <> "" then prerr_string stderr;
    let ast = Source.ast_path path in
    if is_local && stderr <> "" then warning_asts := ast :: !warning_asts;
    copy_existing_file ~ensure_parent:false (Filename.concat build_dir ast)
      (Filename.concat (lib_path config.root "ocaml") (Filename.basename ast));
    copy_existing_file ~ensure_parent:false (Filename.concat config.root path)
      (Filename.concat (lib_path config.root "ocaml") (Filename.basename path))) parsed;
  let raw_dependencies = Hashtbl.create (List.length modules) in
  let parse_dirty_modules = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ ->
      let global_key = Source.compiler_basename config module_.Source.name in
      let dependencies =
        match Hashtbl.find_opt stats.global_raw_dependencies global_key with
        | Some dependencies -> dependencies
        | None ->
          let impl_ast = Source.ast_path module_.Source.implementation in
          let impl_deps = Compiler_process.ast_dependencies ~build_dir impl_ast in
          let intf_deps =
            match module_.interface with
            | None -> []
            | Some path ->
              Compiler_process.ast_dependencies ~build_dir
                (Source.ast_path path)
          in
          List.sort_uniq String.compare (impl_deps @ intf_deps)
      in
      Hashtbl.replace raw_dependencies module_.Source.name dependencies;
      let paths =
        module_.Source.implementation :: Option.to_list module_.Source.interface
      in
      if List.exists (fun path -> List.mem path dirty_parse_paths) paths then
        Hashtbl.replace parse_dirty_modules module_.Source.name ();
      module_.deps <-
        if Hashtbl.mem stats.blocked_modules global_key then []
        else
          List.filter
            (fun dep -> dep <> module_.name && Hashtbl.mem names dep)
            dependencies)
    modules;
  stats.parsed <- stats.parsed + Hashtbl.length parse_dirty_modules;
  let compile_warning_modules = Hashtbl.create 8 in
  let module_is_dirty module_ state =
    let global_key = Source.compiler_basename config module_.Source.name in
    let module_name = Source.module_name module_.Source.implementation in
    let source = Filename.concat root module_.Source.implementation in
    let outputs_exist =
      match Hashtbl.find_opt stats.cleanup_results root with
      | Some cleanup ->
        List.for_all
          (fun spec ->
            Hashtbl.mem cleanup.present_public_outputs
              (generated_js_path config module_.Source.implementation spec))
          config.package_specs
      | None ->
        List.for_all
          (fun spec ->
            Sys.file_exists
              (generated_js_path config module_.Source.implementation spec))
          config.package_specs
    in
    let raw_dependencies =
      Hashtbl.find_opt raw_dependencies module_.Source.name
      |> Option.value ~default:[]
    in
    let dependency_is_newer dependency =
      let dependency_state = Build_state.find_exn build_state dependency in
      Build_state.dependency_compiled_after state dependency_state
    in
    not (Hashtbl.mem stats.blocked_modules global_key)
    &&
    (Hashtbl.mem parse_dirty_modules module_.Source.name
    || List.mem module_name removed_modules
    || (match Compile_assets.ast compile_assets source, state.last_compiled_cmt with
       | Some ast, Some cmt_time -> ast.modified >= cmt_time
       | Some _, None -> true
       | None, _ -> false)
    || not (Build_state.has_complete_compile_assets state)
    || not outputs_exist
    || List.exists (fun dependency -> List.mem dependency removed_modules)
         raw_dependencies
    || List.exists
         (fun dependency -> Hashtbl.mem stats.removed_modules dependency)
         raw_dependencies
    || List.exists dependency_is_newer state.dependencies)
  in
  let prepare_outputs module_ =
    let path = module_.Source.implementation in
    List.iter
      (fun spec ->
        let output = generated_js_path config path spec in
        let dirty_ast = Filename.concat build_dir (Source.ast_path path) in
        ensure_dir (Filename.dirname output);
        if watch then (
          prepare_watch_output stats.watch_outputs stats.watch_output_paths
            ~dirty_ast output;
          prepare_watch_output stats.watch_outputs stats.watch_output_paths
            ~dirty_ast (output ^ ".map")))
      config.package_specs
  in
  let compile_process module_ ~is_interface path =
    Compiler_process.compile_job ~bsc ~runtime ~build_dir ~watch ~config
      ~dependency_dirs:(dependency_dirs_for module_)
      module_ ~is_interface path
  in
  let publish ~is_interface path result =
    Compiler_process.publish ~build_dir ~ocaml_dir ~watch
      ~watch_output_paths:stats.watch_output_paths ~is_local ~config
      ~is_interface path result
  in
  let scheduled =
    List.map
      (fun module_ ->
        let key = Source.compiler_basename config module_.Source.name in
        let state = Build_state.find_exn build_state key in
        (* Rust fixes the initial dirty set before dispatch. Files published by
           concurrently finishing jobs must not change this module's decision;
           only explicit CMI-change propagation may do that. *)
        state.compile_dirty <- module_is_dirty module_ state;
        let dependencies =
          if Hashtbl.mem stats.blocked_modules key then []
          else state.dependencies
        in
        let cmi_path =
          Filename.concat ocaml_dir
            (Source.compiler_asset_basename config module_.Source.implementation
            ^ ".cmi")
        in
        Compiler_scheduler.create ~key ~dependencies ~source:module_ ~state
          ~cmi_path ~prepare:(fun () -> prepare_outputs module_)
          ~compile:(fun ~is_interface path ->
            compile_process module_ ~is_interface path)
          ~publish:(fun ~is_interface path result ->
            publish ~is_interface path result)
          ~package_root:config.root ~is_local
          ~mark_warning:(fun path ->
            Hashtbl.replace compile_warning_modules (Source.module_name path) ()))
      modules
  in
  Option.iter
    (fun namespace ->
      let namespace =
        match config.namespace_entry with
        | Some _ -> "@" ^ namespace
        | None -> namespace
      in
      let package_dirty =
        List.exists
          Compiler_scheduler.requires_compile
          scheduled
      in
      Option.iter
        (fun job -> stats.namespace_jobs := job :: !(stats.namespace_jobs))
        (Compiler_process.namespace_job ~bsc ~runtime ~build_dir ~ocaml_dir
           ~entry:config.namespace_entry ~package_dirty namespace modules))
    config.namespace;
  stats.scheduled_modules := scheduled @ !(stats.scheduled_modules);
  stats.compile_cleanup :=
    (fun () ->
      (* The published AST is the freshness marker. Keep bsc's working AST in
         lib/bs, as Rust does, and remove only the published copy so warnings
         are replayed without deleting a usable intermediate artifact. *)
      if not watch then
        Hashtbl.iter
          (fun module_name () ->
            match
              List.find_opt
                (fun module_ -> module_.Source.name = module_name)
                modules
            with
            | None -> ()
            | Some module_ ->
              let paths =
                module_.Source.implementation
                :: Option.to_list module_.Source.interface
              in
              List.iter
                (fun path ->
                  let ast = Source.ast_path path in
                  remove_file
                    (Filename.concat ocaml_dir (Filename.basename ast)))
                paths)
          compile_warning_modules;
      List.iter
        (fun ast ->
          remove_file (Filename.concat ocaml_dir (Filename.basename ast)))
        !warning_asts)
    :: !(stats.compile_cleanup);
  ()

let run_scheduled_modules stats =
  let build_state =
    match stats.build_state with
    | Some state -> state
    | None -> raise (Error "build state was not initialized")
  in
  let compile_assets =
    match stats.compile_assets with
    | Some state -> state
    | None -> raise (Error "compile asset state was not initialized")
  in
  Compiler_scheduler.run ~poll:stats.poll ~warning_state:stats.warning_state
    ~blocked_modules:stats.blocked_modules ~compile_assets ~build_state
    ~scheduled_modules:!(stats.scheduled_modules)
    ~compile_cleanup:!(stats.compile_cleanup)
    ~mark_compiled:(fun () -> stats.compiled <- stats.compiled + 1)
    ~mark_had_warnings:(fun () -> stats.had_warnings <- true)

let run_namespace_jobs stats =
  let jobs = List.rev !(stats.namespace_jobs) in
  let results = Process.run_parallel ~poll:stats.poll (List.map fst jobs) in
  List.iter2 (fun (_, finish) result -> finish result) jobs results

let write_source_dirs (root_config : Config.t) stats =
  let packages =
    Hashtbl.to_seq_values stats.graph_packages |> List.of_seq
    |> List.sort (fun left right -> String.compare left.graph_root right.graph_root)
  in
  packages
  |> List.iter (fun package ->
       if package.graph_root <> root_config.root then
         remove_file
           (path_of_parts package.graph_root ["lib"; "bs"; ".sourcedirs.json"]));
  let local_packages = List.filter (fun package -> package.graph_is_local) packages in
  let source_directories package =
    package.graph_modules
    |> List.map (fun module_ -> Filename.dirname module_.Source.implementation)
    |> List.sort_uniq String.compare
  in
  let relative_package_root package =
    if package.graph_root = root_config.root then ""
    else Project_context.relative_to root_config.root package.graph_root
  in
  let dirs =
    local_packages
    |> List.concat_map (fun package ->
         let relative_root = relative_package_root package in
         source_directories package
         |> List.map (fun directory ->
              if relative_root = "" then directory
              else Filename.concat relative_root directory))
    |> List.sort_uniq String.compare
  in
  let package_roots = Hashtbl.create 16 in
  local_packages
  |> List.iter (fun package ->
       package.graph_dependency_directories
       |> List.iter (fun ((dependency : Config.dependency), path) ->
            Hashtbl.replace package_roots dependency.name path));
  let package_roots =
    Hashtbl.to_seq package_roots |> List.of_seq
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  let scans =
    local_packages
    |> List.map (fun package ->
         let relative_root = relative_package_root package in
         let build_root =
           if relative_root = "" then path_of_parts "" ["lib"; "bs"]
           else path_of_parts relative_root ["lib"; "bs"]
         in
         Source_dirs.
           {
             build_root;
             scan_dirs = source_directories package;
             also_scan_build_root = true;
           })
    |> List.sort (fun (left : Source_dirs.scan) right ->
         String.compare left.build_root right.build_root)
  in
  Source_dirs.write ~root:root_config.root ~dirs ~packages:package_roots ~scans

let write_build_ninja stats =
  Hashtbl.iter
    (fun _ package ->
      let path = Filename.concat package.graph_build_dir "build.ninja" in
      let channel = open_out_bin path in
      close_out channel)
    stats.graph_packages

let run_with_warning_state ~poll ~warning_state ~compilation_kind ~no_timing
    ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch ~after_build
    ~filter =
  let started_at = Unix.gettimeofday () in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let is_rebuild = compilation_kind = Some "incremental" in
  let should_write_build_ninja = (not watch) || is_rebuild in
  let root = project_root folder in
  let root_config = Config.load_root root in
  if verbosity > 0 then
    Printf.printf "Created project context for %S\n%!" root_config.root;
  let visited = Hashtbl.create 32 in
  let stats = Build_types.create ~warning_state ~poll in
  List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
  let finalize_logs () =
    Hashtbl.iter (fun package_root () -> Compiler_log.finalize package_root)
      stats.initialized_logs;
    Hashtbl.clear stats.initialized_logs
  in
  let outputs_finished = ref false in
  let build_ninja_written = ref false in
  let write_build_ninja_once () =
    if should_write_build_ninja && not !build_ninja_written then (
      write_build_ninja stats;
      build_ninja_written := true)
  in
  let expose_watch_outputs () =
    !(stats.watch_outputs)
    |> List.rev
    |> List.iter (fun (output, pending, _) ->
         if Sys.file_exists pending then (
           remove_file output;
           Unix.rename pending output))
  in
  let finish_watch_outputs ~success =
    !(stats.watch_outputs)
    |> List.rev
    |> List.iter (fun (output, pending, dirty_ast) ->
         if success then (
           if Sys.file_exists pending then (
             remove_file output;
             Unix.rename pending output))
         else (
           remove_file output;
           remove_file pending;
           remove_file dirty_ast));
    stats.watch_outputs := [];
    Hashtbl.clear stats.watch_output_paths;
    outputs_finished := true
  in
  let report ~success () =
    finish_watch_outputs ~success;
    finalize_logs ();
    if not interactive then
      if watch then (
        if success then Printf.printf "Finished compilation\n%!")
      else (
        Printf.printf "Cleaned %d/%d\nParsed %d source files\n%!" stats.cleaned
          stats.previous_asts stats.parsed;
        if success then Printf.printf "Compiled %d modules\n%!" stats.compiled
        else Printf.eprintf "Compiled %d modules\n%!" stats.compiled);
    let diagnostics =
      stats.diagnostics |> List.rev |> List.sort_uniq String.compare
    in
    let warning_entries = Warning_state.entries stats.warning_state in
    warning_entries
    |> List.iter (fun entry -> prerr_string entry.Warning_state.output);
    if warning_entries <> [] && diagnostics = [] then prerr_newline ();
    flush stderr;
    if diagnostics <> [] then
      prerr_endline (String.concat "\n\n" diagnostics);
    if success && interactive then
      let seconds =
        if no_timing then 0. else Unix.gettimeofday () -. started_at
      in
      Printf.printf "\n%s\n%!"
        (Output.finished_compilation_message ~kind:compilation_kind
           ~warnings:
             (stats.had_warnings || diagnostics <> []
             || Warning_state.entries stats.warning_state <> [])
           ~seconds)
  in
  let report_failure output =
    write_build_ninja_once ();
    report ~success:false ();
    prerr_string output;
    prerr_newline ();
    raise
      (Error
        ("Incremental build failed. Error: \027[2K\r  Failed to Compile. "
        ^ "See Errors Above"))
  in
  let report_parse_failure output =
    write_build_ninja_once ();
    finish_watch_outputs ~success:false;
    finalize_logs ();
    if interactive then
      prerr_endline
        (Output.parsing_failed_message ~step:(if is_rebuild then "1/2" else "2/3")
           ~seconds:(if no_timing then 0. else stats.parse_seconds))
    else Printf.printf "Cleaned %d/%d\n%!" stats.cleaned stats.previous_asts;
    prerr_endline output;
    raise
      (Error
         "Incremental build failed. Error: \027[2K\r  Could not parse Source Files")
  in
  let format_cycle cycle by_key =
    let format_node name =
      match Hashtbl.find_opt by_key name with
      | None -> name
      | Some node ->
        let absolute = Filename.concat node.package_root node.source_path in
        let module_name = Source.module_name node.source_path in
        let display_name =
          match node.namespace, node.namespace_entry with
          | Some namespace, Some entry when entry <> module_name ->
            namespace ^ "." ^ module_name
          | Some namespace, None -> namespace ^ "." ^ module_name
          | _ -> module_name
        in
        Printf.sprintf "%s (%s)" display_name
          (Project_context.relative_to root_config.root absolute)
    in
    "\nCan't continue... Found a circular dependency in your code:\n"
    ^ (cycle |> List.map format_node |> String.concat "\n → ")
    ^ "\nPossible solutions:\n- Extract shared code into a new module both depend on.\n"
  in
  let release_build_lock =
    Build_lock.acquire_build (Project_context.workspace_lock_root root)
  in
  let phase_seconds seconds = if no_timing then 0. else seconds in
  let parse_step = if is_rebuild then "1/2" else "2/3" in
  let compile_step = if is_rebuild then "2/2" else "3/3" in
  let execute () =
    poll ();
    let cycle =
      prepare_global_graph ~root_config ~prod ~features ~warn_error ~filter
        ~watch ~stats
        ~on_cleanup:(fun seconds ->
          if interactive && not is_rebuild then (
            if stats.compiler_cleaned then
              print_endline (Output.compiler_cleanup_message ~step:"1/3");
            print_endline
              (Output.cleanup_message ~step:"1/3" ~cleaned:stats.cleaned
                 ~total:stats.previous_asts ~seconds:(phase_seconds seconds))))
    in
    poll ();
    if stats.compiler_cleaned && not interactive then
      print_endline "Cleaned previous build due to compiler update";
    Option.iter
      (fun (_, blocked, _) ->
        List.iter
          (fun name -> Hashtbl.replace stats.blocked_modules name ())
          blocked)
      cycle;
    run_internal ~root_config ~seen:visited ~folder:root ~prod ~features
      ~warn_error ~watch ~filter ~is_local:true ~stats;
    poll ();
    if interactive then
      print_endline
        (Output.parsing_message ~step:parse_step ~count:stats.parsed
           ~seconds:(phase_seconds stats.parse_seconds));
    let compile_started = Unix.gettimeofday () in
    (try
       run_namespace_jobs stats;
       run_scheduled_modules stats
     with Build_failure output ->
       if Option.is_none stats.failure then stats.failure <- Some output);
    if interactive then (
      let seconds = phase_seconds (Unix.gettimeofday () -. compile_started) in
      match stats.failure with
      | None ->
        print_endline
          (Output.compiling_message ~step:compile_step ~count:stats.compiled
             ~seconds)
      | Some _ ->
        prerr_endline
          (Output.compilation_failed_message ~step:compile_step
             ~count:stats.compiled ~seconds));
    (match stats.failure, cycle with
    | Some output, _ -> report_failure output
    | None, Some (names, _, by_key) ->
      let output = format_cycle names by_key in
      names
      |> List.filter_map (Hashtbl.find_opt by_key)
      |> List.map (fun node -> node.package_root)
      |> List.sort_uniq String.compare
      |> List.iter (fun package_root -> Compiler_log.append package_root output);
      report_failure output
    | None, None ->
      Option.iter
        (fun (context : Compiler_info.context) ->
          Hashtbl.iter
            (fun _ package ->
              let package_context =
                {
                  context with
                  build_root = package.graph_build_owner;
                  package_output_specs =
                    Compiler_info.package_output_specs
                      package.graph_compile_config;
                }
              in
              Compiler_info.write_package package_context package.graph_config)
            stats.graph_packages)
        stats.compiler_context;
      write_source_dirs root_config stats;
      write_build_ninja_once ();
      Option.iter
        (fun command ->
          expose_watch_outputs ();
          finish_watch_outputs ~success:true;
          finalize_logs ();
          release_build_lock ();
          run_after_build ~root command)
        after_build;
      report ~success:true ())
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter remove_file !(stats.deferred_artifact_cleanup);
      if not !outputs_finished then finish_watch_outputs ~success:false;
      finalize_logs ();
      release_build_lock ())
    (fun () ->
      try execute () with
      | Build_failure output -> report_failure output
      | Parse_failure output -> report_parse_failure output)

let run ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch ~after_build
    ~filter ~no_timing =
  run_with_warning_state ~warning_state:(Warning_state.create ())
    ~poll:(fun () -> ()) ~compilation_kind:None ~no_timing ~seen ~verbosity
    ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter

let watch ~verbosity ~folder ~prod ~features ~warn_error ~after_build ~filter
    ~clear_screen =
  let root = project_root folder in
  ignore (Config.load_root root);
  let warning_state = Warning_state.create () in
  let initial_build = ref true in
  let build ~poll =
    let compilation_kind =
      if !initial_build then Some "initial" else Some "incremental"
    in
    try
      run_with_warning_state ~poll ~warning_state ~compilation_kind
        ~no_timing:false ~seen:[] ~verbosity ~folder ~prod ~features ~warn_error
        ~watch:true ~after_build ~filter;
      initial_build := false
    with
    | Error message | Config.Error message | Source.Error message
    | Process.Error message -> prerr_endline message
    | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
      prerr_endline (Printexc.to_string exn)
  in
  Watcher.run ~root ~prod ~clear_screen ~build
