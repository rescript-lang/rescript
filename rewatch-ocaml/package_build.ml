exception Error = Project_context.Error
exception Package_error = Project_context.Package_error
exception Build_failure = Compiler_scheduler.Build_failure
exception Parse_failure of string

open Build_types

let rec prepare_tree ~(root_config : Config.t) ~dependency_context ~seen
    ~folder:root ~prod ~features ~warn_error ~watch ~filter ~is_local ~stats =
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
             match
               Project_context.dependency_path_in dependency_context root
                 dependency.name
             with
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
             prepare_tree ~root_config ~dependency_context ~seen
               ~folder:candidate ~prod ~features:dependency.features
               ~warn_error:None ~watch
               ~filter:None
               ~is_local:
                 (Project_context.dependency_is_local_canonical
                    dependency_context candidate)
               ~stats
           with Build_failure output ->
             if Option.is_none stats.failure then stats.failure <- Some output)
        | _ -> ()
      in
      let ocaml = Build_artifacts.lib_path candidate "ocaml" in
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
    | None -> Build_artifacts.lib_path root "bs"
  in
  let ocaml_dir =
    match prepared with
    | Some package -> package.graph_ocaml_dir
    | None -> Build_artifacts.lib_path root "ocaml"
  in
  File_util.ensure_dir build_dir;
  File_util.ensure_dir ocaml_dir;
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
      Build_artifacts.with_root_options config root_config
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
      (Process.run_parallel ?poll:stats.process_poll
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
    if is_local && stderr <> "" then
      warning_asts := (absolute_path, ast) :: !warning_asts;
    let published_ast =
      Filename.concat
        (Build_artifacts.lib_path config.root "ocaml")
        (Filename.basename ast)
    in
    File_util.copy_existing_file ~ensure_parent:false
      (Filename.concat build_dir ast) published_ast;
    Compile_assets.refresh_ast compile_assets ~source:absolute_path
      ~path:published_ast;
    File_util.copy_existing_file ~ensure_parent:false (Filename.concat config.root path)
      (Filename.concat
         (Build_artifacts.lib_path config.root "ocaml")
         (Filename.basename path)))
    parsed;
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
              (Build_artifacts.generated_js_path config
                 module_.Source.implementation spec))
          config.package_specs
      | None ->
        List.for_all
          (fun spec ->
            Sys.file_exists
              (Build_artifacts.generated_js_path config
                 module_.Source.implementation spec))
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
        let output = Build_artifacts.generated_js_path config path spec in
        let dirty_ast = Filename.concat build_dir (Source.ast_path path) in
        File_util.ensure_dir (Filename.dirname output);
        if watch then (
          Build_artifacts.prepare_watch_output stats.watch_outputs
            stats.watch_output_paths
            ~dirty_ast output;
          Build_artifacts.prepare_watch_output stats.watch_outputs
            stats.watch_output_paths
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
        (* Fix the initial dirty set before dispatch so files published by
           concurrently finishing jobs cannot change this module's decision.
           Only explicit CMI-change propagation may do that. *)
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
         lib/bs and remove only the published copy so warnings are replayed
         without deleting a usable intermediate artifact. *)
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
                  File_util.remove_file
                    (Filename.concat ocaml_dir (Filename.basename ast)))
                paths)
          compile_warning_modules;
      List.iter
        (fun (source, ast) ->
          let path = Filename.concat ocaml_dir (Filename.basename ast) in
          File_util.remove_file path;
          Compile_assets.refresh_ast compile_assets ~source ~path)
        !warning_asts)
    :: !(stats.compile_cleanup);
  ()
