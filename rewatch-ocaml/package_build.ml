exception Error = Project_context.Error
exception Build_failure = Compiler_scheduler.Build_failure
exception Parse_failure of string

let rec prepare_tree ~seen ~(package : Build_types.graph_package) ~watch
    ~(stats : Build_types.t) =
  let root = package.graph_root in
  Hashtbl.replace seen root ();
  let is_local = package.graph_is_local in
  let config = package.graph_config in
  stats.diagnostics <-
    List.rev_append
      (Package_diagnostics.for_package ~is_local config)
      stats.diagnostics;
  let dependency_directories =
    package.graph_dependency_directories
    |> List.filter_map (fun ((dependency : Config.dependency), candidate) ->
      let () = match candidate with
        | candidate when Hashtbl.mem seen candidate -> ()
        | candidate when Hashtbl.mem stats.retained.graph_packages candidate ->
          (try
             prepare_tree ~seen
               ~package:
                 (Hashtbl.find stats.retained.graph_packages candidate)
               ~watch ~stats
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
  let regular_dependency_dirs =
    dependency_directories
    |> List.filter_map (fun ((dependency : Config.dependency), directory) ->
         if List.mem dependency.name regular_dependency_names then
           Some directory
         else None)
  in
  let prepared = Build_types.prepared_exn stats in
  let bsc = prepared.compiler_context.bsc_path in
  let runtime = prepared.compiler_context.runtime_path in
  let build_state = prepared.build_state in
  let compile_assets = prepared.compile_assets in
  let build_dir = package.graph_build_dir in
  let ocaml_dir = package.graph_ocaml_dir in
  File_util.ensure_dir build_dir;
  File_util.ensure_dir ocaml_dir;
  Compiler_log.initialize root;
  Hashtbl.replace stats.initialized_logs root ();
  let modules = package.graph_modules in
  let config = package.graph_compile_config in
  let common_args dependency_dirs =
    Compiler_args.compiler_common_arguments ~config ~runtime ~dependency_dirs
      ~watch ~gentype_dependency_args:package.graph_gentype_dependency_args
  in
  let regular_common_args = common_args regular_dependency_dirs in
  let dev_common_args = common_args dependency_dirs in
  let cleanup =
    match Hashtbl.find_opt stats.retained.cleanup_results root with
    | Some result -> result
    | None -> raise (Error ("Package cleanup was not prepared for " ^ root))
  in
  let removed_modules = cleanup.removed_modules in
  let removed_module_names = Hashtbl.create (List.length removed_modules) in
  List.iter
    (fun module_name ->
      Hashtbl.replace removed_module_names module_name ();
      Hashtbl.replace stats.removed_modules module_name ())
    removed_modules;
  let parse_paths =
    List.concat_map (fun module_ ->
      module_.Source.implementation :: Option.to_list module_.interface) modules
  in
  let dirty_parse_paths =
    parse_paths
    |> List.filter (fun path ->
         let forced =
           Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)
         in
         match stats.attempt_kind with
         | Build_types.Retained_attempt -> forced
         | Build_types.Full_attempt ->
           Hashtbl.mem removed_module_names (Source.module_name path)
           || forced
           || Build_freshness.source_is_not_older_than_ast compile_assets ~root
                ~source_mtimes:package.graph_source_mtimes path)
  in
  let dirty_parse_path_set = Hashtbl.create (List.length dirty_parse_paths) in
  List.iter
    (fun path -> Hashtbl.replace dirty_parse_path_set path ())
    dirty_parse_paths;
  let parse_paths_to_run =
    dirty_parse_paths
    |> List.filter (fun path ->
         not
           (Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)))
  in
  let parsed =
    List.map2 (fun path result -> (path, Some result)) parse_paths_to_run
      (Process.run_parallel_map ?poll:stats.process_poll parse_paths_to_run
         ~job:(Compiler_process.parse_job ~bsc ~build_dir ~config))
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
    match result with
    | Some result when not (Process.succeeded result) ->
      let output =
        Printf.sprintf "Error in %s:\n%s%s" config.name result.stderr
          result.stdout
      in
      Compiler_log.append root output;
      stats.parse_messages :=
        Build_types.Parse_error output :: !(stats.parse_messages)
    | _ ->
      let stderr =
        match result with
        | Some result -> result.Process.stderr
        | None ->
          Hashtbl.find_opt stats.preparse_stderr absolute_path
          |> Option.value ~default:""
      in
      let stderr =
        if is_local then stderr
        else Compiler_process.retain_critical_external_warnings stderr
      in
      if stderr <> "" then (
        stats.had_warnings <- true;
        Compiler_log.append root stderr;
        stats.parse_messages :=
          Build_types.Parse_warning stderr :: !(stats.parse_messages));
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
      File_util.copy_existing_file ~ensure_parent:false
        (Filename.concat config.root path)
        (Filename.concat
           (Build_artifacts.lib_path config.root "ocaml")
           (Filename.basename path)))
    parsed;
  if !warning_asts <> [] then
    stats.compile_cleanup :=
      (fun () ->
        List.iter
          (fun (source, ast) ->
            let path = Filename.concat ocaml_dir (Filename.basename ast) in
            File_util.remove_file path;
            Compile_assets.refresh_ast compile_assets ~source ~path)
          !warning_asts)
      :: !(stats.compile_cleanup);
  let parse_dirty_modules = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ ->
      let paths =
        module_.Source.implementation :: Option.to_list module_.Source.interface
      in
      if List.exists (Hashtbl.mem dirty_parse_path_set) paths then (
        Hashtbl.replace parse_dirty_modules module_.Source.name ();
        let key = Source.compiler_basename config module_.Source.name in
        if not (Hashtbl.mem stats.blocked_modules key) then
          (Build_state.find_exn build_state key).compile_dirty <- true))
    modules;
  if
    List.exists
      (function
        | Build_types.Parse_error _ -> true
        | Build_types.Parse_warning _ -> false)
      !(stats.parse_messages)
  then ()
  else (
  stats.parsed <- stats.parsed + Hashtbl.length parse_dirty_modules;
  let compile_warning_modules = Hashtbl.create 8 in
  let module_is_dirty module_ (state : Build_state.module_) =
    let global_key = Source.compiler_basename config module_.Source.name in
    let module_name = Source.module_name module_.Source.implementation in
    let source_artifact_is_pending path =
      let source = Filename.concat root path in
      match Compile_assets.ast compile_assets source, state.last_compiled_cmt with
      | Some ast, Some cmt_time -> ast.modified >= cmt_time
      | Some _, None -> true
      | None, _ -> false
    in
    let outputs_exist =
      List.for_all
        (fun spec ->
          Hashtbl.mem cleanup.present_public_outputs
            (Build_artifacts.generated_js_path config
               module_.Source.implementation spec))
        config.package_specs
    in
    let raw_dependencies =
      match Hashtbl.find_opt stats.retained.global_modules global_key with
      | Some node -> node.raw_dependencies
      | None -> raise (Error ("Build module was not prepared for " ^ global_key))
    in
    let dependency_is_newer dependency =
      let dependency_state = Build_state.find_exn build_state dependency in
      Build_state.dependency_compiled_after state dependency_state
    in
    not (Hashtbl.mem stats.blocked_modules global_key)
    &&
    (Hashtbl.mem parse_dirty_modules module_.Source.name
    || Hashtbl.mem removed_module_names module_name
    || List.exists source_artifact_is_pending
         (module_.Source.implementation
         :: Option.to_list module_.Source.interface)
    || not (Build_state.has_complete_compile_assets state)
    || not outputs_exist
    || List.exists (Hashtbl.mem removed_module_names) raw_dependencies
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
        File_util.ensure_dir (Filename.dirname output))
      config.package_specs
  in
  let compile_process module_ ~is_interface path =
    Compiler_process.compile_job ~bsc ~build_dir ~config
      ~common_args:
        (if module_.Source.is_dev then dev_common_args else regular_common_args)
      module_ ~is_interface path
  in
  let record_published_outputs ~is_interface path =
    if not is_interface then
      List.iter
        (fun spec ->
          let output = Build_artifacts.generated_js_path config path spec in
          [output; output ^ ".map"]
          |> List.iter (fun path ->
               if Sys.file_exists path then
                 Hashtbl.replace cleanup.present_public_outputs path ()))
        config.package_specs;
  in
  let post_build path = Compiler_process.post_build_tasks config path in
  let scheduled =
    List.map
      (fun module_ ->
        let key = Source.compiler_basename config module_.Source.name in
        let state = Build_state.find_exn build_state key in
        (* Fix the initial dirty set before dispatch so files published by
           concurrently finishing jobs cannot change this module's decision.
           Only explicit CMI-change propagation may do that. *)
        state.compile_dirty <-
          (not (Hashtbl.mem stats.blocked_modules key))
          &&
          (state.compile_dirty
          ||
          match stats.attempt_kind with
          | Build_types.Retained_attempt ->
            Hashtbl.mem parse_dirty_modules module_.Source.name
          | Build_types.Full_attempt -> module_is_dirty module_ state);
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
            Compiler_process.publish ~build_dir ~ocaml_dir ~is_local ~config
              ~is_interface path result)
          ~record_published_outputs
          ~post_build
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
      if package_dirty || stats.attempt_kind = Build_types.Full_attempt then
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
          compile_warning_modules)
    :: !(stats.compile_cleanup);
  ())
