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
  package.graph_dependency_directories
  |> List.iter (fun (dependency : Build_types.graph_dependency) ->
      let candidate = dependency.directory in
      match candidate with
      | candidate when Hashtbl.mem seen candidate -> ()
      | candidate -> (
        match Hashtbl.find_opt stats.retained.graph_packages candidate with
        | None -> ()
        | Some package -> (
          try prepare_tree ~seen ~package ~watch ~stats
          with Build_failure output ->
            if Option.is_none stats.failure then stats.failure <- Some output)));
  let prepared = Build_types.prepared_exn stats in
  let prepared_package = Build_types.prepared_package_exn stats root in
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
  let regular_common_args = prepared_package.regular_common_args in
  let dev_common_args = prepared_package.development_common_args in
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
  let parse_paths = prepared_package.parse_paths in
  let dirty_parse_paths =
    parse_paths
    |> List.filter (fun path ->
        let forced =
          Hashtbl.mem stats.preliminary_parses (Filename.concat root path)
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
        not (Hashtbl.mem stats.preliminary_parses (Filename.concat root path)))
  in
  let parsed =
    List.map2
      (fun path result -> (path, Build_types.preliminary_parse result))
      parse_paths_to_run
      (Process.run_parallel_map ?poll:stats.process_poll parse_paths_to_run
         ~job:(Compiler_process.parse_job ~bsc ~build_dir ~config))
    @ (dirty_parse_paths
      |> List.filter (fun path ->
          Hashtbl.mem stats.preliminary_parses (Filename.concat root path))
      |> List.map (fun path ->
          ( path,
            Hashtbl.find stats.preliminary_parses (Filename.concat root path) ))
      )
  in
  let warning_asts = ref [] in
  List.iter
    (fun (path, result) ->
      let absolute_path = Filename.concat root path in
      let pending_path = Platform.normalize_path_for_comparison absolute_path in
      let publish_successful_parse stderr =
        let stderr =
          if is_local then stderr
          else Compiler_process.retain_critical_external_warnings stderr
        in
        if stderr <> "" then (
          stats.had_warnings <- true;
          Compiler_log.append root stderr;
          stats.parse_messages <-
            Build_types.Parse_warning stderr :: stats.parse_messages);
        let ast = Source.ast_path path in
        if is_local && stderr <> "" then
          warning_asts := (absolute_path, ast) :: !warning_asts;
        let published_ast =
          Build_artifacts.published_ast_path ~ocaml_dir path
        in
        File_util.copy_existing_file ~ensure_parent:false
          (Filename.concat build_dir ast)
          published_ast;
        Compile_assets.refresh_ast compile_assets ~source:absolute_path
          ~path:published_ast;
        File_util.copy_existing_file ~ensure_parent:false
          (Filename.concat config.root path)
          (Filename.concat
             (Build_artifacts.lib_path config.root "ocaml")
             (Filename.basename path));
        if is_local && stderr <> "" then
          Hashtbl.replace stats.retained.pending_parse_paths pending_path ()
        else Hashtbl.remove stats.retained.pending_parse_paths pending_path
      in
      match result with
      | Build_types.Parse_failed {stdout; stderr} ->
        Hashtbl.replace stats.retained.pending_parse_paths pending_path ();
        let output =
          Printf.sprintf "Error in %s:\n%s%s" config.name stderr stdout
        in
        Compiler_log.append root output;
        stats.parse_messages <-
          Build_types.Parse_error output :: stats.parse_messages
      | Build_types.Parsed_successfully {stderr} ->
        publish_successful_parse stderr
      | Build_types.Use_existing_ast -> publish_successful_parse "")
    parsed;
  if !warning_asts <> [] then
    stats.compile_cleanup <-
      (fun () ->
        List.iter
          (fun (source, ast) ->
            let path = Filename.concat ocaml_dir (Filename.basename ast) in
            File_util.remove_file path;
            Compile_assets.refresh_ast compile_assets ~source ~path)
          !warning_asts)
      :: stats.compile_cleanup;
  let parse_dirty_modules = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ ->
      let paths =
        module_.Source.implementation :: Option.to_list module_.Source.interface
      in
      if List.exists (Hashtbl.mem dirty_parse_path_set) paths then (
        Hashtbl.replace parse_dirty_modules module_.Source.name ();
        let key = Source.compiler_basename config module_.Source.name in
        (Build_state.find_exn build_state key).compile_dirty <- true))
    modules;
  let module_is_dirty module_ (state : Build_state.module_) =
    let global_key = Source.compiler_basename config module_.Source.name in
    let module_name = Source.module_name module_.Source.implementation in
    let source_artifact_is_pending path =
      let source = Filename.concat root path in
      match
        (Compile_assets.ast compile_assets source, state.last_compiled_cmt)
      with
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
      | None ->
        raise (Error ("Build module was not prepared for " ^ global_key))
    in
    let dependency_is_newer dependency =
      let dependency_state = Build_state.find_exn build_state dependency in
      Build_state.dependency_tree_compiled_after
        ~namespace_freshness:stats.namespace_freshness build_state state
        dependency_state
    in
    Hashtbl.mem parse_dirty_modules module_.Source.name
    || Hashtbl.mem removed_module_names module_name
    || List.exists source_artifact_is_pending
         (module_.Source.implementation
         :: Option.to_list module_.Source.interface)
    || (not (Build_state.has_complete_compile_assets state))
    || (not outputs_exist)
    || List.exists (Hashtbl.mem removed_module_names) raw_dependencies
    || List.exists
         (fun dependency -> Hashtbl.mem stats.removed_modules dependency)
         raw_dependencies
    || List.exists dependency_is_newer state.dependencies
  in
  (* Freshness is persistent state, while cycle blocking controls only whether
     a module may be dispatched in this attempt. Compute the full initial state
     even when another source failed to parse so a retained watch attempt
     cannot forget unrelated missing or stale artifacts. *)
  if stats.attempt_kind = Build_types.Full_attempt then
    List.iter
      (fun module_ ->
        let key = Source.compiler_basename config module_.Source.name in
        let state = Build_state.find_exn build_state key in
        state.compile_dirty <-
          state.compile_dirty || module_is_dirty module_ state)
      modules;
  if
    List.exists
      (function
        | Build_types.Parse_error _ -> true
        | Build_types.Parse_warning _ -> false)
      stats.parse_messages
  then ()
  else (
    stats.parsed <- stats.parsed + Hashtbl.length parse_dirty_modules;
    let compile_warning_modules = Hashtbl.create 8 in
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
          (if module_.Source.is_dev then dev_common_args
           else regular_common_args)
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
          config.package_specs
    in
    let post_build path = Compiler_process.post_build_tasks config path in
    let candidates =
      List.filter_map
        (fun module_ ->
          let key = Source.compiler_basename config module_.Source.name in
          let state = Build_state.find_exn build_state key in
          let blocked = Hashtbl.mem stats.blocked_modules key in
          let cmi_path =
            Filename.concat ocaml_dir
              (Source.compiler_asset_basename config
                 module_.Source.implementation
              ^ ".cmi")
          in
          let warning_paths =
            module_.Source.implementation
            :: Option.to_list module_.Source.interface
            |> List.map (Filename.concat config.root)
          in
          if blocked then None
          else
            let make () =
              Compiler_scheduler.create ~key ~dependencies:state.dependencies
                ~source:module_ ~state ~cmi_path
                ~prepare:(fun () -> prepare_outputs module_)
                ~compile:(fun ~is_interface path ->
                  compile_process module_ ~is_interface path)
                ~publish:(fun ~is_interface path result ->
                  Compiler_process.publish ~build_dir ~ocaml_dir ~is_local
                    ~config ~is_interface path result)
                ~record_published_outputs ~post_build ~package_root:config.root
                ~is_local
                ~mark_warning:(fun path ->
                  Hashtbl.replace compile_warning_modules
                    (Source.module_name path) ())
            in
            Some (Compiler_scheduler.candidate ~key ~state ~warning_paths ~make))
        modules
    in
    Config.namespace_compiler_name config.namespace
    |> Option.iter (fun compiler_name ->
        let namespace_map =
          Hashtbl.find stats.retained.namespace_maps
            (Build_types.namespace_map_key root)
        in
        let namespace_state =
          Build_state.find_exn build_state namespace_map.key
        in
        let package_dirty =
          List.exists Compiler_scheduler.candidate_requires_compile candidates
        in
        if package_dirty || stats.attempt_kind = Build_types.Full_attempt then
          Option.iter
            (fun (job, finish) ->
              let cmi_path =
                Filename.concat ocaml_dir (compiler_name ^ ".cmi")
              in
              let digest_before =
                try Some (Digest.file cmi_path)
                with Sys_error _ | Unix.Unix_error _ -> None
              in
              let finish result =
                finish result;
                let digest_after =
                  try Some (Digest.file cmi_path)
                  with Sys_error _ | Unix.Unix_error _ -> None
                in
                Compile_assets.refresh_cmi compile_assets ~key:compiler_name
                  ~path:cmi_path;
                let cmt_path =
                  Filename.concat ocaml_dir (compiler_name ^ ".cmt")
                in
                Compile_assets.refresh_cmt compile_assets ~key:compiler_name
                  ~path:cmt_path;
                namespace_state.last_compiled_cmi <-
                  Compile_assets.cmi compile_assets compiler_name
                  |> Option.map (fun entry -> entry.Compile_assets.modified);
                namespace_state.last_compiled_cmt <-
                  Compile_assets.cmt compile_assets compiler_name
                  |> Option.map (fun entry -> entry.Compile_assets.modified);
                namespace_state.compile_dirty <- false;
                if digest_before <> digest_after then
                  Build_state.mark_dependents_compile_dirty build_state
                    namespace_state
              in
              stats.namespace_jobs <- (job, finish) :: stats.namespace_jobs)
            (Compiler_process.namespace_job ~bsc ~runtime ~build_dir ~ocaml_dir
               ~entry:(Config.namespace_entry config.namespace)
               ~package_dirty compiler_name modules));
    stats.compile_candidates <- candidates @ stats.compile_candidates;
    stats.compile_cleanup <-
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
                    File_util.remove_file
                      (Build_artifacts.published_ast_path ~ocaml_dir path))
                  paths)
            compile_warning_modules)
      :: stats.compile_cleanup;
    ())
