let run ~(package : Build_types.graph_package) ~(stats : Build_types.t) ~watch
    ~removed_module_names ~parse_dirty_modules =
  let root = package.graph_root in
  let is_local = package.graph_is_local in
  let config = package.graph_compile_config in
  let prepared = Build_types.prepared_exn stats in
  let prepared_package = Build_types.prepared_package_exn stats root in
  let build_state = prepared.build_state in
  let compile_assets = prepared.compile_assets in
  let build_dir = package.graph_build_dir in
  let ocaml_dir = package.graph_ocaml_dir in
  let modules = package.graph_modules in
  let cleanup =
    match Hashtbl.find_opt stats.retained.cleanup_results root with
    | Some result -> result
    | None ->
      raise
        (Project_context.Error ("Package cleanup was not prepared for " ^ root))
  in
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
        raise
          (Project_context.Error
             ("Build module was not prepared for " ^ global_key))
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
  if stats.attempt_kind = Build_types.Full_attempt then
    List.iter
      (fun module_ ->
        let key = Source.compiler_basename config module_.Source.name in
        let state = Build_state.find_exn build_state key in
        state.compile_dirty <-
          state.compile_dirty || module_is_dirty module_ state)
      modules;
  if not (Build_types.has_parse_error stats.parse_messages) then (
    stats.parsed <- stats.parsed + Hashtbl.length parse_dirty_modules;
    let compile_warning_paths = Hashtbl.create 8 in
    let prepare_outputs module_ =
      let path = module_.Source.implementation in
      List.iter
        (fun spec ->
          let output = Build_artifacts.generated_js_path config path spec in
          File_util.ensure_dir (Filename.dirname output))
        config.package_specs
    in
    let compile_process module_ ~is_interface path =
      Compiler_process.compile_job ~bsc:prepared.compiler_context.bsc_path
        ~build_dir ~config
        ~common_args:
          (if module_.Source.is_dev then
             prepared_package.development_common_args
           else prepared_package.regular_common_args)
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
    let candidates =
      List.filter_map
        (fun module_ ->
          let key = Source.compiler_basename config module_.Source.name in
          let state = Build_state.find_exn build_state key in
          if Hashtbl.mem stats.blocked_modules key then None
          else
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
            let make () =
              Compiler_scheduler.create ~key ~dependencies:state.dependencies
                ~source:module_ ~state ~cmi_path
                ~prepare:(fun () -> prepare_outputs module_)
                ~compile:(fun ~is_interface path ->
                  compile_process module_ ~is_interface path)
                ~publish:(fun ~is_interface path result ->
                  Compiler_process.publish ~build_dir ~ocaml_dir ~is_local
                    ~config ~is_interface path result)
                ~record_published_outputs
                ~post_build:(Compiler_process.post_build_tasks config)
                ~package_root:config.root ~is_local
                ~mark_warning:(fun _path ->
                  module_.Source.implementation
                  :: Option.to_list module_.Source.interface
                  |> List.iter (fun path ->
                      Hashtbl.replace compile_warning_paths
                        (Build_artifacts.published_ast_path ~ocaml_dir path)
                        ()))
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
          Compiler_process.namespace_job ~bsc:prepared.compiler_context.bsc_path
            ~runtime:prepared.compiler_context.runtime_path ~build_dir
            ~ocaml_dir
            ~entry:(Config.namespace_entry config.namespace)
            ~package_dirty compiler_name modules
          |> Option.iter (fun (job, finish_namespace) ->
              let cmi_path =
                Filename.concat ocaml_dir (compiler_name ^ ".cmi")
              in
              let digest_before =
                try Some (Digest.file cmi_path)
                with Sys_error _ | Unix.Unix_error _ -> None
              in
              let finish result =
                finish_namespace result;
                let digest_after =
                  try Some (Digest.file cmi_path)
                  with Sys_error _ | Unix.Unix_error _ -> None
                in
                let cmi_change =
                  if digest_before = digest_after then Build_state.Cmi_unchanged
                  else Build_state.Cmi_changed
                in
                Build_state.record_published_cmi build_state ~compile_assets
                  namespace_state ~path:cmi_path cmi_change;
                let cmt_path =
                  Filename.concat ocaml_dir (compiler_name ^ ".cmt")
                in
                Build_state.record_successful_compile ~compile_assets
                  namespace_state ~cmt_path
              in
              stats.namespace_jobs <- (job, finish) :: stats.namespace_jobs));
    stats.compile_candidates <- candidates @ stats.compile_candidates;
    stats.compile_cleanup <-
      (fun () ->
        if not watch then
          Hashtbl.iter
            (fun path () -> File_util.remove_file path)
            compile_warning_paths)
      :: stats.compile_cleanup)
