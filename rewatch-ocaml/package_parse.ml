let run ~(package : Build_types.graph_package) ~(stats : Build_types.t)
    ~removed_module_names =
  let root = package.graph_root in
  let is_local = package.graph_is_local in
  let config = package.graph_compile_config in
  let prepared = Build_types.prepared_exn stats in
  let prepared_package = Build_types.prepared_package_exn stats root in
  let build_state = prepared.build_state in
  let compile_assets = prepared.compile_assets in
  let build_dir = package.graph_build_dir in
  let ocaml_dir = package.graph_ocaml_dir in
  let dirty_parse_paths =
    prepared_package.parse_paths
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
         ~job:
           (Compiler_process.parse_job ~bsc:prepared.compiler_context.bsc_path
              ~build_dir ~config))
    @ (dirty_parse_paths
      |> List.filter_map (fun path ->
          Hashtbl.find_opt stats.preliminary_parses (Filename.concat root path)
          |> Option.map (fun result -> (path, result))))
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
          (Filename.concat ocaml_dir (Filename.basename path));
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
  let dirty_modules = Hashtbl.create (List.length package.graph_modules) in
  List.iter
    (fun module_ ->
      let paths =
        module_.Source.implementation :: Option.to_list module_.Source.interface
      in
      if List.exists (Hashtbl.mem dirty_parse_path_set) paths then (
        Hashtbl.replace dirty_modules module_.Source.name ();
        let key = Source.compiler_basename config module_.Source.name in
        (Build_state.find_exn build_state key).compile_dirty <- true))
    package.graph_modules;
  dirty_modules
