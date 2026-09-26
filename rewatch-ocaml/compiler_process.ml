let retain_critical_external_warnings stderr =
  let marker = "`(. ...)` uncurried syntax" in
  if not (String_util.contains stderr marker) then ""
  else
    stderr
    |> Str.global_replace (Str.regexp_string "\r\n") "\n"
    |> Str.split_delim (Str.regexp_string "\n\n\n")
    |> List.filter (fun block -> String_util.contains block marker)
    |> String.concat "\n\n\n"

let build_identity = Rescript_compiler_driver.build_identity

let compiler_phase args =
  let input =
    match List.rev args with
    | input :: _ -> input
    | [] -> "<missing>"
  in
  let phase =
    if List.mem "-bs-ast" args then "parse"
    else if Filename.check_suffix input ".mlmap" then "namespace"
    else if Filename.check_suffix input ".iast" then "interface"
    else "implementation"
  in
  (phase, input)

let append_log path line =
  let channel = open_out_gen [Open_creat; Open_append; Open_text] 0o644 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel line)

let log_compiler_request (job : Process.job) =
  match Sys.getenv_opt "REWATCH_COMPILER_CALL_LOG" with
  | None -> ()
  | Some path ->
    let phase, input = compiler_phase job.args in
    append_log path (Printf.sprintf "%s\t%s\t%s\n" phase job.cwd input)

let compiler_timing_log = Sys.getenv_opt "REWATCH_COMPILER_TIMING_LOG"
let artifact_export_log = Sys.getenv_opt "REWATCH_ARTIFACT_EXPORT_LOG"

let log_artifact_export event path =
  Option.iter
    (fun log ->
      append_log log
        (Printf.sprintf "%s\t%s\t%.9f\n" event path (Unix.gettimeofday ())))
    artifact_export_log

let time_compiler_request (job : Process.job) run =
  match compiler_timing_log with
  | None -> run ()
  | Some path ->
    let started = Unix.gettimeofday () in
    Fun.protect
      ~finally:(fun () ->
        let finished = Unix.gettimeofday () in
        let phase, input = compiler_phase job.args in
        append_log path
          (Printf.sprintf "%s\t%s\t%s\t%.9f\t%.9f\n" phase job.cwd input started
             finished))
      run

let exit_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let run_in_process ?session ?poll (job : Process.job) =
  log_compiler_request job;
  match List.rev job.args with
  | [] ->
    {
      Process.status = Unix.WEXITED 2;
      stdout = "";
      stderr = "missing compiler input";
    }
  | input :: reversed_argv ->
    let result =
      time_compiler_request job (fun () ->
          Env.with_expanded_snapshot_cache (fun () ->
              let run_external =
                Some
                  (fun command ->
                    let command = Platform.shell_command command in
                    (* Signal handlers are process-wide; domain workers launch
                       PPXs without replacing the scheduler domain's handlers. *)
                    let result =
                      Process.run ?poll ~defer_signals:false ~cwd:job.cwd
                        command.program command.args
                    in
                    (exit_code result.status, result.stdout, result.stderr))
              in
              match session with
              | None ->
                Rescript_compiler_driver.run_request ~cwd:job.cwd
                  ~argv:(List.rev reversed_argv) ~input ~run_external
              | Some session ->
                Rescript_compiler_driver.run_request_in_session session
                  ~cwd:job.cwd ~argv:(List.rev reversed_argv) ~input
                  ~run_external))
    in
    {
      Process.status = Unix.WEXITED result.exit_code;
      stdout = result.stdout;
      stderr = result.stderr;
    }

let run ?session ?poll job =
  Option.iter (fun poll -> poll ()) poll;
  let result = run_in_process ?session ?poll job in
  Option.iter (fun poll -> poll ()) poll;
  result

let task ?session job =
  let cancelled = Atomic.make false in
  Process.concurrent_task
    ~cancel:(fun () -> Atomic.set cancelled true)
    (fun () ->
      run_in_process ?session
        ~poll:(fun () ->
          if Atomic.get cancelled then raise (Process.Interrupted 15))
        job)

let run_jobs ?session ?poll ?on_complete jobs =
  jobs
  |> List.map (task ?session)
  |> Process.run_tasks
       ~max_jobs:(Compiler_execution_mode.configured_count ())
       ?poll ?on_complete

let parse_job ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  File_util.ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents =
    if config.ppx_flags = [] then ""
    else File_util.read_file (Filename.concat config.root path)
  in
  let args = Compiler_args.parser_arguments ~config ~contents ~path in
  Process.{program = bsc; args; cwd = build_dir}

let ast_dependencies ?session ~build_dir ast =
  let path = Filename.concat build_dir ast in
  match
    Option.bind session (fun session ->
        Rescript_compiler_driver.staged_ast_dependencies session ~path)
  with
  | Some dependencies -> dependencies
  | None -> (Ast_header.read path).dependencies

type compiler_artifact = Cmi | Required of string | Optional of string
type artifact_changes = {
  cmi_change: Compiler_scheduler.cmi_change;
  optimization_changed: bool;
}

let session_fingerprint session kind filename =
  Option.bind session (fun session ->
      Rescript_compiler_driver.published_fingerprint session ~kind ~filename)

let changes_from_fingerprints ~previous_interface ~previous_optimization
    ~interface_file ~optimization_file ~session changes =
  let current_interface =
    session_fingerprint session Rescript_compiler_driver.Interface
      interface_file
  in
  let current_optimization =
    Option.bind optimization_file (fun filename ->
        session_fingerprint session Rescript_compiler_driver.Optimization
          filename)
  in
  let cmi_change =
    match (previous_interface, current_interface) with
    | Some previous, Some current ->
      if previous = current then Compiler_scheduler.Cmi_unchanged
      else Compiler_scheduler.Cmi_changed
    | _ -> changes.cmi_change
  in
  let optimization_changed =
    match (previous_optimization, current_optimization) with
    | Some previous, Some current -> previous <> current
    | _ -> changes.optimization_changed
  in
  {cmi_change; optimization_changed}

let publish_compiler_artifacts ?(preserve_source_mtime = false) ~artifact_dir
    ~ocaml_dir ~basename artifacts =
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
  let optimization_changed = ref false in
  try
    List.iter
      (fun artifact ->
        let extension =
          match artifact with
          | Cmi -> "cmi"
          | Required name | Optional name -> name
        in
        let source =
          Filename.concat artifact_dir (basename ^ "." ^ extension)
        in
        let destination =
          Filename.concat ocaml_dir (basename ^ "." ^ extension)
        in
        match artifact with
        | Cmi ->
          let changed =
            File_util.copy_file_if_different ~ensure_parent:false source
              destination
          in
          cmi_change :=
            if changed then Compiler_scheduler.Cmi_changed
            else Compiler_scheduler.Cmi_unchanged;
          if preserve_source_mtime && changed then
            let stats = Unix.stat source in
            Unix.utimes destination stats.st_atime stats.st_mtime
        | Required "cmj" ->
          let changed =
            File_util.copy_file_if_different ~ensure_parent:false source
              destination
          in
          optimization_changed := changed;
          if preserve_source_mtime && changed then
            let stats = Unix.stat source in
            Unix.utimes destination stats.st_atime stats.st_mtime
        | Required _ ->
          File_util.copy_existing_file ~ensure_parent:false source destination
        | Optional _ ->
          File_util.copy_optional_existing_file ~ensure_parent:false source
            destination)
      artifacts;
    {cmi_change = !cmi_change; optimization_changed = !optimization_changed}
  with error ->
    raise
      (Compiler_scheduler.Publication_failure
         ( error,
           if !optimization_changed then Compiler_scheduler.Cmi_change_unknown
           else !cmi_change ))

let namespace_task ?session ~bsc ~runtime ~build_dir ~ocaml_dir ~entry
    ~package_dirty ~force namespace modules =
  let mlmap = Filename.concat build_dir (namespace ^ ".mlmap") in
  let contents =
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer "randjbuildsystem\n";
    Source.namespace_members ~entry modules
    |> List.map (fun module_ -> module_.Source.name)
    |> List.sort String.compare
    |> List.iter (fun name ->
        Buffer.add_string buffer name;
        Buffer.add_char buffer '\n');
    Buffer.contents buffer
  in
  let previous_contents =
    try Some (File_util.read_file mlmap)
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None
  in
  let mlmap_changed = previous_contents <> Some contents in
  if mlmap_changed then
    File_util.write_file_atomic ~ensure_parent:false ~perm:0o644 mlmap contents;
  let outputs_exist =
    ["cmi"; "cmj"; "cmt"; "mlmap"]
    |> List.for_all (fun extension ->
        File_util.is_regular_file
          (Filename.concat ocaml_dir (namespace ^ "." ^ extension)))
  in
  let published_mlmap_matches =
    try
      File_util.read_file (Filename.concat ocaml_dir (namespace ^ ".mlmap"))
      = contents
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
  in
  if
    not
      (force || package_dirty || mlmap_changed
      || (not published_mlmap_matches)
      || not outputs_exist)
  then None
  else
    Some
      Compiler_scheduler.
        {
          task =
            task ?session
              Process.
                {
                  program = bsc;
                  args =
                    [
                      "-runtime-path";
                      runtime;
                      "-w";
                      "-49";
                      "-color";
                      "always";
                      "-no-alias-deps";
                      Filename.basename mlmap;
                    ];
                  cwd = build_dir;
                };
          publish =
            (fun result ->
              if not (Process.succeeded result) then
                raise
                  (Compiler_scheduler.Build_failure
                     (result.Process.stderr ^ result.stdout));
              let interface_file =
                Filename.concat ocaml_dir (namespace ^ ".cmi")
              in
              let optimization_file =
                Filename.concat ocaml_dir (namespace ^ ".cmj")
              in
              let previous_interface =
                session_fingerprint session Rescript_compiler_driver.Interface
                  interface_file
              in
              let previous_optimization =
                session_fingerprint session
                  Rescript_compiler_driver.Optimization optimization_file
              in
              let changes =
                publish_compiler_artifacts ~artifact_dir:build_dir ~ocaml_dir
                  ~basename:namespace
                  [Cmi; Required "cmj"; Required "cmt"; Required "mlmap"]
              in
              Option.iter
                (fun session ->
                  Rescript_compiler_driver.publish_session_cmi session
                    ~retain:true
                    ~source:(Filename.concat build_dir (namespace ^ ".cmi"))
                    ~destination:
                      (Filename.concat ocaml_dir (namespace ^ ".cmi"));
                  Rescript_compiler_driver.publish_session_cmj session
                    ~retain:true
                    ~source:(Filename.concat build_dir (namespace ^ ".cmj"))
                    ~destination:
                      (Filename.concat ocaml_dir (namespace ^ ".cmj"));
                  Rescript_compiler_driver.publish_session_semantic session
                    ~retain:false
                    ~source:(Filename.concat build_dir (namespace ^ ".cmt"))
                    ~destination:
                      (Filename.concat ocaml_dir (namespace ^ ".cmt"));
                  Rescript_compiler_driver.publish_module_result session
                    ~input:mlmap ~interface_file
                    ~optimization_file:(Some optimization_file)
                    ~semantic_file:None ~dependencies:[]
                    ~generated_outputs:
                      (List.map
                         (fun extension ->
                           Filename.concat ocaml_dir
                             (namespace ^ "." ^ extension))
                         ["cmi"; "cmj"; "cmt"; "mlmap"]))
                session;
              let changes =
                changes_from_fingerprints ~previous_interface
                  ~previous_optimization ~interface_file
                  ~optimization_file:(Some optimization_file) ~session changes
              in
              Compiler_scheduler.
                {
                  stderr = result.stderr;
                  cmi_change = changes.cmi_change;
                  optimization_changed = changes.optimization_changed;
                  deferred_export = None;
                  cancel_export = None;
                  staged_cmi_path = None;
                });
        }

let post_build_tasks (config : Config.t) path =
  match config.js_post_build with
  | None -> []
  | Some command ->
    List.map
      (fun spec ->
        let output = Build_artifacts.generated_js_path config path spec in
        let command = Platform.post_build_command ~command ~output in
        Compiler_scheduler.
          {
            output;
            task =
              Process.task ?env:command.env
                Process.
                  {
                    program = command.program;
                    args = command.args;
                    cwd = config.root;
                  };
          })
      config.package_specs

let compile_job ~bsc ~build_dir ~(config : Config.t) ~common_args
    (module_ : Source.module_) ~source_kind path =
  let args =
    Compiler_args.compiler_arguments_with_common ~config ~common_args
      ~module_name:module_.name ~source_kind
      ~has_interface:(Option.is_some module_.interface)
      ~path
  in
  Process.{program = bsc; args; cwd = build_dir}

let publish_immediate ?session ~preserve_source_mtime ~retain_interface
    ~dependencies ~build_dir ~ocaml_dir ~is_local ~(config : Config.t)
    ~source_kind ~compiled_at path result =
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  let basename = Source.compiler_asset_basename config path in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let interface_file = Filename.concat ocaml_dir (basename ^ ".cmi") in
  let optimization_file =
    match source_kind with
    | Source.Interface -> None
    | Source.Implementation ->
      Some (Filename.concat ocaml_dir (basename ^ ".cmj"))
  in
  let previous_interface =
    session_fingerprint session Rescript_compiler_driver.Interface
      interface_file
  in
  let previous_optimization =
    Option.bind optimization_file (fun filename ->
        session_fingerprint session Rescript_compiler_driver.Optimization
          filename)
  in
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
  let optimization_changed = ref false in
  try
    let changes =
      publish_compiler_artifacts ~preserve_source_mtime ~artifact_dir ~ocaml_dir
        ~basename
        (match source_kind with
        | Source.Interface -> [Cmi; Optional "cmti"]
        | Source.Implementation -> [Cmi; Required "cmj"; Optional "cmt"])
    in
    cmi_change := changes.cmi_change;
    optimization_changed := changes.optimization_changed;
    (match source_kind with
    | Source.Interface -> ()
    | Source.Implementation ->
      if
        not
          (File_util.is_regular_file
             (Filename.concat artifact_dir (basename ^ ".cmt")))
      then
        Option.iter
          (fun filename -> Unix.utimes filename compiled_at compiled_at)
          optimization_file);
    let source = Filename.concat config.root path in
    let build_source = Filename.concat build_dir path in
    File_util.ensure_dir (Filename.dirname build_source);
    File_util.copy_existing_file ~ensure_parent:false source build_source;
    File_util.copy_existing_file ~ensure_parent:false source
      (Filename.concat ocaml_dir (Filename.basename path));
    (match source_kind with
    | Source.Interface -> ()
    | Source.Implementation ->
      List.iter
        (fun spec ->
          if spec.Config.in_source then (
            let output = Build_artifacts.generated_js_path config path spec in
            let build_output =
              Build_artifacts.generated_build_js_path ~build_dir config path
                spec
            in
            File_util.ensure_dir (Filename.dirname build_output);
            if File_util.exists output then
              File_util.copy_existing_file ~ensure_parent:false output
                build_output;
            if File_util.exists (output ^ ".map") then
              File_util.copy_existing_file ~ensure_parent:false
                (output ^ ".map") (build_output ^ ".map")
            else File_util.remove_file (build_output ^ ".map")))
        config.package_specs);
    Option.iter
      (fun session ->
        Rescript_compiler_driver.publish_session_cmi session
          ~retain:retain_interface
          ~source:(Filename.concat artifact_dir (basename ^ ".cmi"))
          ~destination:(Filename.concat ocaml_dir (basename ^ ".cmi"));
        (match source_kind with
        | Source.Interface -> ()
        | Source.Implementation ->
          Rescript_compiler_driver.publish_session_cmj session
            ~retain:retain_interface
            ~source:(Filename.concat artifact_dir (basename ^ ".cmj"))
            ~destination:(Filename.concat ocaml_dir (basename ^ ".cmj")));
        let cmt_extension =
          match source_kind with
          | Source.Interface -> ".cmti"
          | Source.Implementation -> ".cmt"
        in
        Rescript_compiler_driver.publish_session_semantic session
          ~retain:is_local
          ~source:(Filename.concat artifact_dir (basename ^ cmt_extension))
          ~destination:(Filename.concat ocaml_dir (basename ^ cmt_extension));
        let semantic_file =
          let filename = Filename.concat ocaml_dir (basename ^ cmt_extension) in
          if File_util.is_regular_file filename then Some filename else None
        in
        let generated_outputs =
          let compiler_outputs =
            List.map
              (fun extension ->
                Filename.concat ocaml_dir (basename ^ extension))
              [".cmi"; ".cmj"; ".cmt"; ".cmti"]
            @ [Build_artifacts.published_ast_path ~ocaml_dir path]
          in
          let js_outputs =
            match source_kind with
            | Source.Interface -> []
            | Source.Implementation ->
              List.concat_map
                (fun spec ->
                  let path =
                    Build_artifacts.generated_js_path config path spec
                  in
                  [path; path ^ ".map"])
                config.package_specs
          in
          List.filter File_util.is_regular_file (compiler_outputs @ js_outputs)
        in
        Rescript_compiler_driver.publish_module_result session
          ~input:(Filename.concat build_dir (Source.ast_path path))
          ~interface_file ~optimization_file ~semantic_file ~dependencies
          ~generated_outputs)
      session;
    let changes =
      changes_from_fingerprints ~previous_interface ~previous_optimization
        ~interface_file ~optimization_file ~session changes
    in
    Compiler_scheduler.
      {
        stderr;
        cmi_change = changes.cmi_change;
        optimization_changed = changes.optimization_changed;
        deferred_export = None;
        cancel_export = None;
        staged_cmi_path = None;
      }
  with
  | Compiler_scheduler.Publication_failure _ as error -> raise error
  | error ->
    raise
      (Compiler_scheduler.Publication_failure
         ( error,
           if !optimization_changed then Compiler_scheduler.Cmi_change_unknown
           else !cmi_change ))

let publish ?session ~retain_interface ~dependencies ~build_dir ~ocaml_dir
    ~is_local ~(config : Config.t) ~source_kind path result =
  (* This is the producer completion time, before its dependents can start.
     A no-CMT build uses it as the CMJ freshness marker even if CMJ bytes did
     not change and the compiler reused its old staging file. *)
  let compiled_at = Unix.gettimeofday () in
  let immediate preserve_source_mtime =
    publish_immediate ?session ~preserve_source_mtime ~retain_interface
      ~dependencies ~build_dir ~ocaml_dir ~is_local ~config ~source_kind
      ~compiled_at path result
  in
  match session with
  | None -> immediate false
  | Some session
    when (not retain_interface)
         || not (Rescript_compiler_driver.session_frozen_enabled session) ->
    immediate false
  | Some session ->
    let basename = Source.compiler_asset_basename config path in
    let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
    let source extension =
      Filename.concat artifact_dir (basename ^ "." ^ extension)
    in
    let destination extension =
      Filename.concat ocaml_dir (basename ^ "." ^ extension)
    in
    let interface_file = destination "cmi" in
    let optimization_file =
      match source_kind with
      | Source.Interface -> None
      | Source.Implementation -> Some (destination "cmj")
    in
    let previous_interface =
      session_fingerprint (Some session) Rescript_compiler_driver.Interface
        interface_file
    in
    let previous_optimization =
      Option.bind optimization_file (fun filename ->
          session_fingerprint (Some session)
            Rescript_compiler_driver.Optimization filename)
    in
    let staged_interface =
      Rescript_compiler_driver.stage_session_cmi session ~source:(source "cmi")
        ~destination:interface_file
    in
    let staged_optimization =
      match optimization_file with
      | None -> true
      | Some filename ->
        Rescript_compiler_driver.stage_session_cmj session
          ~source:(source "cmj") ~destination:filename
    in
    let interface_available =
      staged_interface
      || Option.is_some
           (session_fingerprint (Some session)
              Rescript_compiler_driver.Interface interface_file)
    in
    if (not interface_available) || not staged_optimization then immediate false
    else
      let current_interface =
        session_fingerprint (Some session) Rescript_compiler_driver.Interface
          interface_file
      in
      let current_optimization =
        Option.bind optimization_file (fun filename ->
            session_fingerprint (Some session)
              Rescript_compiler_driver.Optimization filename)
      in
      let cmi_change =
        match (previous_interface, current_interface) with
        | Some old, Some current ->
          if old = current then Compiler_scheduler.Cmi_unchanged
          else Compiler_scheduler.Cmi_changed
        | _ ->
          if File_util.files_equal (source "cmi") interface_file then
            Compiler_scheduler.Cmi_unchanged
          else Compiler_scheduler.Cmi_changed
      in
      let optimization_changed =
        match
          (optimization_file, previous_optimization, current_optimization)
        with
        | None, _, _ -> false
        | Some _, Some old, Some current -> old <> current
        | Some filename, _, _ ->
          not (File_util.files_equal (source "cmj") filename)
      in
      let cancel () =
        Rescript_compiler_driver.discard_pending_session_artifacts session
          ~interface_file ~optimization_file
      in
      let generated_outputs =
        let compiler_outputs =
          [
            destination "cmi";
            destination "cmj";
            destination "cmt";
            destination "cmti";
            Build_artifacts.published_ast_path ~ocaml_dir path;
          ]
        in
        let js_outputs =
          match source_kind with
          | Source.Interface -> []
          | Source.Implementation ->
            List.concat_map
              (fun spec ->
                let output =
                  Build_artifacts.generated_js_path config path spec
                in
                [output; output ^ ".map"])
              config.package_specs
        in
        compiler_outputs @ js_outputs
      in
      (try
         Rescript_compiler_driver.stage_module_result session
           ~input:(Filename.concat build_dir (Source.ast_path path))
           ~interface_source:(source "cmi") ~interface_file
           ~optimization_source:
             (Option.map (fun _ -> source "cmj") optimization_file)
           ~optimization_file
           ~semantic_source:
             (Some
                (source
                   (match source_kind with
                   | Source.Interface -> "cmti"
                   | Source.Implementation -> "cmt")))
           ~dependencies ~generated_outputs
       with error ->
         cancel ();
         raise error);
      let export () =
        log_artifact_export "start" path;
        Fun.protect
          ~finally:(fun () -> log_artifact_export "end" path)
          (fun () ->
            if
              Option.is_none current_interface
              || session_fingerprint (Some session)
                   Rescript_compiler_driver.Interface interface_file
                 <> current_interface
              || Option.exists
                   (fun filename ->
                     Option.is_none current_optimization
                     || session_fingerprint (Some session)
                          Rescript_compiler_driver.Optimization filename
                        <> current_optimization)
                   optimization_file
            then
              failwith
                ("compiler result changed before artifact export: " ^ path);
            ignore (immediate true);
            cancel ())
      in
      let stderr =
        if is_local then result.Process.stderr
        else retain_critical_external_warnings result.stderr
      in
      Compiler_scheduler.
        {
          stderr;
          cmi_change;
          optimization_changed;
          deferred_export = Some export;
          cancel_export = Some cancel;
          staged_cmi_path = Some (source "cmi");
        }
