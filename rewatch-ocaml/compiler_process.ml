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

type request = {args: string list; cwd: string}

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

(* Requests run in parallel domains. Windows emulates append mode as a seek to
   the end followed by a write, so two domains appending at once could write
   at the same offset and lose one of the lines the tests count. *)
let call_log_mutex = Mutex.create ()

let log_compiler_request (request : request) =
  match Sys.getenv_opt "REWATCH_COMPILER_CALL_LOG" with
  | None -> ()
  | Some path ->
    let phase, input = compiler_phase request.args in
    Process_child.with_lock call_log_mutex (fun () ->
        let channel =
          open_out_gen [Open_creat; Open_append; Open_text] 0o644 path
        in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            Printf.fprintf channel "%s\t%s\t%s\n" phase request.cwd input))

let exit_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let run_in_process ?poll (request : request) =
  log_compiler_request request;
  match List.rev request.args with
  | [] ->
    {
      Process.status = Unix.WEXITED 2;
      stdout = "";
      stderr = "missing compiler input";
    }
  | input :: reversed_argv -> (
    match
      Rescript_compiler_driver.run_request ~cwd:request.cwd
        ~argv:(List.rev reversed_argv) ~input
        ~run_external:
          (Some
             (fun command ->
               let command = Platform.shell_command command in
               let result =
                 Process.run ?poll ~cwd:request.cwd command.program command.args
               in
               (exit_code result.status, result.stdout, result.stderr)))
    with
    | result ->
      {
        Process.status = Unix.WEXITED result.exit_code;
        stdout = result.stdout;
        stderr = result.stderr;
      }
    | exception
        Compiler_request_output.Failed {error = Process.Interrupted _ as error}
      ->
      raise error
    | exception Compiler_request_output.Failed {error; stdout; stderr} ->
      (* Keep the warnings written before the failure. *)
      {
        Process.status = Unix.WEXITED 2;
        stdout;
        stderr = stderr ^ Printexc.to_string error ^ "\n";
      })

let run ?poll request =
  Option.iter (fun poll -> poll ()) poll;
  let result = run_in_process ?poll request in
  Option.iter (fun poll -> poll ()) poll;
  result

let sequential_task requests =
  let cancelled = Atomic.make false in
  Process.concurrent_task
    ~cancel:(fun () -> Atomic.set cancelled true)
    (fun () ->
      let poll () =
        if Atomic.get cancelled then raise (Process.Interrupted 15)
      in
      List.fold_left
        (fun result request ->
          if not (Process.succeeded result) then result
          else (
            poll ();
            let next = run_in_process ~poll request in
            {
              next with
              Process.stdout = result.stdout ^ next.stdout;
              stderr = result.stderr ^ next.stderr;
            }))
        Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""}
        requests)

let task request = sequential_task [request]

let run_requests ?poll ?on_complete requests =
  requests |> List.map task
  |> Process.run_tasks
       ~max_jobs:(Compiler_execution_mode.configured_count ())
       ?poll ?on_complete

let parse_request ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  File_util.ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents =
    if config.ppx_flags = [] then ""
    else File_util.read_file (Filename.concat config.root path)
  in
  let args = Compiler_args.parser_arguments ~config ~contents ~path in
  {args; cwd = build_dir}

let ast_dependencies ~build_dir ast =
  (Ast_header.read (Filename.concat build_dir ast)).dependencies

type compiler_artifact = Cmi | Required of string | Optional of string

let publish_compiler_artifacts ~artifact_dir ~ocaml_dir ~basename artifacts =
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
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
          cmi_change :=
            if File_util.copy_file_if_different source destination then
              Compiler_scheduler.Cmi_changed
            else Compiler_scheduler.Cmi_unchanged
        | Required _ -> File_util.copy_existing_file source destination
        | Optional _ -> File_util.copy_optional_existing_file source destination)
      artifacts;
    !cmi_change
  with error ->
    raise (Compiler_scheduler.Publication_failure (error, !cmi_change))

let namespace_task ~runtime ~build_dir ~ocaml_dir ~entry ~package_dirty ~force
    namespace modules =
  let mlmap = Filename.concat build_dir (namespace ^ ".mlmap") in
  let namespace_contents modules =
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
  let contents = namespace_contents modules in
  let public_contents =
    namespace_contents
      (List.filter (fun module_ -> not module_.Source.is_dev) modules)
  in
  (* Local dev sources need the full namespace; consumers and namespace entries
     must share the filtered interface. Stage that interface before publication. *)
  let artifact_dir =
    if public_contents = contents then build_dir
    else Filename.concat build_dir ".public-namespace"
  in
  let internal_cmi = Filename.concat build_dir (namespace ^ ".cmi") in
  let previous_cmi =
    if File_util.is_regular_file internal_cmi then
      Some (File_util.digest_file internal_cmi)
    else None
  in
  let previous_contents =
    try Some (File_util.read_file mlmap)
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None
  in
  let mlmap_changed = previous_contents <> Some contents in
  if mlmap_changed then
    File_util.write_file_atomic ~ensure_parent:false mlmap contents;
  let outputs_exist =
    Option.is_some previous_cmi
    && ["cmi"; "cmj"; "cmt"; "mlmap"]
       |> List.for_all (fun extension ->
           File_util.is_regular_file
             (Filename.concat ocaml_dir (namespace ^ "." ^ extension)))
  in
  let published_mlmap_matches =
    try
      File_util.read_file (Filename.concat ocaml_dir (namespace ^ ".mlmap"))
      = public_contents
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
  in
  if
    not
      (force || package_dirty || mlmap_changed
      || (not published_mlmap_matches)
      || not outputs_exist)
  then None
  else (
    if artifact_dir <> build_dir then (
      File_util.ensure_dir artifact_dir;
      File_util.write_file_atomic ~ensure_parent:false
        (Filename.concat artifact_dir (namespace ^ ".mlmap"))
        public_contents);
    let request cwd =
      {
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
        cwd;
      }
    in
    let requests =
      if artifact_dir = build_dir then [request build_dir]
      else [request build_dir; request artifact_dir]
    in
    Some
      Compiler_scheduler.
        {
          task = sequential_task requests;
          publish =
            (fun result ->
              if not (Process.succeeded result) then
                raise
                  (Compiler_scheduler.Build_failure
                     (result.Process.stderr ^ result.stdout));
              let internal_changed =
                previous_cmi <> Some (File_util.digest_file internal_cmi)
              in
              let include_internal_change change =
                if internal_changed then Compiler_scheduler.Cmi_changed
                else change
              in
              let cmi_change =
                try
                  publish_compiler_artifacts ~artifact_dir ~ocaml_dir
                    ~basename:namespace
                    [Cmi; Required "cmj"; Required "cmt"; Required "mlmap"]
                  |> include_internal_change
                with Compiler_scheduler.Publication_failure (error, change) ->
                  raise
                    (Compiler_scheduler.Publication_failure
                       (error, include_internal_change change))
              in
              Compiler_scheduler.{stderr = result.stderr; cmi_change});
        })

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

let compile_request ~build_dir ~(config : Config.t) ~common_args
    (module_ : Source.module_) ~source_kind path =
  let args =
    Compiler_args.compiler_arguments_with_common ~config ~common_args
      ~module_name:module_.name ~source_kind
      ~has_interface:(Option.is_some module_.interface)
      ~path
  in
  {args; cwd = build_dir}

let publish ~build_dir ~ocaml_dir ~is_local ~(config : Config.t) ~has_interface
    ~source_kind path result =
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  let basename = Source.compiler_asset_basename config path in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
  try
    (cmi_change :=
       match source_kind with
       | Source.Interface ->
         publish_compiler_artifacts ~artifact_dir ~ocaml_dir ~basename
           [Cmi; Optional "cmti"]
       | Source.Implementation when has_interface ->
         (* The implementation is compiled with -bs-read-cmi, so its .cmi is
           the one already published after compiling the interface. *)
         ignore
           (publish_compiler_artifacts ~artifact_dir ~ocaml_dir ~basename
              [Required "cmj"; Optional "cmt"]);
         Compiler_scheduler.Cmi_unchanged
       | Source.Implementation ->
         publish_compiler_artifacts ~artifact_dir ~ocaml_dir ~basename
           [Cmi; Required "cmj"; Optional "cmt"]);
    let source = Filename.concat config.root path in
    let build_source = Filename.concat build_dir path in
    File_util.ensure_dir (Filename.dirname build_source);
    File_util.copy_existing_file source build_source;
    File_util.copy_existing_file source
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
              File_util.copy_existing_file output build_output;
            if File_util.exists (output ^ ".map") then
              File_util.copy_existing_file (output ^ ".map")
                (build_output ^ ".map")
            else File_util.remove_file (build_output ^ ".map")))
        config.package_specs);
    Compiler_scheduler.{stderr; cmi_change = !cmi_change}
  with
  | Compiler_scheduler.Publication_failure _ as error -> raise error
  | error -> raise (Compiler_scheduler.Publication_failure (error, !cmi_change))
