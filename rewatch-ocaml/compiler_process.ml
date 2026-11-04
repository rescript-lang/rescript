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

(* Paths are matched as the build file store keys them. The directories in
   which the build publishes interfaces are spelled from canonical package
   roots, both in a redirected path and on the load path, so a lookup there
   that matches no redirected path names another file. Any other directory,
   such as one from user flags, may be an alias of one of them, and a file
   that differs only in case may be the same one: a lookup there of a
   redirected file name waits, as the file may be stale until then. *)
let cmi_redirect ~cwd ~cmi_directories ~wait_for_adoption redirects =
  match redirects with
  | [] -> fun _ -> None
  | redirects -> (
    let key path =
      Build_file_store.key
        (if Filename.is_relative path then Filename.concat cwd path else path)
    in
    let table length = Hashtbl.create length in
    let contents_by_key = table (List.length redirects) in
    let folded_keys = table (List.length redirects) in
    let folded_names = table (List.length redirects) in
    List.iter
      (fun (published, contents) ->
        let key = key published in
        Hashtbl.replace contents_by_key key contents;
        Hashtbl.replace folded_keys (String.lowercase_ascii key) ();
        Hashtbl.replace folded_names
          (String.lowercase_ascii (Filename.basename key))
          ())
      redirects;
    let directories = table (List.length cmi_directories) in
    List.iter
      (fun directory -> Hashtbl.replace directories (key directory) ())
      cmi_directories;
    fun path ->
      let key = key path in
      match Hashtbl.find_opt contents_by_key key with
      | Some _ as contents -> contents
      | None ->
        let folded = String.lowercase_ascii key in
        if
          Hashtbl.mem folded_names (Filename.basename folded)
          && (Hashtbl.mem folded_keys folded
             || not (Hashtbl.mem directories (Filename.dirname key)))
        then wait_for_adoption ();
        None)

let run_in_process ?poll ?(log = true)
    ?(hooks = Compiler_request_state.no_hooks) (request : request) =
  if log then log_compiler_request request;
  match List.rev request.args with
  | [] ->
    {
      Process.status = Unix.WEXITED 2;
      stdout = "";
      stderr = "missing compiler input";
    }
  | input :: reversed_argv -> (
    match
      Rescript_compiler_driver.run_request_with_hooks ~hooks ~cwd:request.cwd
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

let task ?(hooks = Compiler_scheduler.no_compile_hooks) request =
  let cancelled = Atomic.make false in
  let gate = hooks.Compiler_scheduler.gate in
  (* A speculation logs its call once adopted, on its worker as before, so
     that the test-only REWATCH_COMPILER_CALL_LOG lists only adopted calls. *)
  (* A logging failure fails every later gate pass too, the final one outside
     the compiler included, so nothing is written after it and the task fails
     as when a request logs at its start. The task runs on one domain. *)
  let logged = ref (if Option.is_none gate then Ok () else Error None) in
  let wait_for_adoption () =
    Option.iter Speculation_gate.wait gate;
    match !logged with
    | Ok () -> ()
    | Error (Some error) -> raise error
    | Error None -> (
      match log_compiler_request request with
      | () -> logged := Ok ()
      | exception error ->
        logged := Error (Some error);
        raise error)
  in
  let redirect =
    cmi_redirect ~cwd:request.cwd ~cmi_directories:hooks.cmi_directories
      ~wait_for_adoption hooks.cmi_redirects
  in
  let compiler_hooks =
    Compiler_request_state.
      {
        before_effect = wait_for_adoption;
        interface_computed =
          (match hooks.on_interface_computed with
          | None -> fun _ _ -> ()
          | Some computed ->
            fun path bytes ->
              computed
                (if Filename.is_relative path then
                   Filename.concat request.cwd path
                 else path)
                bytes);
        redirect_cmi =
          (match gate with
          | None -> redirect
          | Some gate ->
            fun path ->
              (* Interfaces load throughout type checking: an abandoned
                 speculation stops at the next one. *)
              if Speculation_gate.is_abandoned gate then
                raise Speculation_gate.Abandoned;
              redirect path);
      }
  in
  Process.concurrent_task
    ~cancel:(fun () ->
      Atomic.set cancelled true;
      Option.iter Speculation_gate.cancel gate)
    (fun () ->
      (* An abandoned speculation that has not started yet does no work. *)
      Option.iter
        (fun gate ->
          if Speculation_gate.is_abandoned gate then
            raise Speculation_gate.Abandoned)
        gate;
      let result =
        run_in_process ~log:(Option.is_none gate) ~hooks:compiler_hooks
          ~poll:(fun () ->
            if Atomic.get cancelled then raise (Process.Interrupted 15))
          request
      in
      (* A speculation's result, and its publication, wait for adoption. *)
      wait_for_adoption ();
      result)

let run_tasks ?poll ?on_complete tasks =
  Process.run_tasks
    ~max_jobs:(Compiler_execution_mode.configured_count ())
    ?poll ?on_complete tasks

let run_requests ?poll ?on_complete requests =
  requests |> List.map task |> run_tasks ?poll ?on_complete

let parse_request ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  File_util.ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents =
    if config.ppx_flags = [] then ""
    else File_util.read_file (Filename.concat config.root path)
  in
  let args = Compiler_args.parser_arguments ~config ~contents ~path in
  {args; cwd = build_dir}

(* Runs on the worker that parsed [path], right after the parse, so that
   publishing the outputs of a large project's parses does not run on one
   domain afterwards. The source is kept: compiling reads it again. *)
let publish_parse ~build_dir ~ocaml_dir ~(config : Config.t) path =
  File_util.copy_build_file ~keep:false
    (Filename.concat build_dir (Source.ast_path path))
    (Build_artifacts.published_ast_path ~ocaml_dir path);
  File_util.copy_build_file
    (Filename.concat config.root path)
    (Filename.concat ocaml_dir (Filename.basename path))

(* A parse that printed warnings is published later, on the main domain, once
   the build has registered the cleanup that makes the next build parse it
   again and repeat them. Published now, it would outlive a build interrupted
   before that, and the next build would not report the warnings. *)
let run_parses ?poll ?on_complete parses =
  parses
  |> List.map (fun (build_dir, ocaml_dir, config, path) ->
      Process.map_result
        (task (parse_request ~build_dir ~config path))
        (fun result ->
          if Process.succeeded result && result.stderr = "" then
            publish_parse ~build_dir ~ocaml_dir ~config path;
          result))
  |> run_tasks ?poll ?on_complete

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
        (* Dependents read the published .cmi and .cmj, so the store keeps
           them; nothing in the build reads the other copies again. *)
        (match artifact with
        | Cmi ->
          cmi_change :=
            if File_util.copy_build_file_if_different source destination then
              Compiler_scheduler.Cmi_changed
            else Compiler_scheduler.Cmi_unchanged
        | Required "cmj" -> File_util.copy_build_file source destination
        | Required _ -> File_util.copy_build_file ~keep:false source destination
        | Optional _ ->
          File_util.copy_optional_build_file ~keep:false source destination);
        Build_file_store.evict source)
      artifacts;
    !cmi_change
  with error ->
    raise (Compiler_scheduler.Publication_failure (error, !cmi_change))

let namespace_task ~runtime ~build_dir ~ocaml_dir ~entry ~package_dirty ~force
    namespace modules =
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
    File_util.write_file_atomic ~ensure_parent:false mlmap contents;
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
            task
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
                cwd = build_dir;
              };
          publish =
            (fun result ->
              if not (Process.succeeded result) then
                raise
                  (Compiler_scheduler.Build_failure
                     (result.Process.stderr ^ result.stdout));
              let cmi_change =
                publish_compiler_artifacts ~artifact_dir:build_dir ~ocaml_dir
                  ~basename:namespace
                  [Cmi; Required "cmj"; Required "cmt"; Required "mlmap"]
              in
              Compiler_scheduler.{stderr = result.stderr; cmi_change});
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
           the one already published after compiling the interface, which
           this compile read again. *)
         ignore
           (publish_compiler_artifacts ~artifact_dir ~ocaml_dir ~basename
              [Required "cmj"; Optional "cmt"]);
         Build_file_store.evict
           (Filename.concat artifact_dir (basename ^ ".cmi"));
         Compiler_scheduler.Cmi_unchanged
       | Source.Implementation ->
         publish_compiler_artifacts ~artifact_dir ~ocaml_dir ~basename
           [Cmi; Required "cmj"; Optional "cmt"]);
    let source = Filename.concat config.root path in
    let build_source = Filename.concat build_dir path in
    File_util.ensure_dir (Filename.dirname build_source);
    File_util.copy_build_file ~keep:false source build_source;
    let published_source = Filename.concat ocaml_dir (Filename.basename path) in
    (* Usually published unchanged when the source was parsed. *)
    ignore
      (File_util.copy_build_file_if_different ~keep:false source
         published_source);
    List.iter Build_file_store.evict
      [
        source;
        published_source;
        Filename.concat build_dir (Source.ast_path path);
      ];
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
            Option.iter
              (File_util.write_build_file ~keep:false build_output)
              (Build_file_store.read output);
            File_util.copy_optional_build_file ~keep:false (output ^ ".map")
              (build_output ^ ".map")))
        config.package_specs;
      List.iter
        (fun spec ->
          let output = Build_artifacts.generated_js_path config path spec in
          Build_file_store.evict output;
          Build_file_store.evict (output ^ ".map"))
        config.package_specs);
    Compiler_scheduler.{stderr; cmi_change = !cmi_change}
  with
  | Compiler_scheduler.Publication_failure _ as error -> raise error
  | error -> raise (Compiler_scheduler.Publication_failure (error, !cmi_change))
