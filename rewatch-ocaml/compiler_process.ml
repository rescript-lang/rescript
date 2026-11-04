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

let task request =
  let cancelled = Atomic.make false in
  Process.concurrent_task
    ~cancel:(fun () -> Atomic.set cancelled true)
    (fun () ->
      run_in_process
        ~poll:(fun () ->
          if Atomic.get cancelled then raise (Process.Interrupted 15))
        request)

let run_tasks ?poll ?on_complete tasks =
  Process.run_tasks
    ~max_jobs:(Compiler_execution_mode.configured_count ())
    ?poll ?on_complete tasks

let run_requests requests = requests |> List.map task |> run_tasks

(* A request writes its ASTs, interfaces and typed trees into the artifact
   directory that later requests and builds read. They are held in memory
   until the request finished (Build_file_store.set_held_writes, which the
   build installs with this predicate), so a failed request leaves the
   previous AST and interface on disk, and a build killed before it published
   an interface leaves the typed tree that marks its module compiled stale. *)
let is_held path =
  List.exists
    (Filename.check_suffix path)
    [".ast"; ".iast"; ".cmi"; ".cmt"; ".cmti"]

(* Renamed into place, so that neither a reader nor a build killed while
   writing sees part of a file. On Windows the rename fails while another
   process, such as an editor, has the file open without delete sharing; the
   file is then written in place, and a write that fails removes its partial
   file. *)
let write_held path contents =
  try File_util.write_file_atomic ~ensure_parent:false path contents
  with Sys_error _ | Unix.Unix_error _ -> (
    try File_util.write_file path contents
    with Sys_error message ->
      File_util.remove_file_best_effort path;
      raise
        (Sys_error
           (if String_util.contains message path then message
            else path ^ ": " ^ message)))

let parse_request ~artifact_dir ~(config : Config.t) path =
  let contents =
    if config.ppx_flags = [] then ""
    else File_util.read_file (Filename.concat config.root path)
  in
  let args = Compiler_args.parser_arguments ~config ~contents ~path in
  {args; cwd = artifact_dir}

(* An AST is written even when it is unchanged: its modification time records
   when its source was parsed. *)
let publish_parse ~artifact_dir path =
  ignore
    (Build_file_store.publish_held ~if_changed:false ~write:write_held
       (Build_artifacts.ast_path ~artifact_dir path))

(* Runs on the worker that parsed [path], right after the parse, so that
   publishing the ASTs of a large project's parses does not run on one domain
   afterwards. A parse that printed warnings is published later, on the main
   domain, once the build has registered the cleanup that makes the next build
   parse it again and repeat them. Published now, it would outlive a build
   interrupted before that, and the next build would not report the
   warnings. *)
let run_parses ?poll ?on_complete parses =
  parses
  |> List.map (fun (artifact_dir, config, path) ->
      Process.map_result
        (task (parse_request ~artifact_dir ~config path))
        (fun result ->
          if not (Process.succeeded result) then
            Build_file_store.forget
              (Build_artifacts.ast_path ~artifact_dir path)
          else if result.stderr = "" then publish_parse ~artifact_dir path;
          result))
  |> run_tasks ?poll ?on_complete

type compiler_artifact = Cmi | Required of string | Typed_tree of string

(* A typed tree marks its module compiled, so it is published last. Not every
   compile writes one (-bs-no-bin-annot). *)
let publish_typed_tree path =
  match
    Build_file_store.publish_held ~if_changed:false ~write:write_held path
  with
  | exception Not_found -> ()
  (* Once published, it is only read by gentype, which then finds it on disk. *)
  | _ -> Build_file_store.evict path

(* The compiler wrote the other artifacts into the artifact directory:
   interfaces are held until now and written only when they changed, which
   keeps the modification time that tells dependents whether to compile
   again. Callers list typed trees last. *)
let publish_compiler_artifacts ~artifact_dir ~basename artifacts =
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
  try
    List.iter
      (fun artifact ->
        match artifact with
        | Cmi ->
          let path = Filename.concat artifact_dir (basename ^ ".cmi") in
          let changed =
            try
              Build_file_store.publish_held ~if_changed:true ~write:write_held
                path
            with Not_found ->
              (* The compiler does not write an interface identical to the
                 one on disk (Cmi_format.create_cmi). *)
              if Build_file_store.exists path then false
              else raise (Unix.Unix_error (Unix.ENOENT, "open", path))
          in
          cmi_change :=
            if changed then Compiler_scheduler.Cmi_changed
            else Compiler_scheduler.Cmi_unchanged
        | Required extension ->
          let path =
            Filename.concat artifact_dir (basename ^ "." ^ extension)
          in
          if not (Build_file_store.exists path) then
            raise (Unix.Unix_error (Unix.ENOENT, "open", path))
        | Typed_tree extension ->
          publish_typed_tree
            (Filename.concat artifact_dir (basename ^ "." ^ extension)))
      artifacts;
    !cmi_change
  with error ->
    raise (Compiler_scheduler.Publication_failure (error, !cmi_change))

let namespace_task ~runtime ~artifact_dir ~entry ~package_dirty ~force namespace
    modules =
  let mlmap = Filename.concat artifact_dir (namespace ^ ".mlmap") in
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
  let typed_tree = Filename.concat artifact_dir (namespace ^ ".cmt") in
  let outputs_current =
    ["cmi"; "cmj"; "cmt"]
    |> List.for_all (fun extension ->
        File_util.is_regular_file
          (Filename.concat artifact_dir (namespace ^ "." ^ extension)))
  in
  if not (force || package_dirty || mlmap_changed || not outputs_current) then
    None
  else (
    (* The typed tree, published last, marks the namespace compiled: without
       it, a build that fails or is killed before then compiles it again. *)
    File_util.remove_file typed_tree;
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
                cwd = artifact_dir;
              };
          publish =
            (fun result ->
              if not (Process.succeeded result) then (
                List.iter
                  (fun extension ->
                    Build_file_store.forget
                      (Filename.concat artifact_dir (namespace ^ extension)))
                  [".cmi"; ".cmt"];
                raise
                  (Compiler_scheduler.Build_failure
                     (result.Process.stderr ^ result.stdout)));
              let cmi_change =
                publish_compiler_artifacts ~artifact_dir ~basename:namespace
                  [Cmi; Required "cmj"; Typed_tree "cmt"]
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

let compile_request ~artifact_dir ~(config : Config.t) ~common_args
    (module_ : Source.module_) ~source_kind path =
  let args =
    Compiler_args.compiler_arguments_with_common ~config ~common_args
      ~module_name:module_.name ~source_kind
      ~has_interface:(Option.is_some module_.interface)
      ~path
  in
  {args; cwd = artifact_dir}

(* A compile that failed leaves the interface on disk. It publishes the
   partial typed tree the compiler wrote, which editor tooling reads, but
   first dates its AST back, so that the next build compiles the module again
   even if this one is killed before the scheduler records the failure (which
   dates the AST back as well). Best effort, so that a file another process
   holds does not replace the compile error with a disk error. *)
let discard_compile ~artifact_dir ~(config : Config.t) path =
  let artifact extension =
    Filename.concat artifact_dir
      (Source.compiler_asset_basename config path ^ extension)
  in
  Build_file_store.forget (artifact ".cmi");
  (try
     Build_artifacts.invalidate_ast
       (Build_artifacts.ast_path ~artifact_dir path)
   with Unix.Unix_error _ -> ());
  let typed_tree =
    artifact (if Filename.check_suffix path ".resi" then ".cmti" else ".cmt")
  in
  try publish_typed_tree typed_tree
  with Sys_error _ -> Build_file_store.forget typed_tree

let publish ~artifact_dir ~is_local ~(config : Config.t) ~has_interface
    ~source_kind path result =
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  let basename = Source.compiler_asset_basename config path in
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
  try
    (cmi_change :=
       match source_kind with
       | Source.Interface ->
         publish_compiler_artifacts ~artifact_dir ~basename
           [Cmi; Typed_tree "cmti"]
       | Source.Implementation when has_interface ->
         (* The implementation is compiled with -bs-read-cmi, so its .cmi is
           the one published after compiling the interface. *)
         ignore
           (publish_compiler_artifacts ~artifact_dir ~basename
              [Required "cmj"; Typed_tree "cmt"]);
         Compiler_scheduler.Cmi_unchanged
       | Source.Implementation ->
         publish_compiler_artifacts ~artifact_dir ~basename
           [Cmi; Required "cmj"; Typed_tree "cmt"]);
    (* The parse and the typed tree's source digest read the source through
       the store. *)
    List.iter Build_file_store.evict
      [
        Filename.concat config.root path;
        Build_artifacts.ast_path ~artifact_dir path;
      ];
    (match source_kind with
    | Source.Interface -> ()
    | Source.Implementation ->
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
