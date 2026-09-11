let retain_critical_external_warnings stderr =
  let marker = "`(. ...)` uncurried syntax" in
  if not (String_util.contains stderr marker) then ""
  else
    stderr
    |> Str.global_replace (Str.regexp_string "\r\n") "\n"
    |> Str.split_delim (Str.regexp_string "\n\n\n")
    |> List.filter (fun block -> String_util.contains block marker)
    |> String.concat "\n\n\n"

let parse_job ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  File_util.ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents =
    if config.ppx_flags = [] then ""
    else File_util.read_file (Filename.concat config.root path)
  in
  let args = Compiler_args.parser_arguments ~config ~contents ~path in
  Process.{program = bsc; args; cwd = build_dir}

let ast_dependencies ~build_dir ast =
  (Ast_header.read (Filename.concat build_dir ast)).dependencies

let namespace_job ~bsc ~runtime ~build_dir ~ocaml_dir ~entry ~package_dirty
    namespace modules =
  let mlmap = Filename.concat build_dir (namespace ^ ".mlmap") in
  let contents =
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer "randjbuildsystem\n";
    modules
    |> List.filter (fun module_ -> Some module_.Source.name <> entry)
    |> List.filter (fun module_ ->
        Source.is_non_exotic_module_name module_.Source.name)
    |> List.map (fun module_ -> module_.Source.name)
    |> List.sort String.compare
    |> List.iter (fun name ->
        Buffer.add_string buffer name;
        Buffer.add_char buffer '\n');
    Buffer.contents buffer
  in
  let previous_contents =
    try Some (File_util.read_file mlmap) with Sys_error _ -> None
  in
  let mlmap_changed = previous_contents <> Some contents in
  if mlmap_changed then
    File_util.write_file_atomic ~ensure_parent:false ~perm:0o644 mlmap contents;
  let outputs_exist =
    ["cmi"; "cmj"; "cmt"; "mlmap"]
    |> List.for_all (fun extension ->
        Sys.file_exists
          (Filename.concat ocaml_dir (namespace ^ "." ^ extension)))
  in
  if not (package_dirty || mlmap_changed || not outputs_exist) then None
  else
    Some
      ( Process.
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
          },
        fun result ->
          if not (Process.succeeded result) then
            raise
              (Compiler_scheduler.Build_failure
                 (result.Process.stderr ^ result.stdout));
          File_util.copy_file_if_changed ~ensure_parent:false
            (Filename.concat build_dir (namespace ^ ".cmi"))
            (Filename.concat ocaml_dir (namespace ^ ".cmi"));
          File_util.copy_existing_file ~ensure_parent:false
            (Filename.concat build_dir (namespace ^ ".cmj"))
            (Filename.concat ocaml_dir (namespace ^ ".cmj"));
          File_util.copy_existing_file ~ensure_parent:false
            (Filename.concat build_dir (namespace ^ ".cmt"))
            (Filename.concat ocaml_dir (namespace ^ ".cmt"));
          File_util.copy_existing_file ~ensure_parent:false mlmap
            (Filename.concat ocaml_dir (namespace ^ ".mlmap")) )

let post_build_tasks (config : Config.t) path =
  match config.js_post_build with
  | None -> []
  | Some command ->
    List.map
      (fun spec ->
        let output = Build_artifacts.generated_js_path config path spec in
        let env, program, args = Platform.post_build_command ~command ~output in
        (output, Process.task ?env Process.{program; args; cwd = config.root}))
      config.package_specs

let compile_job ~bsc ~build_dir ~(config : Config.t) ~common_args
    (module_ : Source.module_) ~is_interface path =
  let args =
    Compiler_args.compiler_arguments_with_common ~config ~common_args
      ~module_name:module_.name ~is_interface
      ~has_interface:(Option.is_some module_.interface)
      ~path
  in
  Process.{program = bsc; args; cwd = build_dir}

let publish ~build_dir ~ocaml_dir ~is_local ~(config : Config.t) ~is_interface
    path result =
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  let basename = Source.compiler_asset_basename config path in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let extensions =
    if is_interface then ["cmi"; "cmti"] else ["cmi"; "cmj"; "cmt"]
  in
  let cmi_change = ref Compiler_scheduler.Cmi_change_unknown in
  try
    List.iter
      (fun extension ->
        let source =
          Filename.concat artifact_dir (basename ^ "." ^ extension)
        in
        let destination =
          Filename.concat ocaml_dir (basename ^ "." ^ extension)
        in
        if extension = "cmi" then
          cmi_change :=
            if
              File_util.copy_file_if_different ~ensure_parent:false source
                destination
            then Compiler_scheduler.Cmi_changed
            else Compiler_scheduler.Cmi_unchanged
        else if extension = "cmt" || extension = "cmti" then
          File_util.copy_optional_existing_file ~ensure_parent:false source
            destination
        else
          File_util.copy_existing_file ~ensure_parent:false source destination)
      extensions;
    let source = Filename.concat config.root path in
    let build_source = Filename.concat build_dir path in
    File_util.ensure_dir (Filename.dirname build_source);
    File_util.copy_existing_file ~ensure_parent:false source build_source;
    File_util.copy_existing_file ~ensure_parent:false source
      (Filename.concat ocaml_dir (Filename.basename path));
    if not is_interface then
      List.iter
        (fun spec ->
          if spec.Config.in_source then (
            let output = Build_artifacts.generated_js_path config path spec in
            let build_output =
              Build_artifacts.generated_build_js_path ~build_dir config path
                spec
            in
            File_util.ensure_dir (Filename.dirname build_output);
            if Sys.file_exists output then
              File_util.copy_existing_file ~ensure_parent:false output
                build_output;
            if Sys.file_exists (output ^ ".map") then
              File_util.copy_existing_file ~ensure_parent:false
                (output ^ ".map") (build_output ^ ".map")
            else File_util.remove_file (build_output ^ ".map")))
        config.package_specs;
    Compiler_scheduler.{stderr; cmi_change = !cmi_change}
  with exn ->
    raise (Compiler_scheduler.Publication_failure (exn, !cmi_change))
