(* Editors type-check unsaved contents of a source file. The contents are
   written to a copy in lib/rescript/incremental, where editor analysis looks
   for the typed tree of the copy, and the copy is compiled with the build's
   arguments by the embedded compiler. *)

let incremental_dir_name = "incremental"

let replace_all ~pattern ~with_ text =
  if pattern = "" then text
  else Str.global_replace (Str.regexp_string pattern) with_ text

let run ?poll ~verbosity ~stdin ~across_files path =
  let contents =
    if stdin then Some (Format.read_stdin_interruptibly ?poll ()) else None
  in
  let ({
         config;
         source;
         relative;
         contents;
         source_kind;
         has_interface;
         dependency_dirs;
         runtime;
         project_root;
       }
        : Compiler_args_command.context) =
    Compiler_args_command.context ~verbosity ~command:"typecheck" ?contents path
  in
  let artifact_dir = Build_artifacts.artifact_dir config.root in
  File_util.ensure_dir (Filename.concat artifact_dir incremental_dir_name);
  (* Spelled from the caller's project root, like the source in the
     diagnostics, so that the copy's path can be mapped back to the source. *)
  let input =
    Filename.concat
      (Filename.concat
         (Build_artifacts.artifact_dir project_root)
         incremental_dir_name)
      (Filename.basename source)
  in
  File_util.write_file input contents;
  let args =
    Compiler_args.typecheck_arguments ~project_root ~config ~runtime
      ~dependency_dirs
      ~module_name:(Source.module_name source)
      ~source_kind ~has_interface ~contents
      ~include_dirs:(if across_files then [incremental_dir_name] else [])
      ~input
  in
  Output.debug ~channel:stderr ~verbosity
    ("Type-checking with: " ^ String.concat " " args);
  let result = Compiler_process.run ?poll {args; cwd = artifact_dir} in
  (* Diagnostics name the source, not its copy. *)
  print_string
    (replace_all ~pattern:input
       ~with_:(Filename.concat project_root relative)
       (result.stdout ^ result.stderr));
  Process.succeeded result
