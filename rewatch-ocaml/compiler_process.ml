open Build_artifacts
open File_util

let contains_text value text =
  try
    ignore (Str.search_forward (Str.regexp_string text) value 0);
    true
  with Not_found -> false

let retain_critical_external_warnings stderr =
  let marker = "`(. ...)` uncurried syntax" in
  if not (contains_text stderr marker) then ""
  else
    stderr |> Str.global_replace (Str.regexp_string "\r\n") "\n"
    |> Str.split_delim (Str.regexp_string "\n\n\n")
    |> List.filter (fun block -> contains_text block marker)
    |> String.concat "\n\n\n"

let parse_job ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents = read_file (Filename.concat config.root path) in
  let args =
    Compiler_args.compiler_flags
      ~ppx_flags:(Compiler_args.filter_ppx_flags config.ppx_flags contents)
      ~source_maps:false ~watch:false ~gentype:false config
    @ [
        "-absname";
        "-bs-ast";
        "-o";
        ast;
        Filename.concat
          (Filename.concat Filename.parent_dir_name Filename.parent_dir_name)
          path;
      ]
  in
  Process.{program = bsc; args; cwd = build_dir}

let ast_dependencies ~build_dir ast =
  let channel = open_in_bin (Filename.concat build_dir ast) in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      (try ignore (input_line channel) with End_of_file -> ());
      let rec loop acc =
        match input_line channel with
        | line ->
          let line = String.trim line in
          if line = "" then loop acc
          else if not (Filename.is_relative line) then List.rev acc
          else
            let dependency = String.split_on_char '.' line |> List.hd in
            loop (dependency :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

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
    try
      let channel = open_in_bin mlmap in
      Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
        Some (really_input_string channel (in_channel_length channel)))
    with Sys_error _ -> None
  in
  let mlmap_changed = previous_contents <> Some contents in
  if mlmap_changed then (
    let channel = open_out_bin mlmap in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel contents));
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
          copy_file_if_changed ~ensure_parent:false
            (Filename.concat build_dir (namespace ^ ".cmi"))
            (Filename.concat ocaml_dir (namespace ^ ".cmi"));
          copy_existing_file ~ensure_parent:false
            (Filename.concat build_dir (namespace ^ ".cmj"))
            (Filename.concat ocaml_dir (namespace ^ ".cmj"));
          copy_existing_file ~ensure_parent:false
            (Filename.concat build_dir (namespace ^ ".cmt"))
            (Filename.concat ocaml_dir (namespace ^ ".cmt"));
          copy_existing_file ~ensure_parent:false mlmap
            (Filename.concat ocaml_dir (namespace ^ ".mlmap")) )

let run_post_build (config : Config.t) path =
  match config.js_post_build with
  | None -> ()
  | Some command ->
    List.iter
      (fun spec ->
        let output = generated_js_path config path spec in
        let env, program, args = Platform.post_build_command ~command ~output in
        let result =
          match env with
          | None -> Process.run ~cwd:config.root program args
          | Some env -> Process.run ~env ~cwd:config.root program args
        in
        if not (Process.succeeded result) then (
          let captured = result.stderr ^ result.stdout in
          raise
            (Compiler_scheduler.Build_failure
               (Printf.sprintf "js-post-build command failed for %s%s" output
                  (if captured = "" then "" else "\n" ^ captured))));
        if result.stdout <> "" then print_string result.stdout;
        if result.stderr <> "" then prerr_string result.stderr)
      config.package_specs

let compile_job ~bsc ~runtime ~build_dir ~watch ~(config : Config.t)
    ~dependency_dirs (module_ : Source.module_) ~is_interface path =
  let ast = Source.ast_path path in
  let namespace_args = Compiler_args.namespace_args config module_.name in
  let interface_args =
    if not is_interface && Option.is_some module_.interface then
      ["-bs-read-cmi"]
    else []
  in
  let output_args =
    if is_interface then []
    else
      List.concat_map
        (fun spec ->
          ["-bs-package-output"; Compiler_args.package_output config path spec])
        config.package_specs
  in
  let args =
    namespace_args @ interface_args
    @ ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
    @ ["-runtime-path"; runtime]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ Compiler_args.compiler_flags ~source_maps:true ~watch ~gentype:true config
    @ Compiler_args.gentype_dependency_args config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  Process.{program = bsc; args; cwd = build_dir}

let publish ~build_dir ~ocaml_dir ~watch ~watch_output_paths ~is_local
    ~(config : Config.t) ~is_interface path result =
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  let basename = Source.compiler_asset_basename config path in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let extensions =
    if is_interface then ["cmi"; "cmti"] else ["cmi"; "cmj"; "cmt"]
  in
  List.iter
    (fun extension ->
      let source = Filename.concat artifact_dir (basename ^ "." ^ extension) in
      let destination = Filename.concat ocaml_dir (basename ^ "." ^ extension) in
      if extension = "cmi" then
        copy_file_if_changed ~ensure_parent:false source destination
      else if extension = "cmt" || extension = "cmti" then
        copy_optional_existing_file ~ensure_parent:false source destination
      else copy_existing_file ~ensure_parent:false source destination)
    extensions;
  let source = Filename.concat config.root path in
  let build_source = Filename.concat build_dir path in
  ensure_dir (Filename.dirname build_source);
  copy_existing_file ~ensure_parent:false source build_source;
  copy_existing_file ~ensure_parent:false source
    (Filename.concat ocaml_dir (Filename.basename path));
  if not is_interface then (
    List.iter
      (fun spec ->
        if spec.Config.in_source then (
          let output = generated_js_path config path spec in
          let build_output =
            generated_build_js_path ~build_dir config path spec
          in
          ensure_dir (Filename.dirname build_output);
          if Sys.file_exists output then
            copy_existing_file ~ensure_parent:false output build_output;
          if Sys.file_exists (output ^ ".map") then
            copy_existing_file ~ensure_parent:false (output ^ ".map")
              (build_output ^ ".map")
          else remove_file (build_output ^ ".map")))
      config.package_specs;
    run_post_build config path;
    if watch then
      List.iter
        (fun spec ->
          let output = generated_js_path config path spec in
          List.iter
            (fun generated ->
              if
                Sys.file_exists generated
                && Hashtbl.mem watch_output_paths generated
              then Unix.rename generated (generated ^ ".rewatch-pending"))
            [output; output ^ ".map"])
        config.package_specs);
  stderr
