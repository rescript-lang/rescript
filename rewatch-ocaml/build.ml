exception Error of string
exception Stop_watch
exception Build_failure of string

let ensure_dir path =
  let rec loop path =
    if path = "" || path = "." || Sys.file_exists path then ()
    else (
      loop (Filename.dirname path);
      Unix.mkdir path 0o755)
  in
  loop path

let copy_file source destination =
  if Sys.file_exists source then (
    ensure_dir (Filename.dirname destination);
    let input = open_in_bin source in
    let output = open_out_bin destination in
    Fun.protect
      ~finally:(fun () ->
        close_in_noerr input;
        close_out_noerr output)
      (fun () ->
        really_input_string input (in_channel_length input)
        |> output_string output))

let files_equal first second =
  if not (Sys.file_exists first && Sys.file_exists second) then false
  else
    let first_stat = Unix.stat first in
    let second_stat = Unix.stat second in
    first_stat.Unix.st_size = second_stat.Unix.st_size
    && let first_channel = open_in_bin first in
       let second_channel = open_in_bin second in
       Fun.protect
         ~finally:(fun () ->
           close_in_noerr first_channel;
           close_in_noerr second_channel)
         (fun () ->
           let buffer_size = 65_536 in
           let first_buffer = Bytes.create buffer_size in
           let second_buffer = Bytes.create buffer_size in
           let rec loop () =
             let first_count = input first_channel first_buffer 0 buffer_size in
             let second_count = input second_channel second_buffer 0 buffer_size in
             first_count = second_count
             && (first_count = 0
                || (Bytes.sub first_buffer 0 first_count
                    = Bytes.sub second_buffer 0 second_count
                   && loop ()))
           in
           loop ())

let copy_file_if_changed source destination =
  if not (files_equal source destination) then copy_file source destination

let compiler_log_path root directory =
  Filename.concat (Filename.concat root directory) ".compiler.log"

let strip_ansi content =
  let length = String.length content in
  let output = Buffer.create length in
  let rec skip_csi index =
    if index >= length then index
    else
      let code = Char.code content.[index] in
      if code >= 0x40 && code <= 0x7e then index + 1
      else skip_csi (index + 1)
  in
  let rec loop index =
    if index < length then
      if
        (content.[index] = '\027' || content.[index] = '\155')
        && index + 1 < length && content.[index + 1] = '['
      then loop (skip_csi (index + 2))
      else (
        Buffer.add_char output content.[index];
        loop (index + 1))
  in
  loop 0;
  Buffer.contents output

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

let initialize_compiler_log root =
  let path = compiler_log_path root "lib/bs" in
  ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    Printf.fprintf channel "#Start(%.6f)\n" (Unix.gettimeofday ()))

let append_compiler_log root content =
  let channel =
    open_out_gen [Open_wronly; Open_append; Open_binary] 0o644
      (compiler_log_path root "lib/bs")
  in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel (strip_ansi content))

let finalize_compiler_log root =
  append_compiler_log root
    (Printf.sprintf "#Done(%.6f)\n" (Unix.gettimeofday ()));
  copy_file (compiler_log_path root "lib/bs")
    (compiler_log_path root "lib/ocaml")

let modification_time path =
  if Sys.file_exists path then Some (Unix.stat path).Unix.st_mtime else None

let remove_file path = if Sys.file_exists path then (try Sys.remove path with Sys_error _ -> ())

let read_lock_owner path =
  try
    let channel = open_in path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      Some (input_line channel))
  with Sys_error _ | End_of_file -> None

let process_is_active value =
  try
    let pid = int_of_string value in
    Unix.kill pid 0;
    let executable = Printf.sprintf "/proc/%d/exe" pid in
    if Sys.file_exists executable then
      (try
         let basename = Unix.realpath executable |> Filename.basename in
         String.starts_with ~prefix:"rescript" basename
       with Unix.Unix_error _ -> true)
    else true
  with
  | Failure _ | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (Unix.EPERM, _, _) -> true

let workspace_lock_root folder =
  let declares_workspaces directory =
    let path = Filename.concat directory "package.json" in
    if not (Sys.file_exists path) then false
    else
      try
        match Yojson.Safe.from_file path with
        | `Assoc fields -> List.mem_assoc "workspaces" fields
        | _ -> false
      with Yojson.Json_error _ | Sys_error _ -> false
  in
  let rec loop directory =
    if declares_workspaces directory then directory
    else
    let parent = Filename.dirname directory in
    if parent = directory then folder else loop parent
  in
  loop folder

let acquire_build_lock root =
  let lock_dir = Filename.concat root "lib" in
  ensure_dir lock_dir;
  let path = Filename.concat lock_dir "build.lock" in
  let pid = string_of_int (Unix.getpid ()) in
  let candidate = Filename.temp_file ~temp_dir:lock_dir ".build-lock-" ".tmp" in
  let channel = open_out candidate in
  output_string channel pid;
  close_out channel;
  let clear_stale_lock () =
    let takeover = path ^ ".takeover" in
    try
      Unix.link candidate takeover;
      Fun.protect
        ~finally:(fun () -> remove_file takeover)
        (fun () ->
          match read_lock_owner path with
          | Some owner when process_is_active owner -> ()
          | _ -> remove_file path);
      true
    with Unix.Unix_error (Unix.EEXIST, _, _) ->
      (match read_lock_owner takeover with
      | Some owner when process_is_active owner -> ()
      | _ -> remove_file takeover);
      false
  in
  let rec acquire attempts =
    if attempts = 0 then
      raise (Error "Timed out waiting for another ReScript build to finish");
    try Unix.link candidate path
    with Unix.Unix_error (Unix.EEXIST, _, _) -> (
      match read_lock_owner path with
      | Some owner when process_is_active owner ->
        ignore (Unix.select [] [] [] 0.05);
        acquire (attempts - 1)
      | _ ->
        if not (clear_stale_lock ()) then ignore (Unix.select [] [] [] 0.05);
        acquire (attempts - 1))
  in
  Fun.protect ~finally:(fun () -> remove_file candidate) (fun () -> acquire 1200);
  let released = ref false in
  fun () ->
    if not !released then (
      if read_lock_owner path = Some pid then remove_file path;
      released := true)

let rec files_under directory =
  try
    if not (Sys.file_exists directory) then []
    else if (Unix.lstat directory).Unix.st_kind <> Unix.S_DIR then [directory]
    else
      Sys.readdir directory |> Array.to_list
      |> List.concat_map (fun name ->
           files_under (Filename.concat directory name))
  with Sys_error _ | Unix.Unix_error _ -> []

let generated_js_path (config : Config.t) path (spec : Config.package_spec) =
  let directory = Filename.dirname path in
  let output_dir =
    if spec.in_source then directory
    else
      Filename.concat
        (match spec.module_format with Config.Esmodule -> "lib/es6" | Config.Commonjs -> "lib/js")
        directory
  in
  Filename.concat config.root
    (Filename.concat output_dir
      (Filename.remove_extension (Filename.basename path) ^ Config.package_spec_suffix config spec))

let generated_build_js_path ~build_dir (config : Config.t) path
    (spec : Config.package_spec) =
  Filename.concat build_dir
    (Filename.remove_extension path ^ Config.package_spec_suffix config spec)

let prepare_watch_output watch_outputs watch_output_paths ~dirty_ast output =
  if
    (not (Sys.file_exists output))
    && not (Hashtbl.mem watch_output_paths output)
  then (
    let pending = output ^ ".rewatch-pending" in
    remove_file pending;
    Hashtbl.add watch_output_paths output ();
    watch_outputs := (output, pending, dirty_ast) :: !watch_outputs)

let with_root_options (config : Config.t) (root_config : Config.t) =
  {
    config with
    package_specs = root_config.package_specs;
    suffix = root_config.suffix;
    jsx_args = root_config.jsx_args;
    source_map_args = root_config.source_map_args;
    source_map_dev = root_config.source_map_dev;
    experimental_args = root_config.experimental_args;
    gentype_args =
      (if config.gentype_args = [] then []
       else
         config.gentype_args
         @ ["-bs-gentype-bsb-project-root"; root_config.root]);
  }

let cleanup_stale ~root ~ocaml_dir (config : Config.t) modules =
  let expected_artifacts = Hashtbl.create (List.length modules * 8) in
  let owned_output_names = Hashtbl.create (List.length modules * 2) in
  let add_expected base extensions =
    List.iter
      (fun extension ->
        Hashtbl.replace expected_artifacts (base ^ extension) ())
      extensions
  in
  let previous_ast_count = ref 0 in
  files_under ocaml_dir
  |> List.iter (fun path ->
       let basename = Filename.basename path in
       if Filename.check_suffix basename ".ast" then (
         incr previous_ast_count;
         Hashtbl.replace owned_output_names
           (Filename.chop_suffix basename ".ast") ())
       else if Filename.check_suffix basename ".iast" then (
         incr previous_ast_count;
         Hashtbl.replace owned_output_names
           (Filename.chop_suffix basename ".iast") ());
     );
  List.iter
    (fun module_ ->
      let source_base =
        module_.Source.implementation |> Filename.basename
        |> Filename.remove_extension
      in
      let compiler_base =
        Source.compiler_basename config module_.Source.name
      in
      Hashtbl.replace owned_output_names source_base ();
      add_expected source_base [".ast"; ".res"];
      if Option.is_some module_.Source.interface then
        add_expected source_base [".iast"; ".resi"];
      add_expected compiler_base [".cmi"; ".cmj"; ".cmt"; ".cmti"])
    modules;
  Option.iter
    (fun namespace ->
      let base =
        match config.namespace_entry with
        | Some _ -> "@" ^ namespace
        | None -> namespace
      in
      add_expected base [".cmi"; ".cmj"; ".cmt"; ".mlmap"])
    config.namespace;
  let removed_modules = ref [] in
  files_under ocaml_dir |> List.iter (fun path ->
    let basename = Filename.basename path in
    let managed =
      List.exists
        (Filename.check_suffix basename)
        [".cmi"; ".cmj"; ".cmt"; ".cmti"; ".ast"; ".iast"; ".res"; ".resi";
         ".mlmap"]
    in
    if managed && not (Hashtbl.mem expected_artifacts basename) then (
      if Filename.check_suffix basename ".ast" then
        removed_modules := Filename.chop_suffix basename ".ast" :: !removed_modules
      else if Filename.check_suffix basename ".iast" then
        removed_modules := Filename.chop_suffix basename ".iast" :: !removed_modules;
      remove_file path));
  let suffixes = [".js"; ".mjs"; ".cjs"; ".bs.js"; ".bs.mjs"; ".bs.cjs"] in
  let expected_outputs = Hashtbl.create (List.length modules * List.length config.package_specs) in
  List.iter (fun module_ -> List.iter (fun spec ->
    Hashtbl.replace expected_outputs (generated_js_path config module_.Source.implementation spec) ()) config.package_specs) modules;
  let owned_output path =
    suffixes
    |> List.find_map (fun suffix ->
         if Filename.check_suffix path suffix then
           Some
             (Filename.basename path |> fun basename ->
              Filename.chop_suffix basename suffix)
         else None)
    |> Option.fold ~none:false
         ~some:(fun name -> Hashtbl.mem owned_output_names name)
  in
  config.sources |> List.iter (fun source ->
    files_under (Filename.concat root source.Config.dir) |> List.iter (fun path ->
      if List.exists (fun suffix -> Filename.check_suffix path suffix) suffixes then
        if owned_output path && not (Hashtbl.mem expected_outputs path) then
          remove_file path));
  ["lib/es6"; "lib/js"] |> List.iter (fun directory ->
    files_under (Filename.concat root directory) |> List.iter (fun path ->
      if List.exists (fun suffix -> Filename.check_suffix path suffix) suffixes then
        if owned_output path && not (Hashtbl.mem expected_outputs path) then
          remove_file path));
  (!removed_modules, !previous_ast_count)

let env_path name fallback =
  match Sys.getenv_opt name with
  | Some path when Sys.file_exists path -> Unix.realpath path
  | Some path ->
    raise (Error (Printf.sprintf "%s points to missing path %s" name path))
  | None when Sys.file_exists fallback -> Unix.realpath fallback
  | None ->
    raise
      (Error
         (Printf.sprintf "%s is unset and fallback %s does not exist" name
            fallback))

let dependency_path root name =
  let existing_realpath path =
    if Sys.file_exists path then Some (Unix.realpath path) else None
  in
  let rec in_ancestors directory =
    let candidate = Filename.concat (Filename.concat directory "node_modules") name in
    match existing_realpath candidate with
    | Some path -> Some path
    | None ->
      let parent = Filename.dirname directory in
      if parent = directory then None else in_ancestors parent
  in
  match in_ancestors root with
  | Some path -> Some path
  | None ->
    let package_name =
      match List.rev (String.split_on_char '/' name) with
      | last :: _ -> last
      | [] -> name
    in
    let sibling = Filename.concat (Filename.dirname root) name in
    let workspace = Filename.concat (Filename.concat root "packages") package_name in
    List.find_map existing_realpath [sibling; workspace]

let report_failure action path result =
  let output = result.Process.stderr ^ result.stdout in
  ignore action;
  ignore path;
  raise (Build_failure output)

let compiler_flags ~source_maps ~watch ~gentype (config : Config.t) =
  let ppx_args =
    config.ppx_flags |> List.concat_map (function
      | [] -> []
      | flag :: arguments ->
      let executable =
        match dependency_path config.root flag with
        | Some path -> path
        | None -> flag
      in ["-ppx"; String.concat " " (executable :: arguments)])
  in
  let source_map_args =
    if not source_maps then []
    else if config.source_map_dev && not watch then
      ["-bs-source-map"; "false"]
    else config.source_map_args
  in
  ppx_args @ config.jsx_args @ source_map_args @ config.experimental_args
  @ (if gentype then config.gentype_args else [])
  @ config.compiler_flags @ config.warning_flags

let parse_file ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let args =
    compiler_flags ~source_maps:false ~watch:false ~gentype:false config
    @ ["-absname"; "-bs-ast"; "-o"; ast; Filename.concat "../.." path]
  in
  let result = Process.run ~cwd:build_dir bsc args in
  if not (Process.succeeded result) then report_failure "Parsing" path result;
  if result.stderr <> "" then prerr_string result.stderr;
  copy_file
    (Filename.concat build_dir ast)
    (Filename.concat
       (Filename.concat config.root "lib/ocaml")
       (Filename.basename ast));
  copy_file
    (Filename.concat config.root path)
    (Filename.concat
       (Filename.concat config.root "lib/ocaml")
       (Filename.basename path));
  ast

let parse_job ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let args = compiler_flags ~source_maps:false ~watch:false ~gentype:false config @ ["-absname"; "-bs-ast"; "-o"; ast; Filename.concat "../.." path] in
  Process.{program = bsc; args; cwd = build_dir}, ast

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

let package_output (config : Config.t) path (spec : Config.package_spec) =
  let directory = Filename.dirname path in
  let output_dir =
    if spec.in_source then directory
    else
      Filename.concat
        (match spec.module_format with
        | Config.Esmodule -> "lib/es6"
        | Config.Commonjs -> "lib/js")
        directory
  in
  Printf.sprintf "%s:%s:%s"
    (Config.module_format_name spec.module_format)
    output_dir
    (Config.package_spec_suffix config spec)

let compile_namespace ~bsc ~runtime ~build_dir ~ocaml_dir ~entry namespace modules =
  let mlmap = Filename.concat build_dir (namespace ^ ".mlmap") in
  let channel = open_out_bin mlmap in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_string channel "randjbuildsystem\n";
      modules
      |> List.filter (fun module_ -> Some module_.Source.name <> entry)
      |> List.map (fun module_ -> module_.Source.name)
      |> List.sort String.compare
      |> List.iter (fun name -> output_string channel name; output_char channel '\n'));
  let result =
    Process.run ~cwd:build_dir bsc
      ["-runtime-path"; runtime; "-w"; "-49"; "-color"; "always";
       "-no-alias-deps"; Filename.basename mlmap]
  in
  if not (Process.succeeded result) then report_failure "Compiling namespace" namespace result;
  copy_file_if_changed (Filename.concat build_dir (namespace ^ ".cmi"))
    (Filename.concat ocaml_dir (namespace ^ ".cmi"));
  copy_file mlmap (Filename.concat ocaml_dir (namespace ^ ".mlmap"))

let path_is_within ~root path =
  let root = Unix.realpath root in
  let path = Unix.realpath path in
  path = root || String.starts_with ~prefix:(root ^ "/") path

let is_local_dependency ~workspace path =
  path_is_within ~root:workspace path
  && not
       (String.split_on_char '/' (Unix.realpath path)
       |> List.exists (( = ) "node_modules"))

let gentype_dependency_args (config : Config.t) =
  if config.gentype_args = [] then []
  else
    config.dependencies |> List.concat_map (fun (dependency : Config.dependency) ->
      match dependency_path config.root dependency.name with
      | None -> []
      | Some path -> ["-bs-gentype-dep-path"; dependency.name ^ "=" ^ path])

let run_post_build (config : Config.t) path =
  match config.js_post_build with
  | None -> ()
  | Some command ->
    List.iter (fun spec ->
      let output = generated_js_path config path spec in
      let result = Process.run ~cwd:config.root "/bin/sh" ["-c"; command ^ " " ^ Filename.quote output] in
      if not (Process.succeeded result) then report_failure "js-post-build" output result;
      if result.stdout <> "" then print_string result.stdout;
      if result.stderr <> "" then prerr_string result.stderr) config.package_specs

let namespace_args (config : Config.t) module_name =
  match config.namespace, config.namespace_entry with
  | None, _ -> []
  | Some namespace, Some entry when entry = module_name -> ["-open"; "@" ^ namespace]
  | Some namespace, Some _ -> ["-bs-ns"; "@" ^ namespace]
  | Some namespace, _ -> ["-bs-ns"; namespace]

let compile_job ~bsc ~runtime ~build_dir ~watch ~(config : Config.t) ~dependency_dirs
    (module_ : Source.module_) ~is_interface path =
  let ast = Source.ast_path path in
  let namespace_args = namespace_args config module_.name in
  let interface_args = if not is_interface && Option.is_some module_.interface then ["-bs-read-cmi"] else [] in
  let output_args = if is_interface then [] else List.concat_map (fun spec -> ["-bs-package-output"; package_output config path spec]) config.package_specs in
  let args = namespace_args @ interface_args @ ["-I"; "../ocaml"]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ ["-runtime-path"; runtime] @ compiler_flags ~source_maps:true ~watch ~gentype:true config
    @ gentype_dependency_args config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  Process.{program = bsc; args; cwd = build_dir}, (module_, is_interface, path)

let publish_compiled ~build_dir ~ocaml_dir ~watch ~watch_output_paths ~is_local
    ~(config : Config.t) (module_, is_interface, path) result =
  if not (Process.succeeded result) then report_failure "Compiling" path result;
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  if stderr <> "" then append_compiler_log config.root stderr;
  if stderr <> "" then prerr_string stderr;
  let basename = Source.compiler_basename config module_.Source.name in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let extensions = if is_interface then ["cmi"; "cmti"] else ["cmi"; "cmj"; "cmt"] in
  List.iter
    (fun extension ->
      let source = Filename.concat artifact_dir (basename ^ "." ^ extension) in
      let destination = Filename.concat ocaml_dir (basename ^ "." ^ extension) in
      if extension = "cmi" then copy_file_if_changed source destination
      else copy_file source destination)
    extensions;
  let source = Filename.concat config.root path in
  let build_source = Filename.concat build_dir path in
  ensure_dir (Filename.dirname build_source);
  copy_file source build_source;
  copy_file source (Filename.concat ocaml_dir (Filename.basename path));
  if not is_interface then (
    List.iter
      (fun spec ->
        if spec.Config.in_source then (
          let output = generated_js_path config path spec in
          let build_output = generated_build_js_path ~build_dir config path spec in
          ensure_dir (Filename.dirname build_output);
          if Sys.file_exists output then copy_file output build_output;
          if Sys.file_exists (output ^ ".map") then
            copy_file (output ^ ".map") (build_output ^ ".map")
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
  stderr <> ""

let compile_batch ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~(config : Config.t)
    ~dependency_dirs_for ~watch_outputs ~watch_output_paths ~is_local jobs =
  List.iter (fun (_, is_interface, path) ->
    if not is_interface then
      List.iter (fun spec ->
        let output = generated_js_path config path spec in
        let dirty_ast = Filename.concat build_dir (Source.ast_path path) in
        ensure_dir (Filename.dirname output);
        if watch then (
          prepare_watch_output watch_outputs watch_output_paths ~dirty_ast output;
          prepare_watch_output watch_outputs watch_output_paths ~dirty_ast
            (output ^ ".map")))
        config.package_specs) jobs;
  let prepared = List.map (fun (module_, is_interface, path) ->
    compile_job ~bsc ~runtime ~build_dir ~watch ~config
      ~dependency_dirs:(dependency_dirs_for module_)
      module_ ~is_interface path) jobs in
  let results = Process.run_parallel (List.map fst prepared) in
  List.map2
    (fun (_, ((_, _, path) as info)) result ->
      if
        publish_compiled ~build_dir ~ocaml_dir ~watch ~watch_output_paths
          ~is_local ~config info result
      then Some path
      else None)
    prepared results
  |> List.filter_map Fun.id

let rec remove_tree path =
  if Sys.file_exists path then
    try
      if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path)
      else Sys.remove path
    with Sys_error _ | Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let rec clean_internal ~(root_config : Config.t) ~seen ~folder ~prod ~is_local =
  let root = Unix.realpath folder in
  if not (Hashtbl.mem seen root) then (
    Hashtbl.add seen root ();
    let config_path = Filename.concat root "rescript.json" in
    if Sys.file_exists config_path then (
      let config = Config.load config_path in
      let dependencies =
        config.dependencies
        @ if prod || not is_local then [] else config.dev_dependencies
      in
      List.iter (fun (dependency : Config.dependency) ->
        match dependency_path root dependency.name with
        | Some directory
          when Sys.file_exists (Filename.concat directory "rescript.json") ->
          clean_internal ~root_config ~seen ~folder:directory ~prod
            ~is_local:(is_local_dependency ~workspace:root_config.root directory)
        | _ -> ()) dependencies;
      let modules =
        Source.discover config ~prod ~features:None ~filter:None
          ~on_missing:(fun _ -> ())
          ~display_root:root_config.root
      in
      let output_config = with_root_options config root_config in
      if is_local then
        List.iter (fun module_ ->
           List.iter (fun spec ->
             let output = generated_js_path output_config module_.Source.implementation spec in
             remove_file output;
             remove_file (output ^ ".map");
             remove_file (output ^ ".rewatch-pending");
             remove_file (output ^ ".rewatch-backup");
             remove_file (output ^ ".map.rewatch-pending");
             remove_file (output ^ ".map.rewatch-backup")) output_config.package_specs) modules);
    List.iter (fun dir -> remove_tree (Filename.concat root dir))
      (["lib/bs"; "lib/ocaml"]
      @ if is_local then ["lib/es6"; "lib/js"] else []))

let clean ~seen ~folder ~prod =
  let root = Unix.realpath folder in
  let release_build_lock = acquire_build_lock (workspace_lock_root root) in
  Fun.protect ~finally:release_build_lock (fun () ->
    let root_config = Config.load (Filename.concat root "rescript.json") in
    let visited = Hashtbl.create 32 in
    List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
    clean_internal ~root_config ~seen:visited ~folder:root ~prod ~is_local:true)

let rec nearest_config directory =
  let config = Filename.concat directory "rescript.json" in
  if Sys.file_exists config then config
  else
    let parent = Filename.dirname directory in
    if parent = directory then raise (Error "could not find a rescript.json parent")
    else nearest_config parent

let relative_to root path =
  let root = if Filename.check_suffix root "/" then root else root ^ "/" in
  if String.starts_with ~prefix:root path then
    String.sub path (String.length root) (String.length path - String.length root)
  else raise (Error (path ^ " is not inside " ^ root))

let rec remove_flag_with_value flag = function
  | current :: _ :: rest when current = flag ->
    remove_flag_with_value flag rest
  | value :: rest -> value :: remove_flag_with_value flag rest
  | [] -> []

let compiler_args path =
  let source = Unix.realpath path in
  if not (Filename.check_suffix source ".res" || Filename.check_suffix source ".resi") then
    raise (Error "compiler-args expects a .res or .resi source file");
  let package_config =
    Config.load (nearest_config (Filename.dirname source))
  in
  let root = workspace_lock_root package_config.root in
  let root_config_path = Filename.concat root "rescript.json" in
  let root_config =
    if root <> package_config.root && Sys.file_exists root_config_path then
      Config.load root_config_path
    else package_config
  in
  let config = with_root_options package_config root_config in
  let config =
    {
      config with
      gentype_args =
        remove_flag_with_value "-bs-gentype-source-dir" config.gentype_args;
    }
  in
  let relative = relative_to config.root source in
  let runtime = env_path "RESCRIPT_RUNTIME" (Filename.concat (Sys.getcwd ()) "packages/@rescript/runtime") in
  let is_interface = Filename.check_suffix source ".resi" in
  let has_interface = not is_interface && Sys.file_exists (source ^ "i") in
  let dependency_dirs =
    config.dependencies |> List.filter_map (fun (dependency : Config.dependency) ->
      match dependency_path config.root dependency.name with
      | Some directory ->
        let ocaml = Filename.concat directory "lib/ocaml" in
        if Sys.file_exists ocaml then Some ocaml else None
      | None -> None)
  in
  let parser_args = compiler_flags ~source_maps:false ~watch:false ~gentype:false config
    @ ["-absname"; "-bs-ast"; "-o"; Source.ast_path relative; relative] in
  let compiler_args =
    let ast = Source.ast_path relative in
    let namespace_args = namespace_args config (Source.module_name source) in
    let interface_args = if not is_interface && has_interface then ["-bs-read-cmi"] else [] in
    let output_args = if is_interface then [] else List.concat_map (fun spec -> ["-bs-package-output"; package_output config relative spec]) config.package_specs in
    namespace_args @ interface_args @ ["-I"; "../ocaml"]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ ["-runtime-path"; runtime] @ compiler_flags ~source_maps:true ~watch:false ~gentype:true config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  Yojson.Safe.pretty_to_string (`Assoc [
    ("compiler_args", `List (List.map (fun value -> `String value) compiler_args));
    ("parser_args", `List (List.map (fun value -> `String value) parser_args));
  ])

type build_stats = {
  mutable cleaned: int;
  mutable previous_asts: int;
  mutable parsed: int;
  mutable compiled: int;
  mutable diagnostics: string list;
  mutable failure: string option;
  removed_modules: (string, unit) Hashtbl.t;
  forced_parse_paths: (string, unit) Hashtbl.t;
  preparse_stderr: (string, string) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  active_features: (string, string list option) Hashtbl.t;
  initialized_logs: (string, unit) Hashtbl.t;
  watch_outputs: (string * string * string) list ref;
  watch_output_paths: (string, unit) Hashtbl.t;
}

let source_is_newer ~source ~artifact =
  match modification_time source, modification_time artifact with
  | Some source_time, Some artifact_time -> source_time > artifact_time
  | Some _, None -> true
  | None, _ -> false

let dependency_artifact dependency_dirs dependency =
  let matches path =
    let basename = Filename.basename path in
    if not (Filename.check_suffix basename ".cmi") then false
    else
      let name = Filename.chop_suffix basename ".cmi" in
      name = dependency || String.trim name = dependency
      || (String.starts_with ~prefix:"@" name
         && String.sub name 1 (String.length name - 1) = dependency)
  in
  dependency_dirs
  |> List.find_map (fun directory ->
       files_under directory |> List.find_opt matches)

type global_module = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  namespace: string option;
  namespace_entry: string option;
  allowed_dependencies: string list;
  raw_dependencies: string list;
}

let global_module_key (config : Config.t) module_name =
  Source.compiler_basename config module_name

let dependency_head dependency =
  match String.split_on_char '.' dependency with
  | head :: _ -> head
  | [] -> dependency

let blocked_dependents graph cycle =
  let blocked = Hashtbl.create (List.length cycle) in
  List.iter (fun name -> Hashtbl.replace blocked name ()) cycle;
  let rec add_dependents () =
    let changed = ref false in
    List.iter
      (fun (name, dependencies) ->
        if
          not (Hashtbl.mem blocked name)
          && List.exists (Hashtbl.mem blocked) dependencies
        then (
          Hashtbl.add blocked name ();
          changed := true))
      graph;
    if !changed then add_dependents ()
  in
  add_dependents ();
  Hashtbl.to_seq_keys blocked |> List.of_seq

let dependent_is_allowed allowed_dependents dependent =
  Option.fold ~none:true
    ~some:(fun allowed -> List.mem dependent allowed)
    allowed_dependents

let prepare_global_graph ~(root_config : Config.t) ~prod ~features ~warn_error
    ~filter ~stats =
  let repository_root = Sys.getcwd () in
  let bsc =
    env_path "RESCRIPT_BSC_EXE"
      (Filename.concat repository_root
         "_build/default/compiler/bsc/rescript_compiler_main.exe")
  in
  let requested_features = Hashtbl.create 32 in
  let unallowed_dependencies = ref [] in
  let add_feature_request root request =
    match Hashtbl.find_opt requested_features root, request with
    | None, request -> Hashtbl.add requested_features root request
    | Some None, _ | Some _, None ->
      Hashtbl.replace requested_features root None
    | Some (Some current), Some requested ->
      Hashtbl.replace requested_features root
        (Some (List.sort_uniq String.compare (current @ requested)))
  in
  let collected = Hashtbl.create 32 in
  let rec collect ~folder ~features ~is_local =
    let root = Unix.realpath folder in
    if root <> root_config.root || not (Hashtbl.mem requested_features root) then
      add_feature_request root features;
    if not (Hashtbl.mem collected root) then (
      Hashtbl.add collected root ();
      let config = Config.load (Filename.concat root "rescript.json") in
      let dependencies =
        List.map (fun dependency -> ("dependencies", dependency))
          config.dependencies
        @ if prod || not is_local then []
          else
            List.map
              (fun dependency -> ("dev-dependencies", dependency))
              config.dev_dependencies
      in
      List.iter
        (fun (kind, (dependency : Config.dependency)) ->
          match dependency_path root dependency.name with
          | Some directory
            when Sys.file_exists (Filename.concat directory "rescript.json") ->
            let dependency_config =
              Config.load (Filename.concat directory "rescript.json")
            in
            if
              not
                (dependent_is_allowed dependency_config.allowed_dependents
                   config.name)
            then
              unallowed_dependencies :=
                (config.name, kind, dependency_config.name)
                :: !unallowed_dependencies;
            collect ~folder:directory ~features:dependency.features
              ~is_local:
                (is_local_dependency ~workspace:root_config.root directory)
          | _ -> ())
        dependencies)
  in
  collect ~folder:root_config.root ~features ~is_local:true;
  (if !unallowed_dependencies <> [] then
    let details =
      !unallowed_dependencies |> List.sort_uniq compare
      |> List.map (fun (dependent, kind, dependency) ->
           Printf.sprintf "%s %s: %s" dependent kind dependency)
      |> String.concat "\n"
    in
    raise
      (Error
         ("The following packages use dependencies that do not allow them:\n"
         ^ details
         ^ "\nUpdate allowed-dependents in the dependency rescript.json files.")));
  Hashtbl.iter
    (fun root features -> Hashtbl.replace stats.active_features root features)
    requested_features;
  let visited = Hashtbl.create 32 in
  let nodes = ref [] in
  let rec visit ~folder ~features ~warn_error ~filter ~is_local =
    let root = Unix.realpath folder in
    if not (Hashtbl.mem visited root) then (
      Hashtbl.add visited root ();
      let features =
        match Hashtbl.find_opt stats.active_features root with
        | Some features -> features
        | None -> features
      in
      let config = Config.load (Filename.concat root "rescript.json") in
      let config =
        match warn_error with
        | None -> config
        | Some value ->
          {config with warning_flags = ["-warn-error"; value]}
      in
      let dependencies =
        config.dependencies
        @ if prod || not is_local then [] else config.dev_dependencies
      in
      List.iter
        (fun (dependency : Config.dependency) ->
          match dependency_path root dependency.name with
          | Some directory
            when Sys.file_exists (Filename.concat directory "rescript.json") ->
            visit ~folder:directory ~features:dependency.features
              ~warn_error:None ~filter:None
              ~is_local:
                (is_local_dependency ~workspace:root_config.root directory)
          | _ -> ())
        dependencies;
      let modules =
        Source.discover config ~prod ~features ~filter
          ~on_missing:(fun _ -> ())
          ~display_root:root_config.root
      in
      let compile_config = with_root_options config root_config in
      let build_dir = Filename.concat root "lib/bs" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      ensure_dir build_dir;
      let dirty_paths =
        modules
        |> List.concat_map (fun module_ ->
             module_.Source.implementation
             :: Option.to_list module_.Source.interface)
        |> List.filter (fun path ->
             source_is_newer ~source:(Filename.concat root path)
               ~artifact:(Filename.concat build_dir (Source.ast_path path)))
      in
      let results =
        Process.run_parallel
          (List.map
             (fun path ->
               fst (parse_job ~bsc ~build_dir ~config:compile_config path))
             dirty_paths)
      in
      List.iter2
        (fun path result ->
          if Process.succeeded result then (
            let absolute_path = Filename.concat root path in
            Hashtbl.replace stats.forced_parse_paths
              absolute_path ();
            if result.stderr <> "" then
              Hashtbl.replace stats.preparse_stderr absolute_path
                result.stderr))
        dirty_paths results;
      List.iter
        (fun module_ ->
          let intf_dependencies =
            match module_.Source.interface with
            | None -> []
            | Some path -> ast_dependencies ~build_dir (Source.ast_path path)
          in
          let raw_dependencies =
            List.sort_uniq String.compare
              (ast_dependencies ~build_dir
                 (Source.ast_path module_.Source.implementation)
              @ intf_dependencies)
          in
          let compiler_base =
            global_module_key compile_config module_.Source.name
          in
          let cmt = Filename.concat ocaml_dir (compiler_base ^ ".cmt") in
          if not (Sys.file_exists cmt) then
            Hashtbl.replace stats.forced_parse_paths
              (Filename.concat root module_.Source.implementation) ();
          nodes :=
            {
              key = compiler_base;
              package_name = config.name;
              package_root = root;
              source_path = module_.Source.implementation;
              namespace = compile_config.namespace;
              namespace_entry = compile_config.namespace_entry;
              allowed_dependencies =
                List.map
                  (fun (dependency : Config.dependency) -> dependency.name)
                  dependencies;
              raw_dependencies;
            }
            :: !nodes)
        modules)
  in
  visit ~folder:root_config.root ~features ~warn_error ~filter ~is_local:true;
  let nodes =
    List.sort (fun first second -> String.compare first.key second.key) !nodes
  in
  let by_key = Hashtbl.create (List.length nodes) in
  List.iter
    (fun node ->
      match Hashtbl.find_opt by_key node.key with
      | None -> Hashtbl.add by_key node.key node
      | Some previous ->
        raise
          (Source.duplicate_error ~display_root:root_config.root "" node.key
             (Filename.concat previous.package_root previous.source_path)
             (Filename.concat node.package_root node.source_path)))
    nodes;
  let resolve_dependency node dependency =
    let raw_name = dependency_head dependency in
    let local_name =
      match node.namespace, String.split_on_char '.' dependency with
      | Some namespace, first :: second :: _ when first = namespace -> second
      | _ -> raw_name
    in
    let local_key =
      match node.namespace with
      | None -> local_name
      | Some namespace -> (
        match node.namespace_entry with
        | Some entry when entry = local_name -> local_name
        | Some _ -> local_name ^ "-@" ^ namespace
        | None -> local_name ^ "-" ^ namespace)
    in
    match Hashtbl.find_opt by_key local_key with
    | Some dependency_node
      when dependency_node.package_name = node.package_name ->
      Some local_key
    | _ ->
      (match Hashtbl.find_opt by_key raw_name with
      | Some dependency_node
        when dependency_node.package_name = node.package_name
             || List.mem dependency_node.package_name node.allowed_dependencies ->
        Some raw_name
      | _ -> None)
  in
  let graph_nodes =
    List.map
      (fun node ->
        ( node,
          node.raw_dependencies
          |> List.filter_map (resolve_dependency node)
          |> List.filter (fun dependency -> dependency <> node.key)
          |> List.sort_uniq String.compare ))
      nodes
  in
  try
    ignore
      (Graph.topological_sort graph_nodes
         ~name:(fun (node, _) -> node.key)
         ~deps:snd);
    None
  with Graph.Cycle cycle ->
    let blocked =
      blocked_dependents
        (List.map (fun (node, dependencies) -> (node.key, dependencies)) graph_nodes)
        cycle
    in
    Some (cycle, blocked, by_key)

let rec run_internal ~(root_config : Config.t) ~seen ~folder ~prod ~features
    ~warn_error ~watch ~filter ~is_local ~stats =
  let root = Unix.realpath folder in
  let features =
    match Hashtbl.find_opt stats.active_features root with
    | Some features -> features
    | None -> features
  in
  Hashtbl.replace seen root ();
  let config = Config.load (Filename.concat root "rescript.json") in
  let config = match warn_error with
    | None -> config
    | Some value -> {config with warning_flags = ["-warn-error"; value]}
  in
  if is_local then
    stats.diagnostics <-
      List.rev_append config.diagnostics stats.diagnostics;
  let dependency_directories =
    let dependencies : Config.dependency list =
      config.dependencies
      @ if prod || not is_local then [] else config.dev_dependencies
    in
    dependencies |> List.filter_map (fun (dependency : Config.dependency) ->
      let name = dependency.name in
      let candidate = dependency_path root name in
      let () = match candidate with
        | None -> ()
        | Some candidate when Hashtbl.mem seen candidate -> ()
        | Some candidate
          when Sys.file_exists (Filename.concat candidate "rescript.json") ->
          (try
             run_internal ~root_config ~seen ~folder:candidate ~prod
               ~features:dependency.features ~warn_error:None ~watch
               ~filter:None
               ~is_local:
                 (is_local_dependency ~workspace:root_config.root candidate)
               ~stats
           with Build_failure output ->
             if Option.is_none stats.failure then stats.failure <- Some output)
        | Some _ -> ()
      in
      match candidate with
      | None -> raise (Error ("Could not resolve dependency " ^ name))
      | Some candidate ->
      let ocaml = Filename.concat candidate "lib/ocaml" in
      if Sys.file_exists ocaml then Some (dependency, ocaml) else None)
  in
  let dependency_dirs = List.map snd dependency_directories in
  let regular_dependency_names =
    config.dependencies
    |> List.map (fun (dependency : Config.dependency) -> dependency.name)
  in
  let dependency_dirs_for (module_ : Source.module_) =
    if module_.is_dev then dependency_dirs
    else
      dependency_directories
      |> List.filter_map (fun ((dependency : Config.dependency), directory) ->
           if List.mem dependency.name regular_dependency_names then
             Some directory
           else None)
  in
  let repository_root = Sys.getcwd () in
  let bsc =
    env_path "RESCRIPT_BSC_EXE"
      (Filename.concat repository_root
         "_build/default/compiler/bsc/rescript_compiler_main.exe")
  in
  let runtime =
    env_path "RESCRIPT_RUNTIME"
      (Filename.concat repository_root "packages/@rescript/runtime")
  in
  let build_dir = Filename.concat root "lib/bs" in
  let ocaml_dir = Filename.concat root "lib/ocaml" in
  ensure_dir build_dir;
  ensure_dir ocaml_dir;
  initialize_compiler_log root;
  Hashtbl.replace stats.initialized_logs root ();
  let modules =
    Source.discover config ~prod ~features ~filter
      ~display_root:root_config.root
      ~on_missing:(fun path ->
        if is_local then Printf.eprintf "Could not read folder %s\n%!" path)
      ~on_orphan:(fun path ->
        Printf.eprintf
          "\027[2K\r No implementation file found for interface file (skipping): %s\n%!"
          path)
  in
  let config = with_root_options config root_config in
  let removed_modules, previous_ast_count =
    cleanup_stale ~root ~ocaml_dir config modules
  in
  stats.cleaned <- stats.cleaned + List.length removed_modules;
  List.iter
    (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
    removed_modules;
  stats.previous_asts <- stats.previous_asts + previous_ast_count;
  Option.iter
    (fun namespace ->
      let namespace =
        match config.namespace_entry with
        | Some _ -> "@" ^ namespace
        | None -> namespace
      in
      compile_namespace ~bsc ~runtime ~build_dir ~ocaml_dir
        ~entry:config.namespace_entry namespace modules)
    config.namespace;
  let names = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ -> Hashtbl.replace names module_.Source.name ())
    modules;
  let parse_paths =
    List.concat_map (fun module_ ->
      module_.Source.implementation :: Option.to_list module_.interface) modules
  in
  let dirty_parse_paths =
    parse_paths
    |> List.filter (fun path ->
         let source_base =
           path |> Filename.basename |> Filename.remove_extension
         in
         List.mem source_base removed_modules
         || Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)
         || source_is_newer ~source:(Filename.concat root path)
              ~artifact:(Filename.concat build_dir (Source.ast_path path)))
  in
  let parse_paths_to_run =
    dirty_parse_paths
    |> List.filter (fun path ->
         not
           (Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)))
  in
  let parsed =
    List.map2 (fun path result -> (path, Some result)) parse_paths_to_run
      (Process.run_parallel
         (List.map
            (fun path -> fst (parse_job ~bsc ~build_dir ~config path))
            parse_paths_to_run))
    @ (dirty_parse_paths
      |> List.filter (fun path ->
           Hashtbl.mem stats.forced_parse_paths (Filename.concat root path))
      |> List.map (fun path -> (path, None)))
  in
  let warning_asts = ref [] in
  List.iter (fun (path, result) ->
    let absolute_path = Filename.concat root path in
    let stderr =
      match result with
      | Some result -> result.Process.stderr
      | None ->
        Hashtbl.find_opt stats.preparse_stderr absolute_path
        |> Option.value ~default:""
    in
    Option.iter
      (fun result ->
        if not (Process.succeeded result) then
          report_failure "Parsing" path result)
      result;
    let stderr =
      if is_local then stderr else retain_critical_external_warnings stderr
    in
    if stderr <> "" then append_compiler_log root stderr;
    if stderr <> "" then prerr_string stderr;
    let ast = Source.ast_path path in
    if is_local && stderr <> "" then warning_asts := ast :: !warning_asts;
    copy_file (Filename.concat build_dir ast)
      (Filename.concat (Filename.concat config.root "lib/ocaml") (Filename.basename ast));
    copy_file (Filename.concat config.root path)
      (Filename.concat (Filename.concat config.root "lib/ocaml") (Filename.basename path))) parsed;
  let raw_dependencies = Hashtbl.create (List.length modules) in
  let parse_dirty_modules = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ ->
      let impl_ast = Source.ast_path module_.Source.implementation in
      let impl_deps = ast_dependencies ~build_dir impl_ast in
      let intf_deps =
        match module_.interface with
        | None -> []
        | Some path -> ast_dependencies ~build_dir (Source.ast_path path)
      in
      let dependencies = List.sort_uniq String.compare (impl_deps @ intf_deps) in
      Hashtbl.replace raw_dependencies module_.Source.name dependencies;
      let paths =
        module_.Source.implementation :: Option.to_list module_.Source.interface
      in
      if List.exists (fun path -> List.mem path dirty_parse_paths) paths then
        Hashtbl.replace parse_dirty_modules module_.Source.name ();
      let global_key = global_module_key config module_.Source.name in
      module_.deps <-
        if Hashtbl.mem stats.blocked_modules global_key then []
        else
          List.filter
            (fun dep -> dep <> module_.name && Hashtbl.mem names dep)
            dependencies)
    modules;
  stats.parsed <- stats.parsed + Hashtbl.length parse_dirty_modules;
  let ordered =
    try
      Graph.topological_sort modules
        ~name:(fun module_ -> module_.Source.name)
        ~deps:(fun module_ -> module_.Source.deps)
    with Graph.Cycle names ->
      raise
        (Error
           ("Can't continue... Found a circular dependency in your code: "
           ^ String.concat " -> " names))
  in
  let depths = Hashtbl.create (List.length ordered) in
  let depth module_ =
    match Hashtbl.find_opt depths module_.Source.name with Some value -> value | None -> 0
  in
  List.iter (fun module_ ->
    let value = 1 + List.fold_left (fun highest dep ->
      match Hashtbl.find_opt depths dep with Some value -> max highest value | None -> highest)
      0 module_.Source.deps in
    Hashtbl.replace depths module_.Source.name value) ordered;
  let levels =
    ordered |> List.fold_left (fun levels module_ ->
      let level = depth module_ in
      let existing = match List.assoc_opt level levels with Some xs -> xs | None -> [] in
      (level, module_ :: existing) :: List.remove_assoc level levels) []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let compile_warning_modules = Hashtbl.create 8 in
  let module_is_dirty module_ =
    let global_key = global_module_key config module_.Source.name in
    let compiler_base = Source.compiler_basename config module_.Source.name in
    let cmt = Filename.concat ocaml_dir (compiler_base ^ ".cmt") in
    let source_base =
      module_.Source.implementation |> Filename.basename
      |> Filename.remove_extension
    in
    let outputs_exist =
      List.for_all
        (fun spec ->
          Sys.file_exists
            (generated_js_path config module_.Source.implementation spec))
        config.package_specs
    in
    let dependencies =
      Hashtbl.find_opt raw_dependencies module_.Source.name
      |> Option.value ~default:[]
    in
    let dependency_is_newer dependency =
      let artifact =
        match Hashtbl.find_opt names dependency with
        | Some () ->
          Some
            (Filename.concat ocaml_dir
               (Source.compiler_basename config dependency ^ ".cmi"))
        | None -> dependency_artifact dependency_dirs dependency
      in
      match artifact, modification_time cmt with
      | Some path, Some cmt_time ->
        Option.fold ~none:false ~some:(fun time -> time > cmt_time)
          (modification_time path)
      | _, None -> true
      | None, Some _ -> false
    in
    not (Hashtbl.mem stats.blocked_modules global_key)
    &&
    (Hashtbl.mem parse_dirty_modules module_.Source.name
    || List.mem source_base removed_modules
    || not (Sys.file_exists cmt && outputs_exist)
    || List.exists (fun dependency -> List.mem dependency removed_modules)
         dependencies
    || List.exists
         (fun dependency -> Hashtbl.mem stats.removed_modules dependency)
         dependencies
    || List.exists dependency_is_newer dependencies)
  in
  List.iter (fun (_, modules) ->
    let modules = List.rev modules in
    let dirty_modules = List.filter module_is_dirty modules in
    stats.compiled <- stats.compiled + List.length dirty_modules;
    let interface_warning_paths =
      compile_batch ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~config
        ~dependency_dirs_for ~watch_outputs:stats.watch_outputs
        ~watch_output_paths:stats.watch_output_paths ~is_local
        (List.filter_map
           (fun module_ ->
             Option.map (fun path -> (module_, true, path)) module_.Source.interface)
           dirty_modules)
    in
    let implementation_warning_paths =
      compile_batch ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~config
        ~dependency_dirs_for ~watch_outputs:stats.watch_outputs
        ~watch_output_paths:stats.watch_output_paths ~is_local
        (List.map
           (fun module_ -> (module_, false, module_.Source.implementation))
           dirty_modules)
    in
    let warning_paths = interface_warning_paths @ implementation_warning_paths in
    if is_local then
      List.iter
        (fun path ->
          Hashtbl.replace compile_warning_modules (Source.module_name path) ())
        warning_paths) levels;
  Hashtbl.iter
    (fun module_name () ->
      match List.find_opt (fun module_ -> module_.Source.name = module_name) modules with
      | None -> ()
      | Some module_ ->
        let paths =
          module_.Source.implementation :: Option.to_list module_.Source.interface
        in
        List.iter
          (fun path ->
            let ast = Source.ast_path path in
            remove_file (Filename.concat build_dir ast);
            remove_file (Filename.concat ocaml_dir (Filename.basename ast)))
          paths)
    compile_warning_modules;
  List.iter
    (fun ast ->
      remove_file (Filename.concat build_dir ast);
      remove_file (Filename.concat ocaml_dir (Filename.basename ast)))
    !warning_asts;
  ()

let run ~seen ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter =
  let root = Unix.realpath folder in
  let root_config = Config.load (Filename.concat root "rescript.json") in
  let visited = Hashtbl.create 32 in
  let stats =
    {
      cleaned = 0;
      previous_asts = 0;
      parsed = 0;
      compiled = 0;
      diagnostics = [];
      failure = None;
      removed_modules = Hashtbl.create 16;
      forced_parse_paths = Hashtbl.create 16;
      preparse_stderr = Hashtbl.create 16;
      blocked_modules = Hashtbl.create 16;
      active_features = Hashtbl.create 16;
      initialized_logs = Hashtbl.create 16;
      watch_outputs = ref [];
      watch_output_paths = Hashtbl.create 16;
    }
  in
  List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
  let finalize_logs () =
    Hashtbl.iter (fun package_root () -> finalize_compiler_log package_root)
      stats.initialized_logs;
    Hashtbl.clear stats.initialized_logs
  in
  let outputs_finished = ref false in
  let expose_watch_outputs () =
    !(stats.watch_outputs)
    |> List.rev
    |> List.iter (fun (output, pending, _) ->
         if Sys.file_exists pending then (
           remove_file output;
           Unix.rename pending output))
  in
  let finish_watch_outputs ~success =
    !(stats.watch_outputs)
    |> List.rev
    |> List.iter (fun (output, pending, dirty_ast) ->
         if success then (
           if Sys.file_exists pending then (
             remove_file output;
             Unix.rename pending output))
         else (
           remove_file output;
           remove_file pending;
           remove_file dirty_ast));
    stats.watch_outputs := [];
    Hashtbl.clear stats.watch_output_paths;
    outputs_finished := true
  in
  let report ~success () =
    finish_watch_outputs ~success;
    finalize_logs ();
    if watch then (
      if success then Printf.printf "Finished compilation\n%!")
    else
    Printf.printf "Cleaned %d/%d\nParsed %d source files\nCompiled %d modules\n%!"
      stats.cleaned stats.previous_asts stats.parsed stats.compiled;
    let diagnostics =
      stats.diagnostics |> List.rev |> List.sort_uniq String.compare
    in
    if diagnostics <> [] then
      prerr_endline (String.concat "\n\n" diagnostics)
  in
  let report_failure output =
    report ~success:false ();
    prerr_string output;
    prerr_newline ();
    raise
      (Error
        ("Incremental build failed. Error: \027[2K\r  Failed to Compile. "
        ^ "See Errors Above"))
  in
  let format_cycle cycle by_key =
    let format_node name =
      match Hashtbl.find_opt by_key name with
      | None -> name
      | Some node ->
        let absolute = Filename.concat node.package_root node.source_path in
        let module_name = Source.module_name node.source_path in
        let display_name =
          match node.namespace, node.namespace_entry with
          | Some namespace, Some entry when entry <> module_name ->
            namespace ^ "." ^ module_name
          | Some namespace, None -> namespace ^ "." ^ module_name
          | _ -> module_name
        in
        Printf.sprintf "%s (%s)" display_name
          (relative_to root_config.root absolute)
    in
    "\nCan't continue... Found a circular dependency in your code:\n"
    ^ (cycle |> List.map format_node |> String.concat "\n → ")
    ^ "\nPossible solutions:\n- Extract shared code into a new module both depend on.\n"
  in
  let release_build_lock = acquire_build_lock (workspace_lock_root root) in
  let execute () =
    let cycle =
      prepare_global_graph ~root_config ~prod ~features ~warn_error ~filter
        ~stats
    in
    Option.iter
      (fun (_, blocked, _) ->
        List.iter
          (fun name -> Hashtbl.replace stats.blocked_modules name ())
          blocked)
      cycle;
    run_internal ~root_config ~seen:visited ~folder:root ~prod ~features
      ~warn_error ~watch ~filter ~is_local:true ~stats;
    (match stats.failure, cycle with
    | Some output, _ -> report_failure output
    | None, Some (names, _, by_key) ->
      let output = format_cycle names by_key in
      names
      |> List.filter_map (Hashtbl.find_opt by_key)
      |> List.map (fun node -> node.package_root)
      |> List.sort_uniq String.compare
      |> List.iter (fun package_root -> append_compiler_log package_root output);
      report_failure output
    | None, None ->
      Option.iter
        (fun command ->
          expose_watch_outputs ();
          finish_watch_outputs ~success:true;
          finalize_logs ();
          release_build_lock ();
          let result = Process.run ~cwd:root "/bin/sh" ["-c"; command] in
          if not (Process.succeeded result) then
            report_failure (result.stderr ^ result.stdout);
          if result.stdout <> "" then print_string result.stdout;
          if result.stderr <> "" then prerr_string result.stderr)
        after_build;
      report ~success:true ())
  in
  Fun.protect
    ~finally:(fun () ->
      if not !outputs_finished then finish_watch_outputs ~success:false;
      finalize_logs ();
      release_build_lock ())
    (fun () -> try execute () with Build_failure output -> report_failure output)

let watch ~folder ~prod ~features ~warn_error ~after_build ~filter ~clear_screen =
  let root = Unix.realpath folder in
  ignore (Config.load (Filename.concat root "rescript.json"));
  let lock_dir = Filename.concat root "lib" in
  ensure_dir lock_dir;
  let lock_path = Filename.concat lock_dir "watch.lock" in
  let pid = string_of_int (Unix.getpid ()) in
  let read_lock () = read_lock_owner lock_path in
  let candidate = Filename.temp_file ~temp_dir:lock_dir ".watch-lock-" ".tmp" in
  let channel = open_out candidate in
  output_string channel pid;
  close_out channel;
  let clear_stale_lock () =
    let takeover = lock_path ^ ".takeover" in
    try
      Unix.link candidate takeover;
      Fun.protect
        ~finally:(fun () -> remove_file takeover)
        (fun () ->
          match read_lock () with
          | Some owner when process_is_active owner -> ()
          | _ -> remove_file lock_path);
      true
    with Unix.Unix_error (Unix.EEXIST, _, _) ->
      (match read_lock_owner takeover with
      | Some owner when process_is_active owner -> ()
      | _ -> remove_file takeover);
      false
  in
  let rec create_lock attempts =
    if attempts = 0 then
      raise (Error "Timed out recovering a stale ReScript watch lock");
    try Unix.link candidate lock_path
    with Unix.Unix_error (Unix.EEXIST, _, _) -> (
      match read_lock () with
      | Some owner when process_is_active owner ->
        raise
          (Error
             (Printf.sprintf
                "Could not start Rescript build: A ReScript build is already running. The process ID (PID) is %s"
                owner))
      | _ ->
        if not (clear_stale_lock ()) then ignore (Unix.select [] [] [] 0.01);
        create_lock (attempts - 1))
  in
  Fun.protect ~finally:(fun () -> remove_file candidate) (fun () -> create_lock 1000);
  let lock_is_owned () = read_lock () = Some pid in
  let remove_owned_lock () = if lock_is_owned () then remove_file lock_path in
  let stop () =
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    Sys.set_signal Sys.sigterm Sys.Signal_ignore;
    raise Stop_watch
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> stop ()));
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> stop ()));
  let watch_roots () =
    let visited = Hashtbl.create 32 in
    Hashtbl.add visited root ();
    let roots = ref [root] in
    let rec visit (config : Config.t) =
      let dependencies =
        config.dependencies @ if prod then [] else config.dev_dependencies
      in
      List.iter
        (fun (dependency : Config.dependency) ->
          match dependency_path config.root dependency.name with
          | Some directory
            when (not (Hashtbl.mem visited directory))
                 && is_local_dependency ~workspace:root directory
                 && Sys.file_exists (Filename.concat directory "rescript.json") ->
            Hashtbl.add visited directory ();
            roots := directory :: !roots;
            visit (Config.load (Filename.concat directory "rescript.json"))
          | _ -> ())
        dependencies
    in
    try
      visit (Config.load (Filename.concat root "rescript.json"));
      List.sort String.compare !roots
    with Config.Error _ -> [root]
  in
  let digest_cache = Hashtbl.create 256 in
  let snapshot roots =
    let visited_directories = Hashtbl.create 64 in
    let seen_files = Hashtbl.create 256 in
    let digest path stat =
      Hashtbl.replace seen_files path ();
      match Hashtbl.find_opt digest_cache path with
      | Some (mtime, ctime, size, digest)
        when mtime = stat.Unix.st_mtime && ctime = stat.Unix.st_ctime
             && size = stat.Unix.st_size ->
        digest
      | _ ->
        let digest = Digest.file path |> Digest.to_hex in
        Hashtbl.replace digest_cache path
          (stat.Unix.st_mtime, stat.Unix.st_ctime, stat.Unix.st_size, digest);
        digest
    in
    let rec walk dir acc =
      try
        let canonical = Unix.realpath dir in
        if Hashtbl.mem visited_directories canonical then acc
        else (
          Hashtbl.add visited_directories canonical ();
          let entries = Sys.readdir dir |> Array.to_list in
          List.fold_left
            (fun acc name ->
              let path = Filename.concat dir name in
              try
                let stat = Unix.lstat path in
                match stat.Unix.st_kind with
                | Unix.S_DIR ->
                  if
                    List.mem name ["lib"; "node_modules"; ".git"; "_build"]
                  then acc
                  else walk path acc
                | Unix.S_LNK ->
                  if (Unix.stat path).Unix.st_kind = Unix.S_DIR then walk path acc
                  else acc
                | Unix.S_REG
                  when Filename.extension path = ".res"
                       || Filename.extension path = ".resi"
                       || name = "rescript.json" || name = "package.json" ->
                  let digest = digest path stat in
                  (path, stat.Unix.st_mtime, stat.Unix.st_size, digest) :: acc
                | _ -> acc
              with Sys_error _ | Unix.Unix_error _ -> acc)
            acc entries)
      with Sys_error _ | Unix.Unix_error _ -> acc
    in
    let result =
      List.sort compare (List.concat_map (fun directory -> walk directory []) roots)
    in
    Hashtbl.filter_map_inplace
      (fun path value ->
        if Hashtbl.mem seen_files path then Some value else None)
      digest_cache;
    result
  in
  let run_build () =
    try
      run ~seen:[] ~folder ~prod ~features ~warn_error ~watch:true ~after_build
        ~filter
    with
    | Error message | Config.Error message | Source.Error message
    | Process.Error message -> prerr_endline message
    | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
      prerr_endline (Printexc.to_string exn)
  in
  let clear_terminal () =
    if clear_screen && Unix.isatty Unix.stdout then
      Printf.printf "\027[2J\027[H%!"
  in
  let rec loop roots previous =
    if lock_is_owned () then (
      let current = snapshot roots in
      if current <> previous then (
        clear_terminal ();
        run_build ();
        let roots = watch_roots () in
        let after_build = snapshot roots in
        ignore (Unix.select [] [] [] 0.2);
        (* Keep the snapshot from before the rebuild when another edit lands
           during compilation. Otherwise that edit would become the new baseline
           and an atomic configuration rewrite could be missed. *)
        if after_build <> current then loop roots current
        else loop roots after_build)
      else (
        ignore (Unix.select [] [] [] 0.2);
        loop roots current))
  in
  Fun.protect
    (fun () ->
      let roots = watch_roots () in
      let before_build = snapshot roots in
      run_build ();
      let roots = watch_roots () in
      let after_build = snapshot roots in
      if after_build <> before_build then loop roots before_build
      else loop roots after_build)
    ~finally:remove_owned_lock
