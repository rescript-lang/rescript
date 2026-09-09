exception Error = Project_context.Error
exception Package_error = Project_context.Package_error
exception Stop_watch
exception Build_failure of string
exception Parse_failure of string
exception Scheduled_failure of string

open Build_artifacts

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

let read_lock_owner path =
  try
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      Some (really_input_string channel (in_channel_length channel)))
  with Sys_error _ -> None

let valid_lock_owner value =
  match Int64.of_string_opt value with
  | Some pid -> pid >= 0L && pid <= 0xffff_ffffL
  | None -> false

let malformed_lock_error () =
  Error
    "Could not start Rescript build: Could not parse lockfile PID\n  (try removing it and running the command again)"

let process_is_active value =
  Platform.process_is_active value ~run:(fun program args ->
    try
      let result =
        Process.run ~cwd:(Filename.get_temp_dir_name ()) program args
      in
      Some (result.Process.status, result.stdout)
    with
    | Process.Error _ | Unix.Unix_error _ | Sys_error _ -> None)

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
          | Some owner when not (valid_lock_owner owner) ->
            raise (malformed_lock_error ())
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
      | Some owner when not (valid_lock_owner owner) ->
        raise (malformed_lock_error ())
      | Some owner when process_is_active owner ->
        if attempts = 1200 then
          print_endline "Waiting for other build to finish...";
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

let bsc_path () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let runtime_path root =
  try Toolchain.runtime ~find_package:(Project_context.dependency_path root)
  with Toolchain.Error message -> raise (Error message)

let report_failure action path result =
  let output = result.Process.stderr ^ result.stdout in
  ignore action;
  ignore path;
  raise (Build_failure output)

let run_after_build ~root command =
  let program, args =
    match Str.split (Str.regexp "[ \t\r\n]+") command with
    | program :: args -> (program, args)
    | [] -> raise (Error "--after-build command cannot be empty")
  in
  let result =
    try Process.run ~cwd:root program args with
    | Process.Error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Sys_error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Unix.Unix_error (error, operation, argument) ->
      let target = if argument = "" then program else argument in
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s (%s %s)"
              command (Unix.error_message error) operation target))
  in
  if not (Process.succeeded result) then (
    let output = result.stderr ^ result.stdout in
    raise
      (Error
         (Printf.sprintf "--after-build command failed with %s%s"
            (Process.status_string result.status)
            (if output = "" then "" else ":\n" ^ output))));
  if result.stdout <> "" then print_string result.stdout;
  if result.stderr <> "" then prerr_string result.stderr

let diagnostics_for_package ~is_local (config : Config.t) =
  if is_local then config.diagnostics
  else
    let report_suffix =
      Package_metadata.issue_tracker_url config.root
      |> Option.map (fun url ->
           "\nPlease report this to the package maintainer: " ^ url)
      |> Option.value ~default:""
    in
    List.map
      (fun diagnostic -> diagnostic ^ report_suffix)
      config.deprecation_diagnostics

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
        report_failure "Compiling namespace" namespace result;
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

let source_discovery_prod ~prod ~is_local = prod || not is_local

let with_gentype_source_dirs directories (config : Config.t) =
  if config.gentype_args = [] then config
  else
    {
      config with
      gentype_args =
        config.gentype_args
        @ List.concat_map
            (fun directory -> ["-bs-gentype-source-dir"; directory])
            directories;
    }

let report_missing_source_folder (config : Config.t) path =
  let prefix = Filename.concat config.root "" in
  let relative =
    if String.starts_with ~prefix path then
      String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    else path
  in
  Printf.eprintf
    "ERROR:\nCould not read folder: %S. Specified in dependency: %s, located %S...\n%!"
    relative config.name config.root

let report_missing_sources ~is_root (config : Config.t) =
  if (not is_root) && not config.sources_defined then
    Printf.eprintf
      "WARN:\nPackage '%s' has not defined any sources, but is not the root package. This is likely a mistake. It is located: %s\n%!"
      config.name config.root

let validate_package_metadata (config : Config.t) =
  match Package_metadata.package_name config.root with
  | Error message -> raise (Error ("Could not initialize build: " ^ message))
  | Ok (Some package_name) when package_name <> config.name ->
    Printf.eprintf
      "WARN:\n\nPackage name mismatch for %s:\nThe package.json name is %S, while the rescript.json name is %S\nThis inconsistency will cause issues with package resolution.\n\n%!"
      config.root package_name config.name
  | Ok (Some _) | Ok None -> ()

let run_post_build (config : Config.t) path =
  match config.js_post_build with
  | None -> ()
  | Some command ->
    List.iter (fun spec ->
      let output = generated_js_path config path spec in
      let env, program, args =
        Platform.post_build_command ~command ~output
      in
      let result =
        match env with
        | None -> Process.run ~cwd:config.root program args
        | Some env -> Process.run ~env ~cwd:config.root program args
      in
      if not (Process.succeeded result) then (
        let captured = result.stderr ^ result.stdout in
        raise
          (Build_failure
             (Printf.sprintf "js-post-build command failed for %s%s" output
                (if captured = "" then "" else "\n" ^ captured))));
      if result.stdout <> "" then print_string result.stdout;
      if result.stderr <> "" then prerr_string result.stderr) config.package_specs

let compile_job ~bsc ~runtime ~build_dir ~watch ~(config : Config.t) ~dependency_dirs
    (module_ : Source.module_) ~is_interface path =
  let ast = Source.ast_path path in
  let namespace_args = Compiler_args.namespace_args config module_.name in
  let interface_args = if not is_interface && Option.is_some module_.interface then ["-bs-read-cmi"] else [] in
  let output_args = if is_interface then [] else List.concat_map (fun spec -> ["-bs-package-output"; Compiler_args.package_output config path spec]) config.package_specs in
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
  Process.{program = bsc; args; cwd = build_dir}, (module_, is_interface, path)

let publish_compiled ~build_dir ~ocaml_dir ~watch ~watch_output_paths ~is_local
    ~(config : Config.t) (_module, is_interface, path) result =
  let stderr =
    if is_local then result.Process.stderr
    else retain_critical_external_warnings result.stderr
  in
  let basename = Source.compiler_asset_basename config path in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let extensions = if is_interface then ["cmi"; "cmti"] else ["cmi"; "cmj"; "cmt"] in
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
          let build_output = generated_build_js_path ~build_dir config path spec in
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

let rec remove_tree path =
  if Sys.file_exists path then
    try
      if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path)
      else Sys.remove path
    with Sys_error _ | Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let rec clean_internal ~(root_config : Config.t) ~seen ~folder:root ~prod
    ~is_local ~on_clean =
  if not (Hashtbl.mem seen root) then (
    Hashtbl.add seen root ();
    let config_path = Config.path_in_root root in
    let should_clean, package_name =
      if Config.exists_in_root root then (
        let config = Config.load config_path in
        validate_package_metadata config;
        report_missing_sources ~is_root:(root = root_config.root) config;
        (* A consumer clean owns dependencies previously built in this build
           context, but not an independently built package's published tree. *)
        let owns_outputs =
          root <> root_config.root && Compiler_info.owns_outputs config
        in
        if owns_outputs then (false, None)
        else (
          let dependencies =
            config.dependencies
            @ if prod || not is_local then [] else config.dev_dependencies
          in
          List.iter (fun (dependency : Config.dependency) ->
            let directory =
              Project_context.require_dependency_directory ~workspace_root:root_config.root root
                dependency
            in
            try
              clean_internal ~root_config ~seen ~folder:directory ~prod
                ~is_local:
                  (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                     directory)
                ~on_clean
            with Config.Error message ->
              raise
                (Package_error
                   (Printf.sprintf
                      "Could not build package tree for '%s' at path '%s'. Error: %s"
                      dependency.name root_config.root message))) dependencies;
          let discovery =
            Source.discover_with_inventory config
              ~prod:(source_discovery_prod ~prod ~is_local)
              ~features:None ~filter:None
              ~on_missing:(report_missing_source_folder config)
              ~display_root:root_config.root
          in
          let output_config = with_root_options config root_config in
          cleanup_watch_output_sidecars
            ~source_files:discovery.inventory_files ~root output_config;
          List.iter
            (fun module_ ->
              List.iter
                (fun spec ->
                  let output =
                    generated_js_path output_config
                      module_.Source.implementation spec
                  in
                  remove_file output;
                  remove_file (output ^ ".map");
                  remove_file (output ^ ".rewatch-pending");
                  remove_file (output ^ ".rewatch-backup");
                  remove_file (output ^ ".map.rewatch-pending");
                  remove_file (output ^ ".map.rewatch-backup"))
                output_config.package_specs)
            discovery.modules;
          (true, Some config.name)))
      else (true, None)
    in
    if should_clean then (
      Option.iter on_clean package_name;
      List.iter
        (fun dir -> remove_tree (Filename.concat root dir))
        [lib_path "" "bs"; lib_path "" "ocaml"]))

let project_root folder =
  if not (Sys.file_exists folder) then
    raise
      (Error
         ("Could not start Rescript build: Could not write lockfile because the specified project folder does not exist: "
         ^ folder));
  Unix.realpath folder

let clean ~seen ~verbosity ~folder ~prod =
  let root = project_root folder in
  let show_plain_progress =
    verbosity >= 0
    && not (Unix.isatty Unix.stdout && Unix.isatty Unix.stderr)
  in
  let on_clean name =
    if show_plain_progress then Printf.printf "Cleaning %s\n%!" name
  in
  let release_build_lock = acquire_build_lock (Project_context.workspace_lock_root root) in
  Fun.protect ~finally:release_build_lock (fun () ->
    let root_config = Config.load_root root in
    let visited = Hashtbl.create 32 in
    List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
    clean_internal ~root_config ~seen:visited ~folder:root ~prod ~is_local:true
      ~on_clean)

let rec nearest_config directory =
  if Config.exists_in_root directory then Config.path_in_root directory
  else
    let parent = Filename.dirname directory in
    if parent = directory then raise (Error "could not find a rescript.json parent")
    else nearest_config parent

let compiler_args path =
  let source =
    try Unix.realpath path
    with
    | Sys_error message ->
      raise (Error (Printf.sprintf "Could not read source file %s: %s" path message))
    | Unix.Unix_error (error, _, _) ->
      raise
        (Error
           (Printf.sprintf "Could not read source file %s: %s" path
              (Unix.error_message error)))
  in
  if not (Filename.check_suffix source ".res" || Filename.check_suffix source ".resi") then
    raise (Error "compiler-args expects a .res or .resi source file");
  let package_config =
    Config.load (nearest_config (Filename.dirname source))
  in
  let root = Project_context.workspace_lock_root package_config.root in
  let root_config_path = Config.path_in_root root in
  let root_config =
    if root <> package_config.root && Config.exists_in_root root then
      Config.load root_config_path
    else package_config
  in
  let config = with_root_options package_config root_config in
  let relative = Project_context.relative_to config.root source in
  let runtime = runtime_path config.root in
  let is_interface = Filename.check_suffix source ".resi" in
  let has_interface = not is_interface && Sys.file_exists (source ^ "i") in
  let dependencies =
    (if Config.source_is_dev config relative then
       List.map (fun dependency -> (false, dependency)) config.dev_dependencies
     else [])
    @ List.map (fun dependency -> (true, dependency)) config.dependencies
  in
  let dependency_dirs =
    dependencies
    |> List.filter_map (fun (required, (dependency : Config.dependency)) ->
         match Project_context.dependency_path config.root dependency.name with
         | Some directory -> Some (lib_path directory "ocaml")
         | None when not required -> None
         | None ->
           raise
             (Error
                (Printf.sprintf "Expected to find dependent package %s of %s"
                   dependency.name config.name)))
  in
  let parser_args =
    Compiler_args.compiler_flags
      ~ppx_flags:(Compiler_args.filter_ppx_flags config.ppx_flags (read_file source))
      ~source_maps:false ~watch:false ~gentype:false config
    @ [
        "-absname";
        "-bs-ast";
        "-o";
        Source.ast_path relative;
        Filename.concat
          (Filename.concat Filename.parent_dir_name Filename.parent_dir_name)
          relative;
      ]
  in
  let compiler_args =
    let ast = Source.ast_path relative in
    let namespace_args =
      Compiler_args.namespace_args config (Source.module_name source)
    in
    let interface_args = if not is_interface && has_interface then ["-bs-read-cmi"] else [] in
    let output_args = if is_interface then [] else List.concat_map (fun spec -> ["-bs-package-output"; Compiler_args.package_output config relative spec]) config.package_specs in
    namespace_args @ interface_args
    @ ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
    @ ["-runtime-path"; runtime]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ Compiler_args.compiler_flags ~source_maps:true ~watch:false ~gentype:true config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  Yojson.Safe.pretty_to_string (`Assoc [
    ("compiler_args", `List (List.map (fun value -> `String value) compiler_args));
    ("parser_args", `List (List.map (fun value -> `String value) parser_args));
  ])

type compile_phase =
  [ `Start | `Interface of string | `Implementation of string | `Done ]

type compile_message = Compile_failure of string * string

type scheduled_module = {
  key: string;
  dependencies: string list;
  source: Source.module_;
  state: Build_state.module_;
  cmi_path: string;
  mutable cmi_digest_before: Digest.t option;
  prepare: unit -> unit;
  compile: is_interface:bool -> string -> Process.job;
  publish: is_interface:bool -> string -> Process.result -> string;
  package_root: string;
  is_local: bool;
  mark_warning: string -> unit;
  messages: compile_message list ref;
  phase: compile_phase ref;
}

type graph_package = {
  graph_root: string;
  graph_build_owner: string;
  graph_is_local: bool;
  graph_config: Config.t;
  graph_compile_config: Config.t;
  graph_build_dir: string;
  graph_ocaml_dir: string;
  graph_dependencies: Config.dependency list;
  graph_dependency_directories: (Config.dependency * string) list;
  graph_modules: Source.module_ list;
  graph_source_mtimes: (string, float) Hashtbl.t;
  graph_source_files: string list;
}

type build_stats = {
  mutable cleaned: int;
  mutable previous_asts: int;
  mutable parsed: int;
  mutable compiled: int;
  mutable parse_seconds: float;
  mutable diagnostics: string list;
  mutable failure: string option;
  removed_modules: (string, unit) Hashtbl.t;
  forced_parse_paths: (string, unit) Hashtbl.t;
  preparse_stderr: (string, string) Hashtbl.t;
  preparse_results: (string, Process.result) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  active_features: (string, string list option) Hashtbl.t;
  initialized_logs: (string, unit) Hashtbl.t;
  watch_outputs: (string * string * string) list ref;
  watch_output_paths: (string, unit) Hashtbl.t;
  global_raw_dependencies: (string, string list) Hashtbl.t;
  graph_packages: (string, graph_package) Hashtbl.t;
  cleanup_results: (string, Build_artifacts.cleanup_result) Hashtbl.t;
  deferred_artifact_cleanup: string list ref;
  namespace_jobs: (Process.job * (Process.result -> unit)) list ref;
  scheduled_modules: scheduled_module list ref;
  compile_cleanup: (unit -> unit) list ref;
  mutable compiler_context: Compiler_info.context option;
  mutable compile_assets: Compile_assets.t option;
  mutable build_state: Build_state.t option;
  mutable compiler_cleaned: bool;
  warning_state: Warning_state.t;
  mutable had_warnings: bool;
  poll: unit -> unit;
}

let source_is_newer ~source ~artifact =
  match modification_time source, modification_time artifact with
  | Some source_time, Some artifact_time -> source_time > artifact_time
  | Some _, None -> true
  | None, _ -> false

let source_is_not_older_than_ast compile_assets ~root ~source_mtimes path =
  let absolute = Filename.concat root path in
  match Hashtbl.find_opt source_mtimes path with
  | None ->
    source_is_newer ~source:absolute
      ~artifact:
        (Filename.concat (lib_path root "ocaml")
           (Filename.basename (Source.ast_path path)))
  | Some source_modified -> (
    match Compile_assets.ast compile_assets absolute with
    | None -> true
    | Some ast -> source_modified >= ast.modified)

let file_digest path =
  try Some (Digest.file path) with Sys_error _ | Unix.Unix_error _ -> None

let published_ast_path ~ocaml_dir source_path =
  (* bsc gives its intermediate AST an epoch mtime. The copy published after a
     successful parse is the stable freshness marker across build cycles. *)
  Filename.concat ocaml_dir (Filename.basename (Source.ast_path source_path))

type global_module = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  source: Source.module_;
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
    ~filter ~watch ~stats ~on_cleanup =
  let bsc = bsc_path () in
  let requested_features = Hashtbl.create 32 in
  let unallowed_dependencies = ref [] in
  let loaded_configs = Hashtbl.create 32 in
  let resolved_dependencies = Hashtbl.create 32 in
  let resolved_packages = Hashtbl.create 32 in
  let reported_duplicate_packages = Hashtbl.create 8 in
  let load_config root =
    match Hashtbl.find_opt loaded_configs root with
    | Some config -> config
    | None ->
      let config = Config.load_root root in
      validate_package_metadata config;
      Hashtbl.add loaded_configs root config;
      config
  in
  let resolve_dependency package_root (dependency : Config.dependency) =
    let key = package_root ^ "\000" ^ dependency.name in
    match Hashtbl.find_opt resolved_dependencies key with
    | Some resolved -> resolved
    | None ->
      let directory =
        Project_context.require_dependency_directory ~workspace_root:root_config.root
          package_root dependency
      in
      let warn_duplicate chosen =
        let warning_key = dependency.name ^ "\000" ^ directory in
        if not (Hashtbl.mem reported_duplicate_packages warning_key) then (
          Hashtbl.add reported_duplicate_packages warning_key ();
          Printf.eprintf
            "Duplicated package: %s ./%s (chosen) vs ./%s in ./%s\n%!"
            dependency.name (Project_context.relative_to root_config.root chosen)
            (Project_context.relative_to root_config.root directory)
            (Project_context.relative_to root_config.root package_root))
      in
      let resolved =
        match Hashtbl.find_opt resolved_packages dependency.name with
        | Some ((chosen, _) as resolved) ->
          if chosen <> directory then warn_duplicate chosen;
          resolved
        | None ->
          let config =
            try load_config directory
            with Config.Error message ->
              raise
                (Package_error
                   (Printf.sprintf
                      "Could not build package tree for '%s' at path '%s'. Error: %s"
                      dependency.name root_config.root message))
          in
          let resolved = (directory, config) in
          Hashtbl.add resolved_packages dependency.name resolved;
          resolved
      in
      Hashtbl.add resolved_dependencies key resolved;
      resolved
  in
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
  let rec collect ~folder:root ~features ~is_local =
    if root <> root_config.root || not (Hashtbl.mem requested_features root) then
      add_feature_request root features;
    if not (Hashtbl.mem collected root) then (
      Hashtbl.add collected root ();
      let config = load_config root in
      let dependencies =
        List.map (fun dependency -> ("dependencies", dependency))
          config.dependencies
        @ if prod || not is_local then []
          else
            List.map
              (fun dependency -> ("dev-dependencies", dependency))
              config.dev_dependencies
      in
      let resolved_dependencies =
        List.map
        (fun (kind, (dependency : Config.dependency)) ->
          let directory, dependency_config =
            resolve_dependency root dependency
          in
          if
            not
              (dependent_is_allowed dependency_config.allowed_dependents
                 config.name)
          then
            unallowed_dependencies :=
              (config.name, kind, dependency_config.name)
              :: !unallowed_dependencies;
          (dependency, directory))
        dependencies
      in
      List.iter
        (fun ((dependency : Config.dependency), directory) ->
          collect ~folder:directory ~features:dependency.features
            ~is_local:
              (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                 directory))
        resolved_dependencies)
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
  let graph_packages = ref [] in
  let rec visit ~folder:root ~features ~warn_error ~filter ~is_local =
    if not (Hashtbl.mem visited root) then (
      Hashtbl.add visited root ();
      let features =
        match Hashtbl.find_opt stats.active_features root with
        | Some features -> features
        | None -> features
      in
      let config = load_config root in
      report_missing_sources ~is_root:(root = root_config.root) config;
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
      let dependency_directories =
        List.map
          (fun dependency ->
            let directory, _ = resolve_dependency root dependency in
            (dependency, directory))
          dependencies
      in
      List.iter
        (fun ((dependency : Config.dependency), directory) ->
          visit ~folder:directory ~features:dependency.features
            ~warn_error:None ~filter:None
            ~is_local:
              (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                 directory))
        dependency_directories;
      let discovery =
        Source.discover_with_inventory config
          ~prod:(source_discovery_prod ~prod ~is_local)
          ~features ~filter
          ~on_missing:(report_missing_source_folder config)
          ~on_orphan:(fun path ->
            Printf.eprintf
              "\027[2K\r No implementation file found for interface file (skipping): %s\n%!"
              path)
          ~display_root:root_config.root
      in
      let modules = discovery.modules in
      let owns_outputs =
        root <> root_config.root && Compiler_info.owns_outputs config
      in
      let compile_config =
        let config =
          with_gentype_source_dirs discovery.gentype_dirs config
        in
        let inherited = with_root_options config root_config in
        let output_config =
          if owns_outputs then
            {
              inherited with
              package_specs = config.package_specs;
              suffix = config.suffix;
            }
          else inherited
        in
        output_config |> Compiler_args.with_local_warning_policy ~is_local
      in
      let build_dir = lib_path root "bs" in
      let ocaml_dir = lib_path root "ocaml" in
      ensure_dir build_dir;
      let package =
        let source_mtimes = Hashtbl.create (List.length discovery.source_mtimes) in
        List.iter
          (fun (path, modified) -> Hashtbl.replace source_mtimes path modified)
          discovery.source_mtimes;
        {
          graph_root = root;
          graph_build_owner = (if owns_outputs then root else root_config.root);
          graph_is_local = is_local;
          graph_config = config;
          graph_compile_config = compile_config;
          graph_build_dir = build_dir;
          graph_ocaml_dir = ocaml_dir;
          graph_dependencies = dependencies;
          graph_dependency_directories = dependency_directories;
          graph_modules = modules;
          graph_source_mtimes = source_mtimes;
          graph_source_files = discovery.inventory_files;
        }
      in
      Hashtbl.replace stats.graph_packages root package;
      graph_packages := package :: !graph_packages)
  in
  visit ~folder:root_config.root ~features ~warn_error ~filter ~is_local:true;
  let runtime = runtime_path root_config.root in
  let source_map_args =
    if root_config.source_map_dev && not watch then
      ["-bs-source-map"; "false"]
    else root_config.source_map_args
  in
  let compiler_context =
    Compiler_info.make_context ~build_root:root_config.root ~bsc_path:bsc
      ~runtime_path:runtime
      ~source_map_args
      ~package_output_specs:(Compiler_info.package_output_specs root_config)
  in
  stats.compiler_context <- Some compiler_context;
  let cleanup_started = Unix.gettimeofday () in
  List.iter
    (fun package ->
      let package_context =
        {
          compiler_context with
          build_root = package.graph_build_owner;
          package_output_specs =
            Compiler_info.package_output_specs package.graph_compile_config;
        }
      in
      if Compiler_info.needs_clean package_context package.graph_config then (
        Compiler_info.changed_package_output_specs package_context
          package.graph_config
        |> Option.iter (fun previous_specs ->
             let previous_config =
               Compiler_info.config_with_package_output_specs
                 package.graph_compile_config previous_specs
             in
             Build_artifacts.remove_public_outputs previous_config
               package.graph_modules);
        let compile_assets =
          Compile_assets.create [package.graph_ocaml_dir]
        in
        ignore
          (Build_artifacts.cleanup_stale
             ~ocaml_files:
               (Compile_assets.files compile_assets package.graph_ocaml_dir)
             ~ast_sources:
               (Compile_assets.ast_sources compile_assets
                  package.graph_ocaml_dir)
             ~root:package.graph_root
             ~ocaml_dir:package.graph_ocaml_dir
             ~source_files:package.graph_source_files
             ~is_local:
               (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                  package.graph_root)
             package.graph_compile_config package.graph_modules);
        Compiler_info.clean_package package.graph_config;
        stats.compiler_cleaned <- true);
      ensure_dir package.graph_build_dir;
      ensure_dir package.graph_ocaml_dir)
    !graph_packages;
  let compile_assets =
    !graph_packages
    |> List.map (fun package -> package.graph_ocaml_dir)
    |> Compile_assets.create
  in
  List.iter
    (fun package ->
      let cleanup =
        Build_artifacts.cleanup_stale
          ~ocaml_files:
            (Compile_assets.files compile_assets package.graph_ocaml_dir)
          ~ast_sources:
            (Compile_assets.ast_sources compile_assets package.graph_ocaml_dir)
          ~root:package.graph_root
          ~ocaml_dir:package.graph_ocaml_dir
          ~source_files:package.graph_source_files
          ~is_local:
            (Project_context.is_local_dependency_canonical ~workspace:root_config.root
               package.graph_root)
          package.graph_compile_config package.graph_modules
      in
      Hashtbl.replace stats.cleanup_results package.graph_root
        cleanup;
      stats.deferred_artifact_cleanup :=
        cleanup.deferred_artifacts @ !(stats.deferred_artifact_cleanup);
      stats.cleaned <- stats.cleaned + List.length cleanup.removed_modules;
      stats.previous_asts <-
        stats.previous_asts + cleanup.previous_ast_count;
      List.iter
        (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
        cleanup.removed_modules)
    !graph_packages;
  stats.compile_assets <- Some compile_assets;
  on_cleanup (Unix.gettimeofday () -. cleanup_started);
  let parse_started = Unix.gettimeofday () in
  let parse_entries =
    !graph_packages
    |> List.concat_map (fun package ->
         package.graph_modules
         |> List.concat_map (fun module_ ->
              module_.Source.implementation
              :: Option.to_list module_.Source.interface)
         |> List.filter_map (fun path ->
              if source_is_not_older_than_ast compile_assets
                   ~root:package.graph_root
                   ~source_mtimes:package.graph_source_mtimes path
              then
                Some (package, path)
              else None))
  in
  let parse_results =
    parse_entries
    |> List.map (fun (package, path) ->
         fst
           (parse_job ~bsc ~build_dir:package.graph_build_dir
              ~config:package.graph_compile_config path))
    |> Process.run_parallel ~poll:stats.poll
  in
  let failed_parse_paths = Hashtbl.create 8 in
  List.iter2
    (fun (package, path) result ->
      let absolute_path = Filename.concat package.graph_root path in
      Hashtbl.replace stats.forced_parse_paths absolute_path ();
      Hashtbl.replace stats.preparse_results absolute_path result;
      if Process.succeeded result then (
        if result.stderr <> "" then
          Hashtbl.replace stats.preparse_stderr absolute_path result.stderr)
      else Hashtbl.replace failed_parse_paths absolute_path ())
    parse_entries parse_results;
  let nodes = ref [] in
  List.iter
    (fun package ->
      List.iter
        (fun module_ ->
          let intf_dependencies =
            match module_.Source.interface with
            | None -> []
            | Some path ->
              if
                Hashtbl.mem failed_parse_paths
                  (Filename.concat package.graph_root path)
              then []
              else
                ast_dependencies ~build_dir:package.graph_build_dir
                  (Source.ast_path path)
          in
          let raw_dependencies =
            List.sort_uniq String.compare
              ((if
                  Hashtbl.mem failed_parse_paths
                    (Filename.concat package.graph_root
                       module_.Source.implementation)
                then []
                else
                  ast_dependencies ~build_dir:package.graph_build_dir
                    (Source.ast_path module_.Source.implementation))
              @ intf_dependencies)
          in
          let compiler_base =
            global_module_key package.graph_compile_config module_.Source.name
          in
          if Option.is_none (Compile_assets.cmt compile_assets compiler_base) then
            Hashtbl.replace stats.forced_parse_paths
              (Filename.concat package.graph_root module_.Source.implementation)
              ();
          Hashtbl.replace stats.global_raw_dependencies compiler_base
            raw_dependencies;
          nodes :=
            {
              key = compiler_base;
              package_name = package.graph_config.name;
              package_root = package.graph_root;
              source_path = module_.Source.implementation;
              source = module_;
              namespace = package.graph_compile_config.namespace;
              namespace_entry = package.graph_compile_config.namespace_entry;
              allowed_dependencies =
                List.map
                  (fun (dependency : Config.dependency) -> dependency.name)
                  package.graph_dependencies;
              raw_dependencies;
            }
            :: !nodes)
        package.graph_modules)
    !graph_packages;
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
    let is_visible dependency_node =
      dependency_node.package_name = node.package_name
      || List.mem dependency_node.package_name node.allowed_dependencies
    in
    match Hashtbl.find_opt by_key local_key with
    | Some dependency_node
      when dependency_node.package_name = node.package_name ->
      [local_key]
    | _ ->
      (match Hashtbl.find_opt by_key raw_name with
      | Some dependency_node when is_visible dependency_node ->
        [raw_name]
      | _ ->
        let explicit_namespaced_module =
          match String.split_on_char '.' dependency with
        | namespace :: module_name :: _ ->
          [module_name ^ "-" ^ namespace; module_name ^ "-@" ^ namespace]
          |> List.find_opt (fun key ->
               match Hashtbl.find_opt by_key key with
               | Some dependency_node
                 when dependency_node.namespace = Some namespace
                      && is_visible dependency_node ->
                 true
               | Some _ | None -> false)
        | _ -> None
        in
        match explicit_namespaced_module with
        | Some key -> [key]
        | None ->
          nodes
          |> List.filter_map (fun dependency_node ->
               if
                 dependency_node.namespace = Some raw_name
                 && is_visible dependency_node
               then Some dependency_node.key
               else None))
  in
  let graph_nodes =
    List.map
      (fun node ->
        ( node,
          node.raw_dependencies
          |> List.concat_map (resolve_dependency node)
          |> List.filter (fun dependency -> dependency <> node.key)
          |> List.sort_uniq String.compare ))
      nodes
  in
  let build_state = Build_state.create (List.length graph_nodes) in
  let modified = Option.map (fun entry -> entry.Compile_assets.modified) in
  List.iter
    (fun (node, _) ->
      Build_state.add build_state ~key:node.key
        ~package_name:node.package_name ~package_root:node.package_root
        ~source:node.source ~raw_dependencies:node.raw_dependencies
        ~last_compiled_cmi:(Compile_assets.cmi compile_assets node.key |> modified)
        ~last_compiled_cmt:(Compile_assets.cmt compile_assets node.key |> modified))
    graph_nodes;
  List.iter
    (fun (node, dependencies) ->
      Build_state.set_dependencies build_state ~key:node.key dependencies)
    graph_nodes;
  stats.build_state <- Some build_state;
  let cycle =
    try
      ignore
        (Graph.topological_sort graph_nodes
           ~name:(fun (node, _) -> node.key)
           ~deps:snd);
      None
    with Graph.Cycle cycle ->
      let blocked =
        blocked_dependents
          (List.map
             (fun (node, dependencies) -> (node.key, dependencies))
             graph_nodes)
          cycle
      in
      Some (cycle, blocked, by_key)
  in
  stats.parse_seconds <- Unix.gettimeofday () -. parse_started;
  cycle

let rec run_internal ~(root_config : Config.t) ~seen ~folder:root ~prod ~features
    ~warn_error ~watch ~filter ~is_local ~stats =
  let features =
    match Hashtbl.find_opt stats.active_features root with
    | Some features -> features
    | None -> features
  in
  Hashtbl.replace seen root ();
  let prepared = Hashtbl.find_opt stats.graph_packages root in
  let config =
    match prepared with
    | Some package -> package.graph_config
    | None ->
      let config = Config.load_root root in
      (match warn_error with
      | None -> config
      | Some value -> {config with warning_flags = ["-warn-error"; value]})
  in
  stats.diagnostics <-
    List.rev_append
      (diagnostics_for_package ~is_local config)
      stats.diagnostics;
  let dependency_directories =
    let candidates =
      match prepared with
      | Some package -> package.graph_dependency_directories
      | None ->
        let dependencies : Config.dependency list =
          config.dependencies
          @ if prod || not is_local then [] else config.dev_dependencies
        in
        dependencies
        |> List.map (fun (dependency : Config.dependency) ->
             match Project_context.dependency_path root dependency.name with
             | Some directory -> (dependency, directory)
             | None ->
               raise
                 (Package_error
                    (Printf.sprintf
                       "Could not build package tree reading dependency '%s' at path '%s'. Error: Could not resolve dependency %s"
                       dependency.name root_config.root dependency.name)))
    in
    candidates
    |> List.filter_map (fun ((dependency : Config.dependency), candidate) ->
      let () = match candidate with
        | candidate when Hashtbl.mem seen candidate -> ()
        | candidate when Config.exists_in_root candidate ->
          (try
             run_internal ~root_config ~seen ~folder:candidate ~prod
               ~features:dependency.features ~warn_error:None ~watch
               ~filter:None
               ~is_local:
                 (Project_context.is_local_dependency_canonical ~workspace:root_config.root
                    candidate)
               ~stats
           with Build_failure output ->
             if Option.is_none stats.failure then stats.failure <- Some output)
        | _ -> ()
      in
      let ocaml = lib_path candidate "ocaml" in
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
  let bsc, runtime =
    match stats.compiler_context with
    | Some context -> (context.bsc_path, context.runtime_path)
    | None -> raise (Error "Compiler context was not initialized")
  in
  let build_state =
    match stats.build_state with
    | Some state -> state
    | None -> raise (Error "build state was not initialized")
  in
  let compile_assets =
    match stats.compile_assets with
    | Some state -> state
    | None -> raise (Error "compile asset state was not initialized")
  in
  let build_dir =
    match prepared with
    | Some package -> package.graph_build_dir
    | None -> lib_path root "bs"
  in
  let ocaml_dir =
    match prepared with
    | Some package -> package.graph_ocaml_dir
    | None -> lib_path root "ocaml"
  in
  ensure_dir build_dir;
  ensure_dir ocaml_dir;
  Compiler_log.initialize root;
  Hashtbl.replace stats.initialized_logs root ();
  let modules =
    match prepared with
    | Some package -> package.graph_modules
    | None ->
      Source.discover config
        ~prod:(source_discovery_prod ~prod ~is_local)
        ~features ~filter
        ~display_root:root_config.root
        ~on_missing:(report_missing_source_folder config)
        ~on_orphan:(fun path ->
          Printf.eprintf
            "\027[2K\r No implementation file found for interface file (skipping): %s\n%!"
            path)
  in
  let config =
    match prepared with
    | Some package -> package.graph_compile_config
    | None ->
      with_root_options config root_config
      |> Compiler_args.with_local_warning_policy ~is_local
  in
  let cleanup =
    match Hashtbl.find_opt stats.cleanup_results root with
    | Some result -> result
    | None ->
      Build_artifacts.cleanup_stale ~root ~ocaml_dir ~is_local config modules
  in
  let removed_modules = cleanup.removed_modules in
  if not (Hashtbl.mem stats.cleanup_results root) then
    stats.deferred_artifact_cleanup :=
      cleanup.deferred_artifacts @ !(stats.deferred_artifact_cleanup);
  List.iter
    (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
    removed_modules;
  let names = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ -> Hashtbl.replace names module_.Source.name module_)
    modules;
  let parse_paths =
    List.concat_map (fun module_ ->
      module_.Source.implementation :: Option.to_list module_.interface) modules
  in
  let dirty_parse_paths =
    parse_paths
    |> List.filter (fun path ->
         List.mem (Source.module_name path) removed_modules
         || Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)
         ||
         match prepared, stats.compile_assets with
         | Some package, Some compile_assets ->
           source_is_not_older_than_ast compile_assets ~root
             ~source_mtimes:package.graph_source_mtimes path
         | None, _ | _, None ->
           source_is_newer ~source:(Filename.concat root path)
             ~artifact:(published_ast_path ~ocaml_dir path))
  in
  let parse_paths_to_run =
    dirty_parse_paths
    |> List.filter (fun path ->
         not
           (Hashtbl.mem stats.forced_parse_paths (Filename.concat root path)))
  in
  let parsed =
    List.map2 (fun path result -> (path, Some result)) parse_paths_to_run
      (Process.run_parallel ~poll:stats.poll
         (List.map
            (fun path -> fst (parse_job ~bsc ~build_dir ~config path))
            parse_paths_to_run))
    @ (dirty_parse_paths
      |> List.filter (fun path ->
           Hashtbl.mem stats.forced_parse_paths (Filename.concat root path))
      |> List.map (fun path ->
           ( path,
             Hashtbl.find_opt stats.preparse_results
               (Filename.concat root path) )))
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
          let output =
            Printf.sprintf "Error in %s:\n%s%s" config.name result.stderr
              result.stdout
          in
          Compiler_log.append root output;
          raise (Parse_failure output))
      result;
    let stderr =
      if is_local then stderr else retain_critical_external_warnings stderr
    in
    if stderr <> "" then stats.had_warnings <- true;
    if stderr <> "" then Compiler_log.append root stderr;
    if stderr <> "" then prerr_string stderr;
    let ast = Source.ast_path path in
    if is_local && stderr <> "" then warning_asts := ast :: !warning_asts;
    copy_existing_file ~ensure_parent:false (Filename.concat build_dir ast)
      (Filename.concat (lib_path config.root "ocaml") (Filename.basename ast));
    copy_existing_file ~ensure_parent:false (Filename.concat config.root path)
      (Filename.concat (lib_path config.root "ocaml") (Filename.basename path))) parsed;
  let raw_dependencies = Hashtbl.create (List.length modules) in
  let parse_dirty_modules = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ ->
      let global_key = global_module_key config module_.Source.name in
      let dependencies =
        match Hashtbl.find_opt stats.global_raw_dependencies global_key with
        | Some dependencies -> dependencies
        | None ->
          let impl_ast = Source.ast_path module_.Source.implementation in
          let impl_deps = ast_dependencies ~build_dir impl_ast in
          let intf_deps =
            match module_.interface with
            | None -> []
            | Some path -> ast_dependencies ~build_dir (Source.ast_path path)
          in
          List.sort_uniq String.compare (impl_deps @ intf_deps)
      in
      Hashtbl.replace raw_dependencies module_.Source.name dependencies;
      let paths =
        module_.Source.implementation :: Option.to_list module_.Source.interface
      in
      if List.exists (fun path -> List.mem path dirty_parse_paths) paths then
        Hashtbl.replace parse_dirty_modules module_.Source.name ();
      module_.deps <-
        if Hashtbl.mem stats.blocked_modules global_key then []
        else
          List.filter
            (fun dep -> dep <> module_.name && Hashtbl.mem names dep)
            dependencies)
    modules;
  stats.parsed <- stats.parsed + Hashtbl.length parse_dirty_modules;
  let compile_warning_modules = Hashtbl.create 8 in
  let module_is_dirty module_ state =
    let global_key = global_module_key config module_.Source.name in
    let module_name = Source.module_name module_.Source.implementation in
    let source = Filename.concat root module_.Source.implementation in
    let outputs_exist =
      match Hashtbl.find_opt stats.cleanup_results root with
      | Some cleanup ->
        List.for_all
          (fun spec ->
            Hashtbl.mem cleanup.present_public_outputs
              (generated_js_path config module_.Source.implementation spec))
          config.package_specs
      | None ->
        List.for_all
          (fun spec ->
            Sys.file_exists
              (generated_js_path config module_.Source.implementation spec))
          config.package_specs
    in
    let raw_dependencies =
      Hashtbl.find_opt raw_dependencies module_.Source.name
      |> Option.value ~default:[]
    in
    let dependency_is_newer dependency =
      let dependency_state = Build_state.find_exn build_state dependency in
      Build_state.dependency_compiled_after state dependency_state
    in
    not (Hashtbl.mem stats.blocked_modules global_key)
    &&
    (Hashtbl.mem parse_dirty_modules module_.Source.name
    || List.mem module_name removed_modules
    || (match Compile_assets.ast compile_assets source, state.last_compiled_cmt with
       | Some ast, Some cmt_time -> ast.modified >= cmt_time
       | Some _, None -> true
       | None, _ -> false)
    || not (Build_state.has_complete_compile_assets state)
    || not outputs_exist
    || List.exists (fun dependency -> List.mem dependency removed_modules)
         raw_dependencies
    || List.exists
         (fun dependency -> Hashtbl.mem stats.removed_modules dependency)
         raw_dependencies
    || List.exists dependency_is_newer state.dependencies)
  in
  let prepare_outputs module_ =
    let path = module_.Source.implementation in
    List.iter
      (fun spec ->
        let output = generated_js_path config path spec in
        let dirty_ast = Filename.concat build_dir (Source.ast_path path) in
        ensure_dir (Filename.dirname output);
        if watch then (
          prepare_watch_output stats.watch_outputs stats.watch_output_paths
            ~dirty_ast output;
          prepare_watch_output stats.watch_outputs stats.watch_output_paths
            ~dirty_ast (output ^ ".map")))
      config.package_specs
  in
  let compile_process module_ ~is_interface path =
    fst
      (compile_job ~bsc ~runtime ~build_dir ~watch ~config
         ~dependency_dirs:(dependency_dirs_for module_)
         module_ ~is_interface path)
  in
  let publish module_ ~is_interface path result =
    publish_compiled ~build_dir ~ocaml_dir ~watch
      ~watch_output_paths:stats.watch_output_paths ~is_local ~config
      (module_, is_interface, path) result
  in
  let scheduled =
    List.map
      (fun module_ ->
        let key = global_module_key config module_.Source.name in
        let state = Build_state.find_exn build_state key in
        (* Rust fixes the initial dirty set before dispatch. Files published by
           concurrently finishing jobs must not change this module's decision;
           only explicit CMI-change propagation may do that. *)
        state.compile_dirty <- module_is_dirty module_ state;
        let dependencies =
          if Hashtbl.mem stats.blocked_modules key then []
          else state.dependencies
        in
        let cmi_path =
          Filename.concat ocaml_dir
            (Source.compiler_asset_basename config module_.Source.implementation
            ^ ".cmi")
        in
        {
          key;
          dependencies;
          source = module_;
          state;
          cmi_path;
          cmi_digest_before = None;
          prepare = (fun () -> prepare_outputs module_);
          compile =
            (fun ~is_interface path ->
              compile_process module_ ~is_interface path);
          publish =
            (fun ~is_interface path result ->
              publish module_ ~is_interface path result);
          package_root = config.root;
          is_local;
          mark_warning =
            (fun path ->
              Hashtbl.replace compile_warning_modules
                (Source.module_name path) ());
          messages = ref [];
          phase = ref `Start;
        })
      modules
  in
  Option.iter
    (fun namespace ->
      let namespace =
        match config.namespace_entry with
        | Some _ -> "@" ^ namespace
        | None -> namespace
      in
      let package_dirty =
        List.exists
          (fun (scheduled : scheduled_module) -> scheduled.state.compile_dirty)
          scheduled
      in
      Option.iter
        (fun job -> stats.namespace_jobs := job :: !(stats.namespace_jobs))
        (namespace_job ~bsc ~runtime ~build_dir ~ocaml_dir
           ~entry:config.namespace_entry ~package_dirty namespace modules))
    config.namespace;
  stats.scheduled_modules := scheduled @ !(stats.scheduled_modules);
  stats.compile_cleanup :=
    (fun () ->
      (* The published AST is the freshness marker. Keep bsc's working AST in
         lib/bs, as Rust does, and remove only the published copy so warnings
         are replayed without deleting a usable intermediate artifact. *)
      if not watch then
        Hashtbl.iter
          (fun module_name () ->
            match
              List.find_opt
                (fun module_ -> module_.Source.name = module_name)
                modules
            with
            | None -> ()
            | Some module_ ->
              let paths =
                module_.Source.implementation
                :: Option.to_list module_.Source.interface
              in
              List.iter
                (fun path ->
                  let ast = Source.ast_path path in
                  remove_file
                    (Filename.concat ocaml_dir (Filename.basename ast)))
                paths)
          compile_warning_modules;
      List.iter
        (fun ast ->
          remove_file (Filename.concat ocaml_dir (Filename.basename ast)))
        !warning_asts)
    :: !(stats.compile_cleanup);
  ()

let run_scheduled_modules stats =
  let build_state =
    match stats.build_state with
    | Some state -> state
    | None -> raise (Error "build state was not initialized")
  in
  let compile_assets =
    match stats.compile_assets with
    | Some state -> state
    | None -> raise (Error "compile asset state was not initialized")
  in
  let finish_successful_compile scheduled =
    let cmi_digest_after = file_digest scheduled.cmi_path in
    let cmi_changed =
      match scheduled.cmi_digest_before, cmi_digest_after with
      | Some before, Some after -> before <> after
      | _ -> true
    in
    let cmt_path = Filename.remove_extension scheduled.cmi_path ^ ".cmt" in
    Compile_assets.refresh_cmi compile_assets ~key:scheduled.key
      ~path:scheduled.cmi_path;
    Compile_assets.refresh_cmt compile_assets ~key:scheduled.key ~path:cmt_path;
    scheduled.state.last_compiled_cmi <-
      (Compile_assets.cmi compile_assets scheduled.key
      |> Option.map (fun entry -> entry.Compile_assets.modified));
    scheduled.state.last_compiled_cmt <-
      (Compile_assets.cmt compile_assets scheduled.key
      |> Option.map (fun entry -> entry.Compile_assets.modified));
    scheduled.state.compile_dirty <- false;
    if cmi_changed then
      Build_state.mark_dependents_compile_dirty build_state scheduled.state
        ~is_blocked:(Hashtbl.mem stats.blocked_modules)
  in
  let warning_paths =
    !(stats.scheduled_modules)
    |> List.concat_map (fun (scheduled : scheduled_module) ->
         (scheduled.source.Source.implementation
         :: Option.to_list scheduled.source.Source.interface)
         |> List.map (fun path -> Filename.concat scheduled.package_root path))
  in
  Warning_state.retain_paths stats.warning_state warning_paths;
  let works =
    !(stats.scheduled_modules)
    |> List.map (fun (scheduled : scheduled_module) ->
         Process.
           {
             key = scheduled.key;
             dependencies = scheduled.dependencies;
             value = scheduled;
           })
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun cleanup -> cleanup ()) !(stats.compile_cleanup))
    (fun () ->
      let record_result scheduled ~is_interface path result =
        let message =
          if Process.succeeded result then
            try
              match scheduled.publish ~is_interface path result with
              | "" ->
                Warning_state.remove stats.warning_state
                  ~package_root:scheduled.package_root ~path;
                None
              | warning ->
                stats.had_warnings <- true;
                Warning_state.set stats.warning_state
                  ~module_name:scheduled.key
                  ~package_root:scheduled.package_root ~path ~output:warning;
                if scheduled.is_local then scheduled.mark_warning path;
                None
            with Build_failure output ->
              Warning_state.remove stats.warning_state
                ~package_root:scheduled.package_root ~path;
              Some (Compile_failure (path, output))
          else (
            Warning_state.remove stats.warning_state
              ~package_root:scheduled.package_root ~path;
            Some
              (Compile_failure
                 (path, result.Process.stderr ^ result.Process.stdout)))
        in
        Option.iter
          (fun message ->
            scheduled.messages := message :: !(scheduled.messages))
          message
      in
      let scheduler_failed =
        try
          Process.run_dependency_graph ~poll:stats.poll works
            ~is_fatal:(function Scheduled_failure _ -> false | _ -> true)
            ~next:(fun scheduled result ->
              match result, !(scheduled.phase) with
              | None, `Start ->
                if scheduled.state.compile_dirty then (
                  stats.compiled <- stats.compiled + 1;
                  scheduled.prepare ();
                  scheduled.cmi_digest_before <- file_digest scheduled.cmi_path;
                  match scheduled.source.Source.interface with
                  | Some path ->
                    scheduled.phase := `Interface path;
                    Some (scheduled.compile ~is_interface:true path)
                  | None ->
                    let path = scheduled.source.Source.implementation in
                    scheduled.phase := `Implementation path;
                    Some (scheduled.compile ~is_interface:false path))
                else (
                  scheduled.phase := `Done;
                  None)
              | Some result, `Interface path ->
                record_result scheduled ~is_interface:true path result;
                let path = scheduled.source.Source.implementation in
                scheduled.phase := `Implementation path;
                Some (scheduled.compile ~is_interface:false path)
              | Some result, `Implementation path ->
                record_result scheduled ~is_interface:false path result;
                scheduled.phase := `Done;
                if !(scheduled.messages) <> [] then
                  raise (Scheduled_failure scheduled.key)
                else (
                  finish_successful_compile scheduled;
                  None)
              | None, (`Interface _ | `Implementation _ | `Done)
              | Some _, (`Start | `Done) ->
                raise (Error "invalid compiler scheduler state"));
          false
        with Scheduled_failure _ -> true
      in
      let failures = ref [] in
      !(stats.scheduled_modules)
      |> List.sort (fun (first : scheduled_module) second ->
           String.compare first.key second.key)
      |> List.iter (fun (scheduled : scheduled_module) ->
           !(scheduled.messages) |> List.rev
           |> List.iter (fun (Compile_failure (_, output)) ->
                failures := (scheduled, output) :: !failures));
      Warning_state.entries stats.warning_state
      |> List.iter (fun entry ->
           Compiler_log.append entry.Warning_state.package_root entry.output);
      let failures = List.rev !failures in
      List.iter
        (fun ((scheduled : scheduled_module), output) ->
          Compiler_log.append scheduled.package_root output)
        failures;
      match failures, scheduler_failed with
      | [], false -> ()
      | [], true -> raise (Error "compiler scheduler stopped without a diagnostic")
      | failures, _ ->
        failures |> List.map snd |> String.concat "" |> fun output ->
        raise (Build_failure output))

let run_namespace_jobs stats =
  let jobs = List.rev !(stats.namespace_jobs) in
  let results = Process.run_parallel ~poll:stats.poll (List.map fst jobs) in
  List.iter2 (fun (_, finish) result -> finish result) jobs results

let write_source_dirs (root_config : Config.t) stats =
  let packages =
    Hashtbl.to_seq_values stats.graph_packages |> List.of_seq
    |> List.sort (fun left right -> String.compare left.graph_root right.graph_root)
  in
  packages
  |> List.iter (fun package ->
       if package.graph_root <> root_config.root then
         remove_file
           (path_of_parts package.graph_root ["lib"; "bs"; ".sourcedirs.json"]));
  let local_packages = List.filter (fun package -> package.graph_is_local) packages in
  let source_directories package =
    package.graph_modules
    |> List.map (fun module_ -> Filename.dirname module_.Source.implementation)
    |> List.sort_uniq String.compare
  in
  let relative_package_root package =
    if package.graph_root = root_config.root then ""
    else Project_context.relative_to root_config.root package.graph_root
  in
  let dirs =
    local_packages
    |> List.concat_map (fun package ->
         let relative_root = relative_package_root package in
         source_directories package
         |> List.map (fun directory ->
              if relative_root = "" then directory
              else Filename.concat relative_root directory))
    |> List.sort_uniq String.compare
  in
  let package_roots = Hashtbl.create 16 in
  local_packages
  |> List.iter (fun package ->
       package.graph_dependency_directories
       |> List.iter (fun ((dependency : Config.dependency), path) ->
            Hashtbl.replace package_roots dependency.name path));
  let package_roots =
    Hashtbl.to_seq package_roots |> List.of_seq
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  in
  let scans =
    local_packages
    |> List.map (fun package ->
         let relative_root = relative_package_root package in
         let build_root =
           if relative_root = "" then path_of_parts "" ["lib"; "bs"]
           else path_of_parts relative_root ["lib"; "bs"]
         in
         Source_dirs.
           {
             build_root;
             scan_dirs = source_directories package;
             also_scan_build_root = true;
           })
    |> List.sort (fun (left : Source_dirs.scan) right ->
         String.compare left.build_root right.build_root)
  in
  Source_dirs.write ~root:root_config.root ~dirs ~packages:package_roots ~scans

let write_build_ninja stats =
  Hashtbl.iter
    (fun _ package ->
      let path = Filename.concat package.graph_build_dir "build.ninja" in
      let channel = open_out_bin path in
      close_out channel)
    stats.graph_packages

let run_with_warning_state ~poll ~warning_state ~compilation_kind ~no_timing
    ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch ~after_build
    ~filter =
  let started_at = Unix.gettimeofday () in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let is_rebuild = compilation_kind = Some "incremental" in
  let should_write_build_ninja = (not watch) || is_rebuild in
  let root = project_root folder in
  let root_config = Config.load_root root in
  if verbosity > 0 then
    Printf.printf "Created project context for %S\n%!" root_config.root;
  let visited = Hashtbl.create 32 in
  let stats =
    {
      cleaned = 0;
      previous_asts = 0;
      parsed = 0;
      compiled = 0;
      parse_seconds = 0.;
      diagnostics = [];
      failure = None;
      removed_modules = Hashtbl.create 16;
      forced_parse_paths = Hashtbl.create 16;
      preparse_stderr = Hashtbl.create 16;
      preparse_results = Hashtbl.create 16;
      blocked_modules = Hashtbl.create 16;
      active_features = Hashtbl.create 16;
      initialized_logs = Hashtbl.create 16;
      watch_outputs = ref [];
      watch_output_paths = Hashtbl.create 16;
      global_raw_dependencies = Hashtbl.create 64;
      graph_packages = Hashtbl.create 32;
      cleanup_results = Hashtbl.create 32;
      deferred_artifact_cleanup = ref [];
      namespace_jobs = ref [];
      scheduled_modules = ref [];
      compile_cleanup = ref [];
      compiler_context = None;
      compile_assets = None;
      build_state = None;
      compiler_cleaned = false;
      warning_state;
      had_warnings = false;
      poll;
    }
  in
  List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
  let finalize_logs () =
    Hashtbl.iter (fun package_root () -> Compiler_log.finalize package_root)
      stats.initialized_logs;
    Hashtbl.clear stats.initialized_logs
  in
  let outputs_finished = ref false in
  let build_ninja_written = ref false in
  let write_build_ninja_once () =
    if should_write_build_ninja && not !build_ninja_written then (
      write_build_ninja stats;
      build_ninja_written := true)
  in
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
    if not interactive then
      if watch then (
        if success then Printf.printf "Finished compilation\n%!")
      else (
        Printf.printf "Cleaned %d/%d\nParsed %d source files\n%!" stats.cleaned
          stats.previous_asts stats.parsed;
        if success then Printf.printf "Compiled %d modules\n%!" stats.compiled
        else Printf.eprintf "Compiled %d modules\n%!" stats.compiled);
    let diagnostics =
      stats.diagnostics |> List.rev |> List.sort_uniq String.compare
    in
    let warning_entries = Warning_state.entries stats.warning_state in
    warning_entries
    |> List.iter (fun entry -> prerr_string entry.Warning_state.output);
    if warning_entries <> [] && diagnostics = [] then prerr_newline ();
    flush stderr;
    if diagnostics <> [] then
      prerr_endline (String.concat "\n\n" diagnostics);
    if success && interactive then
      let seconds =
        if no_timing then 0. else Unix.gettimeofday () -. started_at
      in
      Printf.printf "\n%s\n%!"
        (Output.finished_compilation_message ~kind:compilation_kind
           ~warnings:
             (stats.had_warnings || diagnostics <> []
             || Warning_state.entries stats.warning_state <> [])
           ~seconds)
  in
  let report_failure output =
    write_build_ninja_once ();
    report ~success:false ();
    prerr_string output;
    prerr_newline ();
    raise
      (Error
        ("Incremental build failed. Error: \027[2K\r  Failed to Compile. "
        ^ "See Errors Above"))
  in
  let report_parse_failure output =
    write_build_ninja_once ();
    finish_watch_outputs ~success:false;
    finalize_logs ();
    if interactive then
      prerr_endline
        (Output.parsing_failed_message ~step:(if is_rebuild then "1/2" else "2/3")
           ~seconds:(if no_timing then 0. else stats.parse_seconds))
    else Printf.printf "Cleaned %d/%d\n%!" stats.cleaned stats.previous_asts;
    prerr_endline output;
    raise
      (Error
         "Incremental build failed. Error: \027[2K\r  Could not parse Source Files")
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
          (Project_context.relative_to root_config.root absolute)
    in
    "\nCan't continue... Found a circular dependency in your code:\n"
    ^ (cycle |> List.map format_node |> String.concat "\n → ")
    ^ "\nPossible solutions:\n- Extract shared code into a new module both depend on.\n"
  in
  let release_build_lock = acquire_build_lock (Project_context.workspace_lock_root root) in
  let phase_seconds seconds = if no_timing then 0. else seconds in
  let parse_step = if is_rebuild then "1/2" else "2/3" in
  let compile_step = if is_rebuild then "2/2" else "3/3" in
  let execute () =
    poll ();
    let cycle =
      prepare_global_graph ~root_config ~prod ~features ~warn_error ~filter
        ~watch ~stats
        ~on_cleanup:(fun seconds ->
          if interactive && not is_rebuild then (
            if stats.compiler_cleaned then
              print_endline (Output.compiler_cleanup_message ~step:"1/3");
            print_endline
              (Output.cleanup_message ~step:"1/3" ~cleaned:stats.cleaned
                 ~total:stats.previous_asts ~seconds:(phase_seconds seconds))))
    in
    poll ();
    if stats.compiler_cleaned && not interactive then
      print_endline "Cleaned previous build due to compiler update";
    Option.iter
      (fun (_, blocked, _) ->
        List.iter
          (fun name -> Hashtbl.replace stats.blocked_modules name ())
          blocked)
      cycle;
    run_internal ~root_config ~seen:visited ~folder:root ~prod ~features
      ~warn_error ~watch ~filter ~is_local:true ~stats;
    poll ();
    if interactive then
      print_endline
        (Output.parsing_message ~step:parse_step ~count:stats.parsed
           ~seconds:(phase_seconds stats.parse_seconds));
    let compile_started = Unix.gettimeofday () in
    (try
       run_namespace_jobs stats;
       run_scheduled_modules stats
     with Build_failure output ->
       if Option.is_none stats.failure then stats.failure <- Some output);
    if interactive then (
      let seconds = phase_seconds (Unix.gettimeofday () -. compile_started) in
      match stats.failure with
      | None ->
        print_endline
          (Output.compiling_message ~step:compile_step ~count:stats.compiled
             ~seconds)
      | Some _ ->
        prerr_endline
          (Output.compilation_failed_message ~step:compile_step
             ~count:stats.compiled ~seconds));
    (match stats.failure, cycle with
    | Some output, _ -> report_failure output
    | None, Some (names, _, by_key) ->
      let output = format_cycle names by_key in
      names
      |> List.filter_map (Hashtbl.find_opt by_key)
      |> List.map (fun node -> node.package_root)
      |> List.sort_uniq String.compare
      |> List.iter (fun package_root -> Compiler_log.append package_root output);
      report_failure output
    | None, None ->
      Option.iter
        (fun (context : Compiler_info.context) ->
          Hashtbl.iter
            (fun _ package ->
              let package_context =
                {
                  context with
                  build_root = package.graph_build_owner;
                  package_output_specs =
                    Compiler_info.package_output_specs
                      package.graph_compile_config;
                }
              in
              Compiler_info.write_package package_context package.graph_config)
            stats.graph_packages)
        stats.compiler_context;
      write_source_dirs root_config stats;
      write_build_ninja_once ();
      Option.iter
        (fun command ->
          expose_watch_outputs ();
          finish_watch_outputs ~success:true;
          finalize_logs ();
          release_build_lock ();
          run_after_build ~root command)
        after_build;
      report ~success:true ())
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter remove_file !(stats.deferred_artifact_cleanup);
      if not !outputs_finished then finish_watch_outputs ~success:false;
      finalize_logs ();
      release_build_lock ())
    (fun () ->
      try execute () with
      | Build_failure output -> report_failure output
      | Parse_failure output -> report_parse_failure output)

let run ~seen ~verbosity ~folder ~prod ~features ~warn_error ~watch ~after_build
    ~filter ~no_timing =
  run_with_warning_state ~warning_state:(Warning_state.create ())
    ~poll:(fun () -> ()) ~compilation_kind:None ~no_timing ~seen ~verbosity
    ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter

let watch ~verbosity ~folder ~prod ~features ~warn_error ~after_build ~filter
    ~clear_screen =
  let root = project_root folder in
  ignore (Config.load_root root);
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
          | Some owner when not (valid_lock_owner owner) ->
            raise (malformed_lock_error ())
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
      | Some owner when not (valid_lock_owner owner) ->
        raise (malformed_lock_error ())
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
  let stop_requested = ref false in
  let waiting_for_native_event = ref false in
  let stop () =
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    Sys.set_signal Sys.sigterm Sys.Signal_ignore;
    if !waiting_for_native_event then stop_requested := true else raise Stop_watch
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> stop ()));
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> stop ()));
  let watch_context () =
    let visited = Hashtbl.create 32 in
    Hashtbl.add visited root ();
    let roots = ref [root] in
    let paths = ref [] in
    let add_path directory recursive =
      paths := Native_watcher.{directory; recursive} :: !paths
    in
    let rec nearest_existing_directory package_root directory =
      if Sys.file_exists directory then directory
      else
        let parent = Filename.dirname directory in
        if parent = directory || directory = package_root then package_root
        else nearest_existing_directory package_root parent
    in
    let rec visit (config : Config.t) =
      add_path config.root false;
      config.sources
      |> List.filter (fun source -> (not prod) || not source.Config.is_dev)
      |> List.iter (fun source ->
           let directory = Filename.concat config.root source.Config.dir in
           let existing = nearest_existing_directory config.root directory in
           add_path existing (existing = directory && source.Config.recurse));
      let dependencies =
        config.dependencies @ if prod then [] else config.dev_dependencies
      in
      List.iter
        (fun (dependency : Config.dependency) ->
          match Project_context.dependency_path config.root dependency.name with
          | Some directory
            when (not (Hashtbl.mem visited directory))
                 && Project_context.is_local_dependency_canonical ~workspace:root directory
                 && Config.exists_in_root directory ->
            Hashtbl.add visited directory ();
            roots := directory :: !roots;
            visit (Config.load_root directory)
          | _ -> ())
        dependencies
    in
    try
      visit (Config.load_root root);
      List.sort String.compare !roots, !paths
    with Config.Error _ ->
      ([root], [Native_watcher.{directory = root; recursive = false}])
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
                       || name = "rescript.json" || name = "bsconfig.json"
                       || name = "package.json" ->
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
  let warning_state = Warning_state.create () in
  let initial_build = ref true in
  let keep_running () = (not !stop_requested) && lock_is_owned () in
  let poll () = if not (keep_running ()) then raise Stop_watch in
  let run_build () =
    let compilation_kind =
      if !initial_build then Some "initial" else Some "incremental"
    in
    try
      run_with_warning_state ~poll ~warning_state ~compilation_kind
        ~no_timing:false ~seen:[] ~verbosity ~folder ~prod ~features ~warn_error
        ~watch:true ~after_build ~filter;
      initial_build := false
    with
    | Error message | Config.Error message | Source.Error message
    | Process.Error message -> prerr_endline message
    | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
      prerr_endline (Printexc.to_string exn)
  in
  let clear_terminal () =
    if
      Output.should_clear_screen ~clear_screen
        ~interactive:(Unix.isatty Unix.stdout && Unix.isatty Unix.stderr)
    then
      Printf.printf "\027[2J\027[H%!"
  in
  let rec polling_loop roots previous =
    if keep_running () then (
      let current = snapshot roots in
      if current <> previous then (
        clear_terminal ();
        run_build ();
        let roots, _ = watch_context () in
        let after_build = snapshot roots in
        ignore (Unix.select [] [] [] 0.2);
        (* Keep the snapshot from before the rebuild when another edit lands
           during compilation. Otherwise that edit would become the new baseline
           and an atomic configuration rewrite could be missed. *)
        if after_build <> current then polling_loop roots current
        else polling_loop roots after_build)
      else (
        ignore (Unix.select [] [] [] 0.2);
        polling_loop roots current))
  in
  let native_fallback message =
    prerr_endline
      ("Native file watching is unavailable (" ^ message
     ^ "); falling back to polling")
  in
  let rec native_loop watcher roots previous =
    waiting_for_native_event := true;
    let result =
      Fun.protect
        (fun () -> Native_watcher.wait watcher ~keep_running)
        ~finally:(fun () -> waiting_for_native_event := false)
    in
    match result with
    | Native_watcher.Stopped -> None
    | Native_watcher.Failed message -> Some (message, roots, previous)
    | Native_watcher.Changed ->
      ignore (Unix.select [] [] [] 0.05);
      native_reconcile watcher roots previous
  and native_reconcile watcher roots previous =
    let current = snapshot roots in
    if current <> previous then (
      clear_terminal ();
      run_build ();
      let roots, paths = watch_context () in
      match Native_watcher.refresh watcher ~paths with
      | Error message -> Some (message, roots, current)
      | Ok () ->
        let after_build = snapshot roots in
        if after_build <> current then
          native_reconcile watcher roots current
        else native_loop watcher roots after_build)
    else
      let _, paths = watch_context () in
      match Native_watcher.refresh watcher ~paths with
      | Error message -> Some (message, roots, current)
      | Ok () ->
        let after_refresh = snapshot roots in
        if after_refresh <> current then
          native_reconcile watcher roots current
        else native_loop watcher roots after_refresh
  in
  Fun.protect
    (fun () ->
      let roots, _ = watch_context () in
      let before_build = snapshot roots in
      run_build ();
      let roots, paths = watch_context () in
      match Native_watcher.create ~paths with
      | Error message ->
        native_fallback message;
        polling_loop roots before_build
      | Ok watcher ->
        let fallback =
          Fun.protect
            (fun () -> native_reconcile watcher roots before_build)
            ~finally:(fun () -> Native_watcher.close watcher)
        in
        Option.iter
          (fun (message, roots, previous) ->
            native_fallback message;
            polling_loop roots previous)
          fallback)
    ~finally:remove_owned_lock
