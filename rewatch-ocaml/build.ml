exception Error of string
exception Stop_watch
exception Build_failure of string
exception Scheduled_failure of string

open Build_artifacts

let compiler_log_path root directory =
  Filename.concat (lib_path root directory) ".compiler.log"

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
  let path = compiler_log_path root "bs" in
  ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    Printf.fprintf channel "#Start(%.6f)\n" (Unix.gettimeofday ()))

let append_compiler_log root content =
  let channel =
    open_out_gen [Open_wronly; Open_append; Open_binary] 0o644
      (compiler_log_path root "bs")
  in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel (strip_ansi content))

let finalize_compiler_log root =
  append_compiler_log root
    (Printf.sprintf "#Done(%.6f)\n" (Unix.gettimeofday ()));
  copy_file (compiler_log_path root "bs") (compiler_log_path root "ocaml")

let read_lock_owner path =
  try
    let channel = open_in path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      Some (input_line channel))
  with Sys_error _ | End_of_file -> None

let process_is_active value =
  Platform.process_is_active value ~run:(fun program args ->
    try
      let result =
        Process.run ~cwd:(Filename.get_temp_dir_name ()) program args
      in
      Some (result.Process.status, result.stdout)
    with
    | Process.Error _ | Unix.Unix_error _ | Sys_error _ -> None)

let workspace_lock_root folder =
  let current = Config.load_root folder in
  let rec nearest_parent directory =
    if Config.exists_in_root directory then Some (Config.load_root directory)
    else
      let parent = Filename.dirname directory in
      if parent = directory then None else nearest_parent parent
  in
  match nearest_parent (Filename.dirname folder) with
  | Some parent
    when List.exists
           (fun (dependency : Config.dependency) ->
             dependency.name = current.name)
           (parent.dependencies @ parent.dev_dependencies) ->
    parent.root
  | Some _ | None -> folder

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

let bsc_path () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let runtime_path root =
  try Toolchain.runtime ~find_package:(dependency_path root)
  with Toolchain.Error message -> raise (Error message)

let report_failure action path result =
  let output = result.Process.stderr ^ result.stdout in
  ignore action;
  ignore path;
  raise (Build_failure output)

let ppx_is_enabled ~bisect_enabled flag contents =
  if contains_text flag "bisect" then bisect_enabled
  else
    not
      ((contains_text flag "graphql-ppx" || contains_text flag "graphql_ppx")
      && not (contains_text contents "%graphql")
      || (contains_text flag "spice" && not (contains_text contents "@spice"))
      || (contains_text flag "rescript-relay"
         && not (contains_text contents "%relay"))
      || (contains_text flag "re-formality"
         && not (contains_text contents "%form")))

let filter_ppx_flags ?bisect_enabled flags contents =
  let bisect_enabled =
    Option.value bisect_enabled
      ~default:(Option.is_some (Sys.getenv_opt "BISECT_ENABLE"))
  in
  List.filter
    (function
      | [] -> false
      | flag :: _ -> ppx_is_enabled ~bisect_enabled flag contents)
    flags

let compiler_flags ?(ppx_flags = []) ~source_maps ~watch ~gentype
    (config : Config.t) =
  let ppx_args =
    ppx_flags |> List.concat_map (function
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
  if source_maps then
    ppx_args @ config.jsx_args @ source_map_args @ config.compiler_flags
    @ config.warning_flags
    @ (if gentype then config.gentype_args else [])
    @ config.experimental_args
  else
    ppx_args @ config.jsx_args @ config.experimental_args @ config.warning_flags
    @ config.compiler_flags

let with_local_warning_policy ~is_local (config : Config.t) =
  if is_local then config else {config with warning_flags = []}

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

let parse_file ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents = read_file (Filename.concat config.root path) in
  let args =
    compiler_flags
      ~ppx_flags:(filter_ppx_flags config.ppx_flags contents)
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
  let result = Process.run ~cwd:build_dir bsc args in
  if not (Process.succeeded result) then report_failure "Parsing" path result;
  if result.stderr <> "" then prerr_string result.stderr;
  copy_file
    (Filename.concat build_dir ast)
    (Filename.concat
       (lib_path config.root "ocaml")
       (Filename.basename ast));
  copy_file
    (Filename.concat config.root path)
    (Filename.concat
       (lib_path config.root "ocaml")
       (Filename.basename path));
  ast

let parse_job ~bsc ~build_dir ~(config : Config.t) path =
  let ast = Source.ast_path path in
  ensure_dir (Filename.concat build_dir (Filename.dirname ast));
  let contents = read_file (Filename.concat config.root path) in
  let args =
    compiler_flags
      ~ppx_flags:(filter_ppx_flags config.ppx_flags contents)
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

let package_output (config : Config.t) path (spec : Config.package_spec) =
  let directory = Filename.dirname path in
  let output_dir =
    if spec.in_source then directory
    else
      Filename.concat
        (match spec.module_format with
        | Config.Esmodule -> lib_path "" "es6"
        | Config.Commonjs -> lib_path "" "js")
        directory
  in
  Printf.sprintf "%s:%s:%s"
    (Config.module_format_name spec.module_format)
    output_dir
    (Config.package_spec_suffix config spec)

let namespace_job ~bsc ~runtime ~build_dir ~ocaml_dir ~entry ~package_dirty
    namespace modules =
  let mlmap = Filename.concat build_dir (namespace ^ ".mlmap") in
  let contents =
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer "randjbuildsystem\n";
    modules
    |> List.filter (fun module_ -> Some module_.Source.name <> entry)
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
      copy_file_if_changed (Filename.concat build_dir (namespace ^ ".cmi"))
        (Filename.concat ocaml_dir (namespace ^ ".cmi"));
      copy_file (Filename.concat build_dir (namespace ^ ".cmj"))
        (Filename.concat ocaml_dir (namespace ^ ".cmj"));
      copy_file (Filename.concat build_dir (namespace ^ ".cmt"))
        (Filename.concat ocaml_dir (namespace ^ ".cmt"));
      copy_file mlmap (Filename.concat ocaml_dir (namespace ^ ".mlmap")) )

let path_is_within ~root path =
  let root = Unix.realpath root in
  let path = Unix.realpath path in
  let normalize = Platform.normalize_path_for_comparison in
  let root = normalize root in
  let path = normalize path in
  path = root || String.starts_with ~prefix:(Filename.concat root "") path

let is_local_dependency ~workspace path =
  let equal_component left right =
    Platform.normalize_path_for_comparison left
    = Platform.normalize_path_for_comparison right
  in
  let rec contains_component path component =
    if equal_component (Filename.basename path) component then true
    else
      let parent = Filename.dirname path in
      parent <> path && contains_component parent component
  in
  path_is_within ~root:workspace path
  && not (contains_component (Unix.realpath path) "node_modules")

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
      let env, program, args =
        Platform.post_build_command ~command ~output
      in
      let result =
        match env with
        | None -> Process.run ~cwd:config.root program args
        | Some env -> Process.run ~env ~cwd:config.root program args
      in
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
  let args =
    namespace_args @ interface_args
    @ ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
    @ ["-runtime-path"; runtime]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ compiler_flags ~source_maps:true ~watch ~gentype:true config
    @ gentype_dependency_args config
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

let rec clean_internal ~(root_config : Config.t) ~seen ~folder ~prod ~is_local =
  let root = Unix.realpath folder in
  if not (Hashtbl.mem seen root) then (
    Hashtbl.add seen root ();
    let config_path = Config.path_in_root root in
    if Config.exists_in_root root then (
      let config = Config.load config_path in
      let dependencies =
        config.dependencies
        @ if prod || not is_local then [] else config.dev_dependencies
      in
      List.iter (fun (dependency : Config.dependency) ->
        match dependency_path root dependency.name with
        | Some directory when Config.exists_in_root directory ->
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
      ([lib_path "" "bs"; lib_path "" "ocaml"]
      @ if is_local then [lib_path "" "es6"; lib_path "" "js"] else []))

let project_root folder =
  if not (Sys.file_exists folder) then
    raise
      (Error
         ("Could not start Rescript build: Could not write lockfile because the specified project folder does not exist: "
         ^ folder));
  Unix.realpath folder

let clean ~seen ~folder ~prod =
  let root = project_root folder in
  let release_build_lock = acquire_build_lock (workspace_lock_root root) in
  Fun.protect ~finally:release_build_lock (fun () ->
    let root_config = Config.load_root root in
    let visited = Hashtbl.create 32 in
    List.iter (fun path -> Hashtbl.replace visited (Unix.realpath path) ()) seen;
    clean_internal ~root_config ~seen:visited ~folder:root ~prod ~is_local:true)

let rec nearest_config directory =
  if Config.exists_in_root directory then Config.path_in_root directory
  else
    let parent = Filename.dirname directory in
    if parent = directory then raise (Error "could not find a rescript.json parent")
    else nearest_config parent

let relative_to root path =
  let prefix = Filename.concat root "" in
  let comparable = Platform.normalize_path_for_comparison in
  if String.starts_with ~prefix:(comparable prefix) (comparable path) then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else raise (Error (path ^ " is not inside " ^ root))

let rec remove_flag_with_value flag = function
  | current :: _ :: rest when current = flag ->
    remove_flag_with_value flag rest
  | value :: rest -> value :: remove_flag_with_value flag rest
  | [] -> []

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
  let root = workspace_lock_root package_config.root in
  let root_config_path = Config.path_in_root root in
  let root_config =
    if root <> package_config.root && Config.exists_in_root root then
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
         match dependency_path config.root dependency.name with
         | Some directory -> Some (lib_path directory "ocaml")
         | None when not required -> None
         | None ->
           raise
             (Error
                (Printf.sprintf "Expected to find dependent package %s of %s"
                   dependency.name config.name)))
  in
  let parser_args =
    compiler_flags
      ~ppx_flags:(filter_ppx_flags config.ppx_flags (read_file source))
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
    let namespace_args = namespace_args config (Source.module_name source) in
    let interface_args = if not is_interface && has_interface then ["-bs-read-cmi"] else [] in
    let output_args = if is_interface then [] else List.concat_map (fun spec -> ["-bs-package-output"; package_output config relative spec]) config.package_specs in
    namespace_args @ interface_args
    @ ["-I"; Filename.concat Filename.parent_dir_name "ocaml"]
    @ ["-runtime-path"; runtime]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ compiler_flags ~source_maps:true ~watch:false ~gentype:true config
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
  is_dirty: unit -> bool;
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
  graph_config: Config.t;
  graph_compile_config: Config.t;
  graph_build_dir: string;
  graph_ocaml_dir: string;
  graph_dependencies: Config.dependency list;
  graph_modules: Source.module_ list;
}

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
  preparse_results: (string, Process.result) Hashtbl.t;
  blocked_modules: (string, unit) Hashtbl.t;
  active_features: (string, string list option) Hashtbl.t;
  initialized_logs: (string, unit) Hashtbl.t;
  watch_outputs: (string * string * string) list ref;
  watch_output_paths: (string, unit) Hashtbl.t;
  global_dependencies: (string, string list) Hashtbl.t;
  global_raw_dependencies: (string, string list) Hashtbl.t;
  graph_packages: (string, graph_package) Hashtbl.t;
  cleanup_results: (string, string list * int) Hashtbl.t;
  namespace_jobs: (Process.job * (Process.result -> unit)) list ref;
  scheduled_modules: scheduled_module list ref;
  compile_cleanup: (unit -> unit) list ref;
  mutable compiler_context: Compiler_info.context option;
  mutable compiler_cleaned: bool;
  warning_state: Warning_state.t;
  mutable had_warnings: bool;
}

let source_is_newer ~source ~artifact =
  match modification_time source, modification_time artifact with
  | Some source_time, Some artifact_time -> source_time > artifact_time
  | Some _, None -> true
  | None, _ -> false

let published_ast_path ~ocaml_dir source_path =
  (* bsc gives its intermediate AST an epoch mtime. The copy published after a
     successful parse is the stable freshness marker across build cycles. *)
  Filename.concat ocaml_dir (Filename.basename (Source.ast_path source_path))

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
    ~filter ~watch ~stats =
  let bsc = bsc_path () in
  let requested_features = Hashtbl.create 32 in
  let unallowed_dependencies = ref [] in
  let loaded_configs = Hashtbl.create 32 in
  let load_config root =
    match Hashtbl.find_opt loaded_configs root with
    | Some config -> config
    | None ->
      let config = Config.load_root root in
      Hashtbl.add loaded_configs root config;
      config
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
  let rec collect ~folder ~features ~is_local =
    let root = Unix.realpath folder in
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
      List.iter
        (fun (kind, (dependency : Config.dependency)) ->
          match dependency_path root dependency.name with
          | Some directory when Config.exists_in_root directory ->
            let dependency_config = load_config (Unix.realpath directory) in
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
  let graph_packages = ref [] in
  let rec visit ~folder ~features ~warn_error ~filter ~is_local =
    let root = Unix.realpath folder in
    if not (Hashtbl.mem visited root) then (
      Hashtbl.add visited root ();
      let features =
        match Hashtbl.find_opt stats.active_features root with
        | Some features -> features
        | None -> features
      in
      let config = load_config root in
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
          | Some directory when Config.exists_in_root directory ->
            visit ~folder:directory ~features:dependency.features
              ~warn_error:None ~filter:None
              ~is_local:
                (is_local_dependency ~workspace:root_config.root directory)
          | _ -> ())
        dependencies;
      let modules =
        Source.discover config ~prod ~features ~filter
          ~on_missing:(fun path ->
            if is_local then Printf.eprintf "Could not read folder %s\n%!" path)
          ~on_orphan:(fun path ->
            Printf.eprintf
              "\027[2K\r No implementation file found for interface file (skipping): %s\n%!"
              path)
          ~display_root:root_config.root
      in
      let compile_config =
        with_root_options config root_config
        |> with_local_warning_policy ~is_local
      in
      let build_dir = lib_path root "bs" in
      let ocaml_dir = lib_path root "ocaml" in
      ensure_dir build_dir;
      let package =
        {
          graph_root = root;
          graph_config = config;
          graph_compile_config = compile_config;
          graph_build_dir = build_dir;
          graph_ocaml_dir = ocaml_dir;
          graph_dependencies = dependencies;
          graph_modules = modules;
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
    Compiler_info.make_context ~bsc_path:bsc ~runtime_path:runtime
      ~source_map_args
  in
  stats.compiler_context <- Some compiler_context;
  List.iter
    (fun package ->
      if Compiler_info.needs_clean compiler_context package.graph_config then (
        ignore
          (Build_artifacts.cleanup_stale ~root:package.graph_root
             ~ocaml_dir:package.graph_ocaml_dir
             ~is_local:
               (is_local_dependency ~workspace:root_config.root
                  package.graph_root)
             package.graph_compile_config package.graph_modules);
        Compiler_info.clean_package package.graph_config;
        stats.compiler_cleaned <- true);
      ensure_dir package.graph_build_dir;
      ensure_dir package.graph_ocaml_dir)
    !graph_packages;
  List.iter
    (fun package ->
      let removed_modules, previous_ast_count =
        Build_artifacts.cleanup_stale ~root:package.graph_root
          ~ocaml_dir:package.graph_ocaml_dir
          ~is_local:
            (is_local_dependency ~workspace:root_config.root package.graph_root)
          package.graph_compile_config package.graph_modules
      in
      Hashtbl.replace stats.cleanup_results package.graph_root
        (removed_modules, previous_ast_count);
      List.iter
        (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
        removed_modules)
    !graph_packages;
  let parse_entries =
    !graph_packages
    |> List.concat_map (fun package ->
         package.graph_modules
         |> List.concat_map (fun module_ ->
              module_.Source.implementation
              :: Option.to_list module_.Source.interface)
         |> List.filter_map (fun path ->
              let artifact =
                published_ast_path ~ocaml_dir:package.graph_ocaml_dir path
              in
              if
                source_is_newer
                  ~source:(Filename.concat package.graph_root path)
                  ~artifact
              then Some (package, path)
              else None))
  in
  let parse_results =
    parse_entries
    |> List.map (fun (package, path) ->
         fst
           (parse_job ~bsc ~build_dir:package.graph_build_dir
              ~config:package.graph_compile_config path))
    |> Process.run_parallel
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
          let artifact_base =
            Source.compiler_asset_basename package.graph_compile_config
              module_.Source.implementation
          in
          let cmt =
            Filename.concat package.graph_ocaml_dir (artifact_base ^ ".cmt")
          in
          if not (Sys.file_exists cmt) then
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
  List.iter
    (fun (node, dependencies) ->
      Hashtbl.replace stats.global_dependencies node.key dependencies)
    graph_nodes;
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
        | Some candidate when Config.exists_in_root candidate ->
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
  initialize_compiler_log root;
  Hashtbl.replace stats.initialized_logs root ();
  let modules =
    match prepared with
    | Some package -> package.graph_modules
    | None ->
      Source.discover config ~prod ~features ~filter
        ~display_root:root_config.root
        ~on_missing:(fun path ->
          if is_local then Printf.eprintf "Could not read folder %s\n%!" path)
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
      |> with_local_warning_policy ~is_local
  in
  let removed_modules, previous_ast_count =
    match Hashtbl.find_opt stats.cleanup_results root with
    | Some result -> result
    | None ->
      Build_artifacts.cleanup_stale ~root ~ocaml_dir ~is_local config modules
  in
  stats.cleaned <- stats.cleaned + List.length removed_modules;
  List.iter
    (fun module_name -> Hashtbl.replace stats.removed_modules module_name ())
    removed_modules;
  stats.previous_asts <- stats.previous_asts + previous_ast_count;
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
         || source_is_newer ~source:(Filename.concat root path)
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
      (Process.run_parallel
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
          report_failure "Parsing" path result)
      result;
    let stderr =
      if is_local then stderr else retain_critical_external_warnings stderr
    in
    if stderr <> "" then stats.had_warnings <- true;
    if stderr <> "" then append_compiler_log root stderr;
    if stderr <> "" then prerr_string stderr;
    let ast = Source.ast_path path in
    if is_local && stderr <> "" then warning_asts := ast :: !warning_asts;
    copy_file (Filename.concat build_dir ast)
      (Filename.concat (lib_path config.root "ocaml") (Filename.basename ast));
    copy_file (Filename.concat config.root path)
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
  let module_is_dirty module_ =
    let global_key = global_module_key config module_.Source.name in
    let compiler_base =
      Source.compiler_asset_basename config module_.Source.implementation
    in
    let cmt = Filename.concat ocaml_dir (compiler_base ^ ".cmt") in
    let module_name = Source.module_name module_.Source.implementation in
    let ast =
      Filename.concat build_dir
        (Source.ast_path module_.Source.implementation)
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
        | Some dependency_module ->
          Some
            (Filename.concat ocaml_dir
               (Source.compiler_asset_basename config
                  dependency_module.Source.implementation
               ^ ".cmi"))
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
    || List.mem module_name removed_modules
    || (match modification_time ast, modification_time cmt with
       | Some ast_time, Some cmt_time -> ast_time >= cmt_time
       | Some _, None -> true
       | None, _ -> false)
    || not (Sys.file_exists cmt && outputs_exist)
    || List.exists (fun dependency -> List.mem dependency removed_modules)
         dependencies
    || List.exists
         (fun dependency -> Hashtbl.mem stats.removed_modules dependency)
         dependencies
    || List.exists dependency_is_newer dependencies)
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
        let dependencies =
          if Hashtbl.mem stats.blocked_modules key then []
          else
            Hashtbl.find_opt stats.global_dependencies key
            |> Option.value ~default:[]
        in
        {
          key;
          dependencies;
          source = module_;
          is_dirty = (fun () -> module_is_dirty module_);
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
          (fun (scheduled : scheduled_module) -> scheduled.is_dirty ())
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
                  remove_file (Filename.concat build_dir ast);
                  remove_file
                    (Filename.concat ocaml_dir (Filename.basename ast)))
                paths)
          compile_warning_modules;
      List.iter
        (fun ast ->
          remove_file (Filename.concat build_dir ast);
          remove_file (Filename.concat ocaml_dir (Filename.basename ast)))
        !warning_asts)
    :: !(stats.compile_cleanup);
  ()

let run_scheduled_modules stats =
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
          Process.run_dependency_graph works
            ~is_fatal:(function Scheduled_failure _ -> false | _ -> true)
            ~next:(fun scheduled result ->
              match result, !(scheduled.phase) with
              | None, `Start ->
                if scheduled.is_dirty () then (
                  stats.compiled <- stats.compiled + 1;
                  scheduled.prepare ();
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
                else None
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
           append_compiler_log entry.Warning_state.package_root entry.output);
      let failures = List.rev !failures in
      List.iter
        (fun ((scheduled : scheduled_module), output) ->
          append_compiler_log scheduled.package_root output)
        failures;
      match failures, scheduler_failed with
      | [], false -> ()
      | [], true -> raise (Error "compiler scheduler stopped without a diagnostic")
      | failures, _ ->
        failures |> List.map snd |> String.concat "" |> fun output ->
        raise (Build_failure output))

let run_namespace_jobs stats =
  let jobs = List.rev !(stats.namespace_jobs) in
  let results = Process.run_parallel (List.map fst jobs) in
  List.iter2 (fun (_, finish) result -> finish result) jobs results

let run_with_warning_state ~warning_state ~compilation_kind ~no_timing ~seen
    ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter =
  let started_at = Unix.gettimeofday () in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let root = project_root folder in
  let root_config = Config.load_root root in
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
      preparse_results = Hashtbl.create 16;
      blocked_modules = Hashtbl.create 16;
      active_features = Hashtbl.create 16;
      initialized_logs = Hashtbl.create 16;
      watch_outputs = ref [];
      watch_output_paths = Hashtbl.create 16;
      global_dependencies = Hashtbl.create 64;
      global_raw_dependencies = Hashtbl.create 64;
      graph_packages = Hashtbl.create 32;
      cleanup_results = Hashtbl.create 32;
      namespace_jobs = ref [];
      scheduled_modules = ref [];
      compile_cleanup = ref [];
      compiler_context = None;
      compiler_cleaned = false;
      warning_state;
      had_warnings = false;
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
    if not interactive then
      if watch then (
        if success then Printf.printf "Finished compilation\n%!")
      else
        Printf.printf
          "Cleaned %d/%d\nParsed %d source files\nCompiled %d modules\n%!"
          stats.cleaned stats.previous_asts stats.parsed stats.compiled;
    Warning_state.entries stats.warning_state
    |> List.iter (fun entry -> prerr_string entry.Warning_state.output);
    flush stderr;
    let diagnostics =
      stats.diagnostics |> List.rev |> List.sort_uniq String.compare
    in
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
        ~watch ~stats
    in
    if stats.compiler_cleaned then
      print_endline "Cleaned previous build due to compiler update";
    Option.iter
      (fun (_, blocked, _) ->
        List.iter
          (fun name -> Hashtbl.replace stats.blocked_modules name ())
          blocked)
      cycle;
    run_internal ~root_config ~seen:visited ~folder:root ~prod ~features
      ~warn_error ~watch ~filter ~is_local:true ~stats;
    (try
       run_namespace_jobs stats;
       run_scheduled_modules stats
     with Build_failure output ->
       if Option.is_none stats.failure then stats.failure <- Some output);
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
        (fun context ->
          Hashtbl.iter
            (fun _ package ->
              Compiler_info.write_package context package.graph_config)
            stats.graph_packages)
        stats.compiler_context;
      Option.iter
        (fun command ->
          expose_watch_outputs ();
          finish_watch_outputs ~success:true;
          finalize_logs ();
          release_build_lock ();
          let result =
            match Str.split (Str.regexp "[ \t\r\n]+") command with
            | program :: args -> Process.run ~cwd:root program args
            | [] -> raise (Error "--after-build command cannot be empty")
          in
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

let run ~seen ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter
    ~no_timing =
  run_with_warning_state ~warning_state:(Warning_state.create ())
    ~compilation_kind:None ~no_timing ~seen ~folder ~prod ~features ~warn_error
    ~watch ~after_build ~filter

let watch ~folder ~prod ~features ~warn_error ~after_build ~filter ~clear_screen =
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
                 && Config.exists_in_root directory ->
            Hashtbl.add visited directory ();
            roots := directory :: !roots;
            visit (Config.load_root directory)
          | _ -> ())
        dependencies
    in
    try
      visit (Config.load_root root);
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
  let run_build () =
    let compilation_kind =
      if !initial_build then None else Some "incremental"
    in
    try
      run_with_warning_state ~warning_state ~compilation_kind ~no_timing:false
        ~seen:[] ~folder ~prod ~features ~warn_error ~watch:true ~after_build
        ~filter;
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
