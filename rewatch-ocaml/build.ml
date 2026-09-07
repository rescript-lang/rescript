exception Error of string
exception Stop_watch

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

let remove_file path = if Sys.file_exists path then (try Sys.remove path with Sys_error _ -> ())

let rec files_under directory =
  if not (Sys.file_exists directory) then []
  else if not (Sys.is_directory directory) then [directory]
  else Sys.readdir directory |> Array.to_list
    |> List.concat_map (fun name -> files_under (Filename.concat directory name))

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

let cleanup_stale ~root ~ocaml_dir (config : Config.t) modules =
  let expected = Hashtbl.create (List.length modules) in
  List.iter (fun module_ -> Hashtbl.replace expected module_.Source.name ()) modules;
  files_under ocaml_dir |> List.iter (fun path ->
    let base = Filename.basename path in
    let name =
      List.fold_left (fun value extension ->
        if Filename.check_suffix value extension then Filename.chop_suffix value extension else value)
        base [".cmi"; ".cmj"; ".cmt"; ".cmti"; ".ast"; ".iast"]
    in
    if not (Hashtbl.mem expected name) then remove_file path);
  let suffixes = [".js"; ".mjs"; ".cjs"; ".bs.js"; ".bs.mjs"; ".bs.cjs"] in
  let expected_outputs = Hashtbl.create (List.length modules * List.length config.package_specs) in
  List.iter (fun module_ -> List.iter (fun spec ->
    Hashtbl.replace expected_outputs (generated_js_path config module_.Source.implementation spec) ()) config.package_specs) modules;
  config.sources |> List.iter (fun source ->
    files_under (Filename.concat root source.Config.dir) |> List.iter (fun path ->
      if List.exists (fun suffix -> Filename.check_suffix path suffix) suffixes then
        if not (Hashtbl.mem expected_outputs path) then remove_file path));
  ["lib/es6"; "lib/js"] |> List.iter (fun directory ->
    files_under (Filename.concat root directory) |> List.iter (fun path ->
      if List.exists (fun suffix -> Filename.check_suffix path suffix) suffixes then
        if not (Hashtbl.mem expected_outputs path) then remove_file path))

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

let report_failure action path result =
  let output = result.Process.stderr ^ result.stdout in
  raise
    (Error
       (Printf.sprintf "%s %s failed (%s):\n%s" action path
          (Process.status_string result.status)
       output))

let compiler_flags ~source_maps ~watch ~gentype (config : Config.t) =
  let ppx_args =
    config.ppx_flags |> List.concat_map (function
      | [] -> []
      | flag :: arguments ->
      let candidates = [Filename.concat config.root flag; Filename.concat (Filename.concat config.root "node_modules") flag] in
      let executable = match List.find_opt Sys.file_exists candidates with
        | Some path -> Unix.realpath path | None -> flag
      in ["-ppx"; String.concat " " (executable :: arguments)])
  in
  let source_map_args =
    if source_maps && (watch || not config.source_map_dev) then config.source_map_args else []
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

let compile_namespace ~bsc ~runtime ~build_dir ~ocaml_dir namespace modules =
  let mlmap = Filename.concat build_dir (namespace ^ ".mlmap") in
  let channel = open_out_bin mlmap in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_string channel "randjbuildsystem\n";
      modules |> List.map (fun module_ -> module_.Source.name) |> List.sort String.compare
      |> List.iter (fun name -> output_string channel name; output_char channel '\n'));
  let result =
    Process.run ~cwd:build_dir bsc
      ["-runtime-path"; runtime; "-w"; "-49"; "-color"; "always";
       "-no-alias-deps"; Filename.basename mlmap]
  in
  if not (Process.succeeded result) then report_failure "Compiling namespace" namespace result;
  copy_file (Filename.concat build_dir (namespace ^ ".cmi"))
    (Filename.concat ocaml_dir (namespace ^ ".cmi"))

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
  | Some namespace, _ -> ["-bs-ns"; namespace]

let compile_file ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~(config : Config.t)
    ~dependency_dirs (module_ : Source.module_) ~is_interface path =
  let ast = Source.ast_path path in
  let namespace_args = namespace_args config module_.name in
  let interface_args =
    if (not is_interface) && Option.is_some module_.interface then
      ["-bs-read-cmi"]
    else []
  in
  let output_args =
    if is_interface then []
    else
      List.concat_map
        (fun spec -> ["-bs-package-output"; package_output config path spec])
        config.package_specs
  in
  let args =
    namespace_args @ interface_args
    @ ["-I"; "../ocaml"]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ ["-runtime-path"; runtime]
    @ compiler_flags ~source_maps:true ~watch ~gentype:true config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  let result = Process.run ~cwd:build_dir bsc args in
  if not (Process.succeeded result) then report_failure "Compiling" path result;
  if result.stderr <> "" then prerr_string result.stderr;
  let basename = Source.compiler_basename config module_.name in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let extensions =
    if is_interface then ["cmi"; "cmti"] else ["cmi"; "cmj"; "cmt"]
  in
  List.iter
    (fun extension ->
      copy_file
        (Filename.concat artifact_dir (basename ^ "." ^ extension))
        (Filename.concat ocaml_dir (basename ^ "." ^ extension)))
    extensions

let compile_job ~bsc ~runtime ~build_dir ~watch ~(config : Config.t) ~dependency_dirs
    (module_ : Source.module_) ~is_interface path =
  let ast = Source.ast_path path in
  let namespace_args = namespace_args config module_.name in
  let interface_args = if not is_interface && Option.is_some module_.interface then ["-bs-read-cmi"] else [] in
  let output_args = if is_interface then [] else List.concat_map (fun spec -> ["-bs-package-output"; package_output config path spec]) config.package_specs in
  let args = namespace_args @ interface_args @ ["-I"; "../ocaml"]
    @ List.concat_map (fun dir -> ["-I"; dir]) dependency_dirs
    @ ["-runtime-path"; runtime] @ compiler_flags ~source_maps:true ~watch ~gentype:true config
    @ ["-bs-package-name"; config.name; "-bs-project-root"; config.root]
    @ output_args @ [ast]
  in
  Process.{program = bsc; args; cwd = build_dir}, (module_, is_interface, path)

let publish_compiled ~build_dir ~ocaml_dir ~(config : Config.t)
    (module_, is_interface, path) result =
  if not (Process.succeeded result) then report_failure "Compiling" path result;
  if result.stderr <> "" then prerr_string result.stderr;
  let basename = Source.compiler_basename config module_.Source.name in
  let artifact_dir = Filename.concat build_dir (Filename.dirname path) in
  let extensions = if is_interface then ["cmi"; "cmti"] else ["cmi"; "cmj"; "cmt"] in
  List.iter (fun extension -> copy_file (Filename.concat artifact_dir (basename ^ "." ^ extension))
    (Filename.concat ocaml_dir (basename ^ "." ^ extension))) extensions;
  if not is_interface then run_post_build config path

let compile_batch ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~(config : Config.t)
    ~dependency_dirs jobs =
  List.iter (fun (_, is_interface, path) ->
    if not is_interface then
      List.iter (fun spec -> ensure_dir (Filename.dirname (generated_js_path config path spec))) config.package_specs) jobs;
  let prepared = List.map (fun (module_, is_interface, path) ->
    compile_job ~bsc ~runtime ~build_dir ~watch ~config ~dependency_dirs module_ ~is_interface path) jobs in
  let results = Process.run_parallel (List.map fst prepared) in
  List.iter2 (fun (_, info) result -> publish_compiled ~build_dir ~ocaml_dir ~config info result) prepared results

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path

let dependency_path root name =
  let rec in_ancestors directory =
    let candidate = Filename.concat (Filename.concat directory "node_modules") name in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname directory in
      if parent = directory then None else in_ancestors parent
  in
  match in_ancestors root with
  | Some path -> Some path
  | None ->
    let package_name =
      match List.rev (String.split_on_char '/' name) with last :: _ -> last | [] -> name
    in
    let sibling = Filename.concat (Filename.dirname root) name in
    let workspace = Filename.concat (Filename.concat root "packages") package_name in
    List.find_opt Sys.file_exists [sibling; workspace]

let rec clean ~seen ~folder ~prod =
  let root = Unix.realpath folder in
  if List.mem root seen then raise (Error ("dependency cycle involving " ^ root));
  let config_path = Filename.concat root "rescript.json" in
  if Sys.file_exists config_path then (
    let config = Config.load config_path in
    let dependencies = config.dependencies @ if prod then [] else config.dev_dependencies in
    List.iter (fun (dependency : Config.dependency) ->
      match dependency_path root dependency.name with
      | Some directory when Sys.file_exists (Filename.concat directory "rescript.json") ->
        clean ~seen:(root :: seen) ~folder:directory ~prod
      | _ -> ()) dependencies;
    let modules = Source.discover config ~prod ~features:None ~filter:None in
    List.iter (fun module_ ->
      List.iter (fun spec ->
        let output = generated_js_path config module_.Source.implementation spec in
        remove_file output;
        remove_file (output ^ ".map")) config.package_specs) modules);
  List.iter (fun dir -> remove_tree (Filename.concat root dir))
    ["lib/bs"; "lib/ocaml"; "lib/es6"; "lib/js"]

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

let compiler_args path =
  let source = Unix.realpath path in
  if not (Filename.check_suffix source ".res" || Filename.check_suffix source ".resi") then
    raise (Error "compiler-args expects a .res or .resi source file");
  let config = Config.load (nearest_config (Filename.dirname source)) in
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

let rec run ~seen ~folder ~prod ~features ~warn_error ~watch ~after_build ~filter =
  let root = Unix.realpath folder in
  let config = Config.load (Filename.concat root "rescript.json") in
  let config = match warn_error with
    | None -> config
    | Some value -> {config with warning_flags = ["-warn-error"; value]}
  in
  let dependency_dirs =
    let dependencies : Config.dependency list =
      config.dependencies @ if prod then [] else config.dev_dependencies
    in
    dependencies |> List.filter_map (fun (dependency : Config.dependency) ->
      let name = dependency.name in
      let candidate = dependency_path root name in
      let () = match candidate with
        | None -> ()
        | Some candidate when List.mem candidate (root :: seen) -> ()
        | Some candidate when Sys.file_exists (Filename.concat candidate "rescript.json") ->
          run ~seen:(root :: seen) ~folder:candidate ~prod ~features:dependency.features ~warn_error:None ~watch ~after_build:None ~filter:None
        | Some _ -> ()
      in
      match candidate with
      | None -> raise (Error ("Could not resolve dependency " ^ name))
      | Some candidate ->
      let ocaml = Filename.concat candidate "lib/ocaml" in
      if Sys.file_exists ocaml then Some ocaml else None)
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
  let modules = Source.discover config ~prod ~features ~filter in
  cleanup_stale ~root ~ocaml_dir config modules;
  Option.iter (fun namespace -> compile_namespace ~bsc ~runtime ~build_dir ~ocaml_dir namespace modules) config.namespace;
  let names = Hashtbl.create (List.length modules) in
  List.iter
    (fun module_ -> Hashtbl.replace names module_.Source.name ())
    modules;
  let parse_paths =
    List.concat_map (fun module_ ->
      module_.Source.implementation :: Option.to_list module_.interface) modules
  in
  let parsed =
    List.map2 (fun path result -> (path, result)) parse_paths
      (Process.run_parallel (List.map (fun path -> fst (parse_job ~bsc ~build_dir ~config path)) parse_paths))
  in
  List.iter (fun (path, result) ->
    if not (Process.succeeded result) then report_failure "Parsing" path result;
    if result.stderr <> "" then prerr_string result.stderr;
    let ast = Source.ast_path path in
    copy_file (Filename.concat build_dir ast)
      (Filename.concat (Filename.concat config.root "lib/ocaml") (Filename.basename ast));
    copy_file (Filename.concat config.root path)
      (Filename.concat (Filename.concat config.root "lib/ocaml") (Filename.basename path))) parsed;
  List.iter
    (fun module_ ->
      let impl_ast = Source.ast_path module_.Source.implementation in
      let impl_deps = ast_dependencies ~build_dir impl_ast in
      let intf_deps =
        match module_.interface with
        | None -> []
        | Some path -> ast_dependencies ~build_dir (Source.ast_path path)
      in
      module_.deps <-
        List.filter
          (fun dep -> dep <> module_.name && Hashtbl.mem names dep)
          (List.sort_uniq String.compare (impl_deps @ intf_deps)))
    modules;
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
  List.iter (fun (_, modules) ->
    let modules = List.rev modules in
    compile_batch ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~config ~dependency_dirs
      (List.filter_map (fun module_ -> Option.map (fun path -> (module_, true, path)) module_.Source.interface) modules);
    compile_batch ~bsc ~runtime ~build_dir ~ocaml_dir ~watch ~config ~dependency_dirs
      (List.map (fun module_ -> (module_, false, module_.Source.implementation)) modules)) levels;
  Printf.printf "Finished compilation\n%!";
  match after_build with
  | None -> ()
  | Some command ->
    let result = Process.run ~cwd:root "/bin/sh" ["-c"; command] in
    if not (Process.succeeded result) then report_failure "after-build" root result;
    if result.stdout <> "" then print_string result.stdout;
    if result.stderr <> "" then prerr_string result.stderr

let watch ~folder ~prod ~features ~warn_error ~after_build ~filter =
  let root = Unix.realpath folder in
  let lock_dir = Filename.concat root "lib" in
  ensure_dir lock_dir;
  let lock_path = Filename.concat lock_dir "watch.lock" in
  let lock_fd =
    try Unix.openfile lock_path [Unix.O_CREAT; Unix.O_EXCL; Unix.O_WRONLY] 0o644
    with Unix.Unix_error (Unix.EEXIST, _, _) ->
      raise (Error ("A watcher is already running for " ^ root))
  in
  Unix.close lock_fd;
  let stop () = raise Stop_watch in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> stop ()));
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> stop ()));
  let rec dependency_roots seen (config : Config.t) =
    let dependencies = config.dependencies @ if prod then [] else config.dev_dependencies in
    dependencies |> List.concat_map (fun (dependency : Config.dependency) ->
      match dependency_path config.root dependency.name with
      | Some directory when not (List.mem directory seen)
        && Sys.file_exists (Filename.concat directory "rescript.json") ->
        let dependency_config = Config.load (Filename.concat directory "rescript.json") in
        directory :: dependency_roots (directory :: seen) dependency_config
      | _ -> [])
  in
  let watch_roots () =
    try root :: dependency_roots [root] (Config.load (Filename.concat root "rescript.json"))
    with Config.Error _ -> [root]
  in
  let snapshot roots =
    let rec walk dir acc =
      let entries = try Sys.readdir dir |> Array.to_list with Sys_error _ -> [] in
      List.fold_left (fun acc name ->
        let path = Filename.concat dir name in
        if Sys.is_directory path then
          if List.mem name ["lib"; "node_modules"; ".git"; "_build"] then acc else walk path acc
        else if Filename.extension path = ".res" || Filename.extension path = ".resi"
          || name = "rescript.json" || name = "package.json" then
          let stat = Unix.stat path in (path, stat.Unix.st_mtime) :: acc
        else acc) acc entries
    in List.sort compare (List.concat_map (fun directory -> walk directory []) roots)
  in
  let rec loop roots previous =
    let current = snapshot roots in
    if current <> previous then (
      (try run ~seen:[] ~folder ~prod ~features ~warn_error ~watch:true ~after_build ~filter with Error message -> prerr_endline message);
      let roots = watch_roots () in
      ignore (Unix.select [] [] [] 0.2);
      loop roots (snapshot roots))
    else (
    ignore (Unix.select [] [] [] 0.2);
    loop roots current)
  in
  Fun.protect
    (fun () -> run ~seen:[] ~folder ~prod ~features ~warn_error ~watch:true ~after_build ~filter;
      let roots = watch_roots () in loop roots (snapshot roots))
    ~finally:(fun () -> remove_file lock_path)
