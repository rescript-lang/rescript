exception Error of string

let strip_path path message =
  let prefix = path ^ ": " in
  if String.starts_with ~prefix message then
    String.sub message (String.length prefix) (String.length message - String.length prefix)
  else message

let with_file_error ~action path f =
  try f () with
  | Sys_error message ->
    raise
      (Error
         (Printf.sprintf "Could not %s %s: %s" action path
            (strip_path path message)))
  | Unix.Unix_error (error, _, _) ->
    raise
      (Error
         (Printf.sprintf "Could not %s %s: %s" action path
            (Unix.error_message error)))

let read_file path =
  with_file_error ~action:"read file" path (fun () ->
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel)))

let write_file path contents =
  with_file_error ~action:"write formatted file" path (fun () ->
    (* Formatting changes the contents of a user-owned file. Writing through
       its existing inode preserves symlinks, hard links, ownership, ACLs, and
       extended attributes that replacing the directory entry could lose. *)
    File_util.write_file path contents)

let bsc () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let rec nearest_config directory =
  if Config.exists_in_root directory then Some (Config.path_in_root directory)
  else
    let parent = Filename.dirname directory in
    if parent = directory then None else nearest_config parent

type discovered_package = {
  config: Config.t;
  modules: Source.module_ list;
}

let package_sources (package : discovered_package) =
  package.modules
  |> List.concat_map (fun module_ ->
       let config = package.config in
       Filename.concat config.root module_.Source.implementation
       :: (match module_.interface with
          | None -> []
          | Some path -> [Filename.concat config.root path]))

(* Validate the complete package graph before selecting the local files that
   format owns. Scan it with the effective feature selections so dependency
   diagnostics and the eventual local file set cannot drift apart. *)
let discover_package_graph (current : Config.t) =
  let dependency_context = Project_context.dependency_context current in
  let resolved_packages = Hashtbl.create 32 in
  let package_configs = Hashtbl.create 32 in
  let feature_requests = Hashtbl.create 32 in
  Package_diagnostics.validate_metadata current;
  Hashtbl.add package_configs current.name (current, true);
  let add_feature_request name request =
    let requests =
      Option.value (Hashtbl.find_opt feature_requests name) ~default:[]
    in
    Hashtbl.replace feature_requests name (request :: requests)
  in
  let rec visit ~is_local (config : Config.t) =
    let dependencies =
      config.dependencies @ if is_local then config.dev_dependencies else []
    in
    let pending =
      dependencies
      |> List.filter_map (fun (dependency : Config.dependency) ->
           add_feature_request dependency.name dependency.features;
           let directory =
             Project_context.require_dependency_directory
               ~context:dependency_context config.root dependency
           in
           match Hashtbl.find_opt resolved_packages dependency.name with
           | Some chosen ->
             if chosen <> directory then
               Printf.eprintf
                 "Duplicated package: %s ./%s (chosen) vs ./%s in ./%s\n%!"
                 dependency.name (Project_context.relative_to current.root chosen)
                 (Project_context.relative_to current.root directory)
                 (Project_context.relative_to current.root config.root);
             None
           | None ->
             Hashtbl.add resolved_packages dependency.name directory;
             Some (dependency, directory))
    in
    List.iter
      (fun ((dependency : Config.dependency), directory) ->
          let dependency_config =
            try Config.load_root directory
            with Config.Error message ->
              raise
                (Project_context.Package_error
                   (Printf.sprintf
                      "Could not build package tree for '%s' at path '%s'. Error: %s"
                      dependency.name current.root message))
          in
          Package_diagnostics.validate_metadata dependency_config;
          Package_diagnostics.report_missing_sources ~is_root:false dependency_config;
          let dependency_is_local =
            Project_context.dependency_is_local_canonical dependency_context
              directory
          in
          Hashtbl.replace package_configs dependency.name
            (dependency_config, dependency_is_local);
          visit ~is_local:dependency_is_local dependency_config)
      pending
  in
  visit ~is_local:true current;
  Hashtbl.to_seq package_configs
  |> Seq.map (fun (package_name, ((config : Config.t), is_local)) ->
       let features =
         if config.root = current.root then None
         else
           match Hashtbl.find_opt feature_requests package_name with
           | None -> None
           | Some requests when List.exists Option.is_none requests -> None
           | Some requests ->
             let requested =
               requests |> List.filter_map Fun.id |> List.concat
               |> List.sort_uniq String.compare
             in
             (try ignore (Source.resolve_active_features config requested)
              with Source.Error message ->
                raise
                  (Error
                     (Printf.sprintf "Invalid features for package '%s': %s"
                        package_name message)));
             Some requested
       in
       let modules =
         Source.discover config
           ~prod:(Package_graph.source_discovery_prod ~prod:false ~is_local)
           ~features ~filter:None
           ~on_missing:(Package_diagnostics.report_missing_source_folder config)
           ~display_root:current.root
       in
       {config; modules})
  |> List.of_seq

let files_in_scope () =
  let current_directory = Sys.getcwd () in
  let current =
    try Config.load_root current_directory
    with Config.Error message ->
      raise
        (Error
           (Printf.sprintf "Could not read rescript.json at %s: %s"
              current_directory message))
  in
  let listed_by_parent =
    match nearest_config (Filename.dirname current.root) with
    | None -> false
    | Some path ->
      let parent = Config.load path in
      List.exists
        (fun (dependency : Config.dependency) ->
          dependency.name = current.name)
        (parent.dependencies @ parent.dev_dependencies)
  in
  let packages = discover_package_graph current in
  let dependency_context = Project_context.dependency_context current in
  let roots_in_scope =
    if listed_by_parent then [current.root]
    else
      current.root
      :: (current.dependencies @ current.dev_dependencies
         |> List.filter_map (fun (dependency : Config.dependency) ->
              match
                Project_context.dependency_path_in dependency_context
                  current.root dependency.name
              with
              | Some directory
                when Project_context.dependency_is_local_canonical
                       dependency_context directory ->
                Some directory
              | Some _ | None -> None))
  in
  packages
  |> List.filter (fun package ->
       List.exists (( = ) package.config.root) roots_in_scope)
  |> List.concat_map package_sources |> List.sort_uniq String.compare

let formatting_error target stderr =
  Printf.sprintf "Error formatting %s: %s" target stderr

let formatted ~bsc ~target path =
  let result = Process.run ~cwd:(Sys.getcwd ()) bsc ["-format"; path] in
  if not (Process.succeeded result) then
    raise (Error (formatting_error target result.stderr));
  result.stdout

let format_check_summary = function
| 1 -> "The file listed above needs formatting"
| count -> Printf.sprintf "The %d files listed above need formatting" count

let format_files_with_bsc ?max_jobs ~bsc ~check files =
  let cwd = Sys.getcwd () in
  let incorrect = ref 0 in
  let works =
    files
    |> List.mapi (fun index path ->
         Process.
           {
             key = Printf.sprintf "%08d" index;
             dependencies = [];
             value = path;
           })
  in
  let next path = function
  | None -> Some Process.{program = bsc; args = ["-format"; path]; cwd}
  | Some result ->
    if not (Process.succeeded result) then
      raise (Error (formatting_error path result.stderr));
    let original = read_file path in
    if original <> result.stdout then
      if check then (
        incr incorrect;
        prerr_endline ("[format check] " ^ path))
      else write_file path result.stdout;
    None
  in
  (match max_jobs with
  | None -> Process.run_dependency_graph works ~next
  | Some max_jobs -> Process.run_dependency_graph ~max_jobs works ~next);
  if !incorrect > 0 then (
    prerr_endline (format_check_summary !incorrect);
    raise (Error "Formatting check failed")
  )

let format_files ~check files =
  format_files_with_bsc ~bsc:(bsc ()) ~check files

let format_stdin extension =
  if extension <> ".res" && extension <> ".resi" then
    raise (Error "--stdin must be .res or .resi");
  (* The temporary pathname needs a cleanup owner before termination can
     interrupt the command, otherwise an early signal can leave it behind. *)
  let restore_deferred_signals = Platform.defer_termination_signals () in
  let signals_restored = ref false in
  let restore_signals () =
    if not !signals_restored then (
      signals_restored := true;
      restore_deferred_signals ())
  in
  let temporary = ref None in
  let remove_temporary () =
    Option.iter
      (fun path -> try Sys.remove path with Sys_error _ -> ())
      !temporary
  in
  try
    let path = Filename.temp_file "rescript-ocaml-format-" extension in
    temporary := Some path;
    Fun.protect ~finally:remove_temporary (fun () ->
      restore_signals ();
      let output = open_out_bin path in
      (try
         (try
            while true do
              output_char output (input_char stdin)
            done
          with End_of_file -> ());
         close_out output
       with exn ->
         close_out_noerr output;
         raise exn);
      print_string (formatted ~bsc:(bsc ()) ~target:"stdin" path))
  with exn ->
    remove_temporary ();
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let run ~check ~stdin ~files =
  match stdin with
  | Some extension -> format_stdin extension
  | None -> format_files ~check (if files = [] then files_in_scope () else files)
