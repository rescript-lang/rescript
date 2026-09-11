exception Error of string

let strip_path path message =
  let prefix = path ^ ": " in
  if String.starts_with ~prefix message then
    String.sub message (String.length prefix)
      (String.length message - String.length prefix)
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
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
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

type discovered_package = {config: Config.t; modules: Source.module_ list}

let package_sources (package : discovered_package) =
  package.modules
  |> List.concat_map (fun module_ ->
      let config = package.config in
      Filename.concat config.root module_.Source.implementation
      ::
      (match module_.interface with
      | None -> []
      | Some path -> [Filename.concat config.root path]))

(* Validate the complete package graph before selecting the local files that
   format owns. Scan it with the effective feature selections so dependency
   diagnostics and the eventual local file set cannot drift apart. *)
let discover_package_graph (current : Config.t) =
  let resolution = Package_resolution.create current in
  let package_configs = Hashtbl.create 32 in
  let feature_requests = Feature_requests.create () in
  Package_diagnostics.validate_metadata current;
  Hashtbl.add package_configs current.root (current, true);
  let rec visit ~is_local (config : Config.t) =
    let dependencies =
      Package_traversal.requests ~prod:false ~is_local config
    in
    let pending =
      dependencies
      |> List.filter_map (fun request ->
          let resolved =
            Package_traversal.resolve resolution ~package_root:config.root
              request
          in
          Package_traversal.add_feature_request feature_requests resolved;
          if Hashtbl.mem package_configs resolved.dependency.directory then None
          else Some resolved.dependency)
    in
    List.iter
      (fun (dependency : Package_resolution.dependency) ->
        Package_diagnostics.report_missing_sources ~is_root:false
          dependency.config;
        Hashtbl.replace package_configs dependency.directory
          (dependency.config, dependency.is_local);
        visit ~is_local:dependency.is_local dependency.config)
      pending
  in
  visit ~is_local:true current;
  Hashtbl.to_seq package_configs
  |> Seq.map (fun (package_root, ((config : Config.t), is_local)) ->
      let features =
        if config.root = current.root then None
        else
          match Feature_requests.find feature_requests package_root with
          | None | Some Feature_requests.All -> None
          | Some (Feature_requests.Selected requested) ->
            (try ignore (Source.resolve_active_features config requested)
             with Source.Error message ->
               raise
                 (Error
                    (Printf.sprintf "Invalid features for package '%s': %s"
                       config.name message)));
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
        (fun (dependency : Config.dependency) -> dependency.name = current.name)
        (Package_traversal.requests ~prod:false ~is_local:true parent
        |> List.map (fun request -> request.Package_traversal.declaration))
  in
  let packages = discover_package_graph current in
  let resolution = Package_resolution.create current in
  let roots_in_scope =
    if listed_by_parent then [current.root]
    else
      current.root
      :: (Package_traversal.requests ~prod:false ~is_local:true current
         |> List.map (fun request -> request.Package_traversal.declaration)
         |> List.filter_map (fun (dependency : Config.dependency) ->
             match
               Package_resolution.dependency_path resolution
                 ~package_root:current.root dependency.name
             with
             | Some directory
               when Package_resolution.is_local resolution directory ->
               Some directory
             | Some _ | None -> None))
  in
  packages
  |> List.filter (fun package ->
      List.exists (( = ) package.config.root) roots_in_scope)
  |> List.concat_map package_sources
  |> List.sort_uniq String.compare

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
          {key = Printf.sprintf "%08d" index; dependencies = []; value = path})
  in
  let next path = function
    | None ->
      Some (Process.task Process.{program = bsc; args = ["-format"; path]; cwd})
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
    raise (Error "Formatting check failed"))

let format_stdin extension =
  if extension <> ".res" && extension <> ".resi" then
    raise (Error "--stdin must be .res or .resi");
  let bsc = bsc () in
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
        print_string (formatted ~bsc ~target:"stdin" path))
  with exn ->
    remove_temporary ();
    let exn =
      try
        restore_signals ();
        exn
      with signal_exn -> signal_exn
    in
    raise exn

let run_files ~check paths =
  let bsc = bsc () in
  let files = if paths = [] then files_in_scope () else paths in
  format_files_with_bsc ~bsc ~check files
