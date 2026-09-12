exception Error of string

let strip_path path message =
  String_util.strip_prefix ~prefix:(path ^ ": ") message

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
  with_file_error ~action:"read file" path (fun () -> File_util.read_file path)

let write_file path contents =
  with_file_error ~action:"write formatted file" path (fun () ->
      (* Formatting changes the contents of a user-owned file. Writing through
       its existing inode preserves symlinks, hard links, ownership, ACLs, and
       extended attributes that replacing the directory entry could lose. *)
      File_util.write_file path contents)

let bsc () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

type discovered_package = {config: Config.t; files: string list}

let package_sources (package : discovered_package) =
  package.files |> List.map (Filename.concat package.config.root)

(* Validate the complete package graph before selecting the local files that
   format owns. Scan it with the effective feature selections so dependency
   diagnostics and the eventual local file set cannot drift apart. *)
let discover_package_graph (current : Config.t) =
  let resolution = Package_resolution.create current in
  Package_diagnostics.validate_metadata current;
  let graph =
    Package_traversal.discover ~root_config:current ~prod:false ~features:None
      ~resolution
  in
  graph.packages
  |> List.map (fun (package : Package_traversal.package) ->
      let config = package.config in
      let is_local = package.is_local in
      Package_diagnostics.report_missing_sources
        ~is_root:(config.root = current.root)
        config;
      let features =
        if config.root = current.root then None
        else
          match Package_traversal.find_feature_selection graph config.root with
          | None | Some Package_traversal.All_features -> None
          | Some (Package_traversal.Selected_features requested) ->
            (try ignore (Source.resolve_active_features config requested)
             with Source.Error message ->
               raise
                 (Error
                    (Printf.sprintf "Invalid features for package '%s': %s"
                       config.name message)));
            Some requested
      in
      let files =
        Source.discover_files config
          ~prod:(Package_graph.source_discovery_prod ~prod:false ~is_local)
          ~features ~filter:None
          ~on_missing:(Package_diagnostics.report_missing_source_folder config)
      in
      {config; files})

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
    match
      Project_context.nearest_config_path (Filename.dirname current.root)
    with
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

let formatted ?poll ~bsc ~target path =
  let result = Process.run ?poll ~cwd:(Sys.getcwd ()) bsc ["-format"; path] in
  if not (Process.succeeded result) then
    raise (Error (formatting_error target result.stderr));
  result.stdout

let format_check_summary = function
  | 1 -> "The file listed above needs formatting"
  | count -> Printf.sprintf "The %d files listed above need formatting" count

let format_files_with_bsc ?max_jobs ?poll ~bsc ~check files =
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
  Process.run_dependency_graph ?max_jobs ?poll works ~next;
  if !incorrect > 0 then (
    prerr_endline (format_check_summary !incorrect);
    raise (Error "Formatting check failed"))

type stdin_read_result = Stdin_contents of string | Stdin_error of exn

let read_stdin_interruptibly ?poll () =
  let result = Atomic.make None in
  let reader =
    Thread.create
      (fun () ->
        let value =
          try
            let buffer = Buffer.create 4096 in
            let bytes = Bytes.create 65536 in
            let rec read () =
              match input stdin bytes 0 (Bytes.length bytes) with
              | 0 -> Stdin_contents (Buffer.contents buffer)
              | count ->
                Buffer.add_subbytes buffer bytes 0 count;
                read ()
            in
            read ()
          with exn -> Stdin_error exn
        in
        Atomic.set result (Some value))
      ()
  in
  let rec await () =
    Option.iter (fun poll -> poll ()) poll;
    match Atomic.get result with
    | Some value ->
      Thread.join reader;
      value
    | None ->
      Unix.sleepf 0.02;
      await ()
  in
  match await () with
  | Stdin_contents contents -> contents
  | Stdin_error exn -> raise exn

let format_stdin ?poll extension =
  if extension <> ".res" && extension <> ".resi" then
    raise (Error "--stdin must be .res or .resi");
  let bsc = bsc () in
  (* The temporary pathname needs a cleanup owner before termination can
     interrupt the command, otherwise an early signal can leave it behind. *)
  let deferred_signals = Signal_restore.create ~defer:true in
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
        Signal_restore.restore deferred_signals;
        let contents = read_stdin_interruptibly ?poll () in
        File_util.write_file path contents;
        print_string (formatted ?poll ~bsc ~target:"stdin" path))
  with exn ->
    remove_temporary ();
    raise (Signal_restore.exception_after_restore deferred_signals exn)

let run_files ?poll ~check paths =
  let bsc = bsc () in
  let files = if paths = [] then files_in_scope () else paths in
  format_files_with_bsc ?poll ~bsc ~check files
