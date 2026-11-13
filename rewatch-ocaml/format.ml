exception Error of string

let with_file_error ~action path f =
  try f () with
  | Sys_error message ->
    raise
      (Error
         (Printf.sprintf "Could not %s %s: %s" action path
            (String_util.strip_path path message)))
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

type discovered_package = {config: Config.t; files: string list}

let package_sources (package : discovered_package) =
  package.files |> List.map (Filename.concat package.config.root)

(* Validate the complete package graph before selecting the local files that
   format owns. Scan it with the effective feature selections so dependency
   diagnostics and the eventual local file set cannot drift apart. The
   resolution is returned so its package metadata diagnostics are reported
   once. *)
let discover_package_graph (current : Config.t) =
  let resolution = Package_resolution.create current in
  let graph =
    Package_traversal.discover ~root_config:current ~prod:false ~features:None
      ~resolution
  in
  let packages =
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
            match
              Package_traversal.find_feature_selection graph config.root
            with
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
            ~prod:
              (Package_traversal.source_discovery_prod ~prod:false ~is_local)
            ~features
            ~on_missing:
              (Package_diagnostics.report_missing_source_folder config)
        in
        {config; files})
  in
  (resolution, packages)

let files_in_scope ~verbosity =
  let current_directory = Sys.getcwd () in
  let current =
    try Config.load_root current_directory
    with Config.Error message ->
      raise
        (Error
           (Printf.sprintf "Could not read rescript.json at %s: %s"
              current_directory message))
  in
  let workspace = Project_context.workspace_config_for current in
  let listed_by_parent = workspace.root <> current.root in
  Output.debug ~channel:stderr ~verbosity
    (Project_context.describe current ~workspace);
  let resolution, packages = discover_package_graph current in
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

(* The compiler's diagnostics start and end with blank lines. *)
let formatting_error target stderr =
  Printf.sprintf "Error formatting %s:\n%s" target (String.trim stderr)

let format_request path =
  Compiler_process.{args = ["-format"; path]; cwd = Sys.getcwd ()}

let format_check_summary = function
  | 1 -> "The file listed above needs formatting"
  | count -> Printf.sprintf "The %d files listed above need formatting" count

(* Every file is formatted, also after one fails, and the results are
   reported in path order once all are done, relative to the current
   directory. *)
let format_files ?poll ~check files =
  let files = List.sort_uniq String.compare files in
  let display path =
    if Filename.is_relative path then path
    else Project_context.relative_or_absolute ~root:(Sys.getcwd ()) path
  in
  let unformatted = ref [] in
  let failures = ref [] in
  let works =
    files
    |> List.mapi (fun index path ->
        Process.
          {key = Printf.sprintf "%08d" index; dependencies = []; value = path})
  in
  let next path = function
    | None -> Some (Compiler_process.task (format_request path))
    | Some result ->
      (if not (Process.succeeded result) then
         failures := (path, result.stderr) :: !failures
       else
         try
           if read_file path <> result.stdout then
             if check then unformatted := path :: !unformatted
             else write_file path result.stdout
         with Error message -> failures := (path, message) :: !failures);
      None
  in
  Process.run_dependency_graph
    ~max_jobs:(Compiler_execution_mode.configured_count ())
    ?poll works ~next;
  List.iter
    (fun path -> prerr_endline ("[format check] " ^ display path))
    (List.sort String.compare !unformatted);
  if !unformatted <> [] then
    prerr_endline (format_check_summary (List.length !unformatted));
  List.iter
    (fun (path, message) ->
      prerr_endline
        (Output.compiler_output
           ~color:(Output.colors_enabled ~interactive:(Unix.isatty Unix.stderr))
           (formatting_error (display path) message)))
    (List.sort compare !failures);
  match (!failures, !unformatted) with
  | [], [] -> ()
  | _ :: _, _ -> raise (Error "Formatting failed")
  | [], _ :: _ -> raise (Error "Formatting check failed")

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
  let path = Filename.temp_file "rescript-format-" extension in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let contents = read_stdin_interruptibly ?poll () in
      File_util.write_file path contents;
      let result = Compiler_process.run ?poll (format_request path) in
      if not (Process.succeeded result) then
        (* Diagnostics name the temporary file the input went through. *)
        raise
          (Error
             (formatting_error "<stdin>"
                (Str.global_replace (Str.regexp_string path) "<stdin>"
                   result.stderr)));
      print_string result.stdout)

(* Explicit arguments are checked before any is formatted, so a typo does not
   reach the compiler as an I/O error. *)
let check_argument path =
  if not (Sys.file_exists path) then
    raise (Error (Printf.sprintf "%s: No such file" path))
  else if Sys.is_directory path then
    raise
      (Error
         (Printf.sprintf
            "%s is a directory; give .res or .resi files, or no arguments to \
             format the project"
            path))
  else if
    not (Filename.check_suffix path ".res" || Filename.check_suffix path ".resi")
  then raise (Error (Printf.sprintf "%s is not a .res or .resi file" path))

let run_files ?poll ~verbosity ~check paths =
  List.iter check_argument paths;
  let files = if paths = [] then files_in_scope ~verbosity else paths in
  List.iter
    (fun file -> Output.trace ~channel:stderr ~verbosity ("Formatting " ^ file))
    files;
  format_files ?poll ~check files
