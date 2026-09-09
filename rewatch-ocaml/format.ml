exception Error of string

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let bsc () =
  try Toolchain.bsc () with Toolchain.Error message -> raise (Error message)

let rec nearest_config directory =
  if Config.exists_in_root directory then Some (Config.path_in_root directory)
  else
    let parent = Filename.dirname directory in
    if parent = directory then None else nearest_config parent

let local_dependency root (dependency : Config.dependency) =
  let rec find directory =
    let candidate =
      Filename.concat (Filename.concat directory "node_modules") dependency.name
    in
    if Sys.file_exists candidate then Some (Unix.realpath candidate)
    else
      let parent = Filename.dirname directory in
      if parent = directory then None else find parent
  in
  match find root with
  | None -> None
  | Some path ->
    if Build.is_local_dependency ~workspace:root path then Some path else None

let package_sources (config : Config.t) =
  Source.discover config ~prod:false ~features:None ~filter:None
    ~on_missing:(fun _ -> ())
    ~display_root:config.root
  |> List.concat_map (fun module_ ->
       Filename.concat config.root module_.Source.implementation
       :: (match module_.interface with
          | None -> []
          | Some path -> [Filename.concat config.root path]))

(* Rust constructs the complete package graph before selecting the local files
   that format owns. Retain that validation and its package diagnostics even
   though installed dependencies are never themselves formatted. *)
let validate_package_graph (current : Config.t) =
  let workspace = Build.workspace_lock_root current.root in
  let resolved_packages = Hashtbl.create 32 in
  Build.validate_package_metadata current;
  let rec visit ~is_local (config : Config.t) =
    let dependencies =
      config.dependencies @ if is_local then config.dev_dependencies else []
    in
    let pending =
      dependencies
      |> List.filter_map (fun (dependency : Config.dependency) ->
           let directory =
             Build.require_dependency_directory ~workspace_root:current.root
               config.root dependency
           in
           match Hashtbl.find_opt resolved_packages dependency.name with
           | Some chosen ->
             if chosen <> directory then
               Printf.eprintf
                 "Duplicated package: %s ./%s (chosen) vs ./%s in ./%s\n%!"
                 dependency.name (Build.relative_to current.root chosen)
                 (Build.relative_to current.root directory)
                 (Build.relative_to current.root config.root);
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
                (Build.Package_error
                   (Printf.sprintf
                      "Could not build package tree for '%s' at path '%s'. Error: %s"
                      dependency.name current.root message))
          in
          Build.validate_package_metadata dependency_config;
          Build.report_missing_sources ~is_root:false dependency_config;
          visit
            ~is_local:(Build.is_local_dependency ~workspace directory)
            dependency_config)
      pending
  in
  visit ~is_local:true current

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
  validate_package_graph current;
  let configs =
    if listed_by_parent then [current]
    else
      current
      :: (current.dependencies @ current.dev_dependencies
         |> List.filter_map (local_dependency current.root)
         |> List.filter_map (fun root ->
              if Config.exists_in_root root then Some (Config.load_root root)
              else None))
  in
  configs |> List.concat_map package_sources |> List.sort_uniq String.compare

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

let format_files ~check files =
  let bsc = bsc () in
  let incorrect = ref [] in
  List.iter (fun path ->
    let replacement = formatted ~bsc ~target:path path in
    let original = read_file path in
    if original <> replacement then
      if check then incorrect := path :: !incorrect else write_file path replacement) files;
  match List.rev !incorrect with
  | [] -> ()
  | paths ->
    List.iter (fun path -> prerr_endline ("[format check] " ^ path)) paths;
    prerr_endline (format_check_summary (List.length paths));
    raise (Error "Formatting check failed")

let format_stdin extension =
  if extension <> ".res" && extension <> ".resi" then
    raise (Error "--stdin must be .res or .resi");
  let temporary = Filename.temp_file "rescript-ocaml-format-" extension in
  Fun.protect
    ~finally:(fun () -> try Sys.remove temporary with Sys_error _ -> ())
    (fun () ->
      let output = open_out_bin temporary in
      Fun.protect ~finally:(fun () -> close_out_noerr output)
        (fun () ->
          try while true do output_char output (input_char stdin) done with End_of_file -> ());
      print_string (formatted ~bsc:(bsc ()) ~target:"stdin" temporary))

let run ~check ~stdin ~files =
  match stdin with
  | Some extension -> format_stdin extension
  | None -> format_files ~check (if files = [] then files_in_scope () else files)
