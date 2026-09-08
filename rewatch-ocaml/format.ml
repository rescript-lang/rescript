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
    let prefix = Filename.concat root "" in
    let comparable = Platform.normalize_path_for_comparison in
    if String.starts_with ~prefix:(comparable prefix) (comparable path) then
      Some path
    else None

let package_sources (config : Config.t) =
  Source.discover config ~prod:false ~features:None ~filter:None
    ~on_missing:(fun _ -> ())
    ~display_root:config.root
  |> List.concat_map (fun module_ ->
       Filename.concat config.root module_.Source.implementation
       :: (match module_.interface with
          | None -> []
          | Some path -> [Filename.concat config.root path]))

let files_in_scope () =
  let config_path =
    match nearest_config (Sys.getcwd ()) with
    | Some path -> path
    | None -> raise (Error "Could not find a rescript.json parent")
  in
  let current = Config.load config_path in
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

let formatted ~bsc path =
  let result = Process.run ~cwd:(Sys.getcwd ()) bsc ["-format"; path] in
  if not (Process.succeeded result) then
    raise (Error ("Error formatting " ^ path ^ ":\n" ^ result.stderr));
  result.stdout

let format_files ~check files =
  let bsc = bsc () in
  let incorrect = ref [] in
  List.iter (fun path ->
    let original = read_file path in
    let replacement = formatted ~bsc path in
    if original <> replacement then
      if check then incorrect := path :: !incorrect else write_file path replacement) files;
  match List.rev !incorrect with
  | [] -> ()
  | paths ->
    List.iter (fun path -> prerr_endline ("[format check] " ^ path)) paths;
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
      print_string (formatted ~bsc:(bsc ()) temporary))

let run ~check ~stdin ~files =
  match stdin with
  | Some extension -> format_stdin extension
  | None -> format_files ~check (if files = [] then files_in_scope () else files)
