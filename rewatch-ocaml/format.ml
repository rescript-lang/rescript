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
  match Sys.getenv_opt "RESCRIPT_BSC_EXE" with
  | Some path when Sys.file_exists path -> Unix.realpath path
  | Some path -> raise (Error ("RESCRIPT_BSC_EXE points to missing path " ^ path))
  | None ->
    let path = Filename.concat (Sys.getcwd ()) "_build/default/compiler/bsc/rescript_compiler_main.exe" in
    if Sys.file_exists path then Unix.realpath path
    else raise (Error "could not locate bsc; set RESCRIPT_BSC_EXE")

let source_file path =
  Filename.check_suffix path ".res" || Filename.check_suffix path ".resi"

let rec sources_under directory =
  if not (Sys.file_exists directory) then []
  else if not (Sys.is_directory directory) then if source_file directory then [directory] else []
  else
    Sys.readdir directory |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name ->
      if List.mem name ["node_modules"; "lib"; "_build"; ".git"] then []
      else sources_under (Filename.concat directory name))

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
  | None -> format_files ~check (if files = [] then sources_under (Sys.getcwd ()) else files)
