open Graph_lib

let usage () =
  prerr_endline "Usage: traditional_main.exe <directory-with-graph-files>";
  exit 1

let list_graph_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".graph")
  |> List.map (Filename.concat dir)
  |> List.sort compare

let read_directory dir =
  if not (Sys.file_exists dir) then
    failwith (Printf.sprintf "Path `%s` does not exist" dir);
  if not (Sys.is_directory dir) then
    failwith (Printf.sprintf "`%s` is not a directory" dir);
  match list_graph_files dir with
  | [] ->
      failwith
        (Printf.sprintf "Directory `%s` does not contain any .graph files" dir)
  | files -> files

let format_nodes nodes =
  nodes
  |> List.map string_of_int
  |> String.concat ", "

let main () =
  if Array.length Sys.argv <> 2 then usage ();
  let dir = Sys.argv.(1) in
  let files = read_directory dir in
  let graphs = load_files files in
  let unreachable = unreachable_nodes graphs in
  if List.length unreachable = 0 then
    print_endline "All nodes are reachable from marked nodes."
  else
    Printf.printf "Unreachable nodes: [%s]\n" (format_nodes unreachable)

let () =
  try main () with
  | Duplicate_node_in_file { file; node } ->
      prerr_endline
        (Printf.sprintf
           "Error while parsing `%s`: node %d defined more than once" file node);
      exit 1
  | Duplicate_node_across_files { node; first_file; second_file } ->
      prerr_endline
        (Printf.sprintf
           "Node %d defined in both `%s` and `%s`" node first_file second_file);
      exit 1
  | Edge_source_not_local { file; src; _ } ->
      prerr_endline
        (Printf.sprintf
           "Edge source %d in `%s` must be defined in the same file" src file);
      exit 1
  | Edge_target_undefined { file; src; dst } ->
      prerr_endline
        (Printf.sprintf
           "Edge (%d -> %d) in `%s` references undefined node %d" src dst file
           dst);
      exit 1
  | Invalid_argument msg ->
      prerr_endline msg;
      exit 1
  | Failure msg ->
      prerr_endline msg;
      exit 1

