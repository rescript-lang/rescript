open Graph_lib

let usage () =
  prerr_endline
    "Usage: reactive_main.exe [heap-file] <directory-with-graph-files>";
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

let default_heap_size = 512 * 1024 * 1024
let default_heap_file = "graph.rheap"

let build_pipeline files =
  let inputs = Reactive.input_files (Array.of_list files) in
  let parsed =
    Reactive.map inputs (fun file trackers ->
        if Array.length trackers = 0 then
          failwith "Reactive runtime returned no trackers";
        let raw = Reactive.read_file file trackers.(0) in
        let graph = parse_file ~file raw in
        [| ("graphs", [| graph |]) |])
  in
  Reactive.map parsed (fun _ graphs ->
      let graphs = Array.to_list graphs in
      let unreachable = unreachable_nodes graphs in
      [| ("unreachable", [| unreachable |]) |])

let format_nodes nodes =
  nodes |> List.map string_of_int |> String.concat ", "

let delete_heap_file path =
  if Sys.file_exists path then
    try Sys.remove path with
    | Sys_error msg ->
        prerr_endline
          (Printf.sprintf "Warning: failed to delete heap `%s`: %s" path msg)

let main () =
  let heap_file, dir =
    match Array.length Sys.argv with
    | 2 -> (default_heap_file, Sys.argv.(1))
    | 3 -> (Sys.argv.(1), Sys.argv.(2))
    | _ -> usage ()
  in
  Fun.protect
    ~finally:(fun () -> delete_heap_file heap_file)
    (fun () ->
      let files = read_directory dir in
      Reactive.init heap_file default_heap_size;
      let heap_usage_after_init = Reactive.heap_usage () in
      Printf.printf "Reactive heap initialized (usage %Ld bytes).\n%!"
        heap_usage_after_init;
      let pipeline = build_pipeline files in
      Reactive.exit ();
      let heap_usage_after_exit = Reactive.heap_usage () in
      let heap_delta =
        Int64.sub heap_usage_after_exit heap_usage_after_init
      in
      let results = Reactive.get_array pipeline "unreachable" in
      let unreachable =
        results |> Array.to_list |> List.concat |> List.sort_uniq compare
      in
      if List.length unreachable = 0 then
        Printf.printf
          "Reactive run complete. All nodes reachable (%d files processed; heap delta %+Ld bytes since init).\n"
          (List.length files) heap_delta
      else
        Printf.printf
          "Reactive run complete. Unreachable nodes: [%s] (heap delta %+Ld bytes since init)\n"
          (format_nodes unreachable) heap_delta)

let () =
  (* Keep the CLI small: the traditional entry point demonstrates the full
     pattern of translating Graph_lib exceptions into friendly errors. *)
  try main () with
  | Failure msg ->
      prerr_endline msg;
      exit 1
  | Invalid_argument msg ->
      prerr_endline msg;
      exit 1
  | exn ->
      prerr_endline (Printexc.to_string exn);
      exit 1
