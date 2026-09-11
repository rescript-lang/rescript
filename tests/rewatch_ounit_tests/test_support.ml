let contains_text = String_util.contains

external unsetenv : string -> unit = "rewatch_test_unsetenv"

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let with_temp_dir prefix action =
  let path = Filename.temp_file prefix "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree path)
    (fun () -> action path)
