open OUnit2

let check condition message = assert_bool message condition

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let with_temp_dir f =
  let path = Filename.temp_file "rewatch-ocaml-format-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  Fun.protect ~finally:(fun () -> File_util.remove_tree path) (fun () -> f path)

let tests =
  "format_tests" >:: fun _context ->
  check
    (Format.formatting_error "stdin" "invalid source"
    = "Error formatting stdin: invalid source")
    "stdin formatting failures do not expose a temporary filename";
  check
    (Format.formatting_error "src/A.res" "invalid source"
    = "Error formatting src/A.res: invalid source")
    "file formatting failures retain the source path";
  check
    (Format.format_check_summary 1 = "The file listed above needs formatting")
    "format check uses Rust's singular summary";
  check
    (Format.format_check_summary 2 = "The 2 files listed above need formatting")
    "format check uses Rust's plural summary";
  with_temp_dir (fun root ->
      let parent = Filename.concat root "not-a-directory" in
      let path = Filename.concat parent "A.res" in
      write_file parent "file\n";
      check
        (try
           Format.write_file path "let value = 1\n";
           false
         with Format.Error message ->
           contains message "Could not write formatted file"
           && contains message path)
        "formatter write failures retain their operation and source path");
  with_temp_dir (fun root ->
      let first = Filename.concat root "First.res" in
      let second = Filename.concat root "Second.res" in
      write_file first "let first = 1\n";
      write_file second "let second = 2\n";
      let previous_root = Sys.getenv_opt "REWATCH_FORMAT_TEST_ROOT" in
      Unix.putenv "REWATCH_FORMAT_TEST_ROOT" root;
      Fun.protect
        ~finally:(fun () ->
          match previous_root with
          | Some value -> Unix.putenv "REWATCH_FORMAT_TEST_ROOT" value
          | None -> Unix.unsetenv "REWATCH_FORMAT_TEST_ROOT")
        (fun () ->
          Format.format_files_with_bsc ~max_jobs:2
            ~bsc:(Unix.realpath Sys.executable_name)
            ~check:true [first; second]);
      check
        (Sys.file_exists (first ^ ".started")
        && Sys.file_exists (second ^ ".started"))
        "formatter subprocesses overlap rather than running serially");
  with_temp_dir (fun root ->
      let root_source = Filename.concat root "src/App.res" in
      let installed_source =
        Filename.concat root "node_modules/installed/src/Installed.res"
      in
      write_file
        (Filename.concat root "rescript.json")
        {|{"name":"app","sources":["src"],"dependencies":["installed"]}|};
      write_file root_source "let value = 1\n";
      write_file
        (Filename.concat root "node_modules/installed/rescript.json")
        {|{"name":"installed","sources":["src"]}|};
      write_file installed_source "let value = 2\n";
      let previous = Sys.getcwd () in
      let files =
        Fun.protect
          ~finally:(fun () -> Unix.chdir previous)
          (fun () ->
            Unix.chdir root;
            Format.files_in_scope ())
      in
      check
        (List.mem root_source files)
        "implicit format includes the current package";
      check
        (not (List.mem installed_source files))
        "implicit format does not rewrite installed node_modules dependencies")
