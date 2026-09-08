let check condition message = if not condition then failwith message

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let with_temp_dir f =
  let path = Filename.temp_file "rewatch-ocaml-format-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  Fun.protect ~finally:(fun () -> Build_artifacts.remove_tree path) (fun () ->
    f path)

let () =
  check
    (Format.formatting_error "stdin" "invalid source"
    = "Error formatting stdin: invalid source")
    "stdin formatting failures do not expose a temporary filename";
  check
    (Format.formatting_error "src/A.res" "invalid source"
    = "Error formatting src/A.res: invalid source")
    "file formatting failures retain the source path";
  check
    (Format.format_check_summary 1
    = "The file listed above needs formatting")
    "format check uses Rust's singular summary";
  check
    (Format.format_check_summary 2
    = "The 2 files listed above need formatting")
    "format check uses Rust's plural summary";
  with_temp_dir (fun root ->
    let root_source = Filename.concat root "src/App.res" in
    let installed_source =
      Filename.concat root "node_modules/installed/src/Installed.res"
    in
    write_file (Filename.concat root "rescript.json")
      {|{"name":"app","sources":["src"],"dependencies":["installed"]}|};
    write_file root_source "let value = 1\n";
    write_file (Filename.concat root "node_modules/installed/rescript.json")
      {|{"name":"installed","sources":["src"]}|};
    write_file installed_source "let value = 2\n";
    let previous = Sys.getcwd () in
    let files =
      Fun.protect ~finally:(fun () -> Unix.chdir previous) (fun () ->
        Unix.chdir root;
        Format.files_in_scope ())
    in
    check (List.mem root_source files)
      "implicit format includes the current package";
    check (not (List.mem installed_source files))
      "implicit format does not rewrite installed node_modules dependencies")
