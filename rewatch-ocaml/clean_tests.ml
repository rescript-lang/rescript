let check condition message = if not condition then failwith message

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let with_temp_dir f =
  let path = Filename.temp_file "rewatch-ocaml-clean-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  Fun.protect ~finally:(fun () -> Build_artifacts.remove_tree path) (fun () ->
    f path)

let () =
  with_temp_dir (fun root ->
    write_file (Filename.concat root "rescript.json")
      {|{
        "name": "clean-ownership",
        "sources": ["src"],
        "package-specs": {"module": "esmodule", "in-source": false}
      }|};
    write_file (Filename.concat root "src/A.res") "let value = 1\n";
    let generated = Filename.concat root "lib/es6/src/A.js" in
    let unowned = Filename.concat root "lib/es6/keep.txt" in
    let unowned_javascript = Filename.concat root "lib/es6/src/Manual.js" in
    write_file generated "generated\n";
    write_file (generated ^ ".map") "generated map\n";
    write_file unowned "keep\n";
    write_file unowned_javascript "manual\n";
    write_file (Filename.concat root "lib/bs/compiler-state") "temporary\n";
    write_file (Filename.concat root "lib/ocaml/A.cmj") "temporary\n";
    Build.clean ~seen:[] ~folder:root ~prod:false;
    check (not (Sys.file_exists generated))
      "clean removes the configured generated output";
    check (not (Sys.file_exists (generated ^ ".map")))
      "clean removes the configured generated source map";
    check (Sys.file_exists unowned)
      "clean preserves unrelated files in an out-of-source directory";
    check (Sys.file_exists unowned_javascript)
      "clean preserves JavaScript without a matching source module";
    check (not (Sys.file_exists (Filename.concat root "lib/bs")))
      "clean removes compiler working artifacts";
    check (not (Sys.file_exists (Filename.concat root "lib/ocaml")))
      "clean removes published compiler artifacts")
