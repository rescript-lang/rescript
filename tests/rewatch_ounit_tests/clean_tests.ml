open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let with_temp_dir f =
  let path = Filename.temp_file "rewatch-ocaml-clean-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> File_util.remove_tree path) (fun () -> f path)

let tests =
  "clean_tests" >:: fun _context ->
  with_temp_dir (fun root ->
      write_file
        (Filename.concat root "rescript.json")
        {|{
        "name": "clean-ownership",
        "sources": [{"dir":"src","subdirs":true}],
        "dependencies": ["installed"],
        "package-specs": [
          {"module": "esmodule", "in-source": false},
          {"module": "esmodule", "in-source": true, "suffix": ".in.js"}
        ]
      }|};
      write_file (Filename.concat root "src/A.res") "let value = 1\n";
      write_file
        (Filename.concat root "node_modules/installed/rescript.json")
        {|{"name":"installed","sources":["src"]}|};
      write_file
        (Filename.concat root "node_modules/installed/src/Installed.res")
        "let value = 2\n";
      let generated = Filename.concat root "lib/es6/src/A.js" in
      let unowned = Filename.concat root "lib/es6/keep.txt" in
      let unowned_javascript = Filename.concat root "lib/es6/src/Manual.js" in
      let dependency_generated =
        Filename.concat root "node_modules/installed/lib/es6/src/Installed.js"
      in
      let dependency_unowned =
        Filename.concat root "node_modules/installed/lib/es6/keep.txt"
      in
      let abandoned_source_sidecar =
        Filename.concat root "src/Deleted.js.rewatch-pending"
      in
      let abandoned_map_sidecar =
        Filename.concat root "lib/es6/deleted/Deleted.js.map.rewatch-backup"
      in
      let unrelated_sidecar_name =
        Filename.concat root "lib/es6/notes.rewatch-pending"
      in
      let duplicate_a = Filename.concat root "src/one/Duplicate.res" in
      let duplicate_b = Filename.concat root "src/two/Duplicate.res" in
      let duplicate_output_a =
        Filename.concat root "lib/es6/src/one/Duplicate.js"
      in
      let duplicate_output_b =
        Filename.concat root "lib/es6/src/two/Duplicate.js"
      in
      let duplicate_in_source_a =
        Filename.concat root "src/one/Duplicate.in.js"
      in
      let duplicate_in_source_b =
        Filename.concat root "src/two/Duplicate.in.js"
      in
      write_file generated "generated\n";
      write_file (generated ^ ".map") "generated map\n";
      write_file unowned "keep\n";
      write_file unowned_javascript "manual\n";
      write_file dependency_generated "generated dependency\n";
      write_file dependency_unowned "keep dependency\n";
      write_file abandoned_source_sidecar "abandoned output\n";
      write_file abandoned_map_sidecar "abandoned map\n";
      write_file unrelated_sidecar_name "not a generated output\n";
      write_file duplicate_a "let value = 1\n";
      write_file duplicate_b "let value = 2\n";
      write_file duplicate_output_a "generated duplicate one\n";
      write_file duplicate_output_b "generated duplicate two\n";
      write_file duplicate_in_source_a "generated duplicate one\n";
      write_file duplicate_in_source_b "generated duplicate two\n";
      write_file (Filename.concat root "lib/bs/compiler-state") "temporary\n";
      write_file (Filename.concat root "lib/ocaml/A.cmj") "temporary\n";
      Build.clean ~seen:[] ~verbosity:(-1) ~folder:root ~prod:false;
      check
        (not (Sys.file_exists generated))
        "clean removes the configured generated output";
      check
        (not (Sys.file_exists (generated ^ ".map")))
        "clean removes the configured generated source map";
      check (Sys.file_exists unowned)
        "clean preserves unrelated files in an out-of-source directory";
      check
        (Sys.file_exists unowned_javascript)
        "clean preserves JavaScript without a matching source module";
      check
        (not (Sys.file_exists dependency_generated))
        "clean removes configured outputs from installed dependencies";
      check
        (Sys.file_exists dependency_unowned)
        "clean preserves unrelated files beside installed dependency outputs";
      check
        (not (Sys.file_exists abandoned_source_sidecar))
        "clean removes a staging sidecar whose source was deleted";
      check
        (not (Sys.file_exists abandoned_map_sidecar))
        "clean removes an abandoned staged source map";
      check
        (Sys.file_exists unrelated_sidecar_name)
        "clean preserves staging-like names that are not generated outputs";
      check
        (not (Sys.file_exists duplicate_output_a))
        "clean removes the first output when module names are duplicated";
      check
        (not (Sys.file_exists duplicate_output_b))
        "clean removes the second output when module names are duplicated";
      check
        (not (Sys.file_exists duplicate_in_source_a))
        "invalid-graph clean removes the first in-source output";
      check
        (not (Sys.file_exists duplicate_in_source_b))
        "invalid-graph clean removes the second in-source output";
      check
        (not (Sys.file_exists (Filename.concat root "lib/bs")))
        "clean removes compiler working artifacts";
      check
        (not (Sys.file_exists (Filename.concat root "lib/ocaml")))
        "clean removes published compiler artifacts")
