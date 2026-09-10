open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let with_temp_dir run =
  let root = Filename.temp_file "rewatch-build-artifacts-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () -> run root)

let tests =
  "build_artifacts_tests" >:: fun _context ->
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let source = Filename.concat root "src/Old.res" in
      let public_output = Filename.concat root "src/Old.bs.js" in
      let public_map = public_output ^ ".map" in
      let build_dir = Filename.concat root "lib/bs" in
      let working_dir = Filename.concat build_dir "src" in
      let working_ast = Filename.concat working_dir "Old.ast" in
      let working_cmi = Filename.concat working_dir "Old-Ns.cmi" in
      let working_output = Filename.concat working_dir "Old.bs.js" in
      let working_map = working_output ^ ".map" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let published_ast = Filename.concat ocaml_dir "Old.ast" in
      let published_cmi = Filename.concat ocaml_dir "Old-Ns.cmi" in
      write_file config_path
        {|{"name":"cleanup","namespace":"Ns","sources":{"dir":"src"},"package-specs":{"module":"esmodule","in-source":true,"suffix":".bs.js"}}|};
      List.iter
        (fun path -> write_file path "generated")
        [
          public_output;
          public_map;
          working_ast;
          working_cmi;
          working_output;
          working_map;
          published_ast;
          published_cmi;
        ];
      let config = Config.load_root root in
      let result =
        Build_artifacts.cleanup_stale
          ~ocaml_files:[published_ast; published_cmi]
          ~ast_sources:[(published_ast, source)]
          ~source_files:[public_output; public_map]
          ~root ~ocaml_dir ~is_local:true config []
      in
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("direct cleanup should remove " ^ path))
        [
          public_output;
          public_map;
          working_ast;
          working_output;
          working_map;
          published_ast;
          published_cmi;
        ];
      check
        (Sys.file_exists working_cmi)
        "a directly mapped working CMI remains available through compilation";
      check
        (result.deferred_artifacts = [working_cmi])
        "direct cleanup returns the working CMI for deferred removal";
      check
        (result.removed_modules = ["Old"])
        "a removed AST records its module for invalidation");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let published_cmt = Filename.concat ocaml_dir "Legacy.cmt" in
      let working_cmt = Filename.concat root "lib/bs/nested/Legacy.cmt" in
      write_file config_path {|{"name":"cleanup"}|};
      write_file published_cmt "published";
      write_file working_cmt "working";
      let config = Config.load_root root in
      ignore
        (Build_artifacts.cleanup_stale ~ocaml_files:[published_cmt]
           ~ast_sources:[] ~source_files:[] ~root ~ocaml_dir ~is_local:true
           config []);
      check
        (not (Sys.file_exists working_cmt))
        "unmapped legacy artifacts fall back to the recursive working inventory")
