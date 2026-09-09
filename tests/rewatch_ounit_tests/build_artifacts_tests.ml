open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let with_temp_dir run =
  let root = Filename.temp_file "rewatch-build-artifacts-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree root)
    (fun () -> run root)

let tests =
  "build_artifacts_tests" >:: fun _context ->
  with_temp_dir (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "nested/second" in
      let missing = Filename.concat root "missing" in
      write_file first "same";
      write_file second "same";
      check
        (Build_artifacts.files_equal first second)
        "equal file contents should compare equal";
      write_file second "different";
      check
        (not (Build_artifacts.files_equal first second))
        "different file contents should not compare equal";
      check
        (not (Build_artifacts.files_equal missing first))
        "a missing file should not compare equal";
      check
        (Option.is_some (Build_artifacts.modification_time first))
        "an existing file should have a modification time";
      check
        (Option.is_none (Build_artifacts.modification_time missing))
        "a missing file should not have a modification time";
      let optional_copy = Filename.concat root "optional-copy" in
      Build_artifacts.copy_optional_existing_file first optional_copy;
      check
        (Build_artifacts.read_file optional_copy = "same")
        "an available optional artifact should be copied";
      Build_artifacts.copy_optional_existing_file missing optional_copy;
      check
        (not (Sys.file_exists optional_copy))
        "a stale optional destination should be removed when its source is \
         absent";
      let destination_failure_is_reported =
        try
          Build_artifacts.copy_optional_existing_file ~ensure_parent:false first
            (Filename.concat root "absent/optional-copy");
          false
        with Sys_error _ | Unix.Unix_error _ -> true
      in
      check destination_failure_is_reported
        "an optional copy must not hide destination failures";
      check
        (List.sort String.compare (Build_artifacts.files_under root)
        = List.sort String.compare [first; second])
        "recursive inventory should contain files but not directories";
      if not Sys.win32 then (
        let live_link = Filename.concat root "live-link" in
        let dangling_link = Filename.concat root "dangling-link" in
        Unix.symlink first live_link;
        Unix.symlink missing dangling_link;
        check
          (List.sort String.compare (Build_artifacts.files_under root)
          = List.sort String.compare [first; second; live_link])
          "recursive inventory should retain live links and omit dangling links");
      Build_artifacts.remove_file first;
      Build_artifacts.remove_file first;
      check
        (not (Sys.file_exists first))
        "removing an existing or already-missing file should be idempotent");
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
