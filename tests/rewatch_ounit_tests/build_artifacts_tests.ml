open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let with_temp_dir = Test_support.with_temp_dir "rewatch-build-artifacts-"

let cleanup_stale ~artifact_files ~ast_sources ?(present_source_files = [])
    ?(on_removed_module = ignore) ~root config modules =
  Build_artifacts.cleanup_stale ~artifact_files ~ast_sources
    ~present_source_files ~on_removed_module ~root config modules

let module_ name implementation : Source.module_ =
  {name; implementation; interface = None; is_dev = false}

let tests =
  "build_artifacts_tests" >:: fun _context ->
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let source = Test_support.path root "src/Old.res" in
      let public_output = Test_support.path root "src/Old.bs.js" in
      let public_map = public_output ^ ".map" in
      let artifact_dir = Test_support.path root "lib/rescript" in
      let ast = Filename.concat artifact_dir "Old.ast" in
      let cmi = Filename.concat artifact_dir "Old-Ns.cmi" in
      let cmj = Filename.concat artifact_dir "Old-Ns.cmj" in
      write_file config_path
        {|{"name":"cleanup","namespace":"Ns","sources":{"dir":"src"},"package-specs":{"module":"esmodule","in-source":true,"suffix":".bs.js"}}|};
      List.iter
        (fun path -> write_file path "generated")
        [public_output; public_map; ast; cmi; cmj];
      let config = Config.load_root root in
      let removed = ref [] in
      let result =
        cleanup_stale
          ~on_removed_module:(fun name -> removed := name :: !removed)
          ~artifact_files:[ast; cmi; cmj]
          ~ast_sources:[{Compile_assets.ast_path = ast; source_path = source}]
          ~present_source_files:[public_output; public_map]
          ~root config []
      in
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("a removed source's artifacts and outputs are removed: " ^ path))
        [public_output; public_map; ast; cmi; cmj];
      check
        (result.removed_modules = ["Old"] && !removed = ["Old"])
        "a removed AST records its module for invalidation");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let artifact_dir = Test_support.path root "lib/rescript" in
      let unexpected = Filename.concat artifact_dir "Legacy.cmt" in
      let leftover =
        Filename.concat artifact_dir (File_util.atomic_write_prefix ^ "1a2b.tmp")
      in
      let kept =
        List.map
          (Filename.concat artifact_dir)
          [
            ".compiler.log";
            "compiler-info.json";
            ".sourcedirs.json";
            "build.lock";
            "watch.lock";
            ".build-lock-1a2b.tmp";
          ]
      in
      write_file config_path {|{"name":"cleanup"}|};
      List.iter
        (fun path -> write_file path "contents")
        (unexpected :: leftover :: kept);
      let config = Config.load_root root in
      let inventory = Compile_assets.create [artifact_dir] in
      ignore
        (cleanup_stale
           ~artifact_files:(Compile_assets.files inventory artifact_dir)
           ~ast_sources:[] ~root config []);
      check
        (not (Sys.file_exists leftover))
        "a file a killed build was writing is removed";
      check
        (not (Sys.file_exists unexpected))
        "an artifact of no current module is removed";
      List.iter
        (fun path ->
          check (Sys.file_exists path)
            ("build state is not an artifact: " ^ path))
        kept);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_source = Test_support.path root "src/A.res" in
      let new_source = Test_support.path root "src/nested/A.res" in
      let old_output = Test_support.path root "src/A.js" in
      let old_map = old_output ^ ".map" in
      let new_output = Test_support.path root "src/nested/A.js" in
      let artifact_dir = Test_support.path root "lib/rescript" in
      let ast = Filename.concat artifact_dir "A.ast" in
      write_file config_path
        {|{"name":"moved-source","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true}}|};
      write_file new_source "let value = 1";
      List.iter
        (fun path -> write_file path "generated")
        [old_output; old_map; new_output];
      Test_support.write_ast_header ast ~dependencies:["Dependency"]
        ~source:old_source;
      let config = Config.load_root root in
      let moved = module_ "A" "src/nested/A.res" in
      let compile_assets = Compile_assets.create [artifact_dir] in
      let result =
        cleanup_stale ~artifact_files:[ast]
          ~ast_sources:
            [{Compile_assets.ast_path = ast; source_path = old_source}]
          ~present_source_files:[old_output; old_map; new_output]
          ~root config [moved]
      in
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("moving a source removes its old output: " ^ path))
        [old_output; old_map];
      check
        (Sys.file_exists new_output)
        "moving a source keeps the output at its new location";
      check
        (not (Sys.file_exists ast))
        "moving a source invalidates the AST associated with its old path";
      check
        (List.mem "A" result.removed_modules)
        "moving a source marks its module for recompilation";
      check
        (Build_freshness.source_is_not_older_than_ast compile_assets ~root
           ~source_mtimes:(Hashtbl.create 0) moved.implementation)
        "a moved source cannot reuse the AST for its old path");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let authored = Test_support.path root "src/Authored.js" in
      write_file config_path
        {|{"name":"authored","sources":"src","package-specs":{"module":"commonjs","in-source":true}}|};
      write_file authored "module.exports = {}";
      let config = Config.load_root root in
      ignore
        (cleanup_stale ~artifact_files:[] ~ast_sources:[]
           ~present_source_files:[authored] ~root config []);
      check (Sys.file_exists authored)
        "JavaScript without an AST proving that the build generated it is kept");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_source = Test_support.path root "old/Foo.res" in
      let old_output = Test_support.path root "old/Foo.js" in
      let old_map = old_output ^ ".map" in
      let ast = Test_support.path root "lib/rescript/Foo.ast" in
      write_file config_path
        {|{"name":"cleanup-removed-source-dir","sources":"src","package-specs":{"module":"esmodule","in-source":true}}|};
      List.iter
        (fun path -> write_file path "generated")
        [old_source; old_output; old_map; ast];
      let config = Config.load_root root in
      ignore
        (cleanup_stale ~artifact_files:[ast]
           ~ast_sources:
             [{Compile_assets.ast_path = ast; source_path = old_source}]
           ~root config []);
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("removing a source directory removes its stale outputs: " ^ path))
        [old_output; old_map]);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let source = Test_support.path root "src/Custom.res" in
      let public_output = Test_support.path root "src/Custom.generated.js" in
      let public_map = public_output ^ ".map" in
      let ast = Test_support.path root "lib/rescript/Custom.ast" in
      write_file config_path
        {|{"name":"custom-cleanup","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".generated.js"}}|};
      List.iter
        (fun path -> write_file path "generated")
        [public_output; public_map; ast];
      let config = Config.load_root root in
      ignore
        (cleanup_stale ~artifact_files:[ast]
           ~ast_sources:[{Compile_assets.ast_path = ast; source_path = source}]
           ~present_source_files:[public_output; public_map]
           ~root config []);
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("custom suffix cleanup removes " ^ path))
        [public_output; public_map]);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let output = Test_support.path root "src/Present.output" in
      write_file config_path
        {|{"name":"custom-freshness","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".output"}}|};
      write_file output "generated";
      let config = Config.load_root root in
      let result =
        cleanup_stale ~artifact_files:[] ~ast_sources:[]
          ~present_source_files:[output] ~root config
          [module_ "Present" "src/Present.res"]
      in
      check
        (Hashtbl.mem result.present_public_outputs output)
        "custom suffix outputs participate in unchanged-build freshness");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let cmti = Test_support.path root "lib/rescript/A.cmti" in
      write_file config_path
        {|{"name":"removed-interface","sources":"src","package-specs":{"module":"esmodule","in-source":true}}|};
      write_file cmti "interface";
      let config = Config.load_root root in
      ignore
        (cleanup_stale ~artifact_files:[cmti] ~ast_sources:[] ~root config
           [module_ "A" "src/A.res"]);
      check
        (not (Sys.file_exists cmti))
        "removing an interface removes its typed tree");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_source = Test_support.path root "src/generated/A.res" in
      let generated_output = Test_support.path root "src/generated/A.js" in
      let authored_output = Test_support.path root "src/handwritten/A.js" in
      let ast = Test_support.path root "lib/rescript/A.ast" in
      write_file config_path
        {|{"name":"owned-output-path","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true}}|};
      List.iter
        (fun path -> write_file path "contents")
        [generated_output; authored_output; ast];
      let config = Config.load_root root in
      ignore
        (cleanup_stale ~artifact_files:[ast]
           ~ast_sources:
             [{Compile_assets.ast_path = ast; source_path = old_source}]
           ~present_source_files:[generated_output; authored_output]
           ~root config []);
      check
        (not (Sys.file_exists generated_output))
        "cleanup removes the output at the historical source location";
      check
        (Sys.file_exists authored_output)
        "cleanup preserves unrelated JavaScript with the same basename");
  with_temp_dir (fun root ->
      let artifact_dir = Test_support.path root "lib/rescript" in
      let artifacts =
        List.map
          (Filename.concat artifact_dir)
          ["A.ast"; "A.cmi"; "incremental/A.cmt"]
      in
      let locks =
        List.map
          (Filename.concat artifact_dir)
          ["build.lock"; "watch.lock"; ".build-lock-123"]
      in
      let legacy =
        [
          Test_support.path root "lib/bs/src/A.ast";
          Test_support.path root "lib/ocaml/A.cmi";
        ]
      in
      List.iter
        (fun path -> write_file path "contents")
        (artifacts @ locks @ legacy);
      Build_artifacts.clean root;
      List.iter
        (fun path ->
          check (not (Sys.file_exists path)) ("clean removes " ^ path))
        (artifacts @ legacy);
      List.iter
        (fun path ->
          check (Sys.file_exists path) ("clean keeps the lock " ^ path))
        locks)
