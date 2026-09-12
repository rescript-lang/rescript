open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let with_temp_dir = Test_support.with_temp_dir "rewatch-build-artifacts-"

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
          ~ast_sources:
            [{Compile_assets.ast_path = published_ast; source_path = source}]
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
        "unmapped legacy artifacts fall back to the recursive working inventory");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_output = Filename.concat root "src/A.js" in
      let old_map = old_output ^ ".map" in
      let working_output = Filename.concat root "lib/bs/src/A.js" in
      let working_map = working_output ^ ".map" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      write_file config_path
        {|{"name":"cleanup-map","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true}}|};
      List.iter
        (fun path -> write_file path "generated")
        [old_output; old_map; working_output; working_map];
      let config = Config.load_root root in
      let moved_module : Source.module_ =
        {
          name = "A";
          implementation = "src/nested/A.res";
          interface = None;
          is_dev = false;
        }
      in
      ignore
        (Build_artifacts.cleanup_stale ~ocaml_files:[] ~ast_sources:[]
           ~source_files:[old_output; old_map] ~root ~ocaml_dir ~is_local:true
           config [moved_module]);
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("moving a source removes its stale output family: " ^ path))
        [old_output; old_map; working_output; working_map]);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_source = Filename.concat root "src/A.res" in
      let new_source = Filename.concat root "src/nested/A.res" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let published_ast = Filename.concat ocaml_dir "A.ast" in
      let working_ast = Filename.concat root "lib/bs/src/A.ast" in
      write_file config_path
        {|{"name":"moved-source","sources":{"dir":"src","subdirs":true}}|};
      write_file new_source "let value = 1";
      write_file published_ast
        ("Caml1999X\nDependency\n" ^ old_source ^ "\nbinary payload");
      write_file working_ast "old working AST";
      let config = Config.load_root root in
      let module_ : Source.module_ =
        {
          name = "A";
          implementation = "src/nested/A.res";
          interface = None;
          is_dev = false;
        }
      in
      let compile_assets = Compile_assets.create [ocaml_dir] in
      let result =
        Build_artifacts.cleanup_stale ~ocaml_files:[published_ast]
          ~ast_sources:
            [
              {Compile_assets.ast_path = published_ast; source_path = old_source};
            ]
          ~source_files:[new_source] ~root ~ocaml_dir ~is_local:true config
          [module_]
      in
      check
        (not (Sys.file_exists published_ast || Sys.file_exists working_ast))
        "moving a source invalidates the AST associated with its old path";
      check
        (List.mem "A" result.removed_modules)
        "moving a source marks its module for recompilation";
      check
        (Build_freshness.source_is_not_older_than_ast compile_assets ~root
           ~source_mtimes:(Hashtbl.create 0) module_.implementation)
        "a moved source cannot reuse the published AST for its old path");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_source = Filename.concat root "old/Foo.res" in
      let old_output = Filename.concat root "old/Foo.js" in
      let old_map = old_output ^ ".map" in
      let working_output = Filename.concat root "lib/bs/old/Foo.js" in
      let working_map = working_output ^ ".map" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let published_ast = Filename.concat ocaml_dir "Foo.ast" in
      write_file config_path
        {|{"name":"cleanup-removed-source-dir","sources":"src","package-specs":{"module":"esmodule","in-source":true}}|};
      List.iter
        (fun path -> write_file path "generated")
        [
          old_source;
          old_output;
          old_map;
          working_output;
          working_map;
          published_ast;
        ];
      let config = Config.load_root root in
      ignore
        (Build_artifacts.cleanup_stale ~ocaml_files:[published_ast]
           ~ast_sources:
             [
               {
                 Compile_assets.ast_path = published_ast;
                 source_path = old_source;
               };
             ]
           ~source_files:[] ~root ~ocaml_dir ~is_local:true config []);
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("removing a source directory removes its stale output family: "
           ^ path))
        [old_output; old_map; working_output; working_map]);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let source = Filename.concat root "src/Custom.res" in
      let public_output = Filename.concat root "src/Custom.generated.js" in
      let public_map = public_output ^ ".map" in
      let working_output =
        Filename.concat root "lib/bs/src/Custom.generated.js"
      in
      let working_map = working_output ^ ".map" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let published_ast = Filename.concat ocaml_dir "Custom.ast" in
      write_file config_path
        {|{"name":"custom-cleanup","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".generated.js"}}|};
      List.iter
        (fun path -> write_file path "generated")
        [public_output; public_map; working_output; working_map; published_ast];
      let config = Config.load_root root in
      ignore
        (Build_artifacts.cleanup_stale ~ocaml_files:[published_ast]
           ~ast_sources:
             [{Compile_assets.ast_path = published_ast; source_path = source}]
           ~source_files:[public_output; public_map]
           ~root ~ocaml_dir ~is_local:true config []);
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("custom suffix cleanup removes " ^ path))
        [public_output; public_map; working_output; working_map]);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let output = Filename.concat root "src/Present.output" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      write_file config_path
        {|{"name":"custom-freshness","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".output"}}|};
      write_file output "generated";
      let config = Config.load_root root in
      let module_ : Source.module_ =
        {
          name = "Present";
          implementation = "src/Present.res";
          interface = None;
          is_dev = false;
        }
      in
      let result =
        Build_artifacts.cleanup_stale ~ocaml_files:[] ~ast_sources:[]
          ~source_files:[output] ~present_source_files:[output] ~root ~ocaml_dir
          ~is_local:true config [module_]
      in
      check
        (Hashtbl.mem result.present_public_outputs output)
        "custom suffix outputs participate in unchanged-build freshness");
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let working_dir = Filename.concat root "lib/bs/src" in
      let published_cmti = Filename.concat ocaml_dir "A.cmti" in
      let working_cmti = Filename.concat working_dir "A.cmti" in
      write_file config_path
        {|{"name":"removed-interface","sources":"src","package-specs":{"module":"esmodule","in-source":true}}|};
      write_file published_cmti "published interface";
      write_file working_cmti "working interface";
      let config = Config.load_root root in
      let module_ : Source.module_ =
        {
          name = "A";
          implementation = "src/A.res";
          interface = None;
          is_dev = false;
        }
      in
      ignore
        (Build_artifacts.cleanup_stale ~ocaml_files:[published_cmti]
           ~ast_sources:[] ~source_files:[] ~root ~ocaml_dir ~is_local:true
           config [module_]);
      List.iter
        (fun path ->
          check
            (not (Sys.file_exists path))
            ("removing an interface removes " ^ path))
        [published_cmti; working_cmti]);
  with_temp_dir (fun root ->
      let config_path = Filename.concat root "rescript.json" in
      let old_source = Filename.concat root "src/generated/A.res" in
      let generated_output = Filename.concat root "src/generated/A.js" in
      let authored_output = Filename.concat root "src/handwritten/A.js" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let published_ast = Filename.concat ocaml_dir "A.ast" in
      write_file config_path
        {|{"name":"owned-output-path","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true}}|};
      List.iter
        (fun path -> write_file path "contents")
        [generated_output; authored_output; published_ast];
      let config = Config.load_root root in
      ignore
        (Build_artifacts.cleanup_stale ~ocaml_files:[published_ast]
           ~ast_sources:
             [
               {
                 Compile_assets.ast_path = published_ast;
                 source_path = old_source;
               };
             ]
           ~source_files:[generated_output; authored_output]
           ~root ~ocaml_dir ~is_local:true config []);
      check
        (not (Sys.file_exists generated_output))
        "cleanup removes the output at the historical source location";
      check
        (Sys.file_exists authored_output)
        "cleanup preserves unrelated JavaScript with the same basename")
