open OUnit2

let check condition message = assert_bool message condition

let with_temp_dir f =
  let path = Filename.temp_file "rewatch-compiler-info-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> File_util.remove_tree path) (fun () -> f path)

let write path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let config root =
  write
    (Filename.concat root "rescript.json")
    {|{"name":"compiler-info-test","sources":["src"]}|};
  Unix.mkdir (Filename.concat root "src") 0o755;
  Config.load_root root

let context ?(inherited_compiler_args = []) root config source_map_args =
  let bsc = Filename.concat root "bsc.exe" in
  let runtime = Filename.concat root "runtime" in
  if not (Sys.file_exists bsc) then write bsc "compiler-v1";
  File_util.ensure_dir runtime;
  Compiler_info.make_context ~build_root:root ~bsc_path:bsc
    ~runtime_path:runtime ~source_map_args ~inherited_compiler_args
    ~package_output_specs:(Compiler_info.package_output_specs config)

let tests =
  "compiler_info_tests" >:: fun _context ->
  with_temp_dir (fun root ->
      let config = config root in
      let initial = context root config ["-bs-source-map"; "linked"] in
      check
        (not (Compiler_info.needs_clean initial config))
        "a package without an earlier build is not spuriously cleaned";
      Compiler_info.write_package initial config;
      let marker = File_util.path_of_parts root ["lib"; "ocaml"; "marker"] in
      write marker "keep";
      check
        (not (Compiler_info.needs_clean initial config))
        "matching compiler information is retained";
      check (Sys.file_exists marker) "matching artifacts remain";
      let info_path = Compiler_info.path root in
      Unix.utimes info_path 1_000_000_000. 1_000_000_000.;
      Compiler_info.write_package initial config;
      check
        ((Unix.stat info_path).Unix.st_mtime = 1_000_000_000.)
        "matching compiler information is not rewritten";
      let changed = context root config ["-bs-source-map"; "false"] in
      check
        (Compiler_info.needs_clean changed config)
        "changed source-map arguments invalidate artifacts";
      Compiler_info.clean_package config;
      check (not (Sys.file_exists marker)) "mismatched artifacts are removed");
  with_temp_dir (fun root ->
      let config = config root in
      let initial = context root config [] in
      Compiler_info.write_package initial config;
      write (Filename.concat root "bsc.exe") "compiler-v2";
      let changed = context root config [] in
      check
        (Compiler_info.needs_clean changed config)
        "changed compiler contents invalidate artifacts");
  with_temp_dir (fun root ->
      let config = config root in
      let initial =
        context ~inherited_compiler_args:["-bs-jsx"; "4"] root config []
      in
      Compiler_info.write_package initial config;
      let changed =
        context ~inherited_compiler_args:["-bs-jsx"; "5"] root config []
      in
      check
        (Compiler_info.needs_clean changed config)
        "changed inherited compiler arguments invalidate dependency artifacts");
  with_temp_dir (fun root ->
      let decoded = config root in
      let context = context root decoded [] in
      Compiler_info.write_package context decoded;
      write
        (Filename.concat root "rescript.json")
        {|{"name":"compiler-info-test","sources":["src"],"suffix":".mjs"}|};
      check
        (Compiler_info.matches context decoded)
        "compiler metadata uses the bytes associated with the decoded config";
      let changed = Config.load_root root in
      check
        (not (Compiler_info.matches context changed))
        "reloading changed configuration produces a new fingerprint");
  with_temp_dir (fun root ->
      let config = config root in
      let context = context root config [] in
      let old_log =
        File_util.path_of_parts root ["lib"; "ocaml"; ".compiler.log"]
      in
      write old_log "old build";
      check
        (Compiler_info.needs_clean context config)
        "missing metadata invalidates an existing legacy build";
      Compiler_info.clean_package config;
      check (not (Sys.file_exists old_log)) "legacy build artifacts are removed");
  with_temp_dir (fun root ->
      let dependency = config root in
      let bsc = Filename.concat root "bsc.exe" in
      let runtime = Filename.concat root "runtime" in
      write bsc "compiler-v1";
      File_util.ensure_dir runtime;
      let commonjs =
        [
          {
            Compiler_info.module_format = Config.Commonjs;
            in_source = true;
            suffix = ".js";
          };
        ]
      in
      let esmodule =
        [
          {
            Compiler_info.module_format = Config.Esmodule;
            in_source = true;
            suffix = ".js";
          };
        ]
      in
      let initial =
        Compiler_info.make_context ~build_root:root ~bsc_path:bsc
          ~runtime_path:runtime ~source_map_args:[] ~inherited_compiler_args:[]
          ~package_output_specs:commonjs
      in
      Compiler_info.write_package initial dependency;
      let marker = File_util.path_of_parts root ["lib"; "ocaml"; "marker"] in
      write marker "keep";
      let changed =
        Compiler_info.make_context ~build_root:root ~bsc_path:bsc
          ~runtime_path:runtime ~source_map_args:[] ~inherited_compiler_args:[]
          ~package_output_specs:esmodule
      in
      check
        (Compiler_info.needs_clean changed dependency)
        "same-path module-format changes invalidate dependency artifacts";
      Compiler_info.clean_package dependency;
      check
        (not (Sys.file_exists marker))
        "package-output mismatches remove compiler artifacts");
  with_temp_dir (fun root ->
      let dependency_root = Filename.concat root "dependency" in
      File_util.ensure_dir dependency_root;
      let dependency = config dependency_root in
      let bsc = Filename.concat root "bsc.exe" in
      let runtime = Filename.concat root "runtime" in
      write bsc "compiler-v1";
      File_util.ensure_dir runtime;
      let standalone =
        Compiler_info.make_context ~build_root:dependency_root ~bsc_path:bsc
          ~runtime_path:runtime ~source_map_args:[] ~inherited_compiler_args:[]
          ~package_output_specs:(Compiler_info.package_output_specs dependency)
      in
      Compiler_info.write_package standalone dependency;
      let consumer_root = Filename.concat root "consumer" in
      File_util.ensure_dir consumer_root;
      let consumer_specs =
        [
          {
            Compiler_info.module_format = Config.Commonjs;
            in_source = false;
            suffix = ".cjs";
          };
        ]
      in
      let consumer =
        Compiler_info.make_context ~build_root:consumer_root ~bsc_path:bsc
          ~runtime_path:runtime ~source_map_args:[] ~inherited_compiler_args:[]
          ~package_output_specs:consumer_specs
      in
      check
        (Compiler_info.owns_outputs dependency)
        "a standalone dependency retains ownership of its outputs";
      check
        (Compiler_info.needs_clean consumer dependency)
        "consumer output specs differ when deliberately applied to the \
         dependency";
      check
        (Compiler_info.changed_package_output_specs consumer dependency
        = Some (Compiler_info.package_output_specs dependency))
        "the previous standalone layout remains available for ownership \
         transfer";
      check
        (Compiler_info.matches standalone dependency)
        "standalone dependency metadata remains intact")
