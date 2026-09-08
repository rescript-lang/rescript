let check condition message = if not condition then failwith message

let with_temp_dir f =
  let path = Filename.temp_file "rewatch-compiler-info-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  Fun.protect ~finally:(fun () -> Build_artifacts.remove_tree path) (fun () ->
    f path)

let write path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let config root =
  write (Filename.concat root "rescript.json")
    {|{"name":"compiler-info-test","sources":["src"]}|};
  Unix.mkdir (Filename.concat root "src") 0o755;
  Config.load_root root

let context root source_map_args =
  let bsc = Filename.concat root "bsc.exe" in
  let runtime = Filename.concat root "runtime" in
  if not (Sys.file_exists bsc) then write bsc "compiler-v1";
  Build_artifacts.ensure_dir runtime;
  Compiler_info.make_context ~bsc_path:bsc ~runtime_path:runtime
    ~source_map_args

let () =
  with_temp_dir (fun root ->
    let config = config root in
    let initial = context root ["-bs-source-map"; "linked"] in
    check (not (Compiler_info.verify_package initial config))
      "a package without an earlier build is not spuriously cleaned";
    Compiler_info.write_package initial config;
    let marker =
      Build_artifacts.path_of_parts root ["lib"; "ocaml"; "marker"]
    in
    write marker "keep";
    check (not (Compiler_info.verify_package initial config))
      "matching compiler information is retained";
    check (Sys.file_exists marker) "matching artifacts remain";
    let info_path = Compiler_info.path root in
    Unix.utimes info_path 1_000_000_000. 1_000_000_000.;
    Compiler_info.write_package initial config;
    check ((Unix.stat info_path).Unix.st_mtime = 1_000_000_000.)
      "matching compiler information is not rewritten";
    let changed = context root ["-bs-source-map"; "false"] in
    check (Compiler_info.verify_package changed config)
      "changed source-map arguments invalidate artifacts";
    check (not (Sys.file_exists marker)) "mismatched artifacts are removed");
  with_temp_dir (fun root ->
    let config = config root in
    let initial = context root [] in
    Compiler_info.write_package initial config;
    write (Filename.concat root "bsc.exe") "compiler-v2";
    let changed = context root [] in
    check (Compiler_info.verify_package changed config)
      "changed compiler contents invalidate artifacts");
  with_temp_dir (fun root ->
    let config = config root in
    let context = context root [] in
    let old_log =
      Build_artifacts.path_of_parts root ["lib"; "ocaml"; ".compiler.log"]
    in
    write old_log "old build";
    check (Compiler_info.verify_package context config)
      "missing metadata invalidates an existing legacy build";
    check (not (Sys.file_exists old_log))
      "legacy build artifacts are removed")
