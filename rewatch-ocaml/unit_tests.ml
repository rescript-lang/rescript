let check condition message = if not condition then failwith message

let rec contains_adjacent left right = function
  | current :: next :: _ when current = left && next = right -> true
  | _ :: rest -> contains_adjacent left right rest
  | [] -> false

let write_file path contents =
  Build.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let () =
  check
    (Process.default_max_jobs >= 1 && Process.default_max_jobs <= 32)
    "parallel subprocess bound follows the available CPUs";
  let node name deps = (name, deps) in
  let nodes = [node "C" ["B"]; node "A" []; node "B" ["A"]] in
  let sorted =
    Graph.topological_sort nodes ~name:fst ~deps:snd |> List.map fst
  in
  check (sorted = ["A"; "B"; "C"]) "topological ordering";
  let cycle_detected =
    try
      ignore
        (Graph.topological_sort
           [node "A" ["B"]; node "B" ["A"]]
           ~name:fst ~deps:snd);
      false
    with Graph.Cycle _ -> true
  in
  check cycle_detected "cycle detection";
  let blocked =
    Build.blocked_dependents
      [
        ("A", ["B"]);
        ("B", ["A"]);
        ("C", ["A"]);
        ("D", ["C"]);
        ("Unrelated", []);
      ]
      ["A"; "B"]
  in
  check
    (List.for_all (fun name -> List.mem name blocked) ["A"; "B"; "C"; "D"])
    "cycle transitive dependents are blocked";
  check (not (List.mem "Unrelated" blocked))
    "cycle-unrelated modules remain schedulable";
  let temporary = Filename.temp_file "rewatch-ocaml-package-path-" "" in
  Sys.remove temporary;
  Unix.mkdir temporary 0o755;
  let package = Filename.concat temporary "package" in
  let node_modules = Filename.concat temporary "node_modules" in
  Unix.mkdir package 0o755;
  Unix.mkdir node_modules 0o755;
  Unix.symlink package (Filename.concat node_modules "dependency");
  Fun.protect
    ~finally:(fun () ->
      Sys.remove (Filename.concat node_modules "dependency");
      Unix.rmdir node_modules;
      Unix.rmdir package;
      Unix.rmdir temporary)
    (fun () ->
      match Build.dependency_path temporary "dependency" with
      | Some resolved ->
        check (resolved = Unix.realpath package)
          "dependency paths are canonicalized"
      | None -> failwith "dependency symlink was not resolved");
  check
    (Config.namespace_from_package_name "@testrepo/deprecated-config"
    = "TestrepoDeprecatedConfig")
    "scoped package namespace normalization";
  check
    (Config.namespace_from_package_name "some.namespace/name_here"
    = "SomenamespaceName_here")
    "namespace punctuation normalization";
  check
    (Build.strip_ansi "plain \027[1;31mred\027[0m text" = "plain red text")
    "compiler log ANSI stripping";
  check
    (match Cli.parse [|"rescript-ocaml"; "-vvvv"; "watch"|] with
    | Cli.Watch _ -> true
    | _ -> false)
    "leading verbosity before watch";
  let watch_no_timing_rejected =
    try
      ignore (Cli.parse [|"rescript-ocaml"; "watch"; "--no-timing"|]);
      false
    with Cli.Error _ -> true
  in
  check watch_no_timing_rejected "watch rejects build-only --no-timing";
  check
    (match Cli.parse [|"rescript-ocaml"; "watch"; "--clear-screen"|] with
    | Cli.Watch options -> options.clear_screen
    | _ -> false)
    "watch parses --clear-screen";
  check
    (match
       Cli.parse
         [|"rescript-ocaml"; "build"; "--features"; " native , web "|]
     with
    | Cli.Build options -> options.features = Some ["native"; "web"]
    | _ -> false)
    "feature names are trimmed";
  check
    (Build.dependent_is_allowed (Some ["app"]) "app")
    "listed dependent is allowed";
  check
    (not (Build.dependent_is_allowed (Some ["other"]) "app"))
    "unlisted dependent is rejected";
  let lock_root = Filename.temp_file "rewatch-ocaml-stale-lock-" "" in
  Sys.remove lock_root;
  Unix.mkdir lock_root 0o755;
  let lock_dir = Filename.concat lock_root "lib" in
  Unix.mkdir lock_dir 0o755;
  let lock = Filename.concat lock_dir "build.lock" in
  let takeover = lock ^ ".takeover" in
  let write_owner path owner =
    let channel = open_out path in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel owner)
  in
  write_owner lock "999999999";
  write_owner takeover "999999999";
  Fun.protect
    ~finally:(fun () ->
      Build.remove_file takeover;
      Build.remove_file lock;
      Unix.rmdir lock_dir;
      Unix.rmdir lock_root)
    (fun () ->
      let release = Build.acquire_build_lock lock_root in
      check
        (Build.read_lock_owner lock = Some (string_of_int (Unix.getpid ())))
        "stale build lock is replaced";
      check (not (Sys.file_exists takeover)) "stale takeover marker is removed";
      release ());
  let config_root = Filename.temp_file "rewatch-ocaml-config-" "" in
  Sys.remove config_root;
  Unix.mkdir config_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree config_root)
    (fun () ->
      let config_path = Filename.concat config_root "rescript.json" in
      write_file config_path
        {|{
          "name": "restricted",
          "allowed-dependents": ["app"]
        }|};
      let config = Config.load config_path in
      check
        (config.allowed_dependents = Some ["app"])
        "allowed-dependents is parsed";
      write_file config_path {|{"name":"default-output","suffix":".mjs"}|};
      let config = Config.load config_path in
      check
        (match config.package_specs with
        | [spec] -> Config.package_spec_suffix config spec = ".js"
        | _ -> false)
        "package-specs default output suffix is .js";
      write_file config_path
        {|{"name":"legacy-output","package-specs":{"module":"cjs"}}|};
      let config = Config.load config_path in
      check
        (List.exists
           (fun message -> Build.contains_text message "module 'cjs'")
           config.diagnostics)
        "legacy package module alias is diagnosed";
      write_file config_path
        {|{"name":"missing-module","package-specs":{"in-source":true}}|};
      let missing_module_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Build.contains_text message "missing field \"module\""
      in
      check missing_module_rejected "package output module is required";
      write_file config_path
        {|{
          "name": "duplicate-output",
          "package-specs": [
            {"module": "esmodule", "suffix": ".js"},
            {"module": "commonjs", "suffix": ".js"}
          ]
        }|};
      let duplicate_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Build.contains_text message "Duplicate package-spec suffix"
      in
      check duplicate_rejected "duplicate package output is rejected";
      write_file config_path {|{"name":"source-map","sourceMap":true}|};
      let boolean_source_map_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Build.contains_text message "sourceMap true is unsupported"
      in
      check boolean_source_map_rejected "sourceMap true is rejected";
      write_file config_path
        {|{"name":"source-map","sourceMap":{"mode":"linked"}}|};
      let missing_source_map_enabled_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Build.contains_text message "missing field \"enabled\""
      in
      check missing_source_map_enabled_rejected
        "sourceMap enabled is required";
      write_file config_path
        {|{
          "name": "source-map",
          "sourceMap": {"enabled": "dev", "mode": "linked"}
        }|};
      let config = Config.load config_path in
      check config.source_map_dev "sourceMap dev mode is parsed";
      check
        (contains_adjacent "-bs-source-map" "false"
           (Build.compiler_flags ~source_maps:true ~watch:false
              ~gentype:false config))
        "sourceMap dev mode is disabled for one-shot builds";
      check
        (contains_adjacent "-bs-source-map" "linked"
           (Build.compiler_flags ~source_maps:true ~watch:true
              ~gentype:false config))
        "sourceMap dev mode is enabled for watch builds";
      write_file config_path
        {|{
          "name": "source-map",
          "sourceMap": {"enabled": "always", "mode": "inline"}
        }|};
      let config = Config.load config_path in
      check (not config.source_map_dev) "sourceMap always mode is parsed";
      check
        (contains_adjacent "-bs-source-map" "inline"
           (Build.compiler_flags ~source_maps:true ~watch:false
              ~gentype:false config))
        "sourceMap always mode is enabled for one-shot builds");
  let dependency_root =
    Filename.temp_file "rewatch-ocaml-allowed-dependents-" ""
  in
  Sys.remove dependency_root;
  Unix.mkdir dependency_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree dependency_root)
    (fun () ->
      write_file (Filename.concat dependency_root "rescript.json")
        {|{"name":"app","dependencies":["restricted"]}|};
      write_file
        (Filename.concat dependency_root
           "node_modules/restricted/rescript.json")
        {|{"name":"restricted","allowed-dependents":["other"]}|};
      Unix.putenv "RESCRIPT_BSC_EXE" "/bin/true";
      let rejected =
        try
          Build.run ~seen:[] ~folder:dependency_root ~prod:false
            ~features:None ~warn_error:None ~watch:false ~after_build:None
            ~filter:None;
          false
        with Build.Error message ->
          if Build.contains_text message "app dependencies: restricted" then
            true
          else failwith ("unexpected allowed-dependents error: " ^ message)
      in
      check rejected "unallowed package dependency is rejected")
