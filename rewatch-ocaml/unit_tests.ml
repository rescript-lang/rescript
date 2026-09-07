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
  let parallel_results =
    Process.run_parallel ~max_jobs:2
      [
        {Process.program = "/bin/sh"; args = ["-c"; "printf first"]; cwd = Sys.getcwd ()};
        {Process.program = "/bin/sh"; args = ["-c"; "printf second"]; cwd = Sys.getcwd ()};
        {Process.program = "/bin/sh"; args = ["-c"; "printf third"]; cwd = Sys.getcwd ()};
      ]
  in
  check
    (List.map (fun (result : Process.result) -> result.stdout) parallel_results
    = ["first"; "second"; "third"])
    "parallel subprocess results retain input order";
  let invalid_parallel_bound_rejected =
    try
      ignore (Process.run_parallel ~max_jobs:0 []);
      false
    with Process.Error _ -> true
  in
  check invalid_parallel_bound_rejected "parallel subprocess bound is validated";
  let scheduler_root = Filename.temp_file "rewatch-ocaml-scheduler-" "" in
  Sys.remove scheduler_root;
  Unix.mkdir scheduler_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree scheduler_root)
    (fun () ->
      let marker name = Filename.concat scheduler_root name |> Filename.quote in
      let poll path =
        Printf.sprintf
          {|i=0; while [ ! -f %s ] && [ "$i" -lt 200 ]; do i=$((i + 1)); sleep 0.01; done|}
          (marker path)
      in
      let helper_command =
        String.concat "; "
          [
            poll "first-started";
            poll "second-started";
            Printf.sprintf
              "test \"$(find %s -maxdepth 1 -name '.rewatch-ocaml-*' | wc -l)\" -le 4 || touch %s"
              (Filename.quote scheduler_root) (marker "limit-exceeded");
            Printf.sprintf "touch %s" (marker "release");
          ]
      in
      let helper =
        Unix.create_process "/bin/sh"
          [|"/bin/sh"; "-c"; helper_command|]
          Unix.stdin Unix.stdout Unix.stderr
      in
      let job command =
        {Process.program = "/bin/sh"; args = ["-c"; command]; cwd = scheduler_root}
      in
      let first =
        String.concat "; "
          [
            "touch first-started";
            poll "release";
            poll "third-started";
            "test -f third-started || touch refill-stalled";
            "printf first";
          ]
      in
      let second =
        String.concat "; "
          ["touch second-started"; poll "release"; "printf second"]
      in
      let third = "touch third-started; printf third" in
      let results =
        Process.run_parallel ~max_jobs:2 [job first; job second; job third]
      in
      let _, helper_status = Unix.waitpid [] helper in
      check (helper_status = Unix.WEXITED 0) "scheduler test helper exits";
      check
        (not (Sys.file_exists (Filename.concat scheduler_root "limit-exceeded")))
        "parallel subprocesses respect the concurrency bound";
      check
        (not (Sys.file_exists (Filename.concat scheduler_root "refill-stalled")))
        "parallel scheduler refills a completed slot immediately";
      check
        (List.map (fun (result : Process.result) -> result.stdout) results
        = ["first"; "second"; "third"])
        "dynamically scheduled results retain input order";
      let failure =
        Process.run_parallel ~max_jobs:1
          [job "printf partial; printf diagnostic >&2; exit 7"]
        |> List.hd
      in
      check
        (failure.status = Unix.WEXITED 7 && failure.stdout = "partial"
       && failure.stderr = "diagnostic")
        "parallel subprocess failures preserve status and output";
      check
        (Sys.readdir scheduler_root
        |> Array.for_all (fun name ->
             not (String.starts_with ~prefix:".rewatch-ocaml-" name)))
        "parallel subprocess logs are removed after failure");
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
        "sourceMap always mode is enabled for one-shot builds";
      Sys.remove config_path;
      let legacy_path = Filename.concat config_root "bsconfig.json" in
      write_file legacy_path {|{"name":"legacy-config"}|};
      let config = Config.load_root config_root in
      check (config.path = legacy_path) "bsconfig.json is used as a fallback";
      check
        (List.exists
           (fun message -> Build.contains_text message "filename 'bsconfig.json'")
           config.diagnostics)
        "bsconfig.json emits a deprecation diagnostic";
      write_file config_path {|{"name":"current-config"}|};
      let config = Config.load_root config_root in
      check (config.path = config_path)
        "rescript.json takes precedence over bsconfig.json";
      write_file config_path
        {|{
          "name": "gentype-defaults",
          "package-specs": {"module": "commonjs"},
          "gentypeconfig": {}
        }|};
      let config = Config.load config_path in
      check
        (contains_adjacent "-bs-gentype-module" "commonjs"
           config.gentype_args)
        "GenType inherits object package module";
      check
        (not (List.mem "-bs-gentype-suffix" config.gentype_args))
        "GenType omits an unconfigured suffix";
      write_file config_path
        {|{"name":"gentype-suffix","suffix":".mjs","gentypeconfig":{}}|};
      let config = Config.load config_path in
      check
        (contains_adjacent "-bs-gentype-suffix" ".mjs"
           config.gentype_args)
        "GenType includes an explicitly configured suffix";
      write_file config_path
        {|{"name":"unsupported","generators":["legacy"]}|};
      let config = Config.load config_path in
      check
        (List.exists
           (fun message ->
             Build.contains_text message
               "field 'generators'"
             && Build.contains_text message "is not supported")
           config.diagnostics)
        "known unsupported config fields are distinguished from unknown fields");
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
