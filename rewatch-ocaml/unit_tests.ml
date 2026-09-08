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

let touch_file path = write_file path ""

let wait_for_file path =
  let rec loop attempts =
    if Sys.file_exists path then true
    else if attempts = 0 then false
    else (
      ignore (Unix.select [] [] [] 0.01);
      loop (attempts - 1))
  in
  loop 200

let () =
  let argument index = Sys.argv.(index) in
  if Array.length Sys.argv >= 2 then
    match argument 1 with
    | "--process-result" ->
      print_string (argument 2);
      prerr_string (argument 3);
      exit (int_of_string (argument 4))
    | "--scheduler-helper" ->
      let root = argument 2 in
      ignore (wait_for_file (Filename.concat root "first-started"));
      ignore (wait_for_file (Filename.concat root "second-started"));
      let log_count =
        Sys.readdir root
        |> Array.fold_left
             (fun count name ->
               if String.starts_with ~prefix:".rewatch-ocaml-" name then
                 count + 1
               else count)
             0
      in
      if log_count > 4 then touch_file (Filename.concat root "limit-exceeded");
      touch_file (Filename.concat root "release");
      exit 0
    | "--scheduler-job" ->
      let root = argument 2 in
      let name = argument 3 in
      touch_file (Filename.concat root (name ^ "-started"));
      if name <> "third" then
        ignore (wait_for_file (Filename.concat root "release"));
      if name = "first" then (
        ignore (wait_for_file (Filename.concat root "third-started"));
        if not (Sys.file_exists (Filename.concat root "third-started")) then
          touch_file (Filename.concat root "refill-stalled"));
      print_string name;
      exit 0
    | _ -> ()

let () =
  check
    (Build.generated_output_owner "Foo.bs.js" = Some "Foo")
    "compound .bs.js outputs retain their module owner";
  check
    (Build.generated_output_owner "Foo.res.js" = Some "Foo")
    "compound .res.js outputs retain their module owner";
  check
    (Build.generated_output_owner "Foo.res.js.map" = Some "Foo")
    "compound source maps retain their module owner";
  let test_executable = Unix.realpath Sys.executable_name in
  let process_job args =
    {Process.program = test_executable; args; cwd = Sys.getcwd ()}
  in
  check
    (Process.default_max_jobs >= 1 && Process.default_max_jobs <= 32)
    "parallel subprocess bound follows the available CPUs";
  let parallel_results =
    Process.run_parallel ~max_jobs:2
      [
        process_job ["--process-result"; "first"; ""; "0"];
        process_job ["--process-result"; "second"; ""; "0"];
        process_job ["--process-result"; "third"; ""; "0"];
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
  let graph_completion_order = ref [] in
  let graph_completed = Hashtbl.create 3 in
  let graph_work key dependencies =
    Process.{key; dependencies; value = key}
  in
  Process.run_dependency_graph ~max_jobs:1
    [graph_work "c" []; graph_work "b" ["a"]; graph_work "a" []]
    ~next:(fun key result ->
      match result with
      | None ->
        if key = "b" then
          check (Hashtbl.mem graph_completed "a")
            "dependency work starts only after its prerequisite completes";
        Some
          (process_job ["--process-result"; key; ""; "0"])
      | Some result ->
        check
          (Process.succeeded result && result.stdout = key)
          "dependency scheduler collects subprocess output";
        Hashtbl.add graph_completed key ();
        graph_completion_order := key :: !graph_completion_order;
        None);
  check
    (List.rev !graph_completion_order = ["a"; "b"; "c"])
    "dependency scheduler prioritizes the longest ready path";
  let graph_cycle_rejected =
    try
      Process.run_dependency_graph
        [graph_work "a" ["b"]; graph_work "b" ["a"]]
        ~next:(fun _ _ -> None);
      false
    with Process.Error _ -> true
  in
  check graph_cycle_rejected "subprocess dependency cycles are rejected";
  let drained_failures = ref 0 in
  let deterministic_failure =
    try
      Process.run_dependency_graph ~max_jobs:2
        [graph_work "z" []; graph_work "a" []]
        ~next:(fun key result ->
          match result with
          | None -> Some (process_job ["--process-result"; ""; ""; "1"])
          | Some _ ->
            incr drained_failures;
            raise (Failure key));
      None
    with Failure key -> Some key
  in
  check
    (!drained_failures = 2 && deterministic_failure = Some "a")
    "dependency scheduler drains active work and reports errors deterministically";
  let path_root = Filename.temp_file "rewatch-ocaml-path-" "" in
  Sys.remove path_root;
  Unix.mkdir path_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree path_root)
    (fun () ->
      let first = Filename.concat path_root "first" in
      let second = Filename.concat path_root "second" in
      Unix.mkdir first 0o755;
      Unix.mkdir second 0o755;
      let command = if Sys.win32 then "worker.exe" else "worker" in
      Unix.mkdir (Filename.concat first command) 0o755;
      let executable = Filename.concat second command in
      Build.copy_file test_executable executable;
      Unix.chmod executable 0o755;
      let previous_path = Sys.getenv_opt "PATH" in
      let separator = if Sys.win32 then ";" else ":" in
      Unix.putenv "PATH" (first ^ separator ^ second);
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv "PATH" (Option.value previous_path ~default:""))
        (fun () ->
          let requested = if Sys.win32 then "worker" else command in
          check
            (Process.resolve_program ~cwd:path_root requested = executable)
            "PATH lookup skips directories and applies platform executable suffixes";
          if Sys.win32 then (
            let cwd_executable = Filename.concat path_root "current.exe" in
            Build.copy_file test_executable cwd_executable;
            check
              (Process.resolve_program ~cwd:path_root "current" = cwd_executable)
              "Windows executable lookup searches cwd with PATHEXT")));
  check
    (Build.windows_tasklist_has_process ~pid:123
       {|"rescript.exe","123","Console","1","10,000 K"|})
    "Windows tasklist output recognizes a matching ReScript process";
  check
    (not
       (Build.windows_tasklist_has_process ~pid:124
          {|"rescript.exe","123","Console","1","10,000 K"|}))
    "Windows tasklist output rejects a different process ID";
  check
    (Build.windows_tasklist_probe ~pid:123 "tasklist failed" = None)
    "malformed Windows tasklist output is inconclusive";
  check
    (Build.windows_tasklist_probe ~pid:123 {|"tasklist failed"|} = None)
    "unexpected Windows tasklist CSV schema is inconclusive";
  check
    (Build.windows_tasklist_probe ~pid:123 {|"rescript.exe","12|} = None)
    "truncated Windows tasklist CSV is inconclusive";
  let scheduler_root = Filename.temp_file "rewatch-ocaml-scheduler-" "" in
  Sys.remove scheduler_root;
  Unix.mkdir scheduler_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree scheduler_root)
    (fun () ->
      let helper =
        Spawn.spawn ~prog:test_executable
          ~argv:[test_executable; "--scheduler-helper"; scheduler_root]
          ()
      in
      let job name =
        {
          Process.program = test_executable;
          args = ["--scheduler-job"; scheduler_root; name];
          cwd = scheduler_root;
        }
      in
      let results =
        Process.run_parallel ~temp_dir:scheduler_root ~max_jobs:2
          [job "first"; job "second"; job "third"]
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
        Process.run_parallel ~temp_dir:scheduler_root ~max_jobs:1
          [
            process_job
              ["--process-result"; "partial"; "diagnostic"; "7"];
          ]
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
  (if not Sys.win32 then
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
         | None -> failwith "dependency symlink was not resolved"));
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
        {|{"name":"file-casing","namespace":"FileCasing"}|};
      let file_casing_config = Config.load config_path in
      check
        (Source.compiler_asset_basename file_casing_config "src/produce.res"
        = "produce-FileCasing")
        "compiler artifact basename preserves source filename case";
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
        (List.fold_left Filename.concat dependency_root
           ["node_modules"; "restricted"; "rescript.json"])
        {|{"name":"restricted","allowed-dependents":["other"]}|};
      Unix.putenv "RESCRIPT_BSC_EXE" test_executable;
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
