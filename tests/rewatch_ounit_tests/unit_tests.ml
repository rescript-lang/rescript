open OUnit2

let check condition message = assert_bool message condition

module Checked_windows_platform : module type of Platform = Platform_windows

let rec contains_adjacent left right = function
  | current :: next :: _ when current = left && next = right -> true
  | _ :: rest -> contains_adjacent left right rest
  | [] -> false

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

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
    | "--large-process-result" ->
      let stdout_chunk = String.make 65536 'o' in
      let stderr_chunk = String.make 65536 'e' in
      for _ = 1 to 16 do
        print_string stdout_chunk;
        flush stdout;
        prerr_string stderr_chunk;
        flush stderr
      done;
      exit 0
    | "--wait-forever" ->
      while true do
        ignore (Unix.select [] [] [] 1.)
      done
    | "-format" -> (
      match Sys.getenv_opt "REWATCH_FORMAT_TEST_ROOT" with
      | None -> ()
      | Some root ->
        let source = argument 2 in
        touch_file
          (Filename.concat root (Filename.basename source ^ ".started"));
        let first_started = Filename.concat root "First.res.started" in
        let second_started = Filename.concat root "Second.res.started" in
        if not (wait_for_file first_started && wait_for_file second_started)
        then exit 2;
        print_string (read_file source);
        exit 0)
    | _ -> ()

let tests =
  "unit_tests" >:: fun _context ->
  check
    (Build_artifacts.generated_output_owner "Foo.bs.js" = Some "Foo")
    "compound .bs.js outputs retain their module owner";
  check
    (Build_artifacts.generated_output_owner "Foo.res.js" = Some "Foo")
    "compound .res.js outputs retain their module owner";
  check
    (Build_artifacts.generated_output_owner "Foo.res.js.map" = Some "Foo")
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
  let large_result =
    Process.run ~cwd:(Sys.getcwd ()) test_executable ["--large-process-result"]
  in
  check
    (Process.succeeded large_result
    && String.length large_result.stdout = 1024 * 1024
    && String.length large_result.stderr = 1024 * 1024)
    "stdout and stderr pipes are drained concurrently without truncation";
  let invalid_parallel_bound_rejected =
    try
      ignore (Process.run_parallel ~max_jobs:0 []);
      false
    with Process.Error _ -> true
  in
  check invalid_parallel_bound_rejected "parallel subprocess bound is validated";
  let graph_work key dependencies = Process.{key; dependencies; value = key} in
  let cancellation_polls = ref 0 in
  let dependency_graph_cancelled =
    let exception Cancel in
    try
      Process.run_dependency_graph
        [graph_work "cancel" []]
        ~poll:(fun () ->
          incr cancellation_polls;
          if !cancellation_polls = 2 then raise Cancel)
        ~next:(fun _ result ->
          match result with
          | None -> Some (process_job ["--wait-forever"])
          | Some _ -> None);
      false
    with Cancel -> true
  in
  check dependency_graph_cancelled
    "dependency scheduler cancellation terminates active subprocesses";
  let graph_completion_order = ref [] in
  let graph_completed = Hashtbl.create 3 in
  Process.run_dependency_graph ~max_jobs:1
    [graph_work "c" []; graph_work "b" ["a"]; graph_work "a" []]
    ~next:(fun key result ->
      match result with
      | None ->
        if key = "b" then
          check
            (Hashtbl.mem graph_completed "a")
            "dependency work starts only after its prerequisite completes";
        Some (process_job ["--process-result"; key; ""; "0"])
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
    "dependency scheduler drains active work and reports errors \
     deterministically";
  let path_root = Filename.temp_file "rewatch-ocaml-path-" "" in
  Sys.remove path_root;
  Unix.mkdir path_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree path_root)
    (fun () ->
      let first = Filename.concat path_root "first" in
      let second = Filename.concat path_root "second" in
      Unix.mkdir first 0o755;
      Unix.mkdir second 0o755;
      let command = if Sys.win32 then "worker.exe" else "worker" in
      Unix.mkdir (Filename.concat first command) 0o755;
      let executable = Filename.concat second command in
      Build_artifacts.copy_file test_executable executable;
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
            (Platform.resolve_program ~cwd:path_root requested = executable)
            "PATH lookup skips directories and applies platform executable \
             suffixes";
          if Sys.win32 then (
            let cwd_executable = Filename.concat path_root "current.exe" in
            Build_artifacts.copy_file test_executable cwd_executable;
            check
              (Platform.resolve_program ~cwd:path_root "current"
              = cwd_executable)
              "Windows executable lookup searches cwd with PATHEXT")));
  check
    (Platform_windows.tasklist_has_process ~pid:123
       {|"rescript.exe","123","Console","1","10,000 K"|})
    "Windows tasklist output recognizes a matching ReScript process";
  check
    (not
       (Platform_windows.tasklist_has_process ~pid:124
          {|"rescript.exe","123","Console","1","10,000 K"|}))
    "Windows tasklist output rejects a different process ID";
  check
    (Platform_windows.tasklist_probe ~pid:123 "tasklist failed" = None)
    "malformed Windows tasklist output is inconclusive";
  check
    (Platform_windows.tasklist_probe ~pid:123 {|"tasklist failed"|} = None)
    "unexpected Windows tasklist CSV schema is inconclusive";
  check
    (Platform_windows.tasklist_probe ~pid:123 {|"rescript.exe","12|} = None)
    "truncated Windows tasklist CSV is inconclusive";
  check
    (Platform_windows.process_is_active ~run:(fun _ _ -> None) "123")
    "a failed Windows tasklist probe conservatively preserves the lock";
  let scheduler_root = Filename.temp_file "rewatch-ocaml-scheduler-" "" in
  Sys.remove scheduler_root;
  Unix.mkdir scheduler_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree scheduler_root)
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
        Process.run_parallel ~max_jobs:2
          [job "first"; job "second"; job "third"]
      in
      let _, helper_status = Unix.waitpid [] helper in
      check (helper_status = Unix.WEXITED 0) "scheduler test helper exits";
      check
        (not
           (Sys.file_exists (Filename.concat scheduler_root "refill-stalled")))
        "parallel scheduler refills a completed slot immediately";
      check
        (List.map (fun (result : Process.result) -> result.stdout) results
        = ["first"; "second"; "third"])
        "dynamically scheduled results retain input order";
      let failure =
        Process.run_parallel ~max_jobs:1
          [process_job ["--process-result"; "partial"; "diagnostic"; "7"]]
        |> List.hd
      in
      check
        (failure.status = Unix.WEXITED 7
        && failure.stdout = "partial"
        && failure.stderr = "diagnostic")
        "parallel subprocess failures preserve status and output";
      check
        (Sys.readdir scheduler_root
        |> Array.for_all (fun name ->
            not (String.starts_with ~prefix:".rewatch-ocaml-" name)))
        "pipe capture creates no temporary scheduler logs");
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
  let shortest_cycle =
    try
      ignore
        (Graph.topological_sort
           [
             node "LongA" ["LongB"];
             node "LongB" ["LongC"];
             node "LongC" ["LongA"];
             node "ShortA" ["ShortB"];
             node "ShortB" ["ShortA"];
           ]
           ~name:fst ~deps:snd);
      []
    with Graph.Cycle cycle -> cycle
  in
  check
    (shortest_cycle = ["ShortA"; "ShortB"; "ShortA"])
    "cycle diagnostics select the shortest cycle deterministically";
  let blocked =
    Build.blocked_dependents
      [
        ("A", ["B"]); ("B", ["A"]); ("C", ["A"]); ("D", ["C"]); ("Unrelated", []);
      ]
      ["A"; "B"]
  in
  check
    (List.for_all (fun name -> List.mem name blocked) ["A"; "B"; "C"; "D"])
    "cycle transitive dependents are blocked";
  check
    (not (List.mem "Unrelated" blocked))
    "cycle-unrelated modules remain schedulable";
  check
    (Project_context.is_local_dependency_canonical ~workspace:"/workspace"
       "/workspace/packages/dependency")
    "canonical workspace dependencies are local";
  check
    (not
       (Project_context.is_local_dependency_canonical ~workspace:"/workspace"
          "/workspace/node_modules/dependency"))
    "node_modules dependencies are external";
  check
    (not
       (Project_context.is_local_dependency_canonical ~workspace:"/workspace"
          "/workspace-other/dependency"))
    "path-prefix siblings are outside the workspace";
  if not Sys.win32 then (
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
        match Project_context.dependency_path temporary "dependency" with
        | Some resolved ->
          check
            (resolved = Unix.realpath package)
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
    (Compiler_log.strip_ansi "plain \027[1;31mred\027[0m text"
    = "plain red text")
    "compiler log ANSI stripping";
  let truncated_utf8 =
    "Warning " ^ String.make 1 (Char.chr 0xe2) ^ String.make 1 (Char.chr 0x80)
  in
  let decoded = Process.decode_utf8_lossy truncated_utf8 in
  check
    (String.starts_with ~prefix:"Warning " decoded
    && String.is_valid_utf_8 decoded)
    "compiler output is decoded as lossy UTF-8";
  check
    (Compiler_process.retain_critical_external_warnings
       "\n  Warning number 26\n  foo.res:1:1\n\n  unused variable x.\n"
    = "")
    "ordinary external warnings are suppressed";
  let critical_marker = "`(. ...)` uncurried syntax" in
  let mixed_warnings line_ending =
    String.concat ""
      [
        line_ending;
        "  Warning number 26";
        line_ending;
        "  unused variable x.";
        line_ending;
        line_ending;
        line_ending;
        "  Warning number 3";
        line_ending;
        "  deprecated: The ";
        critical_marker;
        " is deprecated.";
        line_ending;
      ]
  in
  List.iter
    (fun line_ending ->
      let kept =
        Compiler_process.retain_critical_external_warnings
          (mixed_warnings line_ending)
      in
      check
        (Test_support.contains_text kept critical_marker
        && not (Test_support.contains_text kept "unused variable"))
        "critical external warnings are retained without unrelated warnings")
    ["\n"; "\r\n"];
  check
    (Build.dependent_is_allowed (Some ["app"]) "app")
    "listed dependent is allowed";
  check
    (not (Build.dependent_is_allowed (Some ["other"]) "app"))
    "unlisted dependent is rejected";
  check
    (not (Build.source_discovery_prod ~prod:false ~is_local:true))
    "development sources are enabled for a local development build";
  check
    (Build.source_discovery_prod ~prod:true ~is_local:true)
    "production builds exclude local development sources";
  check
    (Build.source_discovery_prod ~prod:false ~is_local:false)
    "installed dependencies always exclude development sources";
  check (Build_lock.valid_owner "0") "zero is a valid serialized u32 owner";
  check
    (Build_lock.valid_owner "4294967295")
    "the maximum u32 is a valid serialized lock owner";
  check (not (Build_lock.valid_owner "")) "an empty lock owner is malformed";
  check (not (Build_lock.valid_owner "-1")) "a negative lock owner is malformed";
  check
    (not (Build_lock.valid_owner "4294967296"))
    "a lock owner outside the Rust u32 range is malformed";
  check
    (not (Build_lock.valid_owner "123\n"))
    "trailing data in a lock owner is malformed";
  let lock_root = Filename.temp_file "rewatch-ocaml-stale-lock-" "" in
  Sys.remove lock_root;
  Unix.mkdir lock_root 0o755;
  let lock_dir = Filename.concat lock_root "lib" in
  Unix.mkdir lock_dir 0o755;
  let lock = Filename.concat lock_dir "build.lock" in
  let takeover = lock ^ ".takeover" in
  let write_owner path owner =
    let channel = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel owner)
  in
  write_owner lock "999999999";
  write_owner takeover "999999999";
  Fun.protect
    ~finally:(fun () ->
      Build_artifacts.remove_file takeover;
      Build_artifacts.remove_file lock;
      Unix.rmdir lock_dir;
      Unix.rmdir lock_root)
    (fun () ->
      let release = Build_lock.acquire_build lock_root in
      check
        (Build_lock.read_owner lock = Some (string_of_int (Unix.getpid ())))
        "stale build lock is replaced";
      check (not (Sys.file_exists takeover)) "stale takeover marker is removed";
      release ());
  let config_root = Filename.temp_file "rewatch-ocaml-config-" "" in
  Sys.remove config_root;
  Unix.mkdir config_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree config_root)
    (fun () ->
      let config_path = Filename.concat config_root "rescript.json" in
      write_file config_path {|{"name":"file-casing","namespace":"FileCasing"}|};
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
      write_file config_path
        {|{
          "name": "source-type",
          "sources": {"dir": "src", "type": "lib"}
        }|};
      let config = Config.load config_path in
      check
        (match config.sources with
        | [source] -> not source.is_dev
        | _ -> false)
        "non-dev source type strings are accepted as ordinary sources";
      write_file config_path
        {|{
          "name": "source-type-inheritance",
          "sources": {
            "dir": "src",
            "subdirs": [{"dir": "test", "type": "dev"}]
          }
        }|};
      let config = Config.load config_path in
      check
        (match config.sources with
        | [_parent; child] -> not child.is_dev
        | _ -> false)
        "an ordinary parent source overrides a nested dev type";
      write_file config_path
        {|{
          "name": "source-type-inheritance",
          "sources": {
            "dir": "src",
            "type": "dev",
            "subdirs": [{"dir": "lib", "type": "lib"}]
          }
        }|};
      let config = Config.load config_path in
      check
        (match config.sources with
        | [_parent; child] -> child.is_dev
        | _ -> false)
        "a dev parent source overrides a nested non-dev type";
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
           (fun message -> Test_support.contains_text message "module 'cjs'")
           config.diagnostics)
        "legacy package module alias is diagnosed";
      write_file config_path
        {|{"name":"missing-module","package-specs":{"in-source":true}}|};
      let missing_module_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Test_support.contains_text message "missing field \"module\""
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
          Test_support.contains_text message "Duplicate package-spec suffix"
      in
      check duplicate_rejected "duplicate package output is rejected";
      write_file config_path {|{"name":"source-map","sourceMap":true}|};
      let boolean_source_map_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Test_support.contains_text message "sourceMap true is unsupported"
      in
      check boolean_source_map_rejected "sourceMap true is rejected";
      write_file config_path
        {|{"name":"source-map","sourceMap":{"mode":"linked"}}|};
      let missing_source_map_enabled_rejected =
        try
          ignore (Config.load config_path);
          false
        with Config.Error message ->
          Test_support.contains_text message "missing field \"enabled\""
      in
      check missing_source_map_enabled_rejected "sourceMap enabled is required";
      write_file config_path
        {|{
          "name": "source-map",
          "sourceMap": {"enabled": "dev", "mode": "linked"}
        }|};
      let config = Config.load config_path in
      check config.source_map_dev "sourceMap dev mode is parsed";
      check
        (contains_adjacent "-bs-source-map" "false"
           (Compiler_args.compiler_flags ~source_maps:true ~watch:false
              ~gentype:false config))
        "sourceMap dev mode is disabled for one-shot builds";
      check
        (contains_adjacent "-bs-source-map" "linked"
           (Compiler_args.compiler_flags ~source_maps:true ~watch:true
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
           (Compiler_args.compiler_flags ~source_maps:true ~watch:false
              ~gentype:false config))
        "sourceMap always mode is enabled for one-shot builds";
      Sys.remove config_path;
      let legacy_path = Filename.concat config_root "bsconfig.json" in
      write_file legacy_path {|{"name":"legacy-config"}|};
      let config = Config.load_root config_root in
      check (config.path = legacy_path) "bsconfig.json is used as a fallback";
      check
        (List.exists
           (fun message ->
             Test_support.contains_text message "filename 'bsconfig.json'")
           config.diagnostics)
        "bsconfig.json emits a deprecation diagnostic";
      write_file config_path {|{"name":"current-config"}|};
      let config = Config.load_root config_root in
      check
        (config.path = config_path)
        "rescript.json takes precedence over bsconfig.json";
      write_file config_path
        {|{
          "name": "gentype-defaults",
          "package-specs": {"module": "commonjs"},
          "gentypeconfig": {}
        }|};
      let config = Config.load config_path in
      check
        (contains_adjacent "-bs-gentype-module" "commonjs" config.gentype_args)
        "GenType inherits object package module";
      check
        (not (List.mem "-bs-gentype-suffix" config.gentype_args))
        "GenType omits an unconfigured suffix";
      write_file config_path
        {|{"name":"gentype-suffix","suffix":".mjs","gentypeconfig":{}}|};
      let config = Config.load config_path in
      check
        (contains_adjacent "-bs-gentype-suffix" ".mjs" config.gentype_args)
        "GenType includes an explicitly configured suffix";
      write_file config_path
        {|{
          "name": "gentype-shims",
          "gentypeconfig": {
            "shims": [" From = First ", "A=B", "From=Last"]
          }
        }|};
      let config = Config.load config_path in
      check
        (contains_adjacent "-bs-gentype-shim" "From=Last" config.gentype_args)
        "legacy GenType shims are trimmed and later duplicates win";
      check
        (List.length
           (List.filter (( = ) "-bs-gentype-shim") config.gentype_args)
        = 2)
        "legacy GenType shims use map semantics";
      write_file config_path {|{"name":"unsupported","generators":["legacy"]}|};
      let config = Config.load config_path in
      check
        (List.exists
           (fun message ->
             Test_support.contains_text message "field 'generators'"
             && Test_support.contains_text message "is not supported")
           config.diagnostics)
        "known unsupported config fields are distinguished from unknown fields");
  let dependency_root =
    Filename.temp_file "rewatch-ocaml-allowed-dependents-" ""
  in
  Sys.remove dependency_root;
  Unix.mkdir dependency_root 0o755;
  Fun.protect
    ~finally:(fun () -> Build_artifacts.remove_tree dependency_root)
    (fun () ->
      write_file
        (Filename.concat dependency_root "rescript.json")
        {|{"name":"app","dependencies":["restricted"]}|};
      write_file
        (List.fold_left Filename.concat dependency_root
           ["node_modules"; "restricted"; "rescript.json"])
        {|{"name":"restricted","allowed-dependents":["other"]}|};
      let previous_bsc = Sys.getenv_opt "RESCRIPT_BSC_EXE" in
      Unix.putenv "RESCRIPT_BSC_EXE" test_executable;
      Fun.protect
        ~finally:(fun () ->
          match previous_bsc with
          | Some value -> Unix.putenv "RESCRIPT_BSC_EXE" value
          | None -> Unix.unsetenv "RESCRIPT_BSC_EXE")
        (fun () ->
          let rejected =
            try
              Build.run ~seen:[] ~verbosity:0 ~folder:dependency_root
                ~prod:false ~features:None ~warn_error:None ~watch:false
                ~after_build:None ~filter:None ~no_timing:false;
              false
            with Build.Error message ->
              if
                Test_support.contains_text message
                  "app dependencies: restricted"
              then true
              else failwith ("unexpected allowed-dependents error: " ^ message)
          in
          check rejected "unallowed package dependency is rejected";
          write_file
            (Filename.concat dependency_root "rescript.json")
            {|{"name":"app","dev-dependencies":["restricted"]}|};
          let rejected =
            try
              Build.run ~seen:[] ~verbosity:0 ~folder:dependency_root
                ~prod:false ~features:None ~warn_error:None ~watch:false
                ~after_build:None ~filter:None ~no_timing:false;
              false
            with Build.Error message ->
              Test_support.contains_text message
                "app dev-dependencies: restricted"
          in
          check rejected "unallowed development dependency is rejected"))
