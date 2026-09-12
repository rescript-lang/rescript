open OUnit2

let check condition message = assert_bool message condition

module Checked_windows_platform : module type of Platform = Platform_windows

type lazy_job = First | Second

let rec contains_adjacent left right = function
  | current :: next :: _ when current = left && next = right -> true
  | _ :: rest -> contains_adjacent left right rest
  | [] -> false

let write_file = Test_support.write_file

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
    | "--sleep" ->
      Thread.delay (float_of_string (argument 2));
      exit 0
    | "--wait-for-release" ->
      let root = argument 2 in
      touch_file (Filename.concat root "child-started");
      ignore (wait_for_file (Filename.concat root "release"));
      exit 0
    | "--exit-with-descendant" ->
      let executable = Unix.realpath Sys.executable_name in
      ignore
        (Spawn.spawn ~prog:executable ~argv:[executable; "--sleep"; "2"] ());
      exit 0
    | "-format" -> (
      match
        ( Sys.getenv_opt "REWATCH_FORMAT_TEST_ROOT",
          Sys.getenv_opt "REWATCH_FORMAT_INVENTORY_TEST_ROOT" )
      with
      | _, Some root ->
        let source = argument 2 in
        let marker = Digest.string source |> Digest.to_hex in
        touch_file (Filename.concat root marker);
        print_string (read_file source);
        exit 0
      | None, None -> ()
      | Some root, None ->
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

let test_executable () = Unix.realpath Sys.executable_name

let process_job args =
  {Process.program = test_executable (); args; cwd = Sys.getcwd ()}

let feature_request_tests _context =
  let feature_requests = Feature_requests.create () in
  Feature_requests.add feature_requests "package" (Some ["browser"]);
  Feature_requests.add feature_requests "package" (Some ["native"; "browser"]);
  check
    (Feature_requests.find feature_requests "package"
    = Some (Feature_requests.Selected ["browser"; "native"]))
    "feature requests merge and deduplicate named selections";
  Feature_requests.add feature_requests "package" None;
  Feature_requests.add feature_requests "package" (Some ["ignored"]);
  check
    (Feature_requests.find feature_requests "package"
    = Some Feature_requests.All)
    "an unrestricted feature request dominates named selections"

let string_util_tests _context =
  [
    ("", "", true);
    ("value", "", true);
    ("", "value", false);
    ("abc", "abc", true);
    ("prefix-value", "prefix", true);
    ("value-suffix", "suffix", true);
    ("abababca", "ababca", true);
    ("aaaaaaaaab", "aaaab", true);
    ("aaaaaaaaaa", "aaaab", false);
    ("short", "longer", false);
  ]
  |> List.iter (fun (value, substring, expected) ->
      check
        (String_util.contains value substring = expected)
        (Printf.sprintf "substring search for %S in %S" substring value))

let process_tests _context =
  let test_executable = test_executable () in
  check
    (Process.default_max_jobs >= 1 && Process.default_max_jobs <= 32)
    "parallel subprocess bound follows the available CPUs";
  let completed_indices = ref [] in
  let parallel_results =
    Process.run_parallel ~max_jobs:2
      ~on_complete:(fun index ->
        completed_indices := index :: !completed_indices)
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
  check
    (List.sort compare !completed_indices = [0; 1; 2])
    "parallel subprocess completion reports every input index once";
  let lazy_root = Filename.temp_file "rewatch-lazy-jobs-" "" in
  Sys.remove lazy_root;
  Unix.mkdir lazy_root 0o755;
  let lazy_results =
    Fun.protect
      ~finally:(fun () -> File_util.remove_tree lazy_root)
      (fun () ->
        Process.run_parallel_map ~max_jobs:2 [First; Second] ~job:(function
          | First -> process_job ["--wait-for-release"; lazy_root]
          | Second ->
            check
              (wait_for_file (Filename.concat lazy_root "child-started"))
              "later job preparation overlaps an already-running child";
            touch_file (Filename.concat lazy_root "release");
            process_job ["--process-result"; "second"; ""; "0"]))
  in
  check
    (List.map (fun (result : Process.result) -> result.stdout) lazy_results
    = [""; "second"])
    "demand-built subprocess results retain input order";
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
          | None -> Some (Process.task (process_job ["--wait-forever"]))
          | Some _ -> None);
      false
    with Cancel -> true
  in
  check dependency_graph_cancelled
    "dependency scheduler cancellation terminates active subprocesses";
  let fatal_finalizer_started = Unix.gettimeofday () in
  let fatal_finalizer_cancels_other_workers =
    try
      Process.run_dependency_graph ~max_jobs:2
        [graph_work "failure" []; graph_work "waiting" []]
        ~next:(fun key result ->
          match result with
          | None ->
            let arguments =
              if key = "failure" then ["--process-result"; ""; ""; "0"]
              else ["--wait-forever"]
            in
            Some
              (Process.task
                 ~on_result:(fun result ->
                   if key = "failure" then raise (Process.Interrupted 130)
                   else result)
                 (process_job arguments))
          | Some _ -> None);
      false
    with Process.Interrupted 130 -> true
  in
  check
    (fatal_finalizer_cancels_other_workers
    && Unix.gettimeofday () -. fatal_finalizer_started < 2.)
    "a fatal worker finalizer cancels other process trees without deadlock";
  let process_polls = ref 0 in
  let process_cancelled =
    let exception Cancel in
    try
      ignore
        (Process.run ~cwd:(Sys.getcwd ()) test_executable ["--wait-forever"]
           ~poll:(fun () ->
             incr process_polls;
             if !process_polls = 2 then raise Cancel));
      false
    with Cancel -> true
  in
  check process_cancelled
    "single subprocess cancellation terminates the active process";
  (if not Sys.win32 then
     let descendant_pipe_polls = ref 0 in
     let started = Unix.gettimeofday () in
     let descendant_pipe_cancelled =
       let exception Cancel in
       try
         Process.run_dependency_graph
           [graph_work "descendant-pipe" []]
           ~poll:(fun () ->
             incr descendant_pipe_polls;
             if !descendant_pipe_polls = 2 then raise Cancel)
           ~next:(fun _ result ->
             match result with
             | None ->
               Some (Process.task (process_job ["--exit-with-descendant"]))
             | Some _ -> None);
         false
       with Cancel -> true
     in
     check
       (descendant_pipe_cancelled && Unix.gettimeofday () -. started < 1.)
       "an exited parent with descendant-held pipes remains cancellable");
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
        Some (Process.task (process_job ["--process-result"; key; ""; "0"]))
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
  let finalizers_entered = Atomic.make 0 in
  let parallel_finalizers_completed =
    let exception Finalizers_serialized in
    try
      Process.run_dependency_graph ~max_jobs:2
        [graph_work "first" []; graph_work "second" []]
        ~next:(fun _ result ->
          match result with
          | Some _ -> None
          | None ->
            Some
              (Process.task
                 ~on_result:(fun result ->
                   ignore (Atomic.fetch_and_add finalizers_entered 1);
                   let deadline = Unix.gettimeofday () +. 2. in
                   while
                     Atomic.get finalizers_entered < 2
                     && Unix.gettimeofday () < deadline
                   do
                     Thread.delay 0.001
                   done;
                   if Atomic.get finalizers_entered < 2 then
                     raise Finalizers_serialized;
                   result)
                 (process_job ["--process-result"; ""; ""; "0"])));
      true
    with Finalizers_serialized -> false
  in
  check parallel_finalizers_completed
    "independent subprocess finalizers run before scheduler dispatch resumes";
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
          | None ->
            Some (Process.task (process_job ["--process-result"; ""; ""; "1"]))
          | Some _ ->
            incr drained_failures;
            raise (Failure key));
      None
    with Failure key -> Some key
  in
  check
    (!drained_failures = 2 && deterministic_failure = Some "a")
    "dependency scheduler drains active work and reports errors \
     deterministically"

let platform_tests _context =
  let test_executable = test_executable () in
  let path_root = Filename.temp_file "rewatch-ocaml-path-" "" in
  Sys.remove path_root;
  Unix.mkdir path_root 0o755;
  let path_root = Unix.realpath path_root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree path_root)
    (fun () ->
      let first = Filename.concat path_root "first" in
      let second = Filename.concat path_root "second" in
      Unix.mkdir first 0o755;
      Unix.mkdir second 0o755;
      let command = if Sys.win32 then "worker.exe" else "worker" in
      Unix.mkdir (Filename.concat first command) 0o755;
      let executable = Filename.concat second command in
      File_util.copy_existing_file ~ensure_parent:true test_executable
        executable;
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
          check
            (Platform.resolve_program ~cwd:path_root
               (Filename.concat "." requested)
            = Filename.concat "." requested)
            "an explicit current-directory executable does not use PATH";
          if Sys.win32 then (
            let cwd_executable = Filename.concat path_root "current.exe" in
            File_util.copy_existing_file ~ensure_parent:true test_executable
              cwd_executable;
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
  check
    (Platform_windows.program_for_working_directory ~cwd:"project"
       "tools/compiler.exe"
    = Filename.concat "project" "tools/compiler.exe")
    "Windows resolves relative executable paths against the child working \
     directory";
  check
    (Platform_windows.program_for_working_directory ~cwd:"project"
       "/toolchain/compiler.exe"
    = "/toolchain/compiler.exe")
    "Windows preserves absolute executable paths";
  check
    (Platform_windows.serialize_command_line ~program:"cmd.exe"
       ~args:["/D"; "/V:OFF"; "/S"; "/C"; {|echo "hello world"|}]
    = {|cmd.exe /D /V:OFF /S /C echo "hello world"|})
    "Windows preserves cmd.exe shell syntax without executable-argument \
     re-quoting"

let scheduler_tests _context =
  let test_executable = test_executable () in
  let scheduler_root = Filename.temp_file "rewatch-ocaml-scheduler-" "" in
  Sys.remove scheduler_root;
  Unix.mkdir scheduler_root 0o755;
  let scheduler_root = Unix.realpath scheduler_root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree scheduler_root)
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
        "pipe capture creates no temporary scheduler logs")

let graph_and_diagnostic_tests _context =
  let node name deps = (name, deps) in
  let shortest_cycle =
    Graph.shortest_cycle
      [
        node "LongA" ["LongB"];
        node "LongB" ["LongC"];
        node "LongC" ["LongA"];
        node "ShortA" ["ShortB"];
        node "ShortB" ["ShortA"];
      ]
      ~name:fst ~deps:snd
    |> Option.value ~default:[]
  in
  check
    (shortest_cycle = ["ShortA"; "ShortB"; "ShortA"])
    "cycle diagnostics select the shortest cycle deterministically";
  let cycle_blocked =
    Graph.cycle_blocked_nodes
      [
        node "A" ["B"];
        node "B" ["A"];
        node "C" ["D"];
        node "D" ["C"];
        node "Dependent" ["A"];
        node "TransitiveDependent" ["Dependent"];
        node "Unrelated" [];
      ]
      ~name:fst ~deps:snd
    |> List.map fst |> List.sort String.compare
  in
  check
    (cycle_blocked = ["A"; "B"; "C"; "D"; "Dependent"; "TransitiveDependent"])
    "a linear scheduling pass retains every cycle and its dependents";
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
    let temporary = Unix.realpath temporary in
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
    (not (Package_graph.source_discovery_prod ~prod:false ~is_local:true))
    "development sources are enabled for a local development build";
  check
    (Package_graph.source_discovery_prod ~prod:true ~is_local:true)
    "production builds exclude local development sources";
  check
    (Package_graph.source_discovery_prod ~prod:false ~is_local:false)
    "installed dependencies always exclude development sources";
  check (Build_lock.valid_owner "0") "zero is a valid serialized u32 owner";
  check
    (not
       (Platform.process_is_active "0" ~run:(fun _ _ ->
            assert_failure "PID zero must not invoke a process-name probe")))
    "PID zero cannot own a live build lock";
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
    "trailing data in a lock owner is malformed"

let lock_tests _context =
  let test_executable = test_executable () in
  let lock_root = Filename.temp_file "rewatch-ocaml-stale-lock-" "" in
  Sys.remove lock_root;
  Unix.mkdir lock_root 0o755;
  let lock_root = Unix.realpath lock_root in
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
  let lock_owner_pid = ref None in
  Fun.protect
    ~finally:(fun () ->
      Option.iter
        (fun pid ->
          (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
          try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
        !lock_owner_pid;
      File_util.remove_tree lock_root)
    (fun () ->
      Build_lock.with_build lock_root (fun ~release ->
          check
            (Build_lock.read_owner lock = Some (string_of_int (Unix.getpid ())))
            "stale build lock is replaced";
          check
            (not (Sys.file_exists takeover))
            "stale takeover marker is removed";
          let candidates =
            Sys.readdir lock_dir |> Array.to_list
            |> List.filter (String.starts_with ~prefix:".build-lock-")
          in
          check (candidates = []) "lock candidate is removed before build work";
          release ();
          check
            (not (Sys.file_exists lock))
            "build lock is released immediately";
          release ();
          check
            (not (Sys.file_exists lock))
            "releasing a build lock twice is harmless");
      check (not (Sys.file_exists lock)) "released build lock is removed";
      let lock_owner_executable =
        Filename.concat lock_root "rescript-lock-owner"
      in
      File_util.copy_existing_file ~ensure_parent:false test_executable
        lock_owner_executable;
      Unix.chmod lock_owner_executable 0o755;
      let owner_pid =
        Spawn.spawn ~prog:lock_owner_executable
          ~argv:[lock_owner_executable; "--wait-forever"]
          ()
      in
      lock_owner_pid := Some owner_pid;
      write_owner lock (string_of_int owner_pid);
      let lock_polls = ref 0 in
      let lock_wait_cancelled =
        let exception Cancel in
        try
          Build_lock.with_build lock_root
            ~poll:(fun () ->
              incr lock_polls;
              if !lock_polls = 2 then raise Cancel)
            (fun ~release:_ -> check false "active lock was acquired");
          false
        with Cancel -> true
      in
      check lock_wait_cancelled "waiting for a build lock remains cancellable";
      check
        (Build_lock.read_owner lock = Some (string_of_int owner_pid))
        "cancelling a lock wait preserves the active owner's lock";
      Unix.kill owner_pid Sys.sigterm;
      ignore (Unix.waitpid [] owner_pid);
      lock_owner_pid := None;
      File_util.remove_file lock)

let config_tests _context =
  let config_root = Filename.temp_file "rewatch-ocaml-config-" "" in
  Sys.remove config_root;
  Unix.mkdir config_root 0o755;
  let config_root = Unix.realpath config_root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree config_root)
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
        "known unsupported config fields are distinguished from unknown fields")

let dependency_validation_tests _context =
  let test_executable = test_executable () in
  let dependency_root =
    Filename.temp_file "rewatch-ocaml-allowed-dependents-" ""
  in
  Sys.remove dependency_root;
  Unix.mkdir dependency_root 0o755;
  let dependency_root = Unix.realpath dependency_root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree dependency_root)
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
          | None -> Test_support.unsetenv "RESCRIPT_BSC_EXE")
        (fun () ->
          let rejected =
            try
              Build.run
                ~poll:(fun () -> ())
                ~verbosity:0 ~folder:dependency_root ~prod:false ~features:None
                ~warn_error:None ~after_build:None ~filter:None ~no_timing:false;
              false
            with Project_context.Error message ->
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
              Build.run
                ~poll:(fun () -> ())
                ~verbosity:0 ~folder:dependency_root ~prod:false ~features:None
                ~warn_error:None ~after_build:None ~filter:None ~no_timing:false;
              false
            with Project_context.Error message ->
              Test_support.contains_text message
                "app dev-dependencies: restricted"
          in
          check rejected "unallowed development dependency is rejected"))

let tests =
  "unit_tests"
  >::: [
         "feature_requests" >:: feature_request_tests;
         "string_util" >:: string_util_tests;
         "process" >:: process_tests;
         "platform" >:: platform_tests;
         "scheduler" >:: scheduler_tests;
         "graph_and_diagnostics" >:: graph_and_diagnostic_tests;
         "locks" >:: lock_tests;
         "configuration" >:: config_tests;
         "dependency_validation" >:: dependency_validation_tests;
       ]
