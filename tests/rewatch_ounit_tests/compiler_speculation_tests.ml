open OUnit2

(* Pipelining in the compiler scheduler, with scripted compile requests in
   place of the compiler: which modules may start early, what they read, and
   that only adopted requests have effects. *)

let check condition message = assert_bool message condition
let write_file = Test_support.write_file
let with_temp_dir action =
  Test_support.with_temp_dir "rewatch-speculation-" action

let await ?(seconds = 5.) condition =
  let deadline = Unix.gettimeofday () +. seconds in
  while (not (condition ())) && Unix.gettimeofday () < deadline do
    Thread.delay 0.001
  done;
  condition ()

let with_env name value action =
  let previous = Sys.getenv_opt name in
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some previous -> Unix.putenv name previous
      | None -> Test_support.unsetenv name)
    (fun () ->
      Unix.putenv name value;
      action ())

let succeeded = Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""}

(* What a scripted request saw when it started. *)
type start = {speculative: bool; redirects: (string * string) list}

type module_spec = {
  key: string;
  dependencies: string list;
  dirty: bool;
  interface: bool;
  cmi_name: string;
  post_build: bool;
  publish_change: Compiler_scheduler.cmi_change;
  run:
    hooks:Compiler_scheduler.compile_hooks ->
    source_kind:Source.source_kind ->
    cmi_path:string ->
    Process.result;
}

let spec ?(dependencies = []) ?(dirty = true) ?(interface = false) ?cmi_name
    ?(post_build = false) ?(publish_change = Compiler_scheduler.Cmi_changed)
    ?(run = fun ~hooks:_ ~source_kind:_ ~cmi_path:_ -> succeeded) key =
  {
    key;
    dependencies;
    dirty;
    interface;
    cmi_name = Option.value cmi_name ~default:(key ^ ".cmi");
    post_build;
    publish_change;
    run;
  }

(* A request as the compiler makes it: announce the interface, then wait at the
   gate before any effect. *)
let announce ~hooks ~cmi_path bytes =
  Option.iter
    (fun announce ->
      announce (Filename.concat "/build" (Filename.basename cmi_path)) bytes)
    hooks.Compiler_scheduler.on_interface_computed

let gate ~hooks =
  Option.iter Speculation_gate.wait hooks.Compiler_scheduler.gate

type outcome = {
  starts: (string, start) Hashtbl.t;
  published: string list;
  log: string list;
  failed: string option;
}

let run_modules ?(in_build = false) specs =
  with_temp_dir (fun root ->
      let ocaml_dir = Build_artifacts.lib_path root "ocaml" in
      File_util.ensure_dir ocaml_dir;
      File_util.ensure_dir (Build_artifacts.lib_path root "bs");
      Compiler_log.initialize root;
      let log_path = Filename.concat root "speculation.log" in
      let build_state = Build_state.create (List.length specs) in
      List.iter
        (fun spec ->
          Build_state.add build_state ~key:spec.key
            ~kind:Build_state.Source_module ~last_compiled_cmi:(Some 0.)
            ~last_compiled_cmt:(Some 0.))
        specs;
      List.iter
        (fun spec ->
          Build_state.set_dependencies build_state ~key:spec.key
            spec.dependencies;
          (Build_state.find_exn build_state spec.key).compile_dirty <-
            spec.dirty)
        specs;
      let starts = Hashtbl.create 8 in
      let published = ref [] in
      let mutex = Mutex.create () in
      let locked action = Process_child.with_lock mutex action in
      let candidates =
        specs
        |> List.map (fun spec ->
            let state = Build_state.find_exn build_state spec.key in
            let cmi_path = Filename.concat ocaml_dir spec.cmi_name in
            write_file cmi_path "old interface";
            let source =
              Source.
                {
                  name = spec.key;
                  implementation = Filename.concat "src" (spec.key ^ ".res");
                  interface =
                    (if spec.interface then
                       Some (Filename.concat "src" (spec.key ^ ".resi"))
                     else None);
                  is_dev = false;
                }
            in
            Compiler_scheduler.candidate ~key:spec.key ~state ~warning_paths:[]
              ~make:(fun () ->
                Compiler_scheduler.create ~key:spec.key
                  ~dependencies:state.Build_state.dependencies ~source ~state
                  ~cmi_path
                  ~prepare:(fun () -> ())
                  ~compile:(fun ~hooks ~source_kind _path ->
                    Process.concurrent_task
                      ~cancel:(fun () ->
                        Option.iter Speculation_gate.cancel
                          hooks.Compiler_scheduler.gate)
                      (fun () ->
                        (match source_kind with
                        | Source.Interface when spec.interface -> ()
                        | Source.Interface | Source.Implementation ->
                          locked (fun () ->
                              Hashtbl.replace starts spec.key
                                {
                                  speculative = Option.is_some hooks.gate;
                                  redirects =
                                    List.map
                                      (fun (path, bytes) ->
                                        (Filename.basename path, bytes))
                                      hooks.cmi_redirects;
                                }));
                        spec.run ~hooks ~source_kind ~cmi_path))
                  ~publish:(fun ~source_kind _path _result ->
                    (match source_kind with
                    | Source.Interface -> ()
                    | Source.Implementation ->
                      locked (fun () -> published := spec.key :: !published));
                    Compiler_scheduler.
                      {stderr = ""; cmi_change = spec.publish_change})
                  ~record_published_outputs:(fun ~source_kind:_ _path -> ())
                  ~post_build:(fun output ->
                    if spec.post_build then
                      [
                        Compiler_scheduler.
                          {
                            output;
                            task =
                              Process.concurrent_task ~cancel:ignore (fun () ->
                                  succeeded);
                          };
                      ]
                    else [])
                  ~package_root:root ~is_local:true
                  ~mark_warning:(fun _ -> ())))
      in
      let failed =
        with_env "REWATCH_COMPILER_DOMAINS" "4" (fun () ->
            with_env "REWATCH_SPECULATION_LOG" log_path (fun () ->
                let in_store action =
                  if in_build then Build_file_store.with_build action
                  else action ()
                in
                try
                  in_store @@ fun () ->
                  Compiler_scheduler.run ~poll:None
                    ~warning_state:(Warning_state.create ())
                    ~compile_assets:(Compile_assets.create [ocaml_dir])
                    ~build_state ~candidates
                    ~mark_compiled:(fun () -> ())
                    ~mark_had_warnings:(fun () -> ())
                    ~progress:
                      (Output.Progress.create ~enabled:false ~color:false)
                    ~compile_step:"1/1" ~namespace_count:0 ~verbosity:0;
                  None
                with Compiler_scheduler.Build_failure message -> Some message))
      in
      let log =
        if Sys.file_exists log_path then
          File_util.read_file log_path
          |> String.split_on_char '\n'
          |> List.map (fun line ->
              (* Tolerate text-mode line endings. *)
              if String.ends_with ~suffix:"\r" line then
                String.sub line 0 (String.length line - 1)
              else line)
          |> List.filter (fun line -> line <> "")
        else []
      in
      {starts; published = List.rev !published; log; failed})

let started outcome key =
  match Hashtbl.find_opt outcome.starts key with
  | Some start -> start
  | None -> assert_failure (key ^ " was not compiled")

(* [a] holds its slot until its dependent has started, if it can. *)
let wait_for_dependent ?(seconds = 5.) dependent_started =
 fun ~hooks ~source_kind:_ ~cmi_path ->
  announce ~hooks ~cmi_path "new A";
  ignore (await ~seconds (fun () -> Atomic.get dependent_started));
  gate ~hooks;
  succeeded

let marking_start flag =
 fun ~hooks ~source_kind:_ ~cmi_path:_ ->
  Atomic.set flag true;
  gate ~hooks;
  succeeded

let chain_tests _context =
  let b_started = Atomic.make false in
  let outcome =
    run_modules
      [
        spec "A" ~run:(wait_for_dependent b_started);
        spec "B" ~dependencies:["A"] ~run:(marking_start b_started);
      ]
  in
  let b = started outcome "B" in
  check b.speculative "a dependent starts while its dependency runs";
  check
    (b.redirects = [("A.cmi", "new A")])
    "it reads the dependency's announced interface";
  check
    (outcome.published = ["A"; "B"])
    "the dependent publishes after its dependency";
  check
    (outcome.log = ["start\tB"; "adopt\tB"])
    "the speculation is logged and adopted"

let excluded_tests _context =
  let not_speculated name specs =
    let outcome = run_modules specs in
    check
      (not (started outcome "B").speculative)
      (name ^ ": the dependent starts only after its dependency");
    check (outcome.log = []) (name ^ ": nothing is speculated");
    check (outcome.published = ["A"; "B"]) (name ^ ": both modules compile")
  in
  let short = Atomic.make false in
  not_speculated "js-post-build"
    [
      spec "A" ~post_build:true ~run:(wait_for_dependent ~seconds:0.2 short);
      spec "B" ~dependencies:["A"];
    ];
  not_speculated "lowercase .cmi"
    [
      spec "A" ~cmi_name:"a.cmi" ~run:(wait_for_dependent ~seconds:0.2 short);
      spec "B" ~dependencies:["A"];
    ];
  let unchanged =
    run_modules
      [
        spec "A" ~publish_change:Compiler_scheduler.Cmi_unchanged
          ~run:(fun ~hooks ~source_kind:_ ~cmi_path ->
            (* The announced interface is the published one. *)
            announce ~hooks ~cmi_path "old interface";
            Thread.delay 0.2;
            succeeded);
        spec "B" ~dependencies:["A"] ~dirty:false;
      ]
  in
  check (unchanged.log = [])
    "a clean dependent of an unchanged interface neither speculates nor \
     compiles"

(* Within a build, whether an announced interface changes the published one
   is read from disk without filling the build file store: a request that is
   abandoned meanwhile could otherwise record the old file over the one its
   successor publishes. *)
let store_tests _context =
  let clean_dependent ~before announced =
    run_modules ~in_build:true
      [
        spec "A" ~run:(fun ~hooks ~source_kind:_ ~cmi_path ->
            let cached_before = before cmi_path in
            announce ~hooks ~cmi_path announced;
            check
              (Build_file_store.cached cmi_path = cached_before)
              "comparing the announced interface leaves the store as it was";
            Thread.delay 0.2;
            gate ~hooks;
            succeeded);
        spec "B" ~dependencies:["A"] ~dirty:false;
      ]
  in
  let nothing_cached _ = None in
  let unchanged = clean_dependent ~before:nothing_cached "old interface" in
  check (unchanged.log = [])
    "an announced interface equal to the published one releases no clean \
     dependent";
  let changed = clean_dependent ~before:nothing_cached "new A" in
  check
    (changed.log = ["start\tB"; "adopt\tB"])
    "an announced interface that differs from the published one does";
  let rewritten =
    clean_dependent
      ~before:(fun cmi_path ->
        Build_file_store.write cmi_path "new A";
        Some "new A")
      "new A"
  in
  check (rewritten.log = [])
    "an interface rewritten through the store mid-build is compared as \
     rewritten"

let interface_file_tests _context =
  let b_started = Atomic.make false in
  let outcome =
    run_modules
      [
        spec "A" ~interface:true ~run:(fun ~hooks ~source_kind ~cmi_path:_ ->
            (match source_kind with
            | Source.Interface -> ()
            | Source.Implementation ->
              ignore (await (fun () -> Atomic.get b_started)));
            gate ~hooks;
            succeeded);
        spec "B" ~dependencies:["A"] ~run:(marking_start b_started);
      ]
  in
  let b = started outcome "B" in
  check b.speculative
    "a dependent starts once the interface file compiled, during the \
     implementation";
  check (b.redirects = []) "it reads the published interface"

let failed_dependency_tests _context =
  let b_started = Atomic.make false in
  let b_effects = Atomic.make false in
  let outcome =
    run_modules
      [
        spec "A" ~run:(fun ~hooks ~source_kind:_ ~cmi_path ->
            announce ~hooks ~cmi_path "new A";
            ignore (await (fun () -> Atomic.get b_started));
            gate ~hooks;
            (* Code generation fails after the interface was announced. *)
            Process.{status = Unix.WEXITED 2; stdout = ""; stderr = "boom\n"});
        spec "B" ~dependencies:["A"]
          ~run:(fun ~hooks ~source_kind:_ ~cmi_path:_ ->
            Atomic.set b_started true;
            gate ~hooks;
            Atomic.set b_effects true;
            succeeded);
      ]
  in
  check
    (match outcome.failed with
    | Some message -> Test_support.contains_text message "boom"
    | None -> false)
    "the dependency's failure is reported";
  check (Atomic.get b_started) "the dependent had started early";
  check (not (Atomic.get b_effects)) "the dependent never passes its gate";
  check (outcome.published = []) "nothing is published";
  check
    (outcome.log = ["start\tB"; "abandon\tB"])
    "the speculation is abandoned"

(* X speculates on A; Y speculates on X. A's publication leaves X clean, so X
   is not compiled and both speculations are abandoned. Y's abandoned request
   announces a stale interface late, while Y's real request runs: Z must only
   ever see the real one. *)
let stale_announcement_tests _context =
  let y_speculated = Atomic.make false in
  let stale_announced = Atomic.make false in
  let z_started = Atomic.make false in
  let outcome =
    run_modules
      [
        spec "A" ~publish_change:Compiler_scheduler.Cmi_unchanged
          ~run:(wait_for_dependent y_speculated);
        spec "X" ~dependencies:["A"] ~dirty:false
          ~run:(fun ~hooks ~source_kind:_ ~cmi_path ->
            announce ~hooks ~cmi_path "speculative X";
            gate ~hooks;
            succeeded);
        spec "Y" ~dependencies:["X"]
          ~run:(fun ~hooks ~source_kind:_ ~cmi_path ->
            match hooks.gate with
            | Some gate ->
              Atomic.set y_speculated true;
              ignore (await (fun () -> Speculation_gate.is_abandoned gate));
              announce ~hooks ~cmi_path "stale Y";
              Atomic.set stale_announced true;
              raise Speculation_gate.Abandoned
            | None ->
              ignore (await (fun () -> Atomic.get stale_announced));
              Thread.delay 0.1;
              announce ~hooks ~cmi_path "real Y";
              ignore (await (fun () -> Atomic.get z_started));
              succeeded);
        spec "Z" ~dependencies:["Y"] ~run:(marking_start z_started);
      ]
  in
  check (Atomic.get y_speculated) "Y speculated on X";
  check
    (not (started outcome "Y").speculative)
    "Y compiles again after its speculation was abandoned";
  let z = started outcome "Z" in
  check
    (not (List.mem ("Y.cmi", "stale Y") z.redirects))
    "an abandoned request's late announcement is ignored";
  check
    ((not z.speculative) || List.mem ("Y.cmi", "real Y") z.redirects)
    "a speculation on Y reads Y's real interface";
  check
    (List.mem "abandon\tX" outcome.log && List.mem "abandon\tY" outcome.log)
    "both speculations are abandoned"

(* Bs_cmi_load candidates are matched to published paths as the build file
   store keys them. A lookup that may be another spelling of a redirected
   file waits: in a directory the build does not publish to, such as an
   alias from user flags, or differing only in case. *)
let redirect_tests _context =
  let root = if Sys.win32 then "C:\\project" else "/project" in
  let ( // ) = Filename.concat in
  let ocaml_dir = root // "lib" // "ocaml" in
  let bs_dir = root // "lib" // "bs" in
  let dependency_dir = root // "node_modules" // "dep" // "lib" // "ocaml" in
  let waits = ref 0 in
  let redirect =
    Compiler_process.cmi_redirect ~cwd:bs_dir
      ~cmi_directories:[ocaml_dir; dependency_dir]
      ~wait_for_adoption:(fun () -> incr waits)
      [("../ocaml/A.cmi", "A contents")]
  in
  let expect name path expected expected_waits =
    waits := 0;
    assert_equal ~msg:name expected (redirect path);
    assert_equal ~msg:(name ^ " waits") ~printer:string_of_int expected_waits
      !waits
  in
  expect "relative spelling" (bs_dir // "../ocaml/A.cmi") (Some "A contents") 0;
  expect "published spelling" (ocaml_dir // "A.cmi") (Some "A contents") 0;
  expect "spelling with a current directory segment"
    (root // "lib/./ocaml/A.cmi")
    (Some "A contents") 0;
  expect "other module" (ocaml_dir // "B.cmi") None 0;
  expect "other module in another directory" (root // "other" // "B.cmi") None 0;
  expect "same name in another published directory"
    (dependency_dir // "A.cmi")
    None 0;
  expect "same name in a directory the build does not publish to"
    (root // "linked" // "A.cmi")
    None 1;
  expect "other case of the name" (ocaml_dir // "a.cmi") None 1;
  expect "other case of the name in another published directory"
    (dependency_dir // "a.cmi")
    None 0;
  expect "other case of the directory"
    (root // "lib" // "OCaml" // "A.cmi")
    None 1;
  if Sys.win32 then
    expect "backslash separators"
      (root ^ "\\lib\\ocaml\\A.cmi")
      (Some "A contents") 0;
  let none =
    Compiler_process.cmi_redirect ~cwd:bs_dir ~cmi_directories:[]
      ~wait_for_adoption:(fun () -> incr waits)
      []
  in
  waits := 0;
  assert_equal None (none (root // "linked" // "A.cmi"));
  check (!waits = 0) "without redirects nothing waits"

(* A speculation logs its compiler call when adopted. If that fails, it fails
   every later gate pass, so the partial .cmt the compiler then tries to save
   is not written either, and the task fails as it did when logging first. *)
let call_log_failure_tests _context =
  with_temp_dir (fun root ->
      write_file (Filename.concat root "Logged.res") "let value = 1\n";
      let gate = Speculation_gate.create () in
      Speculation_gate.open_ gate;
      let task =
        Compiler_process.task
          ~hooks:{Compiler_scheduler.no_compile_hooks with gate = Some gate}
          Compiler_process.
            {
              cwd = root;
              args =
                [
                  "-nostdlib";
                  "-nopervasives";
                  "-bs-package-name";
                  "logged";
                  "-bs-package-output";
                  "commonjs:.:.js";
                  "Logged.res";
                ];
            }
      in
      let failed =
        with_env "REWATCH_COMPILER_CALL_LOG"
          (Filename.concat root "missing/calls.log") (fun () ->
            try
              ignore (Process.run_tasks ~max_jobs:1 [task]);
              false
            with Sys_error _ -> true)
      in
      check failed "the logging failure fails the task";
      List.iter
        (fun extension ->
          check
            (not
               (Sys.file_exists (Filename.concat root ("Logged." ^ extension))))
            ("nothing is written after the logging failure: " ^ extension))
        ["cmi"; "cmt"; "cmj"; "js"])

let tests =
  "compiler_speculation"
  >::: [
         "chain" >:: chain_tests;
         "excluded" >:: excluded_tests;
         "store" >:: store_tests;
         "interface_file" >:: interface_file_tests;
         "failed_dependency" >:: failed_dependency_tests;
         "stale_announcement" >:: stale_announcement_tests;
         "redirect" >:: redirect_tests;
         "call_log_failure" >:: call_log_failure_tests;
       ]
