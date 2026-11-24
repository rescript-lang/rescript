open OUnit2

let check condition message = assert_bool message condition

let succeeded = Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""}
let failed = Process.{status = Unix.WEXITED 1; stdout = ""; stderr = "error"}

(* Runs [action] in a build that holds ASTs and interfaces as the build system
   does. *)
let in_build action =
  Build_file_store.set_held_writes Compiler_process.is_held;
  Fun.protect
    ~finally:(fun () -> Build_file_store.set_held_writes (fun _ -> false))
    (fun () -> Build_file_store.with_build action)

let publication_tests _context =
  Test_support.with_temp_dir "rewatch-compiler-process-" (fun root ->
      let artifact_dir = Filename.concat root "lib/rescript" in
      let path extension = Filename.concat artifact_dir ("Ns." ^ extension) in
      File_util.ensure_dir artifact_dir;
      in_build (fun () ->
          let namespace_task =
            Compiler_process.namespace_task ~runtime:"unused" ~artifact_dir
              ~entry:None ~package_dirty:true ~force:false ~dev_namespace:None
              "Ns" []
            |> Option.get
          in
          (* The compiler wrote an interface, but no implementation. *)
          Build_file_store.write (path "cmi") "new cmi";
          match
            Compiler_scheduler.capture_publication (fun () ->
                namespace_task.Compiler_scheduler.publish succeeded)
          with
          | Compiler_scheduler.Failed_after_cmi_publication
              {cmi_change = Compiler_scheduler.Cmi_changed; _} ->
            assert_equal "new cmi"
              (File_util.read_file (path "cmi"))
              ~msg:
                "namespace publication must publish the changed CMI before a \
                 later artifact is found missing"
          | _ ->
            assert_failure
              "namespace publication must report a changed CMI when a later \
               artifact is missing"));
  Test_support.with_temp_dir "rewatch-namespace-retry-" (fun root ->
      let artifact_dir = Filename.concat root "lib/rescript" in
      let path extension = Filename.concat artifact_dir ("Ns." ^ extension) in
      File_util.ensure_dir artifact_dir;
      List.iter
        (fun extension -> Test_support.write_file (path extension) "old")
        ["cmi"; "cmj"; "cmt"];
      let task ~package_dirty ~force =
        Compiler_process.namespace_task ~runtime:"unused" ~artifact_dir
          ~entry:None ~package_dirty ~force ~dev_namespace:None "Ns" []
      in
      check
        (Option.is_some (task ~package_dirty:false ~force:false))
        "a namespace map that differs from membership is rebuilt";
      check
        (not (Sys.file_exists (path "cmt")))
        "a namespace to compile loses its typed tree until it is published";
      (* A build killed after it wrote the map but before it compiled it. *)
      check
        (Option.is_some (task ~package_dirty:false ~force:false))
        "a namespace without its typed tree is compiled again";
      (* The compile of the new map publishes its typed tree. *)
      Test_support.write_file (path "cmt") "new";
      check
        (Option.is_none (task ~package_dirty:false ~force:false))
        "a current namespace map with its outputs is not rebuilt";
      check
        (Option.is_some (task ~package_dirty:false ~force:true))
        "retained namespace publication dirtiness forces a retry";
      in_build (fun () ->
          let namespace_task =
            Option.get (task ~package_dirty:true ~force:false)
          in
          Build_file_store.write (path "cmi") "failed cmi";
          (try
             ignore (namespace_task.Compiler_scheduler.publish failed);
             assert_failure "a failed namespace compile raises"
           with Compiler_scheduler.Build_failure _ -> ());
          assert_equal "old"
            (File_util.read_file (path "cmi"))
            ~msg:"a failed namespace compile keeps the previous interface");
      check
        (Option.is_some (task ~package_dirty:false ~force:false))
        "a failed namespace compile is retried by the next build")

(* Publishing a module writes its interface only when it changed, so that an
   unchanged interface keeps the modification time its dependents compare
   with, and releases from the build file store what nothing reads again. *)
let publication_releases_files_tests _context =
  Test_support.with_temp_dir "rewatch-publication-store-" (fun root ->
      Test_support.write_file
        (Filename.concat root "rescript.json")
        {|{"name":"publish","sources":"src","package-specs":{"module":"commonjs","in-source":true},"suffix":".js"}|};
      let config = Config.load_root root in
      let artifact_dir = Filename.concat root "lib/rescript" in
      let artifact name = Filename.concat artifact_dir name in
      let output path =
        Build_artifacts.generated_js_path config path
          (List.hd config.package_specs)
      in
      File_util.ensure_dir artifact_dir;
      File_util.ensure_dir (Filename.concat root "src");
      let publish path =
        Compiler_process.publish ~artifact_dir ~is_local:true ~config
          ~has_interface:false ~source_kind:Source.Implementation path succeeded
      in
      Test_support.write_file (artifact "Same.cmi") "same interface";
      Unix.utimes (artifact "Same.cmi") 1_000_000_000. 1_000_000_000.;
      Test_support.write_file (artifact "Kept.cmi") "kept interface";
      Test_support.write_file (artifact "Failed.cmi") "previous interface";
      in_build (fun () ->
          List.iter
            (fun name ->
              Build_file_store.write (artifact (name ^ ".cmi")) ("new " ^ name);
              Build_file_store.write (artifact (name ^ ".cmj")) "code";
              Build_file_store.write (artifact (name ^ ".cmt")) "typed tree")
            ["A"];
          (* The compile read the AST that its parse published. *)
          Test_support.write_file (artifact "A.ast") "ast";
          ignore (Build_file_store.read (artifact "A.ast"));
          Build_file_store.write (output "src/A.res") "js";
          check
            (not (Sys.file_exists (artifact "A.cmi")))
            "an interface is held until its compile is published";
          (* A build killed now leaves no typed tree that would mark the
             module compiled with its previous interface. *)
          check
            (not (Sys.file_exists (artifact "A.cmt")))
            "a typed tree is held until its compile is published";
          check
            ((publish "src/A.res").cmi_change = Compiler_scheduler.Cmi_changed)
            "a new interface is a change";
          assert_equal "new A" (File_util.read_file (artifact "A.cmi"));
          assert_equal "typed tree" (File_util.read_file (artifact "A.cmt"));
          List.iter
            (fun (path, kept) ->
              check
                (Option.is_some (Build_file_store.cached path) = kept)
                (path ^ if kept then " is kept" else " is released"))
            [
              (artifact "A.cmi", true);
              (artifact "A.cmj", true);
              (artifact "A.cmt", false);
              (artifact "A.ast", false);
              (output "src/A.res", false);
            ];
          Build_file_store.write (artifact "Same.cmi") "same interface";
          Build_file_store.write (artifact "Same.cmj") "code";
          check
            ((publish "src/Same.res").cmi_change
           = Compiler_scheduler.Cmi_unchanged)
            "an identical interface is no change";
          check
            ((Unix.stat (artifact "Same.cmi")).st_mtime = 1_000_000_000.)
            "an identical interface keeps its modification time";
          (* The compiler does not write an interface identical to the one on
             disk. *)
          Build_file_store.write (artifact "Kept.cmj") "code";
          check
            ((publish "src/Kept.res").cmi_change
           = Compiler_scheduler.Cmi_unchanged)
            "an interface the compiler did not rewrite is no change";
          Test_support.write_file (artifact "Failed.ast") "ast";
          Build_file_store.write (artifact "Failed.cmi") "failed interface";
          Build_file_store.write (artifact "Failed.cmt") "partial typed tree";
          Compiler_process.discard_compile ~artifact_dir ~config
            "src/Failed.res";
          check
            (Build_file_store.read (artifact "Failed.cmi")
            = Some "previous interface")
            "a failed compile leaves the previous interface";
          check
            ((Unix.stat (artifact "Failed.ast")).st_mtime < 2.)
            "a failed compile dates its AST back, so the next build compiles \
             it again";
          (* Another process holds the typed tree's place. *)
          Unix.mkdir (artifact "Blocked.cmt") 0o755;
          Test_support.write_file (artifact "Blocked.ast") "ast";
          Build_file_store.write (artifact "Blocked.cmi") "failed interface";
          Build_file_store.write (artifact "Blocked.cmt") "partial typed tree";
          Compiler_process.discard_compile ~artifact_dir ~config
            "src/Blocked.res";
          check
            (Option.is_none (Build_file_store.cached (artifact "Blocked.cmt")))
            "a typed tree that could not be published is dropped";
          check
            (not (Sys.file_exists (artifact "Blocked.cmi")))
            "a failed compile whose typed tree cannot be published still \
             leaves the interface alone");
      assert_equal "previous interface"
        (File_util.read_file (artifact "Failed.cmi"));
      assert_equal "partial typed tree"
        (File_util.read_file (artifact "Failed.cmt"))
        ~msg:"a failed compile publishes its partial typed tree for editors")

(* An artifact is renamed into place, and written in place when the rename
   fails, as on Windows while an editor has the file open. Here a read-only
   artifact directory makes the rename fail while the file stays writable. *)
let in_place_fallback_tests _context =
  if (not Sys.win32) && Unix.getuid () <> 0 then
    Test_support.with_temp_dir "rewatch-publication-fallback-" (fun root ->
        let artifact_dir = Filename.concat root "lib/rescript" in
        let ast = Filename.concat artifact_dir "A.ast" in
        Test_support.write_file ast "previous AST";
        Unix.chmod artifact_dir 0o555;
        Fun.protect
          ~finally:(fun () -> Unix.chmod artifact_dir 0o755)
          (fun () ->
            in_build (fun () ->
                Build_file_store.write ast "new AST";
                Compiler_process.publish_parse ~artifact_dir "src/A.res"));
        assert_equal "new AST" (File_util.read_file ast)
          ~msg:"a file that cannot be renamed into place is written in place";
        check
          (Array.for_all
             (fun name ->
               not
                 (String.starts_with ~prefix:File_util.atomic_write_prefix name))
             (Sys.readdir artifact_dir))
          "the fallback leaves no temporary file")

(* A parse that printed warnings is left for the build to publish after it
   registers the cleanup that repeats them, so that a build interrupted before
   then does not leave behind an AST whose warnings the next build skips. A
   parse that failed leaves the previous AST. *)
let parse_publication_tests _context =
  Test_support.with_temp_dir "rewatch-parse-publication-" (fun root ->
      Test_support.write_file
        (Filename.concat root "rescript.json")
        {|{"name":"parse","sources":"src","package-specs":{"module":"commonjs","in-source":true},"suffix":".js"}|};
      let config = Config.load_root root in
      let artifact_dir = Filename.concat root "lib/rescript" in
      File_util.ensure_dir artifact_dir;
      Test_support.write_file
        (Filename.concat root "src/Clean.res")
        "let value = 1\n";
      Test_support.write_file
        (Filename.concat root "src/Warns.res")
        "let f = (. x) => x\n";
      Test_support.write_file
        (Filename.concat root "src/Broken.res")
        "let x =\n";
      let ast path = Build_artifacts.ast_path ~artifact_dir path in
      Test_support.write_file (ast "src/Broken.res") "previous AST";
      let paths = ["src/Clean.res"; "src/Warns.res"; "src/Broken.res"] in
      in_build (fun () ->
          let results =
            paths
            |> List.map (fun path -> (artifact_dir, config, path))
            |> Compiler_process.run_parses
          in
          List.iter2
            (fun path (result : Process.result) ->
              match path with
              | "src/Clean.res" ->
                check
                  (Process.succeeded result && result.stderr = "")
                  "a clean parse prints nothing";
                check (Sys.file_exists (ast path)) "a clean parse is published"
              | "src/Warns.res" ->
                check
                  (Process.succeeded result && result.stderr <> "")
                  "a parse with warnings prints them";
                check
                  (not (Sys.file_exists (ast path)))
                  "a parse with warnings is not published yet";
                check
                  (Option.is_some (Build_file_store.cached (ast path)))
                  "the build reads the unpublished AST";
                Compiler_process.publish_parse ~artifact_dir path;
                check
                  (Sys.file_exists (ast path))
                  "the build publishes it once the cleanup is registered"
              | _ ->
                check (not (Process.succeeded result)) "a broken source fails";
                assert_equal "previous AST"
                  (File_util.read_file (ast path))
                  ~msg:"a failed parse leaves the previous AST")
            paths results))

let restore_environment name previous =
  match previous with
  | Some value -> Unix.putenv name value
  | None -> Test_support.unsetenv name

(* A request for DomainPpx.res whose PPX writes its PID to [marker] and then
   waits. *)
let waiting_ppx_request root marker =
  Test_support.write_file
    (Filename.concat root "DomainPpx.res")
    "let value = 1\n";
  let ppx_command =
    Filename.quote (Unix.realpath Sys.executable_name)
    ^ " --ppx-wait " ^ Filename.quote marker
  in
  Compiler_process.
    {
      cwd = root;
      args =
        [
          "-nostdlib";
          "-nopervasives";
          "-bs-project-root";
          root;
          "-bs-package-name";
          "domain-ppx";
          "-bs-package-output";
          "commonjs:.:.js";
          "-ppx";
          ppx_command;
          "DomainPpx.res";
        ];
    }

let check_ppx_gone marker =
  if not Sys.win32 then
    let pid = File_util.read_file marker |> int_of_string in
    check
      (try
         Unix.kill pid 0;
         false
       with Unix.Unix_error (Unix.ESRCH, _, _) -> true)
      "the domain PPX process is gone when the request returns"

let domain_ppx_cancellation_test _context =
  Test_support.with_temp_dir "rewatch-domain-ppx-cancellation-" (fun root ->
      let marker = Filename.concat root "ppx-started" in
      let job = waiting_ppx_request root marker in
      let started = Unix.gettimeofday () in
      let interrupted =
        try
          ignore
            (Process.run_tasks ~max_jobs:1
               ~poll:(fun () ->
                 if Sys.file_exists marker then raise (Process.Interrupted 15))
               [Compiler_process.task job]);
          false
        with Process.Interrupted 15 -> true
      in
      check interrupted "domain task cancellation is reported";
      (* The PPX waits 30 s: returning sooner shows that cancellation reached
         it instead of waiting for it to finish. *)
      check
        (Unix.gettimeofday () -. started < 10.)
        "domain task cancellation reaches a running PPX";
      check (Sys.file_exists marker) "the domain PPX started";
      check_ppx_gone marker)

(* An exception that escapes the compiler fails only its own request: the
   module fails with exit status 2 and the exception in its output. *)
let escaped_exception_test _context =
  Test_support.with_temp_dir "rewatch-escaped-exception-" (fun root ->
      let marker = Filename.concat root "ppx-started" in
      let raised = ref false in
      let poll () =
        if Sys.file_exists marker && not !raised then (
          raised := true;
          failwith "request failed")
      in
      let result =
        Compiler_process.run ~poll (waiting_ppx_request root marker)
      in
      check !raised "the exception was raised while the PPX ran";
      assert_equal (Unix.WEXITED 2) result.status;
      check
        (String.ends_with ~suffix:"Failure(\"request failed\")\n" result.stderr)
        ("the request reports the exception: " ^ result.stderr);
      check_ppx_gone marker)

let domain_execution_test _context =
  Test_support.with_temp_dir "rewatch-domain-execution-" (fun root ->
      let previous_count = Sys.getenv_opt "REWATCH_COMPILER_DOMAINS" in
      Fun.protect
        ~finally:(fun () ->
          restore_environment "REWATCH_COMPILER_DOMAINS" previous_count)
        (fun () ->
          List.iter
            (fun (cpus, workers) ->
              assert_equal workers
                (Compiler_execution_mode.recommended_domain_count cpus))
            [(1, 1); (2, 1); (4, 3); (8, 7); (10, 8); (16, 8)];
          Test_support.unsetenv "REWATCH_COMPILER_DOMAINS";
          assert_equal
            (Compiler_execution_mode.recommended_domain_count
               (Domain.recommended_domain_count ()))
            (Compiler_execution_mode.configured_count ());
          let count =
            if Sys.getenv_opt "REWATCH_TEST_DOMAIN_CONCURRENCY" = Some "1" then
              2
            else 1
          in
          Unix.putenv "REWATCH_COMPILER_DOMAINS" (string_of_int count);
          assert_equal count (Compiler_execution_mode.configured_count ());
          List.iter
            (fun name ->
              Test_support.write_file
                (Filename.concat root (name ^ ".res"))
                "let value = 1\n")
            ["First"; "Second"];
          let jobs =
            List.map
              (fun name ->
                Compiler_process.
                  {
                    cwd = root;
                    args = ["-bs-ast"; "-o"; name ^ ".ast"; name ^ ".res"];
                  })
              ["First"; "Second"]
          in
          let results = Compiler_process.run_requests jobs in
          List.iter
            (fun result -> check (Process.succeeded result) result.stderr)
            results;
          List.iter
            (fun name ->
              check
                (Sys.file_exists (Filename.concat root (name ^ ".ast")))
                "domain mode retains parser artifacts")
            ["First"; "Second"];
          let marker = Filename.concat root "ppx-started" in
          let ppx_command =
            Filename.quote (Unix.realpath Sys.executable_name)
            ^ " --ppx-copy " ^ Filename.quote marker
          in
          let ppx_result =
            Compiler_process.run
              Compiler_process.
                {
                  cwd = root;
                  args =
                    [
                      "-nostdlib";
                      "-nopervasives";
                      "-bs-project-root";
                      root;
                      "-bs-package-name";
                      "domain-ppx";
                      "-bs-package-output";
                      "commonjs:.:.js";
                      "-ppx";
                      ppx_command;
                      "First.res";
                    ];
                }
          in
          check (Process.succeeded ppx_result) ppx_result.stderr;
          check (Sys.file_exists marker) "a domain request launches its PPX";
          check
            (Sys.file_exists (Filename.concat root "First.js"))
            "the PPX transformed request still writes JavaScript";
          List.iter
            (fun value ->
              Unix.putenv "REWATCH_COMPILER_DOMAINS" value;
              check
                (try
                   ignore (Compiler_execution_mode.configured_count ());
                   false
                 with Process.Error _ -> true)
                ("domain count " ^ value ^ " is reported as invalid"))
            ["0"; "-1"; "abc"];
          Unix.putenv "REWATCH_COMPILER_DOMAINS"
            (string_of_int (Compiler_execution_mode.max_domain_count + 1));
          assert_equal Compiler_execution_mode.max_domain_count
            (Compiler_execution_mode.configured_count ())
            ~msg:"domain counts are capped at the scheduler bound"))

let tests =
  "compiler_process_tests"
  >::: [
         "publication" >:: publication_tests;
         "publication_releases_files" >:: publication_releases_files_tests;
         "parse_publication" >:: parse_publication_tests;
         "in_place_fallback" >:: in_place_fallback_tests;
         "domain_ppx_cancellation" >:: domain_ppx_cancellation_test;
         "escaped_exception" >:: escaped_exception_test;
         "domain_execution" >:: domain_execution_test;
       ]
