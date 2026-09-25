open OUnit2

let check condition message = assert_bool message condition

let publication_tests _context =
  Test_support.with_temp_dir "rewatch-compiler-process-" (fun root ->
      let build_dir = Filename.concat root "build" in
      let ocaml_dir = Filename.concat root "ocaml" in
      File_util.ensure_dir build_dir;
      File_util.ensure_dir ocaml_dir;
      List.iter
        (fun extension ->
          Test_support.write_file
            (Filename.concat build_dir ("Ns." ^ extension))
            ("new " ^ extension))
        ["cmi"; "cmj"; "cmt"];
      File_util.ensure_dir (Filename.concat ocaml_dir "Ns.cmj");
      let namespace_task =
        Compiler_process.namespace_task ~bsc:"unused" ~runtime:"unused"
          ~build_dir ~ocaml_dir ~entry:None ~package_dirty:true ~force:false
          "Ns" []
        |> Option.get
      in
      let result =
        Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""}
      in
      match
        Compiler_scheduler.capture_publication (fun () ->
            namespace_task.Compiler_scheduler.publish result)
      with
      | Compiler_scheduler.Failed_after_cmi_publication
          {cmi_change = Compiler_scheduler.Cmi_changed; _} ->
        assert_equal "new cmi"
          (File_util.read_file (Filename.concat ocaml_dir "Ns.cmi"))
          ~msg:
            "namespace publication must retain the changed CMI before a later \
             artifact copy fails"
      | _ ->
        assert_failure
          "namespace publication must report a changed CMI across a later \
           artifact-copy failure");
  Test_support.with_temp_dir "rewatch-namespace-retry-" (fun root ->
      let build_dir = Filename.concat root "build" in
      let ocaml_dir = Filename.concat root "ocaml" in
      File_util.ensure_dir build_dir;
      File_util.ensure_dir ocaml_dir;
      List.iter
        (fun extension ->
          Test_support.write_file
            (Filename.concat ocaml_dir ("Ns." ^ extension))
            (if extension = "mlmap" then "old map\n" else "old artifact"))
        ["cmi"; "cmj"; "cmt"; "mlmap"];
      check
        (Option.is_some
           (Compiler_process.namespace_task ~bsc:"unused" ~runtime:"unused"
              ~build_dir ~ocaml_dir ~entry:None ~package_dirty:false
              ~force:false "Ns" []))
        "a published namespace map that differs from membership is rebuilt";
      Test_support.write_file
        (Filename.concat ocaml_dir "Ns.mlmap")
        "randjbuildsystem\n";
      check
        (Option.is_some
           (Compiler_process.namespace_task ~bsc:"unused" ~runtime:"unused"
              ~build_dir ~ocaml_dir ~entry:None ~package_dirty:false ~force:true
              "Ns" []))
        "retained namespace publication dirtiness forces a retry")

let restore_environment name previous =
  match previous with
  | Some value -> Unix.putenv name value
  | None -> Test_support.unsetenv name

let domain_ppx_cancellation_test _context =
  Test_support.with_temp_dir "rewatch-domain-ppx-cancellation-" (fun root ->
      let source = Filename.concat root "DomainPpx.res" in
      let marker = Filename.concat root "ppx-started" in
      Test_support.write_file source "let value = 1\n";
      let ppx_command =
        Filename.quote (Unix.realpath Sys.executable_name)
        ^ " --ppx-wait " ^ Filename.quote marker
      in
      let job =
        Process.
          {
            program = "<embedded compiler>";
            cwd = root;
            args =
              [
                "-nostdlib";
                "-nopervasives";
                "-bs-project-root";
                root;
                "-bs-package-name";
                "domain-ppx-cancellation";
                "-bs-package-output";
                "commonjs:.:.js";
                "-ppx";
                ppx_command;
                "DomainPpx.res";
              ];
          }
      in
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
      check interrupted "domain task cancellation reaches a running PPX";
      check (Sys.file_exists marker) "the domain PPX started";
      if not Sys.win32 then
        let pid = File_util.read_file marker |> int_of_string in
        check
          (try
             Unix.kill pid 0;
             false
           with Unix.Unix_error (Unix.ESRCH, _, _) -> true)
          "the domain PPX process is gone when cancellation returns")

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
                Process.
                  {
                    program = "<embedded compiler>";
                    cwd = root;
                    args = ["-bs-ast"; "-o"; name ^ ".ast"; name ^ ".res"];
                  })
              ["First"; "Second"]
          in
          let results = Compiler_process.run_jobs jobs in
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
              Process.
                {
                  program = "<embedded compiler>";
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
          Unix.putenv "REWATCH_COMPILER_DOMAINS" "0";
          check
            (try
               ignore (Compiler_execution_mode.configured_count ());
               false
             with Invalid_argument _ -> true)
            "domain counts must be positive";
          Unix.putenv "REWATCH_COMPILER_DOMAINS"
            (string_of_int (Compiler_execution_mode.max_domain_count + 1));
          check
            (try
               ignore (Compiler_execution_mode.configured_count ());
               false
             with Invalid_argument _ -> true)
            "domain counts must honor the scheduler bound"))

let project_cache_survives_worker_batches_test _context =
  Test_support.with_temp_dir "rewatch-project-worker-cache-" (fun root ->
      let write name contents =
        Test_support.write_file (Filename.concat root name) contents
      in
      write "Api.resi" "let value: int\n";
      write "Api.res" "let value = 1\n";
      write "First.res" "let result = Api.value\n";
      write "Second.res" "let result = Api.value\n";
      let job input =
        Process.
          {
            program = "<embedded compiler>";
            cwd = root;
            args =
              [
                "-nostdlib";
                "-nopervasives";
                "-bs-project-root";
                root;
                "-bs-package-name";
                "project-worker-cache";
                "-bs-package-output";
                "commonjs:.:.js";
                "-I";
                root;
                input;
              ];
          }
      in
      let succeeds result = check (Process.succeeded result) result.stderr in
      succeeds (Compiler_process.run (job "Api.resi"));
      succeeds (Compiler_process.run (job "Api.res"));
      let session =
        Build_session.create ~warning_state:(Warning_state.create ())
        |> Build_session.compiler_session
      in
      let original_loader = !Env.Persistent_signature.load in
      let loads = Atomic.make 0 in
      (Env.Persistent_signature.load :=
         fun ~unit_name ->
           if unit_name = "Api" then ignore (Atomic.fetch_and_add loads 1);
           original_loader ~unit_name);
      Fun.protect
        ~finally:(fun () -> Env.Persistent_signature.load := original_loader)
        (fun () ->
          let compile input =
            Compiler_process.run_jobs ~session [job input] |> List.iter succeeds
          in
          compile "First.res";
          assert_equal 1 (Atomic.get loads);
          compile "Second.res";
          assert_equal ~msg:"a new worker batch reuses the project cache" 1
            (Atomic.get loads)))

let tests =
  "compiler_process_tests"
  >::: [
         "publication" >:: publication_tests;
         "domain_ppx_cancellation" >:: domain_ppx_cancellation_test;
         "domain_execution" >:: domain_execution_test;
         "project_cache_survives_worker_batches"
         >:: project_cache_survives_worker_batches_test;
       ]
