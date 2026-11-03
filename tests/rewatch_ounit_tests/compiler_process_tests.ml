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
        Compiler_process.namespace_task ~runtime:"unused" ~build_dir ~ocaml_dir
          ~entry:None ~package_dirty:true ~force:false "Ns" []
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
           (Compiler_process.namespace_task ~runtime:"unused" ~build_dir
              ~ocaml_dir ~entry:None ~package_dirty:false ~force:false "Ns" []))
        "a published namespace map that differs from membership is rebuilt";
      Test_support.write_file
        (Filename.concat ocaml_dir "Ns.mlmap")
        "randjbuildsystem\n";
      check
        (Option.is_some
           (Compiler_process.namespace_task ~runtime:"unused" ~build_dir
              ~ocaml_dir ~entry:None ~package_dirty:false ~force:true "Ns" []))
        "retained namespace publication dirtiness forces a retry")

(* Publishing a module keeps the interface and .cmj that its dependents read
   in the build file store, and releases the files that nothing reads again,
   including the copies it makes. *)
let publication_releases_files_tests _context =
  Test_support.with_temp_dir "rewatch-publication-store-" (fun root ->
      Test_support.write_file
        (Filename.concat root "rescript.json")
        {|{"name":"publish","sources":"src","package-specs":{"module":"commonjs","in-source":true},"suffix":".js"}|};
      let config = Config.load_root root in
      let build_dir = Filename.concat root "lib/bs" in
      let ocaml_dir = Filename.concat root "lib/ocaml" in
      let path = "src/A.res" in
      let source = Filename.concat root path in
      let built extension = Filename.concat build_dir ("src/A." ^ extension) in
      let published extension = Filename.concat ocaml_dir ("A." ^ extension) in
      let output =
        Build_artifacts.generated_js_path config path
          (List.hd config.package_specs)
      in
      File_util.ensure_dir (Filename.concat build_dir "src");
      File_util.ensure_dir ocaml_dir;
      Test_support.write_file source "let value = 1\n";
      Build_file_store.with_build (fun () ->
          List.iter
            (fun extension ->
              Build_file_store.write (built extension) ("built " ^ extension))
            ["cmi"; "cmj"; "cmt"; "ast"];
          Build_file_store.write output "js";
          ignore (Build_file_store.read source);
          ignore
            (Compiler_process.publish ~build_dir ~ocaml_dir ~is_local:true
               ~config ~has_interface:false ~source_kind:Source.Implementation
               path
               Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""});
          List.iter
            (fun (path, kept) ->
              check
                (Option.is_some (Build_file_store.cached path) = kept)
                (path ^ if kept then " is kept" else " is released"))
            [
              (published "cmi", true);
              (published "cmj", true);
              (published "cmt", false);
              (published "res", false);
              (built "cmi", false);
              (built "cmj", false);
              (built "cmt", false);
              (built "ast", false);
              (Filename.concat build_dir path, false);
              (source, false);
              (output, false);
            ]);
      (* An implementation with an interface is compiled with -bs-read-cmi,
         which reads its interface's .cmi in lib/bs again. *)
      let interface_path = "src/B.res" in
      let built_cmi = Filename.concat build_dir "src/B.cmi" in
      Test_support.write_file
        (Filename.concat root interface_path)
        "let b = 1\n";
      Build_file_store.with_build (fun () ->
          List.iter
            (fun extension ->
              Build_file_store.write
                (Filename.concat build_dir ("src/B." ^ extension))
                ("built " ^ extension))
            ["cmi"; "cmj"; "cmt"];
          ignore
            (Compiler_process.publish ~build_dir ~ocaml_dir ~is_local:true
               ~config ~has_interface:true ~source_kind:Source.Implementation
               interface_path
               Process.{status = Unix.WEXITED 0; stdout = ""; stderr = ""});
          check
            (Option.is_none (Build_file_store.cached built_cmi))
            "the interface read again is released");
      check
        (File_util.read_file (Filename.concat ocaml_dir "A.cmt") = "built cmt")
        "the typed tree is published")

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
         "domain_ppx_cancellation" >:: domain_ppx_cancellation_test;
         "escaped_exception" >:: escaped_exception_test;
         "domain_execution" >:: domain_execution_test;
       ]
