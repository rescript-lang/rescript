open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let process_job () =
  let executable = Unix.realpath Sys.executable_name in
  Process.
    {program = executable; args = ["--process-result"; ""; ""; "0"]; cwd = "."}

let wait_for_release_job root =
  let executable = Unix.realpath Sys.executable_name in
  Process.{program = executable; args = ["--wait-for-release"; root]; cwd = "."}

let with_temp_dir = Test_support.with_temp_dir "rewatch-scheduler-"

let with_single_domain action =
  let previous = Sys.getenv_opt "REWATCH_COMPILER_DOMAINS" in
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv "REWATCH_COMPILER_DOMAINS" value
      | None -> Test_support.unsetenv "REWATCH_COMPILER_DOMAINS")
    (fun () ->
      Unix.putenv "REWATCH_COMPILER_DOMAINS" "1";
      action ())

let source name =
  Source.
    {
      name;
      implementation = Filename.concat "src" (name ^ ".res");
      interface = None;
      is_dev = false;
    }

let tests =
  "compiler_scheduler_tests" >:: fun _context ->
  with_single_domain (fun () ->
      with_temp_dir (fun root ->
          let ocaml_dir = Build_artifacts.lib_path root "ocaml" in
          File_util.ensure_dir ocaml_dir;
          File_util.ensure_dir (Build_artifacts.lib_path root "bs");
          Compiler_log.initialize root;
          let a_source = source "A" in
          let b_source = source "B" in
          let c_source = source "C" in
          let build_state = Build_state.create 3 in
          List.iter
            (fun key ->
              Build_state.add build_state ~key ~kind:Build_state.Source_module
                ~last_compiled_cmi:(Some 0.) ~last_compiled_cmt:(Some 0.))
            ["A"; "B"; "C"];
          Build_state.set_dependencies build_state ~key:"B" ["A"];
          let a_state = Build_state.find_exn build_state "A" in
          let b_state = Build_state.find_exn build_state "B" in
          let c_state = Build_state.find_exn build_state "C" in
          a_state.compile_dirty <- true;
          c_state.compile_dirty <- true;
          let a_cmi = Filename.concat ocaml_dir "A.cmi" in
          let b_cmi = Filename.concat ocaml_dir "B.cmi" in
          let c_cmi = Filename.concat ocaml_dir "C.cmi" in
          let a_ast = Filename.concat ocaml_dir "A.ast" in
          write_file a_cmi "old interface";
          write_file b_cmi "dependent interface";
          Test_support.write_ast_header a_ast ~dependencies:[]
            ~source:"src/A.res";
          let compile_assets = Compile_assets.create [ocaml_dir] in
          let fail_a_publication = ref true in
          let b_compilations = ref 0 in
          let c_compilations = ref 0 in
          let c_candidates_built = ref 0 in
          let scheduler_thread = Thread.id (Thread.self ()) in
          let output_inventory = Hashtbl.create 1 in
          let make_scheduled key source state cmi_path =
            Compiler_scheduler.create ~key
              ~dependencies:state.Build_state.dependencies ~source ~state
              ~cmi_path
              ~prepare:(fun () -> ())
              ~compile:(fun ~source_kind:_ _path ->
                if key = "B" then incr b_compilations;
                if key = "C" then (
                  check (not !fail_a_publication)
                    "independent work starts after A's publication failure";
                  incr c_compilations);
                Process.task (process_job ()))
              ~publish:(fun ~source_kind:_ _path _result ->
                write_file cmi_path
                  (if key = "A" then "new interface" else "dependent interface");
                if key = "A" && !fail_a_publication then (
                  fail_a_publication := false;
                  raise (Failure "later publication failed"));
                Compiler_scheduler.
                  {
                    stderr = "";
                    cmi_change =
                      (if key = "A" then Cmi_changed else Cmi_unchanged);
                  })
              ~record_published_outputs:(fun ~source_kind:_ _path ->
                check
                  (Thread.id (Thread.self ()) = scheduler_thread)
                  "the scheduler thread owns the shared output inventory";
                for index = 0 to 127 do
                  Hashtbl.replace output_inventory
                    (Printf.sprintf "%s-%d" key index)
                    ()
                done)
              ~post_build:(fun _ -> [])
              ~package_root:root ~is_local:true
              ~mark_warning:(fun _ -> ())
          in
          let run () =
            let candidates =
              [
                ("A", a_source, a_state, a_cmi, fun () -> ());
                ("B", b_source, b_state, b_cmi, fun () -> ());
                ( "C",
                  c_source,
                  c_state,
                  c_cmi,
                  fun () -> incr c_candidates_built );
              ]
              |> List.map (fun (key, source, state, cmi_path, on_make) ->
                  Compiler_scheduler.candidate ~key ~state ~warning_paths:[]
                    ~make:(fun () ->
                      on_make ();
                      make_scheduled key source state cmi_path))
            in
            Compiler_scheduler.run ~poll:None
              ~warning_state:(Warning_state.create ()) ~compile_assets
              ~build_state ~candidates
              ~mark_compiled:(fun () -> ())
              ~mark_had_warnings:(fun () -> ())
              ~progress:(Output.Progress.create ~enabled:false ~color:false)
              ~compile_step:"1/1" ~namespace_count:0 ~verbosity:0
          in
          let first_failed =
            try
              run ();
              false
            with Compiler_scheduler.Build_failure message ->
              Test_support.contains_text message "later publication failed"
          in
          check first_failed "a later publication failure is reported normally";
          check
            (not (Sys.file_exists a_ast))
            "failed completion invalidates durable freshness for a later \
             process";
          check b_state.compile_dirty
            "a published CMI change survives a later publication failure";
          check (!b_compilations = 0)
            "a dependent is not compiled during its dependency's failed attempt";
          check (!c_compilations = 1)
            "independent work completes after another module fails";
          run ();
          check
            (!b_compilations = 1 && not b_state.compile_dirty)
            "the retained dependent recompiles after publication recovers";
          check
            (Hashtbl.length output_inventory = 384)
            "scheduler-owned publication inventory updates survive table growth";
          check
            (!c_candidates_built = 1 && !c_compilations = 1)
            "a completed independent module remains fresh on retry";
          let interrupted_state = Build_state.create 1 in
          Build_state.add interrupted_state ~key:"Interrupted"
            ~kind:Build_state.Source_module ~last_compiled_cmi:(Some 0.)
            ~last_compiled_cmt:(Some 0.);
          let interrupted_module =
            Build_state.find_exn interrupted_state "Interrupted"
          in
          interrupted_module.compile_dirty <- true;
          let interrupted_ast = Filename.concat ocaml_dir "Interrupted.ast" in
          let interrupted_cmi = Filename.concat ocaml_dir "Interrupted.cmi" in
          let interrupted_hook_root = Filename.concat root "interrupted-hook" in
          Unix.mkdir interrupted_hook_root 0o755;
          Test_support.write_ast_header interrupted_ast ~dependencies:[]
            ~source:"src/Interrupted.res";
          let interrupted_candidate =
            Compiler_scheduler.candidate ~key:"Interrupted"
              ~state:interrupted_module ~warning_paths:[] ~make:(fun () ->
                Compiler_scheduler.create ~key:"Interrupted" ~dependencies:[]
                  ~source:(source "Interrupted") ~state:interrupted_module
                  ~cmi_path:interrupted_cmi
                  ~prepare:(fun () -> ())
                  ~compile:(fun ~source_kind:_ _path ->
                    Process.task (process_job ()))
                  ~publish:(fun ~source_kind:_ _path _result ->
                    write_file interrupted_cmi "published CMI";
                    Compiler_scheduler.{stderr = ""; cmi_change = Cmi_changed})
                  ~record_published_outputs:(fun ~source_kind:_ _path -> ())
                  ~post_build:(fun output ->
                    [
                      Compiler_scheduler.
                        {
                          output;
                          task =
                            Process.task
                              (wait_for_release_job interrupted_hook_root);
                        };
                    ])
                  ~package_root:root ~is_local:true
                  ~mark_warning:(fun _ -> ()))
          in
          let hook_interrupted =
            try
              Compiler_scheduler.run
                ~poll:
                  (Some
                     (fun () ->
                       if
                         Sys.file_exists
                           (Filename.concat interrupted_hook_root
                              "child-started")
                       then raise Exit))
                ~warning_state:(Warning_state.create ())
                ~compile_assets:(Compile_assets.create [ocaml_dir])
                ~build_state:interrupted_state
                ~candidates:[interrupted_candidate]
                ~mark_compiled:(fun () -> ())
                ~mark_had_warnings:(fun () -> ())
                ~progress:(Output.Progress.create ~enabled:false ~color:false)
                ~compile_step:"1/1" ~namespace_count:0 ~verbosity:0;
              false
            with Exit -> true
          in
          check hook_interrupted "a running post-build hook can be interrupted";
          check
            (not (Sys.file_exists interrupted_ast))
            "interrupted post-build invalidates persistent compiler freshness";
          let marker = Failure "copy after CMI failed" in
          match
            Compiler_scheduler.capture_publication (fun () ->
                raise
                  (Compiler_scheduler.Publication_failure
                     (marker, Compiler_scheduler.Cmi_changed)))
          with
          | Compiler_scheduler.Failed_after_cmi_publication
              {error; cmi_change = Compiler_scheduler.Cmi_changed}
            when error == marker ->
            ()
          | _ ->
            assert_failure
              "publication capture must retain a CMI change across later copy \
               failure"))
