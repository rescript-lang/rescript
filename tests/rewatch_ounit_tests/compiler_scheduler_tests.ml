open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let process_job () =
  let executable = Unix.realpath Sys.executable_name in
  Process.
    {program = executable; args = ["--process-result"; ""; ""; "0"]; cwd = "."}

let with_temp_dir = Test_support.with_temp_dir "rewatch-scheduler-"

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
      let a_cmi = Filename.concat ocaml_dir "A.cmi" in
      let b_cmi = Filename.concat ocaml_dir "B.cmi" in
      let c_cmi = Filename.concat ocaml_dir "C.cmi" in
      let a_ast = Filename.concat ocaml_dir "A.ast" in
      write_file a_cmi "old interface";
      write_file b_cmi "dependent interface";
      write_file a_ast "published AST";
      let compile_assets = Compile_assets.create [ocaml_dir] in
      let fail_a_publication = ref true in
      let b_compilations = ref 0 in
      let c_candidates_built = ref 0 in
      let scheduler_thread = Thread.id (Thread.self ()) in
      let output_inventory = Hashtbl.create 1 in
      let make_scheduled key source state cmi_path =
        Compiler_scheduler.create ~key
          ~dependencies:state.Build_state.dependencies ~source ~state ~cmi_path
          ~prepare:(fun () -> ())
          ~compile:(fun ~is_interface:_ _path ->
            if key = "B" then incr b_compilations;
            process_job ())
          ~publish:(fun ~is_interface:_ _path _result ->
            write_file cmi_path
              (if key = "A" then "new interface" else "dependent interface");
            if key = "A" && !fail_a_publication then (
              fail_a_publication := false;
              raise (Failure "later publication failed"));
            Compiler_scheduler.
              {
                stderr = "";
                cmi_change = (if key = "A" then Cmi_changed else Cmi_unchanged);
              })
          ~record_published_outputs:(fun ~is_interface:_ _path ->
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
            ("C", c_source, c_state, c_cmi, fun () -> incr c_candidates_built);
          ]
          |> List.map (fun (key, source, state, cmi_path, on_make) ->
              Compiler_scheduler.candidate ~key ~state ~warning_paths:[]
                ~make:(fun () ->
                  on_make ();
                  make_scheduled key source state cmi_path))
        in
        Compiler_scheduler.run ~poll:None
          ~warning_state:(Warning_state.create ()) ~compile_assets ~build_state
          ~candidates
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
        "failed completion invalidates durable freshness for a later process";
      check b_state.compile_dirty
        "a published CMI change survives a later publication failure";
      check (!b_compilations = 0)
        "a dependent is not compiled during its dependency's failed attempt";
      run ();
      check
        (!b_compilations = 1 && not b_state.compile_dirty)
        "the retained dependent recompiles after publication recovers";
      check
        (Hashtbl.length output_inventory = 256)
        "scheduler-owned publication inventory updates survive table growth";
      check (!c_candidates_built = 0)
        "an unaffected module does not construct compiler callbacks";
      let abort_state = Build_state.create 4 in
      List.iter
        (fun key ->
          Build_state.add abort_state ~key ~kind:Build_state.Source_module
            ~last_compiled_cmi:(Some 0.) ~last_compiled_cmt:(Some 0.))
        ["AbortA"; "AbortB"; "AbortC"; "AbortD"];
      Build_state.set_dependencies abort_state ~key:"AbortB" ["AbortC"];
      Build_state.set_dependencies abort_state ~key:"AbortD" ["AbortA"];
      let abort_a = Build_state.find_exn abort_state "AbortA" in
      let abort_c = Build_state.find_exn abort_state "AbortC" in
      abort_a.compile_dirty <- true;
      abort_c.compile_dirty <- true;
      let published_marker = Filename.concat root "abort-a-published" in
      let release_marker = Filename.concat root "abort-a-release" in
      let rec wait_for path remaining =
        if Sys.file_exists path then ()
        else if remaining = 0 then failwith ("timed out waiting for " ^ path)
        else (
          Thread.delay 0.001;
          wait_for path (remaining - 1))
      in
      let abort_candidate key dependencies prepare publish =
        let state = Build_state.find_exn abort_state key in
        Compiler_scheduler.candidate ~key ~state ~warning_paths:[]
          ~make:(fun () ->
            Compiler_scheduler.create ~key ~dependencies ~source:(source key)
              ~state
              ~cmi_path:(Filename.concat ocaml_dir (key ^ ".cmi"))
              ~prepare
              ~compile:(fun ~is_interface:_ _path -> process_job ())
              ~publish
              ~record_published_outputs:(fun ~is_interface:_ _path -> ())
              ~post_build:(fun _ -> [])
              ~package_root:root ~is_local:true
              ~mark_warning:(fun _ -> ()))
      in
      let abort_candidates =
        [
          abort_candidate "AbortA" []
            (fun () -> ())
            (fun ~is_interface:_ _path _result ->
              write_file (Filename.concat ocaml_dir "AbortA.cmi") "new A";
              write_file published_marker "";
              wait_for release_marker 5000;
              Compiler_scheduler.{stderr = ""; cmi_change = Cmi_changed});
          abort_candidate "AbortC" []
            (fun () -> ())
            (fun ~is_interface:_ _path _result ->
              wait_for published_marker 5000;
              write_file (Filename.concat ocaml_dir "AbortC.cmi") "new C";
              Compiler_scheduler.{stderr = ""; cmi_change = Cmi_changed});
          abort_candidate "AbortB" ["AbortC"]
            (fun () ->
              write_file release_marker "";
              raise Exit)
            (fun ~is_interface:_ _path _result -> assert false);
          abort_candidate "AbortD" ["AbortA"]
            (fun () -> ())
            (fun ~is_interface:_ _path _result -> assert false);
        ]
      in
      let aborted =
        try
          Compiler_scheduler.run ~poll:None
            ~warning_state:(Warning_state.create ())
            ~compile_assets:(Compile_assets.create [ocaml_dir])
            ~build_state:abort_state ~candidates:abort_candidates
            ~mark_compiled:(fun () -> ())
            ~mark_had_warnings:(fun () -> ())
            ~progress:(Output.Progress.create ~enabled:false ~color:false)
            ~compile_step:"1/1" ~namespace_count:0 ~verbosity:0;
          false
        with Exit -> true
      in
      check aborted "an operational scheduler failure aborts the attempt";
      check (Build_state.find_exn abort_state "AbortD").compile_dirty
        "CMI publication completed during abort still invalidates dependents";
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
           failure")
