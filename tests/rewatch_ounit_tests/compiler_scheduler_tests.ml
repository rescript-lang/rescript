open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let process_job () =
  let executable = Unix.realpath Sys.executable_name in
  Process.
    {program = executable; args = ["--process-result"; ""; ""; "0"]; cwd = "."}

let with_temp_dir run =
  let root = Filename.temp_file "rewatch-scheduler-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () -> run root)

let source name =
  Source.
    {
      name;
      implementation = Filename.concat "src" (name ^ ".res");
      interface = None;
      is_dev = false;
      feature = None;
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
          Build_state.add build_state ~key ~package_name:"scheduler-test"
            ~package_root:root ~kind:Build_state.Source_module
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
      write_file a_cmi "old interface";
      write_file b_cmi "dependent interface";
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
        "an unaffected module does not construct compiler callbacks")
