open OUnit2

let create_full () =
  Build_attempt.create_full ~warning_state:(Warning_state.create ())
    ~process_poll:None
    ~progress:(Output.Progress.create ~enabled:false ~color:false)
    ~verbosity:0

let retained_attempts_start_with_fresh_attempt_state _context =
  let first = create_full () in
  first.diagnostics <- ["old diagnostic"];
  let cleanup_count = ref 0 in
  Build_attempt.register_cleanup first (fun () -> incr cleanup_count);
  let second =
    Build_attempt.create_retained ~session:first.session ~process_poll:None
      ~progress:(Output.Progress.create ~enabled:false ~color:false)
      ~verbosity:0
  in
  assert_equal Build_attempt.Initialize_freshness second.freshness_mode;
  assert_equal [] second.diagnostics;
  Build_attempt.cleanup_artifacts second;
  assert_equal 0 !cleanup_count;
  Build_attempt.cleanup_artifacts first;
  assert_equal 1 !cleanup_count

let output_inventory_survives_without_cleanup_work _context =
  let first = create_full () in
  let outputs = Hashtbl.create 1 in
  Hashtbl.add outputs "src/A.js" ();
  Build_attempt.set_cleanup_result first "root"
    Build_artifacts.
      {
        removed_modules = ["Old"];
        previous_ast_count = 2;
        present_public_outputs = outputs;
      };
  let second =
    Build_attempt.create_retained ~session:first.session ~process_poll:None
      ~progress:(Output.Progress.create ~enabled:false ~color:false)
      ~verbosity:0
  in
  let cleanup = Build_attempt.find_cleanup_result second "root" |> Option.get in
  assert_equal [] cleanup.removed_modules;
  assert_equal 0 cleanup.previous_ast_count;
  assert_bool "the stable output inventory is retained"
    (Hashtbl.mem cleanup.present_public_outputs "src/A.js")

let pending_work_is_drained_once _context =
  let attempt = create_full () in
  let process_job =
    Process.{program = "unused"; args = []; cwd = Sys.getcwd ()}
  in
  Build_attempt.add_namespace_job attempt
    Build_attempt.{job = process_job; finish = ignore};
  let build_state = Build_state.create 1 in
  Build_state.add build_state ~key:"A" ~kind:Build_state.Source_module
    ~last_compiled_cmi:None ~last_compiled_cmt:None;
  let candidate =
    Compiler_scheduler.candidate ~key:"A"
      ~state:(Build_state.find_exn build_state "A") ~warning_paths:[]
      ~make:(fun () -> failwith "unused")
  in
  Build_attempt.add_compile_candidates attempt [candidate];
  assert_equal 1 (List.length (Build_attempt.take_namespace_jobs attempt));
  assert_equal [] (Build_attempt.take_namespace_jobs attempt);
  assert_equal 1 (List.length (Build_attempt.take_compile_candidates attempt));
  assert_equal [] (Build_attempt.take_compile_candidates attempt)

let log_finalization_runs_once _context =
  Test_support.with_temp_dir "rewatch-attempt-log" (fun root ->
      let attempt = create_full () in
      File_util.ensure_dir (Build_artifacts.lib_path root "ocaml");
      Compiler_log.initialize root;
      Build_attempt.mark_log_initialized attempt root;
      Build_attempt.finalize_logs attempt;
      Build_attempt.finalize_logs attempt;
      let contents =
        File_util.read_file
          (Filename.concat (Build_artifacts.lib_path root "bs") ".compiler.log")
      in
      let done_lines =
        contents |> String.split_on_char '\n'
        |> List.filter (String.starts_with ~prefix:"#Done(")
      in
      assert_equal 1 (List.length done_lines))

let tests =
  "build_attempt_tests"
  >::: [
         "retained attempts start with fresh attempt state"
         >:: retained_attempts_start_with_fresh_attempt_state;
         "output inventory survives without cleanup work"
         >:: output_inventory_survives_without_cleanup_work;
         "pending work is drained once" >:: pending_work_is_drained_once;
         "log finalization runs once" >:: log_finalization_runs_once;
       ]
