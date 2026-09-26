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

let full_rebuild_keeps_project_compiler_session _context =
  let first = create_full () in
  Build_session.mark_parse_pending first.session "Old.res";
  let compiler_session = Build_session.compiler_session first.session in
  let second =
    Build_attempt.create_full_with_compiler_session ~compiler_session
      ~warning_state:(Warning_state.create ()) ~process_poll:None
      ~progress:(Output.Progress.create ~enabled:false ~color:false)
      ~verbosity:0
  in
  assert_bool "a full rebuild retains the compiler dependency session"
    (Build_session.compiler_session second.session == compiler_session);
  assert_bool "a full rebuild reconstructs the build graph"
    (first.session != second.session);
  assert_equal [] (Build_session.pending_parse_paths second.session)

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
    Build_attempt.{task = Process.task process_job; finish = ignore};
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

let parse_export_preserves_parser_time _context =
  Test_support.with_temp_dir "rewatch-parse-export" (fun root ->
      let attempt = create_full () in
      let source = Test_support.path root "src/A.res" in
      let staged_ast = Test_support.path root "lib/bs/src/A.ast" in
      let published_ast = Test_support.path root "lib/ocaml/A.ast" in
      Test_support.write_file source "let value = 1\n";
      Test_support.write_file staged_ast "parsed AST";
      File_util.ensure_dir (Filename.dirname published_ast);
      Unix.utimes staged_ast 1000. 1000.;
      let compile_assets =
        Compile_assets.create [Filename.dirname published_ast]
      in
      Build_attempt.add_parse_export attempt ~staged_ast ~published_ast ~source
        ~compile_assets;
      Build_attempt.start_parse_exports attempt;
      Build_attempt.finish_parse_exports attempt;
      assert_equal "parsed AST" (File_util.read_file published_ast);
      assert_equal 1000. (Unix.stat published_ast).Unix.st_mtime;
      assert_equal (Some 1000.)
        (Option.map
           (fun entry -> entry.Compile_assets.modified)
           (Compile_assets.ast compile_assets source));
      Build_attempt.finish_parse_exports attempt)

let invalidated_parse_export_is_removed _context =
  Test_support.with_temp_dir "rewatch-parse-invalidation" (fun root ->
      let attempt = create_full () in
      let source = Test_support.path root "src/A.res" in
      let staged_ast = Test_support.path root "lib/bs/src/A.ast" in
      let published_ast = Test_support.path root "lib/ocaml/A.ast" in
      Test_support.write_file source "let value = 1\n";
      Test_support.write_file staged_ast "parsed AST";
      File_util.ensure_dir (Filename.dirname published_ast);
      let compile_assets =
        Compile_assets.create [Filename.dirname published_ast]
      in
      Build_attempt.add_parse_export attempt ~staged_ast ~published_ast ~source
        ~compile_assets;
      Build_attempt.start_parse_exports attempt;
      Build_attempt.invalidate_parse_export attempt ~path:published_ast;
      Build_attempt.finish_parse_exports attempt;
      assert_bool "failed compile must remove a concurrent AST export"
        (not (File_util.exists published_ast));
      assert_equal None (Compile_assets.ast compile_assets source))

let failed_parse_export_forces_reparse _context =
  Test_support.with_temp_dir "rewatch-parse-export-failure" (fun root ->
      let attempt = create_full () in
      let source = Test_support.path root "src/A.res" in
      let staged_ast = Test_support.path root "lib/bs/src/A.ast" in
      let published_ast = Test_support.path root "lib/ocaml/A.ast" in
      Test_support.write_file source "let value = 1\n";
      File_util.ensure_dir (Filename.dirname published_ast);
      let compile_assets =
        Compile_assets.create [Filename.dirname published_ast]
      in
      Test_support.write_file published_ast "old AST";
      Build_attempt.add_parse_export attempt ~staged_ast ~published_ast ~source
        ~compile_assets;
      Build_attempt.start_parse_exports attempt;
      assert_raises
        (Unix.Unix_error (Unix.ENOENT, "open", staged_ast))
        (fun () -> Build_attempt.finish_parse_exports attempt);
      assert_bool "failed export must remove the old AST"
        (not (File_util.exists published_ast));
      assert_equal None (Compile_assets.ast compile_assets source);
      assert_bool "failed export must retry parsing in the retained session"
        (List.mem
           (Platform.normalize_path_for_comparison source)
           (Build_session.pending_parse_paths attempt.session)))

let tests =
  "build_attempt_tests"
  >::: [
         "retained attempts start with fresh attempt state"
         >:: retained_attempts_start_with_fresh_attempt_state;
         "full rebuild keeps project compiler session"
         >:: full_rebuild_keeps_project_compiler_session;
         "output inventory survives without cleanup work"
         >:: output_inventory_survives_without_cleanup_work;
         "pending work is drained once" >:: pending_work_is_drained_once;
         "log finalization runs once" >:: log_finalization_runs_once;
         "parse export preserves parser time"
         >:: parse_export_preserves_parser_time;
         "invalidated parse export is removed"
         >:: invalidated_parse_export_is_removed;
         "failed parse export forces reparse"
         >:: failed_parse_export_forces_reparse;
       ]
