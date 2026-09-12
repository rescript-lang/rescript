open OUnit2

let create_full () =
  Build_attempt.create_full ~warning_state:(Warning_state.create ())
    ~process_poll:None
    ~progress:(Output.Progress.create ~enabled:false ~color:false)
    ~verbosity:0

let retained_attempts_start_with_fresh_attempt_state _context =
  let first = create_full () in
  first.diagnostics <- ["old diagnostic"];
  first.failure <- Some "old failure";
  Build_attempt.register_cleanup first ignore;
  let second =
    Build_attempt.create_retained ~session:first.session ~process_poll:None
      ~progress:(Output.Progress.create ~enabled:false ~color:false)
      ~verbosity:0
  in
  assert_equal Build_attempt.Initialize_freshness second.freshness_mode;
  assert_equal [] second.diagnostics;
  assert_equal None second.failure;
  assert_equal [] (Build_attempt.take_cleanup second).actions

let output_inventory_survives_without_cleanup_work _context =
  let first = create_full () in
  let outputs = Hashtbl.create 1 in
  Hashtbl.add outputs "src/A.js" ();
  Build_attempt.set_cleanup_result first "root"
    Build_artifacts.
      {
        removed_modules = ["Old"];
        previous_ast_count = 2;
        deferred_artifacts = ["old.cmi"];
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
  assert_equal [] cleanup.deferred_artifacts;
  assert_bool "the stable output inventory is retained"
    (Hashtbl.mem cleanup.present_public_outputs "src/A.js")

let tests =
  "build_attempt_tests"
  >::: [
         "retained attempts start with fresh attempt state"
         >:: retained_attempts_start_with_fresh_attempt_state;
         "output inventory survives without cleanup work"
         >:: output_inventory_survives_without_cleanup_work;
       ]
