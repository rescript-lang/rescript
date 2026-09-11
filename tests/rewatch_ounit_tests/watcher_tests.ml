open OUnit2

let check condition message = assert_bool message condition

let signal_is_ignored signal =
  match Sys.signal signal Sys.Signal_ignore with
  | Sys.Signal_ignore -> true
  | previous ->
    ignore (Sys.signal signal previous);
    false

let tests =
  "watcher_tests" >:: fun _context ->
  let snapshot path digest = [(path, 1., 1, digest)] in
  let previous = snapshot "A.res" "old-a" @ snapshot "B.res" "old-b" in
  let trigger = snapshot "A.res" "new-a" @ snapshot "B.res" "old-b" in
  let before_build = snapshot "A.res" "new-a" @ snapshot "B.res" "new-b" in
  let polling_changes =
    Watcher.For_test.polling_build_changes ~previous ~trigger ~before_build
  in
  check
    (List.map (fun (change : Watcher.change) -> change.path) polling_changes
    = ["A.res"; "B.res"])
    "polling includes edits that arrive after the triggering snapshot";
  check
    (Watcher.For_test.changes_are_incremental polling_changes)
    "polling source modifications use incremental presentation";
  check
    (not
       (Watcher.For_test.changes_are_incremental
          [Watcher.{path = "rescript.json"; kind = Modified}]))
    "polling control-file changes use full-rebuild presentation";
  let root = Filename.temp_file "rewatch-watcher-lifecycle-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  let config_path = Filename.concat root "rescript.json" in
  let channel = open_out config_path in
  output_string channel {|{"name":"watcher-lifecycle","sources":[]}|};
  close_out channel;
  let original_sigint = Sys.signal Sys.sigint Sys.Signal_ignore in
  let original_sigterm = Sys.signal Sys.sigterm Sys.Signal_ignore in
  let restore () =
    ignore (Sys.signal Sys.sigterm original_sigterm);
    ignore (Sys.signal Sys.sigint original_sigint);
    File_util.remove_tree root
  in
  Fun.protect ~finally:restore (fun () ->
      let raised =
        try
          Watcher.run ~root ~prod:false ~features:None ~filter:None
            ~clear_screen:false ~show_progress:true ~verbosity:0
            ~build:(fun ~poll:_ ~changes:_ -> raise Exit);
          false
        with Exit -> true
      in
      check raised "build callback exception is preserved";
      let lock = Filename.concat root "lib/watch.lock" in
      check
        (not (Sys.file_exists lock))
        "watch lock is released when setup or the initial build fails";
      check
        (signal_is_ignored Sys.sigint)
        "SIGINT handler is restored after failure";
      check
        (signal_is_ignored Sys.sigterm)
        "SIGTERM handler is restored after failure";
      Watcher.run ~root ~prod:false ~features:None ~filter:None
        ~clear_screen:false ~show_progress:true ~verbosity:0
        ~build:(fun ~poll ~changes:_ ->
          poll ();
          Sys.remove lock;
          Watcher.Succeeded);
      check
        (signal_is_ignored Sys.sigint)
        "SIGINT handler is restored after normal shutdown";
      check
        (signal_is_ignored Sys.sigterm)
        "SIGTERM handler is restored after normal shutdown")
