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
  let root = Filename.temp_file "rewatch-watcher-lifecycle-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
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
          Watcher.run ~root ~prod:false ~clear_screen:false ~show_progress:true
            ~build:(fun ~poll:_ -> raise Exit);
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
      Watcher.run ~root ~prod:false ~clear_screen:false ~show_progress:true
        ~build:(fun ~poll ->
          poll ();
          Sys.remove lock);
      check
        (signal_is_ignored Sys.sigint)
        "SIGINT handler is restored after normal shutdown";
      check
        (signal_is_ignored Sys.sigterm)
        "SIGTERM handler is restored after normal shutdown")
