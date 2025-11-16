let () =
  let heap =
    Filename.concat (Filename.get_temp_dir_name ()) "reanalyze_skip_smoke.rheap"
  in
  (try Sys.remove heap with _ -> ());
  print_endline "Initializing Skip reactive runtime...";
  Reactive.init heap (16 * 1024 * 1024);
  Reactive.exit ();
  (try Sys.remove heap with _ -> ());
  print_endline "Skip reactive runtime init/exit OK."

