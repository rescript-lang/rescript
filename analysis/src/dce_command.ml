let command () =
  Reanalyze.Run_config.dce ();
  let dce_config = Reanalyze.Dce_config.current () in
  Reanalyze.run_analysis ~dce_config ~cmt_root:None ~reactive_collection:None
    ~reactive_merge:None ~reactive_liveness:None ~reactive_solver:None
    ~skip_file:None ();
  let issues = !Reanalyze.Log_.Stats.issues in
  Printf.printf "issues:%d\n" (List.length issues)
