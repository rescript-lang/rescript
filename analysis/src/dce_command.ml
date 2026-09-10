let command () =
  Reanalyze.Run_config.dce ();
  let dce_config = Reanalyze.Dce_config.current () in
  let pipeline = Reanalyze.Dce_pipeline.create ~config:dce_config in
  Reanalyze.run_analysis ~dce_config ~cmt_root:None ~pipeline ~skip_file:None ();
  let issues = !Reanalyze.Log_.Stats.issues in
  Printf.printf "issues:%d\n" (List.length issues)
