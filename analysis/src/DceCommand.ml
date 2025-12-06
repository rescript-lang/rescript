let command () =
  Reanalyze.RunConfig.dce ();
  let dce_config = Reanalyze.DceConfig.current () in
  Reanalyze.runAnalysis ~dce_config ~cmtRoot:None;
  let issues = !Reanalyze.Log_.Stats.issues in
  Printf.printf "issues:%d\n" (List.length issues)
