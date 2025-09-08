let command () =
  Reanalyze.RunConfig.dce ();
  Reanalyze.runAnalysis ~cmtRoot:None;
  let issues = !Reanalyze.Log_.Stats.issues in
  Printf.printf "issues:%d\n" (List.length issues)
