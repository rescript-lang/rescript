open Analysis

type run_result = {output: string; has_findings: bool}

let run ?config_path ?(json = false) target =
  let target =
    if Filename.is_relative target then Filename.concat (Sys.getcwd ()) target
    else target
  in
  if not (Files.exists target) then
    Error ("error: no such file or directory: " ^ target)
  else
    let target = Unix.realpath target in
    Lint_config.load ?config_path target
    |> Result.map (fun config ->
           let {Lint_analysis.display_base; findings} =
             Lint_analysis.analyze_target ~config target
           in
           {
             output = Lint_output.render ~json ~display_base findings;
             has_findings = findings <> [];
           })
