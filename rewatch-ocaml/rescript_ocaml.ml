let () =
  try
    match Cli.parse Sys.argv with
    | Cli.Help -> print_endline Cli.usage
    | Cli.Version -> print_endline "rescript-ocaml experimental"
    | Cli.Build {folder; prod; features; warn_error} -> Build.run ~seen:[] ~folder ~prod ~features ~warn_error
    | Cli.Watch {folder; prod; features; warn_error} -> Build.watch ~folder ~prod ~features ~warn_error
    | Cli.Format {check; stdin; files} -> Format.run ~check ~stdin ~files
    | Cli.Clean folder -> Build.clean ~folder
  with
  | Cli.Error message
  | Config.Error message
  | Source.Error message
  | Build.Error message
  | Process.Error message ->
    prerr_endline message;
    exit 1
  | Format.Error message ->
    prerr_endline message;
    exit 1
  | Build.Stop_watch -> exit 0
  | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
    prerr_endline (Printexc.to_string exn);
    exit 1
