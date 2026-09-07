let () =
  try
    match Cli.parse Sys.argv with
    | Cli.Help command -> print_endline (Cli.command_usage command)
    | Cli.Version -> Printf.printf "rescript %s\n" Cli.version
    | Cli.Build
        {folder; prod; features; warn_error; after_build; filter; clear_screen}
      ->
      ignore clear_screen;
      Build.run ~seen:[] ~folder ~prod ~features ~warn_error ~watch:false
        ~after_build ~filter
    | Cli.Watch {folder; prod; features; warn_error; after_build; filter; clear_screen} -> Build.watch ~folder ~prod ~features ~warn_error ~after_build ~filter ~clear_screen
    | Cli.Format {check; stdin; files} -> Format.run ~check ~stdin ~files
    | Cli.Compiler_args path -> print_endline (Build.compiler_args path)
    | Cli.Clean {folder; prod} -> Build.clean ~seen:[] ~folder ~prod
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
