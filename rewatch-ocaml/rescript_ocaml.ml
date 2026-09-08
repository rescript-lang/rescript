let run = function
  | Cli.Build
      {
        folder;
        prod;
        features;
        warn_error;
        after_build;
        filter;
        clear_screen;
        no_timing;
      }
    ->
    ignore clear_screen;
    Build.run ~seen:[] ~folder ~prod ~features ~warn_error ~watch:false
      ~after_build ~filter ~no_timing
  | Cli.Watch
      {
        folder;
        prod;
        features;
        warn_error;
        after_build;
        filter;
        clear_screen;
        no_timing;
      }
    ->
    ignore no_timing;
    Build.watch ~folder ~prod ~features ~warn_error ~after_build ~filter
      ~clear_screen
  | Cli.Format {check; stdin; files} -> Format.run ~check ~stdin ~files
  | Cli.Compiler_args path -> print_endline (Build.compiler_args path)
  | Cli.Clean {folder; prod} -> Build.clean ~seen:[] ~folder ~prod

let () =
  try
    match Cli.eval Sys.argv with
    | Cli.Run command -> run command
    | Cli.Exit code -> exit code
  with
  | Config.Error message
  | Source.Error message
  | Build.Error message
  | Process.Error message
  | Format.Error message ->
    prerr_endline message;
    exit 1
  | Build.Stop_watch -> exit 0
  | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
    prerr_endline (Printexc.to_string exn);
    exit 1
