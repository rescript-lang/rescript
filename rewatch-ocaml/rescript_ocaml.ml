exception Interrupted of int

(* Raising on the main thread lets command-scoped process, lock, and temporary
   output owners unwind before the conventional shell exit status is returned.
   Watch mode uses a cooperative flag instead because libuv callbacks must not
   be unwound by an asynchronous exception. *)
let with_termination_handlers action =
  let interrupt signal =
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    Sys.set_signal Sys.sigterm Sys.Signal_ignore;
    raise
      (Interrupted
         (if signal = Sys.sigint then 130
          else if signal = Sys.sigterm then 143
          else 1))
  in
  let previous_sigint = Sys.signal Sys.sigint (Sys.Signal_handle interrupt) in
  Fun.protect
    (fun () ->
      let previous_sigterm =
        Sys.signal Sys.sigterm (Sys.Signal_handle interrupt)
      in
      Fun.protect action ~finally:(fun () ->
          ignore (Sys.signal Sys.sigterm previous_sigterm)))
    ~finally:(fun () -> ignore (Sys.signal Sys.sigint previous_sigint))

let run_command = function
  | Cli.Build
      {
        verbosity;
        folder;
        prod;
        features;
        warn_error;
        after_build;
        filter;
        clear_screen;
        no_timing;
      } ->
    ignore clear_screen;
    Build.run ~seen:[] ~verbosity ~folder ~prod ~features ~warn_error
      ~watch:false ~after_build ~filter ~no_timing
  | Cli.Watch
      {
        verbosity;
        folder;
        prod;
        features;
        warn_error;
        after_build;
        filter;
        clear_screen;
        no_timing;
      } ->
    ignore no_timing;
    Build.watch ~verbosity ~folder ~prod ~features ~warn_error ~after_build
      ~filter ~clear_screen
  | Cli.Format (Cli.Format_stdin extension) -> Format.format_stdin extension
  | Cli.Format (Cli.Format_files {check; paths}) ->
    Format.run_files ~check paths
  | Cli.Compiler_args path -> print_endline (Build.compiler_args path)
  | Cli.Clean {verbosity; folder; prod} ->
    Build.clean ~seen:[] ~verbosity ~folder ~prod

let run = function
  | Cli.Watch _ as command -> run_command command
  | command -> with_termination_handlers (fun () -> run_command command)

let () =
  try
    match Cli.eval Sys.argv with
    | Cli.Run command -> run command
    | Cli.Exit code -> exit code
  with
  | Build.Package_error message ->
    prerr_endline message;
    exit 2
  | Config.Error message
  | Source.Error message
  | Build.Error message
  | Process.Error message
  | Format.Error message ->
    prerr_endline message;
    exit 1
  | Build.Stop_watch -> exit 0
  | Interrupted exit_code -> exit exit_code
  | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
    prerr_endline (Printexc.to_string exn);
    exit 1
