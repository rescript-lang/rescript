(* Signal handlers can run on any domain, including between spawning a child
   and registering its cleanup owner. Recording the request and raising from
   the command's poll point keeps unwinding on the domain that owns scheduler,
   lock, and temporary-output cleanup. *)
let with_termination_handlers action =
  let requested_exit = Atomic.make 0 in
  let interrupt signal =
    let exit_code =
      if signal = Sys.sigint then 130
      else if signal = Sys.sigterm then 143
      else 1
    in
    ignore (Atomic.compare_and_set requested_exit 0 exit_code)
  in
  let poll () =
    let exit_code = Atomic.get requested_exit in
    if exit_code <> 0 then raise (Process.Interrupted exit_code)
  in
  let previous_sigint = Sys.signal Sys.sigint (Sys.Signal_handle interrupt) in
  let result =
    Fun.protect
      (fun () ->
        let previous_sigterm =
          Sys.signal Sys.sigterm (Sys.Signal_handle interrupt)
        in
        Fun.protect
          (fun () -> action ~poll)
          ~finally:(fun () -> ignore (Sys.signal Sys.sigterm previous_sigterm)))
      ~finally:(fun () -> ignore (Sys.signal Sys.sigint previous_sigint))
  in
  poll ();
  result

let run_command ~poll = function
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
    Build.run ~poll ~verbosity ~folder ~prod ~features ~warn_error ~after_build
      ~filter ~no_timing
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
  | Cli.Format (Cli.Format_stdin extension) ->
    Format.format_stdin ~poll extension
  | Cli.Format (Cli.Format_files {check; paths}) ->
    Format.run_files ~poll ~check paths
  | Cli.Compiler_args path -> print_endline (Compiler_args_command.run path)
  | Cli.Clean {verbosity; folder; prod} ->
    Clean.run ~poll ~verbosity ~folder ~prod

let run = function
  | Cli.Watch _ as command -> run_command ~poll:(fun () -> ()) command
  | command ->
    with_termination_handlers (fun ~poll -> run_command ~poll command)

let () =
  Platform.configure_standard_streams ();
  try
    match Cli.eval Sys.argv with
    | Cli.Run command -> run command
    | Cli.Exit code -> exit code
  with
  | Project_context.Package_error message ->
    prerr_endline message;
    exit 2
  | Config.Error message
  | Source.Error message
  | Project_context.Error message
  | Process.Error message
  | Format.Error message ->
    prerr_endline message;
    exit 1
  | Watcher.Stop -> exit 0
  | Process.Interrupted exit_code -> exit exit_code
  | (Sys_error _ as exn) | (Unix.Unix_error _ as exn) ->
    prerr_endline (Printexc.to_string exn);
    exit 1
