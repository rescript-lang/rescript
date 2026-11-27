exception Error = Project_context.Error

(* The command runs through the shell (/bin/sh -c, or cmd.exe /C on Windows),
   so quoting, pipes and an exit status work as typed, and the shell reports a
   missing program. *)
let run ?poll ~root command =
  if String.trim command = "" then
    raise (Error "--after-build command cannot be empty");
  let {Platform.program; args; env = _} = Platform.shell_command command in
  let result =
    try Process.run_streaming ?poll ~cwd:root program args with
    | Process.Error message | Sys_error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Unix.Unix_error (error, _, _) ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              (Unix.error_message error)))
  in
  if not (Process.succeeded result) then
    raise
      (Error
         (Printf.sprintf "--after-build command failed with %s"
            (Process.status_string result.status)))
