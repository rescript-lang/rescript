exception Error = Project_context.Error

let run ?poll ~root command =
  let program, args =
    match Str.split (Str.regexp "[ \t\r\n]+") command with
    | program :: args -> (program, args)
    | [] -> raise (Error "--after-build command cannot be empty")
  in
  let result =
    try Process.run_streaming ?poll ~cwd:root program args with
    | Process.Error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Sys_error message ->
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s" command
              message))
    | Unix.Unix_error (error, operation, argument) ->
      let target = if argument = "" then program else argument in
      raise
        (Error
           (Printf.sprintf "Could not run --after-build command %S: %s (%s %s)"
              command (Unix.error_message error) operation target))
  in
  if not (Process.succeeded result) then (
    raise
      (Error
         (Printf.sprintf "--after-build command failed with %s"
            (Process.status_string result.status))))
