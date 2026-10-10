exception Error of string

let canonical_existing ~message path =
  try
    if File_util.exists path then Platform.canonicalize_path path
    else raise (Error (message path))
  with
  | Unix.Unix_error (error, function_name, argument) ->
    raise
      (Error
         (Printf.sprintf "%s: %s (%s %s)" (message path)
            (Unix.error_message error) function_name argument))
  | Sys_error detail -> raise (Error (message path ^ ": " ^ detail))

let absolute_program ~cwd program =
  let resolved = Platform.resolve_program ~cwd program in
  if Filename.is_relative resolved then Filename.concat cwd resolved
  else resolved

(* Builds use the embedded compiler. The bsc.exe shipped next to the
   executable is still recorded in compiler-info.json, where editor tooling
   looks for bsc.exe and the other platform binaries. *)
let bundled_bsc ~cwd ~executable =
  let executable = absolute_program ~cwd executable in
  let executable =
    canonical_existing
      ~message:(fun path -> "Could not locate current executable " ^ path)
      executable
  in
  Filename.concat (Filename.dirname executable) "bsc.exe"

let runtime ~find_package =
  match Sys.getenv_opt "RESCRIPT_RUNTIME" with
  | Some path ->
    canonical_existing
      ~message:(fun missing ->
        "RESCRIPT_RUNTIME points to missing path " ^ missing)
      path
  | None -> (
    match find_package "@rescript/runtime" with
    | Some path -> path
    | None ->
      raise
        (Error
           "The rescript runtime package could not be found.\n\
            Please set RESCRIPT_RUNTIME environment variable or make sure the \
            runtime package is installed."))
