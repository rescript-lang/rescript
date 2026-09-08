exception Error of string

let canonical_existing ~message path =
  try
    if Sys.file_exists path then Platform.canonicalize_path path
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
  if Filename.is_relative resolved then Filename.concat cwd resolved else resolved

let sibling_bsc_candidate ~cwd ~executable =
  let executable = absolute_program ~cwd executable in
  let executable =
    canonical_existing
      ~message:(fun path -> "Could not locate current executable " ^ path)
      executable
  in
  Filename.concat (Filename.dirname executable) "bsc.exe"

let bsc () =
  let candidate, message =
    match Sys.getenv_opt "RESCRIPT_BSC_EXE" with
    | Some path ->
      ( path,
        fun missing -> "RESCRIPT_BSC_EXE points to missing path " ^ missing )
    | None ->
      ( sibling_bsc_candidate ~cwd:(Sys.getcwd ())
          ~executable:Sys.executable_name,
        fun missing ->
          "Could not locate bsc next to the ReScript executable at " ^ missing
          ^ "; set RESCRIPT_BSC_EXE to override it" )
  in
  canonical_existing ~message candidate

let runtime ~find_package =
  match Sys.getenv_opt "RESCRIPT_RUNTIME" with
  | Some path ->
    canonical_existing
      ~message:(fun missing -> "RESCRIPT_RUNTIME points to missing path " ^ missing)
      path
  | None -> (
    match find_package "@rescript/runtime" with
    | Some path -> path
    | None ->
      raise
        (Error
           "The rescript runtime package could not be found.\nPlease set \
            RESCRIPT_RUNTIME environment variable or make sure the runtime \
            package is installed."))
