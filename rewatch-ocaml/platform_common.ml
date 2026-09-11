let resolve_program ~path_separator ~executable_extensions ~search_directories
    ~executable_is_usable ~cwd program =
  if (not (Filename.is_implicit program)) || Filename.dirname program <> "."
  then program
  else
    let path_directories =
      Sys.getenv_opt "PATH" |> Option.value ~default:""
      |> String.split_on_char path_separator
    in
    search_directories ~cwd path_directories
    |> List.find_map (fun directory ->
        let directory =
          let directory = String.trim directory in
          let length = String.length directory in
          let directory =
            if
              length >= 2 && directory.[0] = '"' && directory.[length - 1] = '"'
            then String.sub directory 1 (length - 2)
            else directory
          in
          if directory = "" then cwd
          else if Filename.is_relative directory then
            Filename.concat cwd directory
          else directory
        in
        executable_extensions ~program
        |> List.find_map (fun extension ->
            let candidate = Filename.concat directory (program ^ extension) in
            if executable_is_usable candidate then Some candidate else None))
    |> Option.value ~default:program

let process_is_active ~probe value =
  try
    let pid = int_of_string value in
    (* PID zero names a process group on Unix and is not a usable child-process
       identity on Windows, so it can never prove ownership of a build lock. *)
    if pid = 0 then false else probe pid
  with
  | Failure _ | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (Unix.EPERM, _, _) -> true
