(* The current directory is not searched: a project file named like a tool
   (cmd.exe, node) must not run in its place. A program in the current
   directory needs an explicit path. *)
let resolve_program ~path_separator ~executable_extensions ~normalize_directory
    ~executable_is_usable ~cwd program =
  if (not (Filename.is_implicit program)) || Filename.dirname program <> "."
  then program
  else
    let path_directories =
      Sys.getenv_opt "PATH" |> Option.value ~default:""
      |> String.split_on_char path_separator
    in
    path_directories
    |> List.find_map (fun directory ->
        let directory = normalize_directory directory in
        let directory =
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

(* A lock records the PID of the rescript process that owns it. A PID that has
   since been reused by an unrelated process must not keep the lock alive, so
   only the name of the build-system executable counts. The packaged executable
   and the dune development executable are both called rescript. *)
let is_lock_owner_name name =
  let name = String.lowercase_ascii name in
  name = "rescript" || name = "rescript.exe"

let process_is_active ~probe value =
  try
    let pid = int_of_string value in
    (* PID zero names a process group on Unix and is not a usable child-process
       identity on Windows, so it can never prove ownership of a build lock. *)
    if pid = 0 then false else probe pid
  with
  | Failure _ | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (Unix.EPERM, _, _) -> true

let create_capture_pipes () =
  let stdout = Spawn.safe_pipe () in
  try (stdout, Spawn.safe_pipe ())
  with exn ->
    Unix.close (fst stdout);
    Unix.close (snd stdout);
    raise exn
