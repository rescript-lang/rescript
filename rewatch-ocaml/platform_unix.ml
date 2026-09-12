let path_separator = ':'
let normalize_path_for_comparison value = value
let directory_identity ~path:_ metadata =
  Printf.sprintf "%d:%d" metadata.Unix.st_dev metadata.Unix.st_ino

let canonicalize_path = Unix.realpath
let executable_extensions ~program:_ = [""]
let search_directories ~cwd:_ directories = directories

let executable_is_usable candidate =
  try
    (Unix.stat candidate).Unix.st_kind = Unix.S_REG
    &&
      try
        Unix.access candidate [Unix.X_OK];
        true
      with Unix.Unix_error _ -> false
  with Unix.Unix_error _ -> false

let resolve_program =
  Platform_common.resolve_program ~path_separator ~executable_extensions
    ~search_directories ~executable_is_usable

type command = {env: Spawn.Env.t option; program: string; args: string list}

let post_build_command ~command ~output =
  {
    env = None;
    program = "/bin/sh";
    args = ["-c"; command ^ " " ^ Filename.quote output];
  }

type process = int

let spawn ~env ~cwd ~program ~args ~stdout ~stderr =
  let program = resolve_program ~cwd program in
  Spawn.spawn ?env ~cwd:(Spawn.Working_dir.Path cwd) ~prog:program
    ~argv:(program :: args) ~stdout ~stderr
    ~setpgid:Spawn.Pgid.new_process_group ()

let process_id process = process
let release_process _process = ()

let create_capture_pipes = Platform_common.create_capture_pipes

let rec signal_process_tree ~root_reaped process signal =
  try
    Unix.kill (-process) signal;
    true
  with
  (* A missing group is already in the requested terminal state. Other errors
     must prevent the caller from waiting indefinitely for an undelivered
     signal. *)
  | Unix.Unix_error (Unix.ESRCH, _, _) -> true
  | Unix.Unix_error (Unix.EINTR, _, _) ->
    signal_process_tree ~root_reaped process signal
  | Unix.Unix_error _ -> false

let defer_termination_signals () =
  let previous = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigterm] in
  fun () -> ignore (Unix.sigprocmask Unix.SIG_SETMASK previous)

let graceful_termination_signal = Sys.sigterm
let escalate_process_groups = true

let process_name_from_ps ~run pid =
  match run "/bin/ps" ["-p"; string_of_int pid; "-o"; "comm="] with
  | Some (Unix.WEXITED 0, output) ->
    output |> String.trim |> Filename.basename
    |> String.starts_with ~prefix:"rescript"
  | Some (Unix.WEXITED _, _) -> false
  | Some (Unix.WSIGNALED _, _) | Some (Unix.WSTOPPED _, _) | None -> true

let probe_process ~run pid =
  Unix.kill pid 0;
  let executable = Printf.sprintf "/proc/%d/exe" pid in
  if Sys.file_exists executable then
    try
      let basename = Unix.realpath executable |> Filename.basename in
      String.starts_with ~prefix:"rescript" basename
    with Unix.Unix_error _ -> true
  else
    (* macOS has no procfs. `ps` supplies the same executable-name check so a
       reused PID from an abandoned lock is not mistaken for this tool. A
       failed probe remains conservative because stealing a live build's lock
       is worse than asking the user to remove an inconclusive stale lock. *)
    process_name_from_ps ~run pid

let process_is_active ~run value =
  Platform_common.process_is_active ~probe:(probe_process ~run) value
