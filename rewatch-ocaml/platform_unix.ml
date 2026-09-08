let path_separator = ':'
let normalize_path_for_comparison value = value
let canonicalize_path = Unix.realpath
let executable_extensions ~program:_ = [""]
let search_directories ~cwd:_ directories = directories

let executable_is_usable candidate =
  try
    (Unix.stat candidate).Unix.st_kind = Unix.S_REG
    && try
         Unix.access candidate [Unix.X_OK];
         true
       with Unix.Unix_error _ -> false
  with Unix.Unix_error _ -> false

let resolve_program =
  Platform_common.resolve_program ~path_separator ~executable_extensions
    ~search_directories ~executable_is_usable

let post_build_command ~command ~output =
  (None, "/bin/sh", ["-c"; command ^ " " ^ Filename.quote output])

let spawn ~env ~cwd ~program ~args ~stdout ~stderr =
  let program = resolve_program ~cwd program in
  Spawn.spawn ?env ~cwd:(Spawn.Working_dir.Path cwd) ~prog:program
    ~argv:(program :: args) ~stdout ~stderr
    ~setpgid:Spawn.Pgid.new_process_group ()

let signal_process_tree pid signal =
  try Unix.kill (-pid) signal with Unix.Unix_error _ -> ()

let defer_termination_signals () =
  let previous = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigterm] in
  fun () -> ignore (Unix.sigprocmask Unix.SIG_SETMASK previous)

let graceful_termination_signal = Sys.sigterm
let escalate_process_groups = true

let probe_process pid =
  Unix.kill pid 0;
  let executable = Printf.sprintf "/proc/%d/exe" pid in
  if Sys.file_exists executable then
    try
      let basename = Unix.realpath executable |> Filename.basename in
      String.starts_with ~prefix:"rescript" basename
    with Unix.Unix_error _ -> true
  else true

let process_is_active ~run:_ value =
  Platform_common.process_is_active ~probe:probe_process value
