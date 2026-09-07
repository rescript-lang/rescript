type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary_log ~cwd stream =
  Filename.temp_file ~temp_dir:cwd (".rewatch-ocaml-" ^ stream ^ "-") ".log"

let run ~cwd program args =
  let stdout_path = temporary_log ~cwd "stdout" in
  let stderr_path = temporary_log ~cwd "stderr" in
  let child_pid = ref None in
  let stdout_fd =
    Unix.openfile stdout_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  in
  let cleanup () =
    (try Sys.remove stdout_path with Sys_error _ -> ());
    try Sys.remove stderr_path with Sys_error _ -> ()
  in
  try
    match Unix.fork () with
    | 0 -> (
      try
        Unix.chdir cwd;
        Unix.dup2 stdout_fd Unix.stdout;
        Unix.dup2 stderr_fd Unix.stderr;
        Unix.close stdout_fd;
        Unix.close stderr_fd;
        Unix.execv program (Array.of_list (program :: args))
      with _ -> Unix._exit 127)
    | pid ->
      child_pid := Some pid;
      Unix.close stdout_fd;
      Unix.close stderr_fd;
      let _, status = Unix.waitpid [] pid in
      child_pid := None;
      let stdout = read_file stdout_path in
      let stderr = read_file stderr_path in
      cleanup ();
      {status; stdout; stderr}
  with exn ->
    (try Unix.close stdout_fd with Unix.Unix_error _ -> ());
    (try Unix.close stderr_fd with Unix.Unix_error _ -> ());
    Option.iter
      (fun pid ->
        (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
        try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
      !child_pid;
    cleanup ();
    raise exn

let succeeded result = result.status = Unix.WEXITED 0

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

(* Jobs are launched in bounded batches.  Each child writes to private files, so
   diagnostics cannot interleave and a failed child cannot block its siblings. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

let run_parallel ?(max_jobs = default_max_jobs) jobs =
  let run_batch batch =
    let children = ref [] in
    let cleanup_children () =
      List.iter
        (fun (pid, stdout_path, stderr_path) ->
          (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
          (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
          (try Sys.remove stdout_path with Sys_error _ -> ());
          try Sys.remove stderr_path with Sys_error _ -> ())
        !children
    in
    try
      List.iter
        (fun job ->
          let stdout_path = temporary_log ~cwd:job.cwd "stdout" in
          let stderr_path = temporary_log ~cwd:job.cwd "stderr" in
          let stdout_fd = Unix.openfile stdout_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
          let stderr_fd = Unix.openfile stderr_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
          match Unix.fork () with
          | 0 ->
            (try
               Unix.chdir job.cwd;
               Unix.dup2 stdout_fd Unix.stdout;
               Unix.dup2 stderr_fd Unix.stderr;
               Unix.close stdout_fd; Unix.close stderr_fd;
               Unix.execv job.program (Array.of_list (job.program :: job.args))
             with _ -> Unix._exit 127)
          | pid ->
            Unix.close stdout_fd; Unix.close stderr_fd;
            children := (pid, stdout_path, stderr_path) :: !children)
        batch;
      List.rev !children
      |> List.map (fun (pid, stdout_path, stderr_path) ->
        let _, status = Unix.waitpid [] pid in
        let result = {status; stdout = read_file stdout_path; stderr = read_file stderr_path} in
        (try Sys.remove stdout_path with Sys_error _ -> ());
        (try Sys.remove stderr_path with Sys_error _ -> ());
        result)
    with exn ->
      cleanup_children ();
      raise exn
  in
  let rec batches acc = function
    | [] -> List.rev acc
    | jobs ->
      let batch, rest =
        let rec take n left acc =
          if n = 0 || left = [] then (List.rev acc, left)
          else take (n - 1) (List.tl left) (List.hd left :: acc)
        in
        take max_jobs jobs []
      in
      batches (List.rev_append (run_batch batch) acc) rest
  in
  batches [] jobs
