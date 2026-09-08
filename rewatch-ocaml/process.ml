type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

let read_file path =
  if (Unix.stat path).Unix.st_size = 0 then ""
  else
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))

let open_temporary_log ?temp_dir stream =
  let path, channel =
    Filename.open_temp_file ?temp_dir ~mode:[Open_binary]
      (".rewatch-ocaml-" ^ stream ^ "-") ".log"
  in
  (path, channel, Unix.descr_of_out_channel channel)

let run ?env ~cwd program args =
  let restore_signals = Platform.defer_termination_signals () in
  let child_pid = ref None in
  let stdout_path = ref None in
  let stderr_path = ref None in
  let stdout_channel = ref None in
  let stderr_channel = ref None in
  let close_channel channel = close_out_noerr channel in
  let remove_log path = try Sys.remove path with Sys_error _ -> () in
  let cleanup () =
    Option.iter close_channel !stdout_channel;
    Option.iter close_channel !stderr_channel;
    stdout_channel := None;
    stderr_channel := None;
    Option.iter remove_log !stdout_path;
    Option.iter remove_log !stderr_path
  in
  try
    let stdout_log, stdout, out = open_temporary_log "stdout" in
    stdout_path := Some stdout_log;
    stdout_channel := Some stdout;
    let stderr_log, stderr, err = open_temporary_log "stderr" in
    stderr_path := Some stderr_log;
    stderr_channel := Some stderr;
    let pid =
      Platform.spawn ~env ~cwd ~program ~args ~stdout:out ~stderr:err
    in
    child_pid := Some pid;
    close_channel stdout;
    stdout_channel := None;
    close_channel stderr;
    stderr_channel := None;
    restore_signals ();
    let rec wait () =
      let restore_signals = Platform.defer_termination_signals () in
      try
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ ->
          restore_signals ();
          ignore (Unix.select [] [] [] 0.00001);
          wait ()
        | _, status ->
          child_pid := None;
          restore_signals ();
          status
      with exn ->
        let exn = try restore_signals (); exn with signal_exn -> signal_exn in
        raise exn
    in
    let status = wait () in
    let stdout = read_file stdout_log in
    let stderr = read_file stderr_log in
    cleanup ();
    {status; stdout; stderr}
  with exn ->
    Option.iter
      (fun pid ->
        Platform.signal_process_tree pid Sys.sigkill;
        try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
      !child_pid;
    cleanup ();
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let succeeded result = result.status = Unix.WEXITED 0

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

(* Each child writes to private files, so diagnostics cannot interleave.  The
   scheduler refills a slot as soon as any child exits while returning results
   in input order. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

type 'a running = {
  payload: 'a;
  pid: int;
  stdout_path: string;
  stderr_path: string;
}

let remove_log path = try Sys.remove path with Sys_error _ -> ()

let remove_running_logs child =
  remove_log child.stdout_path;
  remove_log child.stderr_path

let launch ?temp_dir payload job =
  let restore_signals = Platform.defer_termination_signals () in
  let stdout_path = ref None in
  let stderr_path = ref None in
  let stdout_channel = ref None in
  let stderr_channel = ref None in
  let child_pid = ref None in
  try
    let stdout_log, stdout, out = open_temporary_log ?temp_dir "stdout" in
    stdout_path := Some stdout_log;
    stdout_channel := Some stdout;
    let stderr_log, stderr, err = open_temporary_log ?temp_dir "stderr" in
    stderr_path := Some stderr_log;
    stderr_channel := Some stderr;
    let pid =
      Platform.spawn ~env:None ~cwd:job.cwd ~program:job.program ~args:job.args
        ~stdout:out ~stderr:err
    in
    child_pid := Some pid;
    close_out_noerr stdout;
    stdout_channel := None;
    close_out_noerr stderr;
    stderr_channel := None;
    restore_signals ();
    {payload; pid; stdout_path = stdout_log; stderr_path = stderr_log}
  with exn ->
    Option.iter close_out_noerr !stdout_channel;
    Option.iter close_out_noerr !stderr_channel;
    Option.iter
      (fun pid ->
        Platform.signal_process_tree pid Sys.sigkill;
        try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
      !child_pid;
    Option.iter remove_log !stdout_path;
    Option.iter remove_log !stderr_path;
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let wait_for_running active =
  let rec wait = function
    | [] ->
      ignore (Unix.select [] [] [] 0.00001);
      wait active
    | child :: rest ->
      let restore_signals = Platform.defer_termination_signals () in
      try
        match Unix.waitpid [Unix.WNOHANG] child.pid with
        | 0, _ ->
          restore_signals ();
          wait rest
        | _, status -> ((child, status), restore_signals)
      with exn ->
        let exn = try restore_signals (); exn with signal_exn -> signal_exn in
        raise exn
  in
  wait active

let collect_result child status =
  Fun.protect
    ~finally:(fun () -> remove_running_logs child)
    (fun () ->
      {
        status;
        stdout = read_file child.stdout_path;
        stderr = read_file child.stderr_path;
      })

let with_signal_restore restore_signals action =
  try
    let result = action () in
    restore_signals ();
    result
  with exn ->
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let terminate_running children =
  if children <> [] then (
    let signal_group signal child = Platform.signal_process_tree child.pid signal in
    let graceful_signal = Platform.graceful_termination_signal in
    List.iter (signal_group graceful_signal) children;
    let deadline = Unix.gettimeofday () +. 0.25 in
    let rec reap_until_deadline children =
      let remaining =
        List.filter
          (fun child ->
            try
              match Unix.waitpid [Unix.WNOHANG] child.pid with
              | 0, _ -> true
              | _ ->
                remove_running_logs child;
                false
            with
            | Unix.Unix_error (Unix.EINTR, _, _) -> true
            | Unix.Unix_error (Unix.ECHILD, _, _) ->
              remove_running_logs child;
              false)
          children
      in
      if remaining <> [] && Unix.gettimeofday () < deadline then (
        ignore (Unix.select [] [] [] 0.01);
        reap_until_deadline remaining)
      else remaining
    in
    let remaining = reap_until_deadline children in
    (* A direct child may have exited while a PPX/helper in its process group
       remains alive, so escalate every original group rather than only the
       direct children that still need reaping. *)
    if Platform.escalate_process_groups then
      List.iter (signal_group Sys.sigkill) children;
    List.iter
      (fun child ->
        (try ignore (Unix.waitpid [] child.pid) with Unix.Unix_error _ -> ());
        remove_running_logs child)
      remaining)

let run_parallel ?temp_dir ?(max_jobs = default_max_jobs) jobs =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  let indexed = List.mapi (fun index job -> (index, job)) jobs in
  let results = Array.make (List.length jobs) None in
  let active = ref [] in
  let launch_indexed (index, job) =
    active := launch ?temp_dir index job :: !active
  in
  let rec fill slots queued =
    if slots = 0 then queued
    else
      match queued with
      | [] -> []
      | job :: rest ->
        launch_indexed job;
        fill (slots - 1) rest
  in
  let rec schedule queued =
    let queued = fill (max_jobs - List.length !active) queued in
    match !active with
    | [] -> ()
    | _ ->
      let (child, status), restore_signals = wait_for_running !active in
      with_signal_restore restore_signals (fun () ->
        active :=
          List.filter (fun running -> running.pid <> child.pid) !active;
        results.(child.payload) <- Some (collect_result child status));
      schedule queued
  in
  try
    schedule indexed;
    Array.to_list results
    |> List.map (function
         | Some result -> result
         | None -> raise (Error "subprocess result was not collected"))
  with exn ->
    terminate_running !active;
    raise exn

type 'a work = {key: string; dependencies: string list; value: 'a}

module Work_ready = Set.Make (struct
  type t = int * string

  let compare (first_priority, first_key) (second_priority, second_key) =
    let by_priority = compare second_priority first_priority in
    if by_priority <> 0 then by_priority else String.compare first_key second_key
end)

let run_dependency_graph ?temp_dir ?(max_jobs = default_max_jobs)
    ?(is_fatal = function Sys.Break -> true | _ -> false) works ~next =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  let count = List.length works in
  let by_key = Hashtbl.create count in
  List.iter
    (fun work ->
      if Hashtbl.mem by_key work.key then
        raise (Error ("duplicate subprocess work key: " ^ work.key));
      Hashtbl.add by_key work.key work)
    works;
  let dependents = Hashtbl.create count in
  let dependencies_by_key = Hashtbl.create count in
  let pending = Hashtbl.create count in
  List.iter
    (fun work ->
      let dependencies = List.sort_uniq String.compare work.dependencies in
      Hashtbl.add dependencies_by_key work.key dependencies;
      List.iter
        (fun dependency ->
          if not (Hashtbl.mem by_key dependency) then
            raise
              (Error
                 (Printf.sprintf "unknown dependency %s for subprocess work %s"
                    dependency work.key));
          let current =
            Hashtbl.find_opt dependents dependency |> Option.value ~default:[]
          in
          Hashtbl.replace dependents dependency (work :: current))
        dependencies;
      Hashtbl.add pending work.key (List.length dependencies))
    works;
  let priorities = Hashtbl.create count in
  let remaining_dependents = Hashtbl.create count in
  let leaves = Queue.create () in
  List.iter
    (fun work ->
      let dependent_count =
        Hashtbl.find_opt dependents work.key |> Option.value ~default:[]
        |> List.length
      in
      Hashtbl.add remaining_dependents work.key dependent_count;
      if dependent_count = 0 then (
        Hashtbl.add priorities work.key 1;
        Queue.add work.key leaves))
    works;
  let prioritized = ref 0 in
  while not (Queue.is_empty leaves) do
    let key = Queue.take leaves in
    incr prioritized;
    let key_priority = Hashtbl.find priorities key in
    Hashtbl.find dependencies_by_key key
    |> List.iter (fun dependency ->
         let candidate = key_priority + 1 in
         let current =
           Hashtbl.find_opt priorities dependency |> Option.value ~default:1
         in
         if candidate > current then
           Hashtbl.replace priorities dependency candidate;
         let remaining = Hashtbl.find remaining_dependents dependency - 1 in
         Hashtbl.replace remaining_dependents dependency remaining;
         if remaining = 0 then Queue.add dependency leaves)
  done;
  if !prioritized <> count then
    raise (Error "subprocess dependency graph contains a cycle");
  let ready = ref Work_ready.empty in
  let add_ready work =
    ready :=
      Work_ready.add (Hashtbl.find priorities work.key, work.key) !ready
  in
  List.iter
    (fun work -> if Hashtbl.find pending work.key = 0 then add_ready work)
    works;
  let active = ref [] in
  let completed = ref 0 in
  let stopped = ref false in
  let errors = ref [] in
  let record_error work exn =
    if is_fatal exn then raise exn
    else (
      stopped := true;
      errors := (work.key, exn) :: !errors)
  in
  let complete work =
    incr completed;
    Hashtbl.find_opt dependents work.key |> Option.value ~default:[]
    |> List.iter (fun dependent ->
         let remaining = Hashtbl.find pending dependent.key - 1 in
         Hashtbl.replace pending dependent.key remaining;
         if remaining = 0 then add_ready dependent)
  in
  let rec fill () =
    if (not !stopped) && List.length !active < max_jobs then
      match Work_ready.min_elt_opt !ready with
      | None -> ()
      | Some ((_, key) as ready_key) ->
        ready := Work_ready.remove ready_key !ready;
        let work = Hashtbl.find by_key key in
        (try
           match next work.value None with
           | None -> complete work
           | Some job -> active := launch ?temp_dir work job :: !active
         with exn -> record_error work exn);
        fill ()
  in
  let rec schedule () =
    fill ();
    match !active with
    | [] ->
      (match
         !errors
         |> List.sort (fun (first, _) (second, _) -> String.compare first second)
       with
      | (_, exn) :: _ -> raise exn
      | [] when !completed <> count ->
        raise (Error "subprocess dependency graph stalled")
      | [] -> ())
    | _ ->
      let (child, status), restore_signals = wait_for_running !active in
      let result =
        with_signal_restore restore_signals (fun () ->
          active :=
            List.filter (fun running -> running.pid <> child.pid) !active;
          collect_result child status)
      in
      (try
         match next child.payload.value (Some result) with
         | Some job ->
           active := launch ?temp_dir child.payload job :: !active
         | None -> complete child.payload
       with exn -> record_error child.payload exn);
      schedule ()
  in
  try schedule ()
  with exn ->
    terminate_running !active;
    raise exn
