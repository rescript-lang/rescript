type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

let decode_utf8_lossy value =
  if String.is_valid_utf_8 value then value
  else
    let output = Buffer.create (String.length value) in
    let rec loop index =
      if index < String.length value then (
        let decoded = String.get_utf_8_uchar value index in
        let length = max 1 (Uchar.utf_decode_length decoded) in
        if Uchar.utf_decode_is_valid decoded then
          Buffer.add_substring output value index length
        else Buffer.add_utf_8_uchar output Uchar.rep;
        loop (index + length))
    in
    loop 0;
    Buffer.contents output

let succeeded result = result.status = Unix.WEXITED 0

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

(* Blocking reader threads are portable to Windows, where select cannot wait on
   anonymous pipes, and prevent either stream from filling while the child is
   writing to the other one. Scheduling stays single-threaded. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

type capture = {
  thread: Thread.t;
  outcome: (string, exn) Stdlib.result option ref;
}

type 'a running = {
  payload: 'a;
  pid: int;
  stdout_capture: capture;
  stderr_capture: capture;
}

let close_noerr descriptor =
  try Unix.close descriptor with Unix.Unix_error _ -> ()

let start_capture descriptor =
  let outcome = ref None in
  let thread =
    Thread.create
      (fun () ->
        outcome :=
          Some
            (try
               let output = Buffer.create 4096 in
               let bytes = Bytes.create 65536 in
               let rec read () =
                 try
                   match Unix.read descriptor bytes 0 (Bytes.length bytes) with
                   | 0 -> ()
                   | count ->
                     Buffer.add_subbytes output bytes 0 count;
                     read ()
                 with Unix.Unix_error (Unix.EINTR, _, _) -> read ()
               in
               Fun.protect ~finally:(fun () -> close_noerr descriptor) read;
               Ok (Buffer.contents output |> decode_utf8_lossy)
             with exn ->
               close_noerr descriptor;
               Error exn))
      ()
  in
  {thread; outcome}

let capture_outcome capture =
  Thread.join capture.thread;
  match !(capture.outcome) with
  | Some outcome -> outcome
  | None -> Error (Failure "subprocess output reader did not finish")

let capture_error exn =
  Error ("failed to capture subprocess output: " ^ Printexc.to_string exn)

let launch ?env payload job =
  (* Signals are deferred before opening pipes so asynchronous watch
     termination cannot interrupt the gap between acquiring descriptors and
     installing their cleanup owner. *)
  let restore_signals = Platform.defer_termination_signals () in
  let opened_pipes = ref None in
  let stdout_capture = ref None in
  let stderr_capture = ref None in
  let child_pid = ref None in
  try
    let pipes = Platform.create_capture_pipes () in
    opened_pipes := Some pipes;
    let (stdout_read, stdout_write), (stderr_read, stderr_write) = pipes in
    let stdout = start_capture stdout_read in
    stdout_capture := Some stdout;
    let stderr = start_capture stderr_read in
    stderr_capture := Some stderr;
    let pid =
      Platform.spawn ~env ~cwd:job.cwd ~program:job.program ~args:job.args
        ~stdout:stdout_write ~stderr:stderr_write
    in
    child_pid := Some pid;
    close_noerr stdout_write;
    close_noerr stderr_write;
    restore_signals ();
    {payload; pid; stdout_capture = stdout; stderr_capture = stderr}
  with exn ->
    Option.iter
      (fun ((stdout_read, stdout_write), (stderr_read, stderr_write)) ->
        Option.iter
          (fun pid ->
            Platform.signal_process_tree pid Sys.sigkill;
            try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
          !child_pid;
        close_noerr stdout_write;
        close_noerr stderr_write;
        if Option.is_none !stdout_capture then close_noerr stdout_read;
        if Option.is_none !stderr_capture then close_noerr stderr_read;
        Option.iter (fun capture -> Thread.join capture.thread) !stdout_capture;
        Option.iter (fun capture -> Thread.join capture.thread) !stderr_capture)
      !opened_pipes;
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let wait_for_running ?(poll = fun () -> ()) active =
  let rec wait = function
    | [] ->
      poll ();
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
  let stdout = capture_outcome child.stdout_capture in
  let stderr = capture_outcome child.stderr_capture in
  match stdout, stderr with
  | Ok stdout, Ok stderr -> {status; stdout; stderr}
  | Error exn, _ | _, Error exn -> raise (capture_error exn)

let discard_capture capture = ignore (capture_outcome capture)

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
              | _ -> false
            with
            | Unix.Unix_error (Unix.EINTR, _, _) -> true
            | Unix.Unix_error (Unix.ECHILD, _, _) -> false)
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
        try ignore (Unix.waitpid [] child.pid) with Unix.Unix_error _ -> ())
      remaining;
    List.iter
      (fun child ->
        discard_capture child.stdout_capture;
        discard_capture child.stderr_capture)
      children)

let run_parallel ?(max_jobs = default_max_jobs) ?(poll = fun () -> ()) jobs =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  let indexed = List.mapi (fun index job -> (index, job)) jobs in
  let results = Array.make (List.length jobs) None in
  let active = ref [] in
  let launch_indexed (index, job) = active := launch index job :: !active in
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
      let (child, status), restore_signals = wait_for_running ~poll !active in
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

let run_dependency_graph ?(max_jobs = default_max_jobs)
    ?(is_fatal = function Sys.Break -> true | _ -> false)
    ?(poll = fun () -> ()) works ~next =
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
           | Some job -> active := launch work job :: !active
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
      let (child, status), restore_signals = wait_for_running ~poll !active in
      let result =
        with_signal_restore restore_signals (fun () ->
          active :=
            List.filter (fun running -> running.pid <> child.pid) !active;
          collect_result child status)
      in
      (try
         match next child.payload.value (Some result) with
         | Some job ->
           active := launch child.payload job :: !active
         | None -> complete child.payload
       with exn -> record_error child.payload exn);
      schedule ()
  in
  try schedule ()
  with exn ->
    terminate_running !active;
    raise exn

let run ?env ~cwd program args =
  let child = launch ?env () {program; args; cwd} in
  let reaped = ref false in
  try
    let (child, status), restore_signals = wait_for_running [child] in
    reaped := true;
    with_signal_restore restore_signals (fun () -> collect_result child status)
  with exn ->
    if not !reaped then terminate_running [child];
    raise exn
