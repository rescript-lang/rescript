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

(* Reader threads are needed because a child can block when either output pipe
   fills, and Windows cannot use select to drain anonymous pipes concurrently.
   The threads perform only blocking I/O; dependency scheduling stays on the
   calling thread. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

type capture = {
  thread: Thread.t;
  outcome: (string, exn) Stdlib.result option ref;
}

type child_wait = {
  thread: Thread.t;
  direct_outcome: (Unix.process_status, exn) Stdlib.result option Atomic.t;
  outcome: (result, exn) Stdlib.result option Atomic.t;
}

type 'a running = {
  payload: 'a;
  pid: int;
  child_wait: child_wait;
}

type completion_notifier = {
  mutex: Mutex.t;
  condition: Condition.t;
  mutable generation: int;
  mutable stopped: bool;
}

let with_mutex mutex action =
  Mutex.lock mutex;
  (* Unlocking in the exception path prevents one failed callback from
     permanently blocking every waiter that shares this notifier. *)
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) action

let create_completion_notifier () =
  {
    mutex = Mutex.create ();
    condition = Condition.create ();
    generation = 0;
    stopped = false;
  }

let notify_completion notifier =
  with_mutex notifier.mutex (fun () ->
    if not notifier.stopped then (
      notifier.generation <- notifier.generation + 1;
      Condition.broadcast notifier.condition))

let notifier_generation notifier =
  with_mutex notifier.mutex (fun () -> notifier.generation)

let await_notification notifier generation =
  with_mutex notifier.mutex (fun () ->
    while notifier.generation = generation && not notifier.stopped do
      Condition.wait notifier.condition notifier.mutex
    done;
    notifier.generation)

let with_completion_notifier ?(ticker_enabled = false) action =
  (* The scheduler needs immediate child completion without repeatedly asking
     the operating system about every running PID. A condition variable wakes
     it when status and captured output are both ready; one ticker also wakes a
     supplied watch poll callback every five milliseconds while children are
     busy. *)
  let notifier = create_completion_notifier () in
  let rec send_tick () =
    Thread.delay 0.005;
    let continue =
      with_mutex notifier.mutex (fun () ->
        if notifier.stopped then false
        else (
          notifier.generation <- notifier.generation + 1;
          Condition.broadcast notifier.condition;
          true))
    in
    if continue then send_tick ()
  in
  let restore_signals = Platform.defer_termination_signals () in
  let ticker = ref None in
  let stopped = ref false in
  let stop () =
    if not !stopped then (
      with_mutex notifier.mutex (fun () ->
        notifier.stopped <- true;
        Condition.broadcast notifier.condition);
      Option.iter Thread.join !ticker;
      stopped := true)
  in
  try
    if ticker_enabled then ticker := Some (Thread.create send_tick ());
    Fun.protect ~finally:stop (fun () ->
      restore_signals ();
      action notifier)
  with exn ->
    stop ();
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let close_noerr descriptor =
  try Unix.close descriptor with Unix.Unix_error _ -> ()

let start_capture descriptor : capture =
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

let capture_outcome (capture : capture) =
  Thread.join capture.thread;
  match !(capture.outcome) with
  | Some outcome -> outcome
  | None -> Error (Failure "subprocess output reader did not finish")

let capture_error exn =
  Error ("failed to capture subprocess output: " ^ Printexc.to_string exn)

let start_child_wait pid notifier stdout_capture stderr_capture : child_wait =
  let direct_outcome = Atomic.make None in
  let outcome = Atomic.make None in
  let rec wait () =
    try
      let _, status = Unix.waitpid [] pid in
      status
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  let thread =
    Thread.create
      (fun () ->
        let status = try Ok (wait ()) with exn -> Error exn in
        Atomic.set direct_outcome (Some status);
        let stdout = capture_outcome stdout_capture in
        let stderr = capture_outcome stderr_capture in
        let result =
          match status, stdout, stderr with
          | Ok status, Ok stdout, Ok stderr -> Ok {status; stdout; stderr}
          | Error exn, _, _ -> Error exn
          | _, Error exn, _ | _, _, Error exn -> Error (capture_error exn)
        in
        Atomic.set outcome (Some result);
        notify_completion notifier)
      ()
  in
  {thread; direct_outcome; outcome}

let launch ?env ~notifier payload job =
  (* Capture descriptors need a cleanup owner before asynchronous watch
     termination can raise. Signals are therefore deferred across pipe
     acquisition and restored only after every descriptor has an owner. *)
  let restore_signals = Platform.defer_termination_signals () in
  let opened_pipes = ref None in
  let stdout_capture = ref None in
  let stderr_capture = ref None in
  let child_pid = ref None in
  let child_wait = ref None in
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
    let wait = start_child_wait pid notifier stdout stderr in
    child_wait := Some wait;
    restore_signals ();
    {payload; pid; child_wait = wait}
  with exn ->
    Option.iter
      (fun ((stdout_read, stdout_write), (stderr_read, stderr_write)) ->
        Option.iter
          (fun pid ->
            let root_reaped =
              match !child_wait with
              | Some wait -> Option.is_some (Atomic.get wait.direct_outcome)
              | None -> false
            in
            Platform.signal_process_tree ~root_reaped pid Sys.sigkill;
            if Option.is_none !child_wait then
              try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
          !child_pid;
        close_noerr stdout_write;
        close_noerr stderr_write;
        if Option.is_none !stdout_capture then close_noerr stdout_read;
        if Option.is_none !stderr_capture then close_noerr stderr_read;
        (match !child_wait with
        | Some wait -> Thread.join wait.thread
        | None ->
          Option.iter
            (fun (capture : capture) -> Thread.join capture.thread)
            !stdout_capture;
          Option.iter
            (fun (capture : capture) -> Thread.join capture.thread)
            !stderr_capture))
      !opened_pipes;
    let exn = try restore_signals (); exn with signal_exn -> signal_exn in
    raise exn

let wait_for_running ?(poll = fun () -> ()) notifier active =
  let rec find_completed = function
    | [] -> None
    | child :: rest -> (
      match Atomic.get child.child_wait.outcome with
      | Some outcome -> Some (child, outcome)
      | None -> find_completed rest)
  in
  let rec wait generation =
    match find_completed active with
    | Some (child, Ok result) ->
      let restore_signals = Platform.defer_termination_signals () in
      ((child, result), restore_signals)
    | Some (_, Error exn) -> raise exn
    | None ->
      poll ();
      await_notification notifier generation |> wait
  in
  notifier_generation notifier |> wait

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
    let root_identity_lost child =
      Option.is_some (Atomic.get child.child_wait.direct_outcome)
    in
    let signal_group signal child =
      let root_reaped = root_identity_lost child in
      Platform.signal_process_tree ~root_reaped child.pid signal
    in
    let graceful_signal = Platform.graceful_termination_signal in
    List.iter (signal_group graceful_signal) children;
    let deadline = Unix.gettimeofday () +. 0.25 in
    let rec wait_until_deadline children =
      let remaining =
        List.filter
          (fun child -> not (root_identity_lost child))
          children
      in
      if remaining <> [] && Unix.gettimeofday () < deadline then (
        ignore (Unix.select [] [] [] 0.01);
        wait_until_deadline remaining)
      else remaining
    in
    ignore (wait_until_deadline children);
    (* Every original process group needs escalation because a direct child can
       exit while a PPX or helper in its group remains alive. *)
    if Platform.escalate_process_groups then
      List.iter (signal_group Sys.sigkill) children;
    List.iter (fun child -> Thread.join child.child_wait.thread) children)

let run_parallel_with_notifier ~max_jobs ~poll ~on_complete notifier jobs =
  let indexed = List.mapi (fun index job -> (index, job)) jobs in
  let results = Array.make (List.length jobs) None in
  let active = ref [] in
  let launch_indexed (index, job) =
    active := launch ~notifier index job :: !active
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
      let (child, result), restore_signals =
        wait_for_running ~poll notifier !active
      in
      with_signal_restore restore_signals (fun () ->
        active :=
          List.filter (fun running -> running.pid <> child.pid) !active;
        results.(child.payload) <- Some result;
        on_complete child.payload);
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

let run_parallel ?(max_jobs = default_max_jobs) ?poll
    ?(on_complete = fun _ -> ()) jobs =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  match jobs with
  | [] -> []
  | _ ->
    let poll, ticker_enabled =
      match poll with
      | Some poll -> (poll, true)
      | None -> ((fun () -> ()), false)
    in
    with_completion_notifier ~ticker_enabled (fun notifier ->
      run_parallel_with_notifier ~max_jobs ~poll ~on_complete notifier jobs)

type 'a work = {key: string; dependencies: string list; value: 'a}

module Work_ready = Set.Make (struct
  type t = int * string

  let compare (first_priority, first_key) (second_priority, second_key) =
    let by_priority = compare second_priority first_priority in
    if by_priority <> 0 then by_priority else String.compare first_key second_key
end)

let run_dependency_graph_with_notifier ~max_jobs ~is_fatal ~poll notifier works
    ~next =
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
  let () =
    if !prioritized <> count then
      let cycle =
        Graph.shortest_cycle works ~name:(fun work -> work.key)
          ~deps:(fun work -> work.dependencies)
        |> Option.value ~default:[]
      in
      let details =
        match cycle with
        | [] -> ""
        | cycle -> ": " ^ String.concat " -> " cycle
      in
      raise (Error ("subprocess dependency graph contains a cycle" ^ details))
  in
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
           | Some job -> active := launch ~notifier work job :: !active
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
      let (child, result), restore_signals =
        wait_for_running ~poll notifier !active
      in
      with_signal_restore restore_signals (fun () ->
        active :=
          List.filter (fun running -> running.pid <> child.pid) !active);
      (try
         match next child.payload.value (Some result) with
         | Some job ->
           active := launch ~notifier child.payload job :: !active
         | None -> complete child.payload
       with exn -> record_error child.payload exn);
      schedule ()
  in
  try schedule ()
  with exn ->
    terminate_running !active;
    raise exn

let run_dependency_graph ?(max_jobs = default_max_jobs)
    ?(is_fatal = function Sys.Break -> true | _ -> false)
    ?poll works ~next =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  match works with
  | [] -> ()
  | _ ->
    let poll, ticker_enabled =
      match poll with
      | Some poll -> (poll, true)
      | None -> ((fun () -> ()), false)
    in
    with_completion_notifier ~ticker_enabled (fun notifier ->
      run_dependency_graph_with_notifier ~max_jobs ~is_fatal ~poll notifier
        works ~next)

let run ?env ?poll ~cwd program args =
  let poll, ticker_enabled =
    match poll with
    | Some poll -> (poll, true)
    | None -> ((fun () -> ()), false)
  in
  with_completion_notifier ~ticker_enabled (fun notifier ->
    let child = launch ?env ~notifier () {program; args; cwd} in
    let reaped = ref false in
    try
      let (_, result), restore_signals =
        wait_for_running ~poll notifier [child]
      in
      reaped := true;
      with_signal_restore restore_signals (fun () -> result)
    with exn ->
      if not !reaped then terminate_running [child];
      raise exn)
