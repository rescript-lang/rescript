module Child = Process_child

type result = Child.result = {
  status: Unix.process_status;
  stdout: string;
  stderr: string;
}

type job = Child.job = {program: string; args: string list; cwd: string}
type task_kind =
  | External of {job: job; env: Spawn.Env.t option}
  | Concurrent of {run: unit -> result; cancel: unit -> unit}
  | Adopt_speculation

type task = {kind: task_kind; on_result: result -> result}

exception Error = Child.Error
exception Interrupted of int

let succeeded = Child.succeeded
let status_string = Child.status_string

(* Reader threads are needed because a child can block when either output pipe
   fills, and Windows cannot use select to drain anonymous pipes concurrently.
   The threads perform only blocking I/O; dependency scheduling stays on the
   calling thread. *)
let default_max_jobs = min 32 (max 1 (Domain.recommended_domain_count ()))

let with_completion_notifier ~poll action =
  match poll with
  | Some poll ->
    Child.with_completion_notifier ~ticker_enabled:true (action ~poll)
  | None ->
    Child.with_completion_notifier ~ticker_enabled:false
      (action ~poll:(fun () -> ()))

let task ?env ?(on_result = fun result -> result) job =
  {kind = External {job; env}; on_result}

let concurrent_task ~cancel ?(on_result = fun result -> result) run =
  {kind = Concurrent {run; cancel}; on_result}

let map_result task map =
  {task with on_result = (fun result -> map (task.on_result result))}

let adopt_speculation = {kind = Adopt_speculation; on_result = Fun.id}

(* Messages from tasks to the domain that schedules a dependency graph. The
   notifier is attached only while a graph runs; a message posted outside that
   window waits for the next drain or is dropped with the mailbox. *)
type mailbox = {
  mailbox_mutex: Mutex.t;
  messages: (unit -> unit) Queue.t;
  mutable attached: Child.completion_notifier option;
}

let create_mailbox () =
  {mailbox_mutex = Mutex.create (); messages = Queue.create (); attached = None}

let post mailbox message =
  let notifier =
    Child.with_lock mailbox.mailbox_mutex (fun () ->
        Queue.add message mailbox.messages;
        mailbox.attached)
  in
  Option.iter Child.notify_completion notifier

let take_messages mailbox =
  Child.with_lock mailbox.mailbox_mutex (fun () ->
      let messages = List.of_seq (Queue.to_seq mailbox.messages) in
      Queue.clear mailbox.messages;
      messages)

let with_attached_mailbox mailbox notifier action =
  match mailbox with
  | None -> action ()
  | Some mailbox ->
    Child.with_lock mailbox.mailbox_mutex (fun () ->
        mailbox.attached <- Some notifier);
    Fun.protect action ~finally:(fun () ->
        Child.with_lock mailbox.mailbox_mutex (fun () ->
            mailbox.attached <- None))

type 'a speculation = {
  mailbox: mailbox;
  candidates: unit -> string list;
  start: 'a -> task option;
  abandon: 'a -> unit;
}

type 'a work = {key: string; dependencies: string list; value: 'a}
type failure_action =
  | Abort_immediately
  | Stop_new_work
  | Continue_independent_work

(* Higher priority first, then by key. *)
let compare_ready (first_priority, first_key) (second_priority, second_key) =
  let by_priority = compare second_priority first_priority in
  if by_priority <> 0 then by_priority else String.compare first_key second_key

module Work_ready = Set.Make (struct
  type t = int * string

  let compare = compare_ready
end)

type cancellation_state =
  | Cancellation_not_requested
  | Cancellation_requested
  | Cancellation_failed of exn

type termination_state =
  | Termination_confirmed
  | Termination_unconfirmed of exn

type pool_active = {
  id: int;
  child: Child.running;
  mutable cancellation: cancellation_state;
}

type pool_action = {id: int; cancel: unit -> unit}

let cancellation_was_requested = function
  | Cancellation_not_requested -> false
  | Cancellation_requested | Cancellation_failed _ -> true

type 'a pool_completion =
  | Task_completed of 'a * result
  | Task_failed of 'a * exn

type 'a worker_pool = {
  notifier: Child.completion_notifier;
  mutex: Mutex.t;
  work_available: Condition.t;
  cancellation_finished: Condition.t;
  queued: ('a * task) Queue.t;
  completed_tasks: 'a pool_completion Queue.t;
  mutable active_children: pool_active list;
  mutable active_actions: pool_action list;
  mutable next_active_id: int;
  mutable stopping: bool;
  mutable signalling_cancellation: bool;
  mutable workers: unit Domain.t list;
  mutable worker_error: exn option;
}

let remove_active pool (active : pool_active) =
  Child.with_lock pool.mutex (fun () ->
      while
        pool.signalling_cancellation
        && cancellation_was_requested active.cancellation
      do
        Condition.wait pool.cancellation_finished pool.mutex
      done;
      pool.active_children <-
        List.filter
          (fun (current : pool_active) -> current.id <> active.id)
          pool.active_children)

let complete_pool_task pool completion =
  Child.with_lock pool.mutex (fun () ->
      Queue.add completion pool.completed_tasks);
  Child.notify_completion pool.notifier

let run_concurrent_task pool payload task run cancel =
  let active, cancel_now =
    Child.with_lock pool.mutex (fun () ->
        let active = {id = pool.next_active_id; cancel} in
        pool.next_active_id <- pool.next_active_id + 1;
        pool.active_actions <- active :: pool.active_actions;
        (active, pool.stopping))
  in
  let completion =
    try
      if cancel_now then cancel ();
      Task_completed (payload, task.on_result (run ()))
    with exn -> Task_failed (payload, exn)
  in
  Child.with_lock pool.mutex (fun () ->
      pool.active_actions <-
        List.filter (fun current -> current.id <> active.id) pool.active_actions);
  complete_pool_task pool completion

let run_pool_task pool payload task =
  match task.kind with
  | Adopt_speculation ->
    complete_pool_task pool
      (Task_failed
         (payload, Invalid_argument "a speculation cannot run as a new task"))
  | Concurrent {run; cancel} -> run_concurrent_task pool payload task run cancel
  | External {job; env} -> (
    match
      try Ok (Child.launch ?env ~notifier:pool.notifier job)
      with exn -> Error exn
    with
    | Error exn -> complete_pool_task pool (Task_failed (payload, exn))
    | Ok child ->
      let active, cancel_after_launch =
        Child.with_lock pool.mutex (fun () ->
            let active =
              {
                id = pool.next_active_id;
                child;
                cancellation =
                  (if pool.stopping then Cancellation_requested
                   else Cancellation_not_requested);
              }
            in
            pool.next_active_id <- pool.next_active_id + 1;
            pool.active_children <- active :: pool.active_children;
            (active, cancellation_was_requested active.cancellation))
      in
      let wait_result =
        try
          if cancel_after_launch then Child.signal_running [child];
          Ok
            (Child.wait_for_running
               ~poll:(fun () ->
                 match
                   Child.with_lock pool.mutex (fun () -> active.cancellation)
                 with
                 | Cancellation_failed exn -> raise exn
                 | Cancellation_not_requested | Cancellation_requested -> ())
               pool.notifier [child])
        with exn -> Error exn
      in
      let completion =
        match wait_result with
        | Ok (_, result) -> (
          remove_active pool active;
          try
            Child.release_running child;
            Task_completed (payload, task.on_result result)
          with exn -> Task_failed (payload, exn))
        | Error exn -> (
          let termination =
            try
              Child.signal_running [child];
              Termination_confirmed
            with cancellation_exn -> Termination_unconfirmed cancellation_exn
          in
          match termination with
          | Termination_confirmed -> (
            remove_active pool active;
            try
              Child.release_running child;
              Task_failed (payload, exn)
            with release_exn -> Task_failed (payload, release_exn))
          | Termination_unconfirmed cancellation_exn ->
            remove_active pool active;
            Child.release_after_completion child;
            Task_failed (payload, cancellation_exn))
      in
      complete_pool_task pool completion)

let rec worker_loop pool =
  let queued =
    Child.with_lock pool.mutex (fun () ->
        while Queue.is_empty pool.queued && not pool.stopping do
          Condition.wait pool.work_available pool.mutex
        done;
        if Queue.is_empty pool.queued then None
        else Some (Queue.take pool.queued))
  in
  match queued with
  | None -> ()
  | Some (payload, task) ->
    run_pool_task pool payload task;
    worker_loop pool

(* Every minor collection stops all domains, and each one costs every worker
   the wait for the slowest domain to empty its heap, so the default 256k-word
   minor heap makes busy compiler workers stall each other. On a JSX-heavy
   project with 7 workers, 4M words (32 MB per worker) instead of 2M cut the
   collections by ~30% and clean-build CPU by ~10%, for ~200 MB more peak
   memory. 8M added memory for little more speed. *)
let worker_minor_heap_words = 4 * 1024 * 1024

let create_worker_pool ~max_jobs notifier =
  let pool =
    {
      notifier;
      mutex = Mutex.create ();
      work_available = Condition.create ();
      cancellation_finished = Condition.create ();
      queued = Queue.create ();
      completed_tasks = Queue.create ();
      active_children = [];
      active_actions = [];
      next_active_id = 0;
      stopping = false;
      signalling_cancellation = false;
      workers = [];
      worker_error = None;
    }
  in
  (* Workers run in-process compiler requests, which are OCaml computations, so
     they must be domains to compile in parallel: threads would serialize them
     on the runtime lock. A worker records an escaped exception instead of
     letting it escape [Domain.join], so [stop_worker_pool] joins every worker
     before re-raising the first error. *)
  let run_worker () =
    Gc.set {(Gc.get ()) with minor_heap_size = worker_minor_heap_words};
    try worker_loop pool
    with exn ->
      Child.with_lock pool.mutex (fun () ->
          if Option.is_none pool.worker_error then pool.worker_error <- Some exn)
  in
  let rec start_workers remaining =
    if remaining > 0 then (
      let worker = Domain.spawn run_worker in
      pool.workers <- worker :: pool.workers;
      start_workers (remaining - 1))
  in
  try
    start_workers max_jobs;
    pool
  with exn ->
    Child.with_lock pool.mutex (fun () ->
        pool.stopping <- true;
        Condition.broadcast pool.work_available);
    List.iter Domain.join pool.workers;
    raise exn

let submit_pool_task pool payload task =
  Child.with_lock pool.mutex (fun () ->
      Queue.add (payload, task) pool.queued;
      Condition.signal pool.work_available)

type task_origin = Regular | Speculative

type 'a speculation_state =
  | Speculation_running
  | Speculation_adopted
  | Speculation_abandoned
  | Speculation_held of 'a pool_completion

type 'a pool_event =
  | Pool_completion of 'a pool_completion
  | Mailbox_messages of (unit -> unit) list

let await_pool_event ~poll ?mailbox pool =
  let rec wait generation =
    match
      Child.with_lock pool.mutex (fun () ->
          if Queue.is_empty pool.completed_tasks then None
          else Some (Queue.take pool.completed_tasks))
    with
    | Some completion -> Pool_completion completion
    | None -> (
      match Option.map take_messages mailbox with
      | Some (_ :: _ as messages) -> Mailbox_messages messages
      | Some [] | None ->
        poll ();
        Child.await_notification pool.notifier generation |> wait)
  in
  Child.notifier_generation pool.notifier |> wait

let stop_worker_pool ~cancel pool =
  let active, actions =
    Child.with_lock pool.mutex (fun () ->
        pool.stopping <- true;
        Queue.clear pool.queued;
        Condition.broadcast pool.work_available;
        if cancel then (
          pool.signalling_cancellation <- true;
          let children =
            pool.active_children
            |> List.filter_map (fun active ->
                match active.cancellation with
                | Cancellation_requested | Cancellation_failed _ -> None
                | Cancellation_not_requested ->
                  active.cancellation <- Cancellation_requested;
                  Some active.child)
          in
          (children, List.map (fun active -> active.cancel) pool.active_actions))
        else ([], []))
  in
  let signal_error =
    try
      Child.signal_running active;
      None
    with exn -> Some exn
  in
  let action_error =
    List.fold_left
      (fun first_error cancel ->
        try
          cancel ();
          first_error
        with exn -> (
          match first_error with
          | Some _ -> first_error
          | None -> Some exn))
      None actions
  in
  let cancellation_error =
    match signal_error with
    | Some _ -> signal_error
    | None -> action_error
  in
  Child.with_lock pool.mutex (fun () ->
      Option.iter
        (fun exn ->
          List.iter
            (fun active ->
              match active.cancellation with
              | Cancellation_requested ->
                active.cancellation <- Cancellation_failed exn
              | Cancellation_not_requested | Cancellation_failed _ -> ())
            pool.active_children)
        cancellation_error;
      pool.signalling_cancellation <- false;
      Condition.broadcast pool.cancellation_finished);
  Option.iter
    (fun _ -> Child.notify_completion pool.notifier)
    cancellation_error;
  List.iter Domain.join pool.workers;
  (* A worker that crashed is the root cause; a failure to cancel the other
     work is at most a consequence of it. *)
  match (pool.worker_error, cancellation_error) with
  | Some exn, _ | None, Some exn -> raise exn
  | None, None -> ()

let with_worker_pool ~max_jobs notifier action =
  let pool = create_worker_pool ~max_jobs notifier in
  match action pool with
  | result ->
    stop_worker_pool ~cancel:false pool;
    result
  | exception exn ->
    let exn =
      try
        stop_worker_pool ~cancel:true pool;
        exn
      with cancellation_exn -> cancellation_exn
    in
    raise exn

let run_dependency_graph_with_notifier ~max_jobs ~on_failure ~poll ?speculation
    notifier works ~next =
  let graph =
    Graph.create_index works
      ~name:(fun work -> work.key)
      ~deps:(fun work -> work.dependencies)
      ~validation:
        (Graph.Reject_invalid
           {
             duplicate_node =
               (fun key -> Error ("duplicate subprocess work key: " ^ key));
             unknown_dependency =
               (fun ~node ~dependency ->
                 Error
                   (Printf.sprintf
                      "unknown dependency %s for subprocess work %s" dependency
                      node));
           })
  in
  let count = Graph.node_count graph in
  let pending = Hashtbl.create count in
  List.iter
    (fun work ->
      Hashtbl.add pending work.key (Graph.dependency_count graph work.key))
    works;
  let priorities = Hashtbl.create count in
  let remaining_dependents = Hashtbl.create count in
  let leaves = Queue.create () in
  List.iter
    (fun work ->
      let dependent_count = List.length (Graph.dependents graph work.key) in
      Hashtbl.add remaining_dependents work.key dependent_count;
      if dependent_count = 0 then (
        Hashtbl.add priorities work.key 1;
        Queue.add work.key leaves))
    works;
  while not (Queue.is_empty leaves) do
    let key = Queue.take leaves in
    let key_priority = Hashtbl.find priorities key in
    Graph.dependencies graph key
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
  (* A speculation is a work's first task, started before its dependencies
     completed. It counts against [max_jobs] while it runs, but only an adopted
     speculation's result advances the graph. *)
  let speculations = Hashtbl.create 16 in
  (* A work speculates at most once: an abandoned speculation is not retried,
     so abandoning always makes progress. *)
  let speculated = Hashtbl.create 16 in
  let unadopted_in_flight = ref 0 in
  let adopted_completions = Queue.create () in
  let has_live_speculation key =
    match Hashtbl.find_opt speculations key with
    | Some (Speculation_running | Speculation_held _) -> true
    | Some (Speculation_adopted | Speculation_abandoned) | None -> false
  in
  let ready = ref Work_ready.empty in
  (* Ready works with a live speculation: adopting one takes no new job slot. *)
  let adoptable = Queue.create () in
  let add_ready work =
    if has_live_speculation work.key then Queue.add work adoptable
    else
      ready :=
        Work_ready.add (Hashtbl.find priorities work.key, work.key) !ready
  in
  List.iter
    (fun work -> if Hashtbl.find pending work.key = 0 then add_ready work)
    works;
  let in_flight = ref 0 in
  let stopped = ref false in
  let errors = ref [] in
  let abandon_one work =
    match (Hashtbl.find_opt speculations work.key, speculation) with
    | Some Speculation_running, Some speculation ->
      Hashtbl.replace speculations work.key Speculation_abandoned;
      speculation.abandon work.value
    | Some (Speculation_held _), Some speculation ->
      Hashtbl.remove speculations work.key;
      speculation.abandon work.value
    | (Some (Speculation_adopted | Speculation_abandoned) | None), _
    | Some (Speculation_running | Speculation_held _), None ->
      ()
  in
  (* A dependent's speculation may have read what this work published before
     completing, so it goes too. *)
  let abandon_dependent_speculations work =
    if Hashtbl.length speculations > 0 then
      let visited = Hashtbl.create 16 in
      let rec visit key =
        Graph.dependents graph key
        |> List.iter (fun dependent ->
            if not (Hashtbl.mem visited dependent) then (
              Hashtbl.add visited dependent ();
              abandon_one (Graph.find_node graph dependent);
              visit dependent))
      in
      visit work.key
  in
  let abandon_speculation work =
    match Hashtbl.find_opt speculations work.key with
    | Some (Speculation_running | Speculation_held _) ->
      abandon_one work;
      abandon_dependent_speculations work
    | Some (Speculation_adopted | Speculation_abandoned) | None -> ()
  in
  let abandon_all_speculations () =
    Hashtbl.fold (fun key _ keys -> key :: keys) speculations []
    |> List.sort String.compare
    |> List.iter (fun key -> abandon_one (Graph.find_node graph key))
  in
  let record_error work exn =
    match on_failure exn with
    | Abort_immediately -> raise exn
    | Stop_new_work ->
      stopped := true;
      errors := (work.key, exn) :: !errors;
      abandon_all_speculations ()
    | Continue_independent_work ->
      errors := (work.key, exn) :: !errors;
      abandon_dependent_speculations work
  in
  let complete work =
    Graph.dependents graph work.key
    |> List.iter (fun dependent_key ->
        let remaining = Hashtbl.find pending dependent_key - 1 in
        Hashtbl.replace pending dependent_key remaining;
        if remaining = 0 then add_ready (Graph.find_node graph dependent_key))
  in
  let submit pool work origin task =
    submit_pool_task pool (work, origin) task;
    incr in_flight
  in
  let adopt work =
    match Hashtbl.find_opt speculations work.key with
    | Some Speculation_running ->
      Hashtbl.replace speculations work.key Speculation_adopted;
      decr unadopted_in_flight
    | Some (Speculation_held completion) ->
      Hashtbl.remove speculations work.key;
      Queue.add completion adopted_completions
    | Some (Speculation_adopted | Speculation_abandoned) | None ->
      invalid_arg ("no speculation to adopt for " ^ work.key)
  in
  let start pool work =
    try
      match next work.value None with
      | None ->
        abandon_speculation work;
        complete work
      | Some {kind = Adopt_speculation; _} -> adopt work
      | Some task ->
        abandon_speculation work;
        submit pool work Regular task
    with exn ->
      abandon_speculation work;
      record_error work exn
  in
  let start_speculations pool =
    match speculation with
    | None -> ()
    | Some speculation ->
      if (not !stopped) && !in_flight < max_jobs then
        speculation.candidates ()
        |> List.sort_uniq String.compare
        |> List.filter_map (fun key ->
            match Hashtbl.find_opt pending key with
            | Some remaining
              when remaining > 0 && not (Hashtbl.mem speculated key) ->
              Some (Hashtbl.find priorities key, key)
            | Some _ | None -> None)
        |> List.sort compare_ready
        |> List.iter (fun (_, key) ->
            if !in_flight < max_jobs then
              let work = Graph.find_node graph key in
              match speculation.start work.value with
              | None -> ()
              | Some task ->
                Hashtbl.replace speculations key Speculation_running;
                Hashtbl.replace speculated key ();
                incr unadopted_in_flight;
                submit pool work Speculative task)
  in
  let rec fill pool =
    (* Synchronous tasks and queued domain completions can keep the scheduler
       from reaching the wait path's poll. Check before admitting more work so
       an interrupt stops the next compiler request. *)
    poll ();
    if not !stopped then (
      while not (Queue.is_empty adoptable) do
        let work = Queue.take adoptable in
        (* A speculation abandoned since its work became ready: the work
           waits for a slot like any other. *)
        if has_live_speculation work.key then start pool work
        else add_ready work
      done;
      (* An adoption can fail the graph; a result adopted from a finished
         speculation is processed before more work is admitted, so its next
         task is queued ahead of dependents that may wait for it. *)
      if
        (not !stopped)
        && Queue.is_empty adopted_completions
        && !in_flight < max_jobs
      then
        match Work_ready.min_elt_opt !ready with
        | None -> start_speculations pool
        | Some ((_, key) as ready_key) ->
          ready := Work_ready.remove ready_key !ready;
          start pool (Graph.find_node graph key);
          fill pool)
  in
  let process_completion pool = function
    | Task_failed (work, exn) -> record_error work exn
    | Task_completed (work, result) -> (
      try
        match next work.value (Some result) with
        | Some task -> submit pool work Regular task
        | None -> complete work
      with exn -> record_error work exn)
  in
  let receive pool completion =
    decr in_flight;
    let (work, origin), completion =
      match completion with
      | Task_failed ((work, origin), exn) ->
        ((work, origin), Task_failed (work, exn))
      | Task_completed ((work, origin), result) ->
        ((work, origin), Task_completed (work, result))
    in
    match origin with
    | Regular -> process_completion pool completion
    | Speculative -> (
      match Hashtbl.find_opt speculations work.key with
      | Some Speculation_adopted ->
        Hashtbl.remove speculations work.key;
        process_completion pool completion
      | Some Speculation_abandoned ->
        Hashtbl.remove speculations work.key;
        decr unadopted_in_flight
      | Some Speculation_running ->
        (* The task finished before its work was ready. Its result is kept
           until the work is started or the speculation abandoned. A compiler
           request waits for adoption before it returns, so it only gets here
           by failing before its first gate. *)
        decr unadopted_in_flight;
        Hashtbl.replace speculations work.key (Speculation_held completion)
      | Some (Speculation_held _) | None ->
        invalid_arg ("unexpected speculation result for " ^ work.key))
  in
  let rec schedule pool =
    fill pool;
    if not (Queue.is_empty adopted_completions) then (
      process_completion pool (Queue.take adopted_completions);
      schedule pool)
    else if !in_flight = 0 then (
      (* Speculations that finished but were never adopted. *)
      abandon_all_speculations ();
      match
        !errors
        |> List.sort (fun (first, _) (second, _) -> String.compare first second)
      with
      | (_, exn) :: _ -> raise exn
      | [] ->
        (* Without errors, only a cycle leaves a node waiting on dependencies. *)
        if
          Hashtbl.fold
            (fun _ count blocked -> blocked || count > 0)
            pending false
        then invalid_arg "dependency graph contains a cycle"
        else ())
    else (
      (* Only completed work makes a speculation's work ready. When nothing
         else runs, the speculations can never be adopted. *)
      if !in_flight = !unadopted_in_flight then abandon_all_speculations ();
      (match
         await_pool_event ~poll
           ?mailbox:
             (Option.map (fun speculation -> speculation.mailbox) speculation)
           pool
       with
      | Pool_completion completion -> receive pool completion
      | Mailbox_messages messages ->
        List.iter (fun message -> message ()) messages);
      schedule pool)
  in
  let run pool =
    with_attached_mailbox
      (Option.map (fun speculation -> speculation.mailbox) speculation)
      notifier
      (fun () -> schedule pool)
  in
  with_worker_pool ~max_jobs:(min max_jobs count) notifier run

let run_dependency_graph ?(max_jobs = default_max_jobs)
    ?(on_failure =
      function
      | Interrupted _ -> Abort_immediately
      | _ -> Stop_new_work) ?poll ?speculation works ~next =
  if max_jobs < 1 then raise (Error "max_jobs must be at least one");
  match works with
  | [] -> ()
  | _ ->
    with_completion_notifier ~poll (fun ~poll notifier ->
        run_dependency_graph_with_notifier ~max_jobs ~on_failure ~poll
          ?speculation notifier works ~next)

let run_tasks ?(max_jobs = default_max_jobs) ?poll ?(on_complete = fun _ -> ())
    tasks =
  let tasks = Array.of_list tasks in
  let results = Array.make (Array.length tasks) None in
  let works =
    Array.to_list
      (Array.mapi
         (fun index _ ->
           {
             key = Printf.sprintf "%010d" index;
             dependencies = [];
             value = index;
           })
         tasks)
  in
  run_dependency_graph ~max_jobs ?poll works ~next:(fun index result ->
      match result with
      | None -> Some tasks.(index)
      | Some result ->
        results.(index) <- Some result;
        on_complete index;
        None);
  Array.to_list results |> List.map Option.get

let run_one ?poll ?stdout_chunk ?stderr_chunk ?stdin ?env ~cwd program args =
  with_completion_notifier ~poll (fun ~poll notifier ->
      let child =
        Child.launch ?stdout_chunk ?stderr_chunk ?stdin ?env ~notifier
          {program; args; cwd}
      in
      let completion_received = ref false in
      try
        let _, result = Child.wait_for_running ~poll notifier [child] in
        completion_received := true;
        Child.release_running child;
        result
      with exn ->
        if not !completion_received then Child.terminate_running [child];
        raise exn)

let run ?poll ~cwd program args = run_one ?poll ~cwd program args

let run_streaming ?poll ~cwd program args =
  let write channel bytes count =
    output channel bytes 0 count;
    flush channel
  in
  let stdin =
    if Platform.inherit_streaming_terminal_stdin || not (Unix.isatty Unix.stdin)
    then Child.Inherit_stdin
    else Child.Null_stdin
  in
  run_one ?poll ~stdout_chunk:(write stdout) ~stderr_chunk:(write stderr) ~stdin
    ~cwd program args
