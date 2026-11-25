(* See the interface. The writer domain starts with the first queued write and
   stops when the process exits. *)
let enabled =
  match Sys.getenv_opt "RESCRIPT_BACKGROUND_WRITES" with
  | Some ("1" | "true") -> true
  | _ -> false

type job = {
  write: path:string -> string -> unit;
  path: string;
  contents: string;
  after: unit -> unit;
}

let mutex = Mutex.create ()
let job_available = Condition.create ()
let idle = Condition.create ()
let queue : job Queue.t = Queue.create ()

(* The writer took a job and has not finished it *)
let busy = ref false
let stopping = ref false
let error : exn option ref = ref None
let writer : unit Domain.t option ref = ref None

let kind path =
  match Filename.extension path with
  | "" -> "other"
  | extension -> String.sub extension 1 (String.length extension - 1)

let run_job job =
  let start = if Build_trace.enabled then Build_trace.now () else 0. in
  Fun.protect
    ~finally:(fun () ->
      Build_trace.span
        ~name:(Filename.basename job.path)
        ~args:
          [
            ("bytes", `Int (String.length job.contents));
            ("kind", `String (kind job.path));
          ]
        ~start ())
    (fun () -> job.write ~path:job.path job.contents);
  job.after ()

let rec writer_loop () =
  let next =
    Process_child.with_lock mutex (fun () ->
        while Queue.is_empty queue && not !stopping do
          Condition.wait job_available mutex
        done;
        if Queue.is_empty queue then None
        else (
          busy := true;
          Some (Queue.take queue)))
  in
  match next with
  | None -> ()
  | Some job ->
    (try run_job job
     with exn ->
       Process_child.with_lock mutex (fun () ->
           if Option.is_none !error then error := Some exn));
    Process_child.with_lock mutex (fun () ->
        busy := false;
        if Queue.is_empty queue then Condition.broadcast idle);
    writer_loop ()

let stop () =
  match !writer with
  | None -> ()
  | Some domain ->
    Process_child.with_lock mutex (fun () ->
        stopping := true;
        Condition.broadcast job_available);
    Domain.join domain;
    writer := None

(* Called with the lock held *)
let start_writer () =
  if Option.is_none !writer then (
    writer :=
      Some
        (Domain.spawn (fun () ->
             Build_trace.set_writer_track ();
             writer_loop ()));
    at_exit stop)

let write ~write ?(after = ignore) path contents =
  let job = {write; path; contents; after} in
  if not enabled then run_job job
  else
    Process_child.with_lock mutex (fun () ->
        start_writer ();
        Queue.add job queue;
        Condition.signal job_available)

let wait_idle () =
  Process_child.with_lock mutex (fun () ->
      while (not (Queue.is_empty queue)) || !busy do
        Condition.wait idle mutex
      done;
      let failed = !error in
      error := None;
      failed)

let flush () =
  if enabled then (
    let start = if Build_trace.enabled then Build_trace.now () else 0. in
    let failed = wait_idle () in
    Build_trace.span ~name:"wait for writer" ~start ();
    Option.iter raise failed)

let flush_quietly () = if enabled then ignore (wait_idle ())
