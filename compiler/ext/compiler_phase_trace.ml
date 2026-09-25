(* Diagnostic request timings. Sections account for their own time only: a
   nested section pauses its parent. Disabled unless an output path is set. *)
type bucket = {mutable seconds: float; mutable bytes: float; mutable calls: int}

type state = {
  path: string;
  cwd: string;
  input: string;
  buckets: (string, bucket) Hashtbl.t;
  mutable phase: string;
  mutable clock: float;
  mutable allocation: float;
  started: float;
  started_allocation: float;
  gc: Gc.stat;
}

let state_key = Domain.DLS.new_key (fun () -> None)
let output_lock = Mutex.create ()

let charge state now allocation =
  let bucket = Hashtbl.find state.buckets state.phase in
  bucket.seconds <- bucket.seconds +. (now -. state.clock);
  bucket.bytes <- bucket.bytes +. (allocation -. state.allocation);
  state.clock <- now;
  state.allocation <- allocation

let section name action =
  match Domain.DLS.get state_key with
  | None -> action ()
  | Some state ->
    let now = Unix.gettimeofday () in
    let allocation = Gc.allocated_bytes () in
    charge state now allocation;
    let previous = state.phase in
    state.phase <- name;
    let bucket =
      match Hashtbl.find_opt state.buckets name with
      | Some bucket -> bucket
      | None ->
        let bucket = {seconds = 0.; bytes = 0.; calls = 0} in
        Hashtbl.add state.buckets name bucket;
        bucket
    in
    bucket.calls <- bucket.calls + 1;
    Fun.protect action ~finally:(fun () ->
        charge state (Unix.gettimeofday ()) (Gc.allocated_bytes ());
        state.phase <- previous)

let dependency name action =
  match Domain.DLS.get state_key with
  | Some {phase; _} when String.starts_with ~prefix:"artifact." phase ->
    action ()
  | _ -> section name action

let open_signature action =
  match Domain.DLS.get state_key with
  | Some {phase = "setup.initial_env"; _} -> section "setup.open" action
  | _ -> section "source.open" action

let request ~cwd ~input action =
  match Sys.getenv_opt "REWATCH_TYPECHECK_TRACE" with
  | None | Some "" -> action ()
  | Some path ->
    let started = Unix.gettimeofday () in
    let started_allocation = Gc.allocated_bytes () in
    let buckets = Hashtbl.create 23 in
    Hashtbl.add buckets "request.setup" {seconds = 0.; bytes = 0.; calls = 1};
    let state =
      {
        path;
        cwd;
        input;
        buckets;
        phase = "request.setup";
        clock = started;
        allocation = started_allocation;
        started;
        started_allocation;
        gc = Gc.quick_stat ();
      }
    in
    let previous = Domain.DLS.get state_key in
    Domain.DLS.set state_key (Some state);
    Fun.protect action ~finally:(fun () ->
        let finished = Unix.gettimeofday () in
        let final_allocation = Gc.allocated_bytes () in
        charge state finished final_allocation;
        Domain.DLS.set state_key previous;
        let gc = Gc.quick_stat () in
        Mutex.lock output_lock;
        Fun.protect
          (fun () ->
            let channel =
              open_out_gen [Open_creat; Open_append; Open_text] 0o644 path
            in
            Fun.protect
              (fun () ->
                let emit phase bucket =
                  Printf.fprintf channel
                    "%s\t%s\t%s\t%.6f\t%.0f\t%d\t%.6f\t%.0f\t%d\t%d\t%d\t%d\n"
                    state.cwd state.input phase bucket.seconds bucket.bytes
                    bucket.calls
                    (finished -. state.started)
                    (final_allocation -. state.started_allocation)
                    (gc.minor_collections - state.gc.minor_collections)
                    (gc.major_collections - state.gc.major_collections)
                    (gc.compactions - state.gc.compactions)
                    gc.top_heap_words
                in
                Hashtbl.to_seq state.buckets
                |> List.of_seq
                |> List.sort (fun (a, _) (b, _) -> String.compare a b)
                |> List.iter (fun (phase, bucket) -> emit phase bucket))
              ~finally:(fun () -> close_out_noerr channel))
          ~finally:(fun () -> Mutex.unlock output_lock))
