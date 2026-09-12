(** Process scheduling has two policies but one child-lifecycle owner. Parallel
    lists stop immediately on interruption; dependency graphs can stop admitting
    new work while already-started children drain. {!Process_child} owns the
    operating-system resources in both cases. *)

type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}
type task

exception Error of string
exception Interrupted of int

val decode_utf8_lossy : string -> string
val succeeded : result -> bool
val status_string : Unix.process_status -> string
val default_max_jobs : int

val task : ?env:Spawn.Env.t -> ?on_result:(result -> result) -> job -> task

val run_parallel :
  ?max_jobs:int ->
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  job list ->
  result list

val run_parallel_map :
  ?max_jobs:int ->
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  'a list ->
  job:('a -> job) ->
  result list

type 'a work = {key: string; dependencies: string list; value: 'a}

(* [Stop_new_work] preserves results from children that already started while
    ensuring no newly ready dependency is launched after a build failure.
    [Continue_independent_work] also drains work that was already ready, while
    failed prerequisites continue to block their dependents. *)
type failure_action = Abort_immediately | Stop_new_work | Continue_independent_work

val run_dependency_graph :
  ?max_jobs:int ->
  ?on_failure:(exn -> failure_action) ->
  ?poll:(unit -> unit) ->
  'a work list ->
  next:('a -> result option -> task option) ->
  unit

val run : ?poll:(unit -> unit) -> cwd:string -> string -> string list -> result

val run_streaming :
  ?poll:(unit -> unit) -> cwd:string -> string -> string list -> result
