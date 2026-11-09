(** Process scheduling has two policies but one child-lifecycle owner. Parallel
    lists stop immediately on interruption; dependency graphs can stop admitting
    new work while already-started children drain. {!Process_child} owns the
    operating-system resources in both cases. *)

type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}
type task

exception Error of string
exception Interrupted of int

val succeeded : result -> bool
val status_string : Unix.process_status -> string
val default_max_jobs : int

val worker_allocated_words : unit -> int
(** The words that the worker domains of finished worker pools allocated,
    which the runtime's report at exit leaves out. *)

val task : ?env:Spawn.Env.t -> ?on_result:(result -> result) -> job -> task
val concurrent_task :
  cancel:(unit -> unit) ->
  ?on_result:(result -> result) ->
  (unit -> result) ->
  task
val map_result : task -> (result -> result) -> task

val adopt_speculation : task
(** Returned by [next value None] to make the work's running speculation its
    first task instead of starting a new one. *)

type mailbox
(** Messages from running tasks to the domain that schedules a dependency
    graph. *)

val create_mailbox : unit -> mailbox

val post : mailbox -> (unit -> unit) -> unit
(** Queue [message] to run on the scheduling domain between completions, and
    wake it. Safe from any domain. *)

type 'a speculation = {
  mailbox: mailbox;
  candidates: unit -> string list;
      (** Keys of works that may start before their dependencies complete. *)
  start: 'a -> task option;
      (** The speculative first task of a waiting work, or [None]. *)
  abandon: 'a -> unit;
      (** The work's speculation will not be adopted. Its task must then finish
          without effects; its result is discarded. *)
}
(** Speculation lets a work start while its dependencies are still running,
    when free job slots would otherwise idle. The task must not let its result
    or effects become visible before the work is ready: [next value None] then
    adopts it with {!adopt_speculation}, and its result advances the graph like
    any first task. Any other answer abandons it. A speculation whose work can
    no longer become ready (a dependency failed, the graph stopped, or nothing
    else is running) is abandoned as well. *)

type 'a work = {key: string; dependencies: string list; value: 'a}

(* [Stop_new_work] preserves results from children that already started while
    ensuring no newly ready dependency is launched after a build failure.
    [Continue_independent_work] also drains work that was already ready, while
    failed prerequisites continue to block their dependents. *)
type failure_action =
  | Abort_immediately
  | Stop_new_work
  | Continue_independent_work

val run_dependency_graph :
  ?max_jobs:int ->
  ?on_failure:(exn -> failure_action) ->
  ?poll:(unit -> unit) ->
  ?speculation:'a speculation ->
  'a work list ->
  next:('a -> result option -> task option) ->
  unit

val run_tasks :
  ?max_jobs:int ->
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  task list ->
  result list

val run : ?poll:(unit -> unit) -> cwd:string -> string -> string list -> result

val run_streaming :
  ?poll:(unit -> unit) -> cwd:string -> string -> string list -> result
