type result = {status: Unix.process_status; stdout: string; stderr: string}
type job = {program: string; args: string list; cwd: string}

exception Error of string

val decode_utf8_lossy : string -> string
val succeeded : result -> bool
val status_string : Unix.process_status -> string
val default_max_jobs : int

val run_parallel :
  ?max_jobs:int ->
  ?poll:(unit -> unit) ->
  ?on_complete:(int -> unit) ->
  job list ->
  result list

type 'a work = {key: string; dependencies: string list; value: 'a}

val run_dependency_graph :
  ?max_jobs:int ->
  ?is_fatal:(exn -> bool) ->
  ?poll:(unit -> unit) ->
  'a work list ->
  next:('a -> result option -> job option) ->
  unit

val run :
  ?env:Spawn.Env.t ->
  ?poll:(unit -> unit) ->
  cwd:string ->
  string ->
  string list ->
  result
