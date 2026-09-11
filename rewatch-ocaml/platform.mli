val normalize_path_for_comparison : string -> string
val directory_identity : path:string -> Unix.stats -> string
val canonicalize_path : string -> string
val resolve_program : cwd:string -> string -> string

val post_build_command :
  command:string -> output:string -> Spawn.Env.t option * string * string list

type process

val spawn :
  env:Spawn.Env.t option ->
  cwd:string ->
  program:string ->
  args:string list ->
  stdout:Unix.file_descr ->
  stderr:Unix.file_descr ->
  process

val process_id : process -> int
val release_process : process -> unit

val create_capture_pipes :
  unit ->
  (Unix.file_descr * Unix.file_descr) * (Unix.file_descr * Unix.file_descr)

val signal_process_tree : root_reaped:bool -> process -> int -> bool
val defer_termination_signals : unit -> unit -> unit
val graceful_termination_signal : int
val escalate_process_groups : bool

val process_is_active :
  run:(string -> string list -> (Unix.process_status * string) option) ->
  string ->
  bool
