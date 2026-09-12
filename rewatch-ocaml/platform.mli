(** Platform-specific filesystem and process operations live behind this
    interface because path identity, command-line quoting, handle inheritance,
    and process-tree termination have materially different Unix and Windows
    contracts. Higher layers should not infer those rules from path strings. *)

val normalize_path_for_comparison : string -> string
val directory_identity : path:string -> Unix.stats -> string
val canonicalize_path : string -> string
val resolve_program : cwd:string -> string -> string
val terminal_supports_color_without_term : bool
val inherit_streaming_terminal_stdin : bool
val configure_standard_streams : unit -> unit
val clean_symbol : string
val parse_symbol : string
val build_symbol : string
val success_symbol : string
val warning_symbol : string
val error_symbol : string

type command = {env: Spawn.Env.t option; program: string; args: string list}

val post_build_command : command:string -> output:string -> command

type process

val spawn :
  env:Spawn.Env.t option ->
  cwd:string ->
  program:string ->
  args:string list ->
  stdin:Unix.file_descr ->
  stdout:Unix.file_descr ->
  stderr:Unix.file_descr ->
  process

val null_device : string

val current_process_id : unit -> int
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
