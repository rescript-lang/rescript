exception Build_failure of string

type scheduled_module

val create :
  key:string ->
  dependencies:string list ->
  source:Source.module_ ->
  state:Build_state.module_ ->
  cmi_path:string ->
  prepare:(unit -> unit) ->
  compile:(is_interface:bool -> string -> Process.job) ->
  publish:(is_interface:bool -> string -> Process.result -> string) ->
  post_build:(string -> (string * Process.task) list) ->
  package_root:string ->
  is_local:bool ->
  mark_warning:(string -> unit) ->
  scheduled_module

val requires_compile : scheduled_module -> bool

val run :
  poll:(unit -> unit) option ->
  warning_state:Warning_state.t ->
  blocked_modules:(string, unit) Hashtbl.t ->
  compile_assets:Compile_assets.t ->
  build_state:Build_state.t ->
  scheduled_modules:scheduled_module list ->
  compile_cleanup:(unit -> unit) list ->
  mark_compiled:(unit -> unit) ->
  mark_had_warnings:(unit -> unit) ->
  progress:Output.Progress.t ->
  compile_step:string ->
  namespace_count:int ->
  verbosity:int ->
  unit
