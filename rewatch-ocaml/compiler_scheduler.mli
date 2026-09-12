exception Build_failure of string
type cmi_change = Build_state.cmi_change =
  | Cmi_changed
  | Cmi_unchanged
  | Cmi_change_unknown
exception Publication_failure of exn * cmi_change

type publish_result = {stderr: string; cmi_change: cmi_change}

type scheduled_module
type candidate

val create :
  key:string ->
  dependencies:string list ->
  source:Source.module_ ->
  state:Build_state.module_ ->
  cmi_path:string ->
  prepare:(unit -> unit) ->
  compile:(is_interface:bool -> string -> Process.job) ->
  publish:(is_interface:bool -> string -> Process.result -> publish_result) ->
  record_published_outputs:(is_interface:bool -> string -> unit) ->
  post_build:(string -> (string * Process.task) list) ->
  package_root:string ->
  is_local:bool ->
  mark_warning:(string -> unit) ->
  scheduled_module

val candidate :
  key:string ->
  state:Build_state.module_ ->
  warning_paths:string list ->
  make:(unit -> scheduled_module) ->
  candidate

val candidate_requires_compile : candidate -> bool

val run :
  poll:(unit -> unit) option ->
  warning_state:Warning_state.t ->
  compile_assets:Compile_assets.t ->
  build_state:Build_state.t ->
  candidates:candidate list ->
  mark_compiled:(unit -> unit) ->
  mark_had_warnings:(unit -> unit) ->
  progress:Output.Progress.t ->
  compile_step:string ->
  namespace_count:int ->
  verbosity:int ->
  unit
