exception Build_failure of string

(* CMI change has an explicit unknown case because a later publication failure
    must not erase the fact that an interface may already have become visible.
    Unknown therefore invalidates conservatively instead of pretending that the
    CMI was unchanged. *)
type cmi_change = Build_state.cmi_change =
  | Cmi_changed
  | Cmi_unchanged
  | Cmi_change_unknown
exception Publication_failure of exn * cmi_change

type publish_result = {stderr: string; cmi_change: cmi_change}
type namespace_task = {
  job: Process.job;
  publish: Process.result -> publish_result;
}
type post_build_task = {output: string; task: Process.task}

(* Publication can fail after the CMI was copied. The partial outcome is kept
    so dependents are still invalidated even though the module itself remains
    dirty for retry. *)
type publication =
  | Published of publish_result
  | Failed_after_cmi_publication of {error: exn; cmi_change: cmi_change}

val capture_publication : (unit -> publish_result) -> publication

type scheduled_module
type candidate

val create :
  key:string ->
  dependencies:string list ->
  source:Source.module_ ->
  state:Build_state.module_ ->
  cmi_path:string ->
  prepare:(unit -> unit) ->
  compile:(source_kind:Source.source_kind -> string -> Process.job) ->
  publish:
    (source_kind:Source.source_kind ->
    string ->
    Process.result ->
    publish_result) ->
  record_published_outputs:(source_kind:Source.source_kind -> string -> unit) ->
  post_build:(string -> post_build_task list) ->
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
