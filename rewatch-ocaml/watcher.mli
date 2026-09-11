exception Stop

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed

type snapshot_entry = {path: string; modified: float; size: int; digest: string}

module For_test : sig
  val is_control_file_name : string -> bool

  val polling_build_changes :
    previous:snapshot_entry list ->
    trigger:snapshot_entry list ->
    before_build:snapshot_entry list ->
    change list

  val changes_are_incremental : change list -> bool

  val run_with_native_failure :
    message:string ->
    on_fallback:(string -> unit) ->
    root:string ->
    prod:bool ->
    features:string list option ->
    filter:Source_filter.t option ->
    clear_screen:bool ->
    show_progress:bool ->
    verbosity:int ->
    build:(poll:(unit -> unit) -> changes:change list option -> build_result) ->
    unit
end

val run :
  root:string ->
  prod:bool ->
  features:string list option ->
  filter:Source_filter.t option ->
  clear_screen:bool ->
  show_progress:bool ->
  verbosity:int ->
  build:(poll:(unit -> unit) -> changes:change list option -> build_result) ->
  unit
