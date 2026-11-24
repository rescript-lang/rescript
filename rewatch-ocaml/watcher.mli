exception Stop

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed

module For_test : sig
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

  val run_with_discover :
    discover:
      (root:string ->
      prod:bool ->
      features:string list option ->
      filter:Source_filter.t option ->
      Watch_scope.t) ->
    root:string ->
    prod:bool ->
    features:string list option ->
    filter:Source_filter.t option ->
    clear_screen:bool ->
    show_progress:bool ->
    verbosity:int ->
    build:(poll:(unit -> unit) -> changes:change list option -> build_result) ->
    unit
  (** {!run} with [discover] in place of [Watch_scope.discover], to change
      files at the moments the watcher looks for them. *)
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
