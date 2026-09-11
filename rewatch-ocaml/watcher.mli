exception Stop

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed

module For_test : sig
  val polling_build_changes :
    previous:(string * float * int * string) list ->
    trigger:(string * float * int * string) list ->
    before_build:(string * float * int * string) list ->
    change list

  val changes_are_incremental : change list -> bool

  val run_with_native_failure :
    message:string ->
    on_fallback:(string -> unit) ->
    root:string ->
    prod:bool ->
    features:string list option ->
    filter:string option ->
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
  filter:string option ->
  clear_screen:bool ->
  show_progress:bool ->
  verbosity:int ->
  build:(poll:(unit -> unit) -> changes:change list option -> build_result) ->
  unit
