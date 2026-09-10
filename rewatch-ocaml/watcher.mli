exception Stop

type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}
type build_result = Succeeded | Failed

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
