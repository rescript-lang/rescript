exception Stop

val run :
  root:string ->
  prod:bool ->
  features:string list option ->
  filter:string option ->
  clear_screen:bool ->
  show_progress:bool ->
  build:(poll:(unit -> unit) -> unit) ->
  unit
