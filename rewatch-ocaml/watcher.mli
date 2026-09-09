exception Stop

val run :
  root:string ->
  prod:bool ->
  clear_screen:bool ->
  build:(poll:(unit -> unit) -> unit) ->
  unit
