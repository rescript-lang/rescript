val run :
  poll:(unit -> unit) ->
  verbosity:int ->
  folder:string ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  after_build:string option ->
  filter:Source_filter.t option ->
  no_timing:bool ->
  unit

val watch :
  verbosity:int ->
  folder:string ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  after_build:string option ->
  filter:Source_filter.t option ->
  clear_screen:bool ->
  unit
