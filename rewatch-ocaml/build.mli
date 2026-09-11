exception Error of string
exception Package_error of string
exception Stop_watch

val clean :
  poll:(unit -> unit) ->
  seen:string list ->
  verbosity:int ->
  folder:string ->
  prod:bool ->
  unit

val compiler_args : string -> string

val run :
  poll:(unit -> unit) ->
  seen:string list ->
  verbosity:int ->
  folder:string ->
  prod:bool ->
  features:string list option ->
  warn_error:string option ->
  watch:bool ->
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
