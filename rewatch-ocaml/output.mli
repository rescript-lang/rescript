val debug : verbosity:int -> string -> unit
val trace : verbosity:int -> string -> unit
val trace_enabled : int -> bool

module Progress : sig
  type t

  val create : enabled:bool -> color:bool -> t

  val start :
    t -> step:string -> symbol:string -> label:string -> total:int -> unit

  val advance : t -> unit
  val tick : t -> unit
  val finish : t -> unit
  val debug : t -> verbosity:int -> string -> unit

  val start_grouped :
    t ->
    step:string ->
    symbol:string ->
    label:string ->
    'a list ->
    int ->
    unit
end

val yellow : string -> string

val colors_enabled_with :
  getenv:(string -> string option) ->
  win32:bool ->
  interactive:bool ->
  bool

val colors_enabled : interactive:bool -> bool

val cleanup_message :
  color:bool ->
  step:string ->
  cleaned:int ->
  total:int ->
  seconds:float ->
  string

val compiler_cleanup_message : color:bool -> step:string -> string

val parsing_message :
  color:bool -> step:string -> count:int -> seconds:float -> string

val parsing_failed_message : color:bool -> step:string -> seconds:float -> string

val compiling_message :
  color:bool -> step:string -> count:int -> seconds:float -> string

val compilation_failed_message :
  color:bool -> step:string -> count:int -> seconds:float -> string

val finished_compilation_message :
  kind:string option -> warnings:bool -> seconds:float -> string

val should_clear_screen :
  clear_screen:bool -> show_progress:bool -> interactive:bool -> bool
