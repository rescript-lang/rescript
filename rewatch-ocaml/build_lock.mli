type watch

val read_owner : string -> string option
val valid_owner : string -> bool
val acquire_build : string -> (unit -> unit)
val acquire_watch : string -> watch
val is_owned : watch -> bool
val release : watch -> unit
