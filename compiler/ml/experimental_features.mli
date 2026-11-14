type feature = LetUnwrap

val from_string : string -> feature option
val enable_from_string : string -> unit

val is_enabled : feature -> bool
val to_string : feature -> string
val reset : unit -> unit
