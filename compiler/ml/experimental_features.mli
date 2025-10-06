type feature = LetUnwrap | DeprecatedMigrations

val enable_from_string : string -> unit
val is_enabled : feature -> bool
val to_string : feature -> string
val reset : unit -> unit
