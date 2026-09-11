type watch

val read_owner : string -> string option
val valid_owner : string -> bool
val with_build : ?poll:(unit -> unit) -> string -> (release:(unit -> unit) -> 'a) -> 'a
val with_watch : string -> (watch -> 'a) -> 'a
val is_owned : watch -> bool
