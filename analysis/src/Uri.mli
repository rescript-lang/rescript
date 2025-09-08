type t

val fromPath : string -> t
val isInterface : t -> bool
val stripPath : bool ref
val toPath : t -> string
val toString : t -> string
val toTopLevelLoc : t -> Location.t
val encodeURIComponent : string -> string
