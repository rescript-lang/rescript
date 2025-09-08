type t

val compare : t -> t -> int
val create : ?isInterface:bool -> string -> t
val isUnderscore : t -> bool
val startsWithUnderscore : t -> bool
val toImplementation : t -> t
val toInterface : t -> t
val toString : t -> string
