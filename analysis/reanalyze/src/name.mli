type t

val compare : t -> t -> int
val create : ?is_interface:bool -> string -> t
val is_underscore : t -> bool
val starts_with_underscore : t -> bool
val to_implementation : t -> t
val to_interface : t -> t
val to_string : t -> string
