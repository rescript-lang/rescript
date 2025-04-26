type t

val compare : t -> t -> int
val assertFailure : t
val decodeError : t
val divisionByZero : t
val endOfFile : t
val exit : t
val failure : t
val fromLid : Longident.t -> t
val fromString : string -> t
val invalidArgument : t
val jsExn : t
val matchFailure : t
val notFound : t
val sysError : t
val toString : t -> string
val yojsonJsonError : t
val yojsonTypeError : t
