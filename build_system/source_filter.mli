type t

val compile : string -> (t, string) result
val pattern : t -> string
val matches_basename : t -> string -> bool
