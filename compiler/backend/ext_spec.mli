type 'a t = (string * 'a * string) array

val assoc3 : 'a t -> string -> 'a option
