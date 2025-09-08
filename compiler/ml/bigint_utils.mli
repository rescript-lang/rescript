val is_neg : string -> bool
val is_pos : string -> bool
val to_string : bool -> string -> string
val remove_leading_sign : string -> bool * string
val remove_leading_zeros : string -> string
val parse_bigint : string -> bool * string
val is_valid : string -> bool
val compare : bool * string -> bool * string -> int
