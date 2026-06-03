type t

val compare : t -> t -> int
val assert_failure : t
val decode_error : t
val division_by_zero : t
val end_of_file : t
val exit : t
val failure : t
val from_lid : Longident.t -> t
val from_string : string -> t
val invalid_argument : t
val js_exn : t
val match_failure : t
val not_found : t
val sys_error : t
val to_string : t -> string
val yojson_json_error : t
val yojson_type_error : t
