val fail : string -> string -> 'a
val fail_read : string -> string -> 'a
val strip_read_path : string -> string -> string
val member : string -> (string * 'a) list -> 'a option

val optional_member :
  string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t option

val deduplicate_last : ('a * 'b) list -> ('a * 'b) list
val last_member : string -> (string * 'a) list -> 'a option

val last_optional_member :
  string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t option

val reject_duplicate_fields :
  string -> string -> string list -> (string * 'a) list -> unit

val string : string -> string -> Yojson.Safe.t -> string
val strings : string -> string -> Yojson.Safe.t -> string list

val namespace_from_package_name : string -> string
val compiler_flags : string -> string -> Yojson.Safe.t -> string list

val dependency_alias :
  string ->
  string ->
  string ->
  (string * Yojson.Safe.t) list ->
  Config_types.dependency list

val parse_sources :
  string -> (string * Yojson.Safe.t) list -> Config_types.source list

val unknown_fields : (string * Yojson.Safe.t) list -> string list
val parse_package_spec : string -> Yojson.Safe.t -> Config_types.package_spec
val package_specs_use_alias : string -> Yojson.Safe.t -> bool

val gentype_args :
  string ->
  string option ->
  Yojson.Safe.t option ->
  Config_types.dependency list ->
  Yojson.Safe.t ->
  string list
