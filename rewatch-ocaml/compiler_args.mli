val filter_ppx_flags :
  ?bisect_enabled:bool -> string list list -> string -> string list list

val compiler_flags :
  ?ppx_flags:string list list ->
  source_maps:bool ->
  watch:bool ->
  gentype:bool ->
  Config.t ->
  string list

val with_local_warning_policy : is_local:bool -> Config.t -> Config.t
val package_output : Config.t -> string -> Config.package_spec -> string
val gentype_dependency_args : Config.t -> string list
val namespace_args : Config.t -> string -> string list
