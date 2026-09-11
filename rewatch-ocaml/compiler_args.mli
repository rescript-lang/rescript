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

val parser_arguments :
  config:Config.t -> contents:string -> path:string -> string list

val compiler_arguments :
  config:Config.t ->
  runtime:string ->
  dependency_dirs:string list ->
  module_name:string ->
  is_interface:bool ->
  has_interface:bool ->
  watch:bool ->
  gentype_dependency_args:string list ->
  path:string ->
  string list
