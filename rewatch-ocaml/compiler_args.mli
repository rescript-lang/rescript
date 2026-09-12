val filter_ppx_flags :
  ?bisect_enabled:bool -> string list list -> string -> string list list

val source_map_args : Config.t -> watch:bool -> string list

val compiler_flags :
  ?ppx_flags:string list list ->
  source_maps:bool ->
  watch:bool ->
  gentype:bool ->
  Config.t ->
  string list

val with_local_warning_policy : is_local:bool -> Config.t -> Config.t
val gentype_dependency_args_from_paths :
  Config.t -> (Config.dependency * string) list -> string list

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

val compiler_common_arguments :
  config:Config.t ->
  runtime:string ->
  dependency_dirs:string list ->
  watch:bool ->
  gentype_dependency_args:string list ->
  string list

val compiler_arguments_with_common :
  config:Config.t ->
  common_args:string list ->
  module_name:string ->
  is_interface:bool ->
  has_interface:bool ->
  path:string ->
  string list
