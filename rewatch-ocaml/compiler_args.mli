val filter_ppx_flags :
  ?bisect_enabled:bool -> string list list -> string -> string list list

val source_map_args : Config.t -> watch:bool -> string list

val parser_flags : ?ppx_flags:string list list -> Config.t -> string list
(** The flags of a parse, in the order they are passed to the parser. *)

val compiler_flags : watch:bool -> Config.t -> string list
(** The configured flags of a compile, including source maps and genType. *)

val with_local_warning_policy : is_local:bool -> Config.t -> Config.t
val gentype_dependency_args_from_paths :
  Config.t -> (Config.dependency * string) list -> string list

val namespace_args : Config.t -> is_dev:bool -> string -> string list
(** The namespace arguments of the module with the given name. Dev modules
    also open their package's dev map. *)

val parser_arguments :
  config:Config.t -> contents:string -> path:string -> string list

val compiler_arguments :
  project_root:string ->
  config:Config.t ->
  runtime:string ->
  dependency_dirs:string list ->
  module_name:string ->
  is_dev:bool ->
  source_kind:Source.source_kind ->
  has_interface:bool ->
  watch:bool ->
  gentype_dependency_args:string list ->
  path:string ->
  string list

val compiler_common_arguments :
  project_root:string ->
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
  is_dev:bool ->
  source_kind:Source.source_kind ->
  has_interface:bool ->
  path:string ->
  string list
