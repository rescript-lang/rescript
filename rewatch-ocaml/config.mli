include module type of Config_types

val namespace_name : namespace -> string option
val namespace_entry : namespace -> string option
val namespace_compiler_name : namespace -> string option
val namespaced_module_name : namespace -> string -> string
val path_in_root : string -> string
val exists_in_root : string -> bool
val source_is_dev : t -> string -> bool
val load : string -> t
val load_root : string -> t
val package_spec_suffix : t -> package_spec -> string
val module_format_name : module_format -> string
