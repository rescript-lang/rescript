type module_format = Config_types.module_format = Esmodule | Commonjs

type package_spec = Config_types.package_spec = {
  module_format: module_format;
  in_source: bool;
  suffix: string option;
}

type source = Config_types.source = {
  dir: string;
  recurse: bool;
  is_dev: bool;
  feature: string option;
}

type dependency = Config_types.dependency = {
  name: string;
  features: string list option;
}

type t = Config_types.t = {
  path: string;
  root: string;
  file_hash: string;
  name: string;
  sources: source list;
  sources_defined: bool;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  compiler_flags: string list;
  package_specs: package_spec list;
  suffix: string;
  namespace: string option;
  namespace_entry: string option;
  features: (string * string list) list;
  warning_flags: string list;
  ppx_flags: string list list;
  jsx_args: string list;
  source_map_args: string list;
  source_map_dev: bool;
  experimental_args: string list;
  gentype_args: string list;
  js_post_build: string option;
  allowed_dependents: string list option;
  deprecation_diagnostics: string list;
  diagnostics: string list;
}

exception Error of string

val namespace_from_package_name : string -> string
val path_in_root : string -> string
val exists_in_root : string -> bool
val source_is_dev : t -> string -> bool
val load : string -> t
val load_root : string -> t
val package_spec_suffix : t -> package_spec -> string
val module_format_name : module_format -> string
