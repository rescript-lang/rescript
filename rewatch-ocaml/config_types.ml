type module_format = Esmodule | Commonjs
type namespace =
  | No_namespace
  | Namespace of string
  | Namespace_with_entry of {name: string; entry: string}

type package_spec = {
  module_format: module_format;
  in_source: bool;
  suffix: string option;
}

type source = {dir: string; recurse: bool; is_dev: bool; feature: string option}

type dependency = {name: string; features: string list option}

type t = {
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
  namespace: namespace;
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
