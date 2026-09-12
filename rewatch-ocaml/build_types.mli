type graph_dependency = {
  declaration: Config.dependency;
  directory: string;
  kind: Package_traversal.dependency_kind;
}

type graph_package = {
  graph_root: string;
  graph_build_owner: string;
  graph_is_local: bool;
  graph_config: Config.t;
  graph_compile_config: Config.t;
  graph_build_dir: string;
  graph_ocaml_dir: string;
  graph_dependencies: Config.dependency list;
  graph_dependency_directories: graph_dependency list;
  graph_gentype_dependency_args: string list;
  graph_modules: Source.module_ list;
  graph_source_mtimes: (string, float) Hashtbl.t;
  graph_source_files: string list;
  graph_present_source_files: string list;
}

type global_module = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  namespace: Config.namespace;
  allowed_dependencies: string list;
  mutable raw_dependencies: string list;
}

type namespace_map = {
  key: string;
  compiler_name: string;
  namespace: string;
  package_name: string;
  package_root: string;
  members: string list;
}

val namespace_map_key : string -> string

type parse_message = Parse_warning of string | Parse_error of string
val has_parse_error : parse_message list -> bool

type preliminary_parse =
  | Parsed_successfully of {stderr: string}
  | Parse_failed of {stdout: string; stderr: string}
  | Use_existing_ast

val preliminary_parse : Process.result -> preliminary_parse

type prepared_package = {
  regular_common_args: string list;
  development_common_args: string list;
  parse_paths: string list;
}

type prepared = {
  compiler_context: Compiler_info.context;
  compile_assets: Compile_assets.t;
  build_state: Build_state.t;
  packages: (string, prepared_package) Hashtbl.t;
}

type source_reference = {
  package_root: string;
  module_: Source.module_;
  relative_path: string;
  absolute_path: string;
}
