type dependency = {
  declaration: Config.dependency;
  directory: string;
  kind: Package_traversal.dependency_kind;
}

type t = {
  root: string;
  build_owner: string;
  is_local: bool;
  config: Config.t;
  compile_config: Config.t;
  build_dir: string;
  ocaml_dir: string;
  dependencies: dependency list;
  gentype_dependency_args: string list;
  modules: Source.module_ list;
  source_mtimes: (string, float) Hashtbl.t;
  source_files: string list;
  present_source_files: string list;
}

type compilation = {
  regular_common_args: string list;
  development_common_args: string list;
  parse_paths: string list;
}
