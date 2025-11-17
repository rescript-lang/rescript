open Common

type value_decl = {
  name: Name.t;
  path: Path.t;
  loc: Location.t;
  module_loc: Location.t;
  optional_args: OptionalArgs.t;
  side_effects: bool;
  is_toplevel: bool;
  pos_start: Lexing.position option;
  pos_end: Lexing.position option;
}

type general_decl = {
  name: Name.t;
  path: Path.t;
  loc: Location.t;
  module_loc: Location.t;
  decl_kind: DeclKind.t;
  pos_adjustment: posAdjustment;
  pos_start: Lexing.position option;
  pos_end: Lexing.position option;
}

type decl =
  | Value_decl of value_decl
  | General_decl of general_decl

type value_reference = {
  loc_from: Location.t;
  loc_to: Location.t;
  add_file_reference: bool;
  target_path: Common.Path.t option;
}

type type_reference = {pos_from: Lexing.position; pos_to: Lexing.position}

type t = {
  decls: Common.decl list;
  value_references: value_reference list;
  type_references: type_reference list;
}

val empty : t
val key_of_decl : decl -> Lexing.position
val to_common_decl :
  current_src:string -> current_module:string -> decl -> Common.decl option

