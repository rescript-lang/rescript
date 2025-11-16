open Common

type position = {file: string; line: int; column: int; cnum: int; bol: int}

type range = {start_: position; end_: position}

type optional_args_snapshot = {
  unused: string list;
  always_used: (string * int) list;
  call_count: int;
}

type value_kind = {
  is_toplevel: bool;
  side_effects: bool;
  optional_args: optional_args_snapshot;
}

type decl_kind =
  | Value of value_kind
  | Exception
  | RecordLabel
  | VariantCase

type decl = {
  path: string list;
  module_path: string list;
  name: string;
  loc: range;
  module_loc: range option;
  decl_kind: decl_kind;
  pos_adjustment: posAdjustment option;
}

type value_reference = {
  loc_from: position;
  loc_to: position;
  add_file_edge: bool;
}

type type_reference = {pos_from: position; pos_to: position}

type file_edge = {from_file: string; to_file: string}

type t = {
  version: int;
  source_file: string;
  digest: string;
  decls: decl list;
  value_references: value_reference list;
  type_references: type_reference list;
  file_edges: file_edge list;
}

val version : int

exception Invalid_format of string

val of_collected : source_file:string -> Collected_types.t -> t

val to_json : ?include_digest:bool -> t -> Json.t
val of_json : Json.t -> t
val to_string : ?pretty:bool -> t -> string
val of_string : string -> t
val canonical_string : t -> string
val verify_digest : t -> unit
val to_common_decl : decl -> Common.decl
val to_common_decls : t -> Common.decl list

