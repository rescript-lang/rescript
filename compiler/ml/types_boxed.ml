(* The boxed representation of type nodes and of the declarations holding
   them: [Types] as it was before type nodes became indices into a store
   (see types.mli). It is the format of the type graphs [Marshal] writes to
   [.cmi] and [.cmt] files and reads back, so it must not change without the
   compiled files changing with it.

   Only the types which contain type nodes are repeated here; the others are
   [Types]' own. [Type_arena.Import] and [Type_arena.Export] convert between
   these graphs and the store, keeping their sharing, and therefore their
   marshalled bytes. *)

type type_expr = {mutable desc: type_desc; mutable level: int; id: int}

and arg = {lbl: Asttypes.arg_label; typ: type_expr}

and type_desc =
  | Tvar of string option
  | Tarrow of arg list * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr
  | Tfield of {
      name: string;
      mutability: Types.field_mutability ref;
      typ: type_expr;
      rest: type_expr;
    }
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * Longident.t list * type_expr list

and row_desc = {
  row_fields: (Asttypes.label * row_field) list;
  row_more: type_expr;
  row_closed: bool;
  row_fixed: bool;
  row_name: (Path.t * type_expr list) option;
}

and row_field =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
  | Rabsent

and abbrev_memo =
  | Mnil
  | Mcons of
      Asttypes.private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

type value_description = {
  val_type: type_expr;
  val_kind: Types.value_kind;
  val_loc: Location.t;
  val_attributes: Parsetree.attributes;
}

type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_kind;
  type_private: Asttypes.private_flag;
  type_manifest: type_expr option;
  type_variance: Types.Variance.t list;
  type_newtype_level: (int * int) option;
  type_loc: Location.t;
  type_attributes: Parsetree.attributes;
  type_immediate: bool;
  type_representation: Types.type_representation;
  type_shape: Variant_runtime.value_shape;
  type_inlined_types: type_inlined_type list;
}

and type_inlined_type =
  | Record of {type_name: string; labels: label_declaration list}

and type_kind =
  | Type_abstract
  | Type_record of label_declaration list * Types.record_representation
  | Type_variant of constructor_declaration list * Variant_runtime.layout_ref
  | Type_open

and label_declaration = {
  ld_id: Ident.t;
  ld_runtime_name: string option;
  ld_mutable: Asttypes.mutable_flag;
  ld_optional: bool;
  ld_type: type_expr;
  ld_loc: Location.t;
  ld_attributes: Parsetree.attributes;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_runtime_tag: Variant_runtime.literal_tag option;
  cd_args: constructor_arguments;
  cd_res: type_expr option;
  cd_loc: Location.t;
  cd_attributes: Parsetree.attributes;
}

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

type extension_constructor = {
  ext_type_path: Path.t;
  ext_type_params: type_expr list;
  ext_args: constructor_arguments;
  ext_ret_type: type_expr option;
  ext_private: Asttypes.private_flag;
  ext_loc: Location.t;
  ext_attributes: Parsetree.attributes;
  ext_is_exception: bool;
}

type module_type =
  | Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of Ident.t * module_type option * module_type
  | Mty_alias of Types.alias_presence * Path.t

and signature = signature_item list

and signature_item =
  | Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * Types.rec_status
  | Sig_typext of Ident.t * extension_constructor * Types.ext_status
  | Sig_module of Ident.t * module_declaration * Types.rec_status
  | Sig_modtype of Ident.t * modtype_declaration

and module_declaration = {
  md_type: module_type;
  md_attributes: Parsetree.attributes;
  md_loc: Location.t;
}

and modtype_declaration = {
  mtd_type: module_type option;
  mtd_attributes: Parsetree.attributes;
  mtd_loc: Location.t;
}

type constructor_description = {
  cstr_name: string;
  cstr_res: type_expr;
  cstr_existentials: type_expr list;
  cstr_args: type_expr list;
  cstr_arity: int;
  cstr_kind: Types.constructor_kind;
  cstr_generalized: bool;
  cstr_private: Asttypes.private_flag;
  cstr_loc: Location.t;
  cstr_attributes: Parsetree.attributes;
  cstr_inlined: type_declaration option;
}

type label_description = {
  lbl_name: string;
  lbl_runtime_name: string;
  lbl_res: type_expr;
  lbl_arg: type_expr;
  lbl_mut: Asttypes.mutable_flag;
  lbl_optional: bool;
  lbl_pos: int;
  mutable lbl_all: label_description array;
  lbl_repres: Types.record_representation;
  lbl_private: Asttypes.private_flag;
  lbl_loc: Location.t;
  lbl_attributes: Parsetree.attributes;
}
