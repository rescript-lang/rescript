(* The boxed typed tree: [Typedtree]'s types with the type nodes, the
   declarations holding them and the environments of [Types_boxed]. It is the
   format [.cmt] files hold. [Cmt_boundary] converts between it and
   [Typedtree]. Keep it in step with typedtree.mli: the converters do not
   compile otherwise. Types without type nodes are [Typedtree]'s own. *)

open Asttypes
module B = Types_boxed

type summary =
  | Env_empty
  | Env_value of summary * Ident.t * B.value_description
  | Env_type of summary * Ident.t * B.type_declaration
  | Env_extension of summary * Ident.t * B.extension_constructor
  | Env_module of summary * Ident.t * B.module_declaration
  | Env_modtype of summary * Ident.t * B.modtype_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * B.type_declaration Env.Path_map.t
  | Env_copy_types of summary * string list

(* An environment reduced to its summary ([Env.keep_only_summary]): a record
   of [Env.t]'s shape whose summary and local constraints are boxed, built by
   [Cmt_boundary] from [Env.of_summary_parts]. *)
type env = Obj.t

type partial = Typedtree.partial = Partial | Total
type attribute = Parsetree.attribute
type attributes = attribute list
type pattern = {
  pat_desc: pattern_desc;
  pat_loc: Location.t;
  pat_extra: (pat_extra * Location.t * attributes) list;
  pat_type: B.type_expr;
  mutable pat_env: env;
  pat_attributes: attributes;
}
and record_pat_rest = {
  rest_ident: Ident.t;
  rest_name: string loc;
  rest_type: B.type_expr;
  excluded_runtime_labels: string list;
}
and pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path.t * Longident.t loc
  | Tpat_unpack
and pattern_desc =
  | Tpat_any
  | Tpat_var of Ident.t * string loc
  | Tpat_alias of pattern * Ident.t * string loc
  | Tpat_constant of constant
  | Tpat_interval of constant * constant
  | Tpat_tuple of pattern list
  | Tpat_construct of Longident.t loc * B.constructor_description * pattern list
  | Tpat_variant of label * pattern option * B.row_desc ref
  | Tpat_record of
      (Longident.t loc * B.label_description * pattern * bool) list
      * closed_flag
      * record_pat_rest option
  | Tpat_dict of dict_pattern_entry list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * B.row_desc option
and dict_pattern_entry = {
  tdp_key: string loc;
  tdp_pattern: pattern;
  tdp_optional: bool;
}
and expression = {
  exp_desc: expression_desc;
  exp_loc: Location.t;
  exp_extra: (exp_extra * Location.t * attributes) list;
  exp_type: B.type_expr;
  exp_env: env;
  exp_attributes: attributes;
}
and exp_extra =
  | Texp_constraint of core_type
  | Texp_coerce of {
      source_type: B.type_expr;
      target: core_type;
      target_type: B.type_expr;
    }
  | Texp_open of override_flag * Path.t * Longident.t loc * env
and expression_desc =
  | Texp_ident of Path.t * Longident.t loc * B.value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_function of {
      params: function_param list;
      body: expression;
      async: bool;
    }
  | Texp_apply of {
      funct: expression;
      args: (arg_label * expression option) list;
      partial: bool;
      transformed_jsx: bool;
    }
  | Texp_match of expression * case list * case list * partial
  | Texp_try of expression * case list * expression option
  | Texp_tuple of expression list
  | Texp_construct of
      Longident.t loc * B.constructor_description * expression list
  | Texp_variant of label * expression option
  | Texp_record of {
      fields: (B.label_description * Longident.t loc * expression) array;
      representation: Types.record_representation;
      extended_expression: expression option;
    }
  | Texp_field of expression * Longident.t loc * B.label_description
  | Texp_setfield of
      expression * Longident.t loc * B.label_description * expression
  | Texp_array of expression list
  | Texp_dict of dict_entry list
  | Texp_spread of Parsetree.spread_kind * expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_break
  | Texp_continue
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t
      * Parsetree.pattern
      * expression
      * expression
      * direction_flag
      * expression
  | Texp_object_get of expression * string Asttypes.loc
  | Texp_object_set of expression * string Asttypes.loc * expression
  | Texp_object_literal of (string Asttypes.loc * expression) list
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression
  | Texp_pack of module_expr
  | Texp_for_of of Ident.t * Parsetree.pattern * expression * expression
  | Texp_for_await_of of Ident.t * Parsetree.pattern * expression * expression
  | Texp_tagged_template of {
      tag: expression;
      raw_sources: string list;
      values: expression list;
    }
  | Texp_template of {segments: template_segment list; values: expression list}
and case = {c_lhs: pattern; c_guard: expression option; c_rhs: expression}
and dict_entry =
  | Tdict_entry of string loc * expression
  | Tdict_spread of expression
and function_param = {
  fp_lbl: arg_label;
  fp_param: Ident.t;
  fp_pat: pattern;
  fp_partial: partial;
  fp_has_default: bool;
}
and module_expr = {
  mod_desc: module_expr_desc;
  mod_loc: Location.t;
  mod_type: B.module_type;
  mod_env: env;
  mod_attributes: attributes;
}
and module_type_constraint =
  | Tmodtype_implicit
  | Tmodtype_explicit of module_type
and module_expr_desc =
  | Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * string loc * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * B.module_type * module_type_constraint * module_coercion
  | Tmod_unpack of expression * B.module_type
and structure = {
  str_items: structure_item list;
  str_type: B.signature;
  str_final_env: env;
}
and structure_item = {
  str_desc: structure_item_desc;
  str_loc: Location.t;
  str_env: env;
}
and structure_item_desc =
  | Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of extension_constructor
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_description
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute
and module_binding = {
  mb_id: Ident.t;
  mb_name: string loc;
  mb_expr: module_expr;
  mb_attributes: attributes;
  mb_loc: Location.t;
}
and value_binding = {
  vb_pat: pattern;
  vb_expr: expression;
  vb_attributes: attributes;
  vb_loc: Location.t;
}
and module_coercion =
  | Tcoerce_none
  | Tcoerce_structure of
      (int * module_coercion) list
      * (Ident.t * int * module_coercion) list
      * string list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Path.t * module_coercion
and module_type = {
  mty_desc: module_type_desc;
  mty_type: B.module_type;
  mty_env: env;
  mty_loc: Location.t;
  mty_attributes: attributes;
}
and module_type_desc =
  | Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * string loc * module_type option * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc
and primitive_coercion = {
  pc_desc: Primitive.description;
  pc_type: B.type_expr;
  pc_env: env;
  pc_loc: Location.t;
  pc_id: Ident.t;
}
and signature = {
  sig_items: signature_item list;
  sig_type: B.signature;
  sig_final_env: env;
}
and signature_item = {
  sig_desc: signature_item_desc;
  sig_env: env;
  sig_loc: Location.t;
}
and signature_item_desc =
  | Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of extension_constructor
  | Tsig_module of module_declaration
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_attribute of attribute
and module_declaration = {
  md_id: Ident.t;
  md_name: string loc;
  md_type: module_type;
  md_attributes: attributes;
  md_loc: Location.t;
}
and module_type_declaration = {
  mtd_id: Ident.t;
  mtd_name: string loc;
  mtd_type: module_type option;
  mtd_attributes: attributes;
  mtd_loc: Location.t;
}
and open_description = Typedtree.open_description
and 'a include_infos = {
  incl_mod: 'a;
  incl_type: B.signature;
  incl_loc: Location.t;
  incl_attributes: attribute list;
}
and include_description = module_type include_infos
and include_declaration = module_expr include_infos
and with_constraint =
  | Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc
and core_type = {
  ctyp_desc: core_type_desc;
  ctyp_type: B.type_expr;
  ctyp_env: env;
  ctyp_loc: Location.t;
  ctyp_attributes: attributes;
}
and arg = {attrs: attributes; lbl: arg_label; typ: core_type}
and core_type_desc =
  | Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg list * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of object_field list * closed_flag
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type
and package_type = {
  pack_path: Path.t;
  pack_fields: (Longident.t loc * core_type) list;
}
and row_field =
  | Ttag of string loc * attributes * bool * core_type list
  | Tinherit of core_type
and object_field =
  | OTtag of string loc * attributes * core_type
  | OTinherit of core_type
and value_description = {
  val_id: Ident.t;
  val_name: string loc;
  val_desc: core_type;
  val_val: B.value_description;
  val_prim: Parsetree.primitive_repr option;
  val_loc: Location.t;
  val_attributes: attributes;
}
and type_declaration = {
  typ_id: Ident.t;
  typ_name: string loc;
  typ_params: (core_type * variance) list;
  typ_type: B.type_declaration;
  typ_cstrs: (core_type * core_type * Location.t) list;
  typ_kind: type_kind;
  typ_private: private_flag;
  typ_manifest: core_type option;
  typ_loc: Location.t;
  typ_attributes: attributes;
}
and type_kind =
  | Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open
and label_declaration = {
  ld_id: Ident.t;
  ld_name: string loc;
  ld_runtime_name: string option;
  ld_mutable: mutable_flag;
  ld_optional: bool;
  ld_type: core_type;
  ld_loc: Location.t;
  ld_attributes: attributes;
}
and constructor_declaration = {
  cd_id: Ident.t;
  cd_name: string loc;
  cd_runtime_tag: Variant_runtime.literal_tag option;
  cd_args: constructor_arguments;
  cd_res: core_type option;
  cd_loc: Location.t;
  cd_attributes: attributes;
}
and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list
and type_extension = {
  tyext_path: Path.t;
  tyext_params: (core_type * variance) list;
  tyext_constructors: extension_constructor list;
  tyext_private: private_flag;
  tyext_attributes: attributes;
}
and extension_constructor = {
  ext_id: Ident.t;
  ext_name: string loc;
  ext_type: B.extension_constructor;
  ext_kind: extension_constructor_kind;
  ext_loc: Location.t;
  ext_attributes: attributes;
}
and extension_constructor_kind =
  | Text_decl of constructor_arguments * core_type option
  | Text_rebind of Path.t * Longident.t loc
