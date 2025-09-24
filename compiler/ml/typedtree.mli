(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Abstract syntax tree after typing *)

(** By comparison with {!Parsetree}:
    - Every {!Longindent.t} is accompanied by a resolved {!Path.t}.

*)

open Asttypes
open Types

(* Value expressions for the core language *)

type partial = Partial | Total

(** {1 Extension points} *)

type attribute = Parsetree.attribute
type attributes = attribute list

(** {1 Core language} *)

type pattern = {
  pat_desc: pattern_desc;
  pat_loc: Location.t;
  pat_extra: (pat_extra * Location.t * attributes) list;
  pat_type: type_expr;
  mutable pat_env: Env.t;
  pat_attributes: attributes;
}

and pat_extra =
  | Tpat_constraint of core_type
      (** P : T          { pat_desc = P
                           ; pat_extra = (Tpat_constraint T, _, _) :: ... }
         *)
  | Tpat_type of Path.t * Longident.t loc
      (** #tconst        { pat_desc = disjunction
                           ; pat_extra = (Tpat_type (P, "tconst"), _, _) :: ...}

                           where [disjunction] is a [Tpat_or _] representing the
                           branches of [tconst].
         *)
  | Tpat_open of Path.t * Longident.t loc * Env.t
  | Tpat_unpack
      (** (module P)     { pat_desc  = Tpat_var "P"
                           ; pat_extra = (Tpat_unpack, _, _) :: ... }
         *)

and pattern_desc =
  | Tpat_any  (** _ *)
  | Tpat_var of Ident.t * string loc  (** x *)
  | Tpat_alias of pattern * Ident.t * string loc  (** P as a *)
  | Tpat_constant of constant  (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Tpat_tuple of pattern list
      (** (P1, ..., Pn)

            Invariant: n >= 2
         *)
  | Tpat_construct of Longident.t loc * constructor_description * pattern list
      (** C                []
            C P              [P]
            C (P1, ..., Pn)  [P1; ...; Pn]
          *)
  | Tpat_variant of label * pattern option * row_desc ref
      (** `A             (None)
            `A P           (Some P)

            See {!Types.row_desc} for an explanation of the last parameter.
         *)
  | Tpat_record of
      (Longident.t loc * label_description * pattern * bool (* optional *)) list
      * closed_flag
      (** { l1=P1; ...; ln=Pn }     (flag = Closed)
            { l1=P1; ...; ln=Pn; _}   (flag = Open)

            Invariant: n > 0
         *)
  | Tpat_array of pattern list  (** [| P1; ...; Pn |] *)
  | Tpat_or of pattern * pattern * row_desc option
      (** P1 | P2

            [row_desc] = [Some _] when translating [Ppat_type _],
                         [None] otherwise.
         *)

and expression = {
  exp_desc: expression_desc;
  exp_loc: Location.t;
  exp_extra: (exp_extra * Location.t * attributes) list;
  exp_type: type_expr;
  exp_env: Env.t;
  exp_attributes: attributes;
}

and exp_extra =
  | Texp_constraint of core_type  (** E : T *)
  | Texp_coerce of core_type  (** E :> T           [Texp_coerce T]
         *)
  | Texp_open of override_flag * Path.t * Longident.t loc * Env.t
      (** let open[!] M in    [Texp_open (!, P, M, env)]
                                where [env] is the environment after opening [P]
         *)
  | Texp_newtype of string  (** fun (type t) ->  *)

and expression_desc =
  | Texp_ident of Path.t * Longident.t loc * Types.value_description
      (** x
            M.x
         *)
  | Texp_constant of constant  (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Texp_let of rec_flag * value_binding list * expression
      (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of {
      arg_label: Noloc.arg_label;
      arity: arity;
      param: Ident.t;
      case: case;
      partial: partial;
      async: bool;
    }
      (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            [param] is the identifier that is to be used to name the
            parameter of the function.

            partial =
              [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of {
      funct: expression;
      args: (Noloc.arg_label * expression option) list;
      partial: bool;
      transformed_jsx: bool;
    }
      (** E0 ~l1:E1 ... ~ln:En

            The expression can be None if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, None);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])
         *)
  | Texp_match of expression * case list * case list * partial
      (** match E0 with
            | P1 -> E1
            | P2 -> E2
            | exception P3 -> E3

            [Texp_match (E0, [(P1, E1); (P2, E2)], [(P3, E3)], _)]
         *)
  | Texp_try of expression * case list
      (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_tuple of expression list  (** (E1, ..., EN) *)
  | Texp_construct of
      Longident.t loc * constructor_description * expression list
      (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | Texp_variant of label * expression option
  | Texp_record of {
      fields:
        (Types.label_description
        * record_label_definition
        * bool (* optional *))
        array;
      representation: Types.record_representation;
      extended_expression: expression option;
    }
      (** { l1=P1; ...; ln=Pn }           (extended_expression = None)
            { E0 with l1=P1; ...; ln=Pn }   (extended_expression = Some E0)

            Invariant: n > 0

            If the type is { l1: t1; l2: t2 }, the expression
            { E0 with t2=P2 } is represented as
            Texp_record
              { fields = [| l1, Kept t1; l2 Override P2 |]; representation;
                extended_expression = Some E0 }
        *)
  | Texp_field of expression * Longident.t loc * label_description
  | Texp_setfield of
      expression * Longident.t loc * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t
      * Parsetree.pattern
      * expression
      * expression
      * direction_flag
      * expression
  | Texp_send of expression * meth * expression option
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression
  | Texp_pack of module_expr
  | Texp_extension_constructor of Longident.t loc * Path.t

and meth = Tmeth_name of string

and case = {c_lhs: pattern; c_guard: expression option; c_rhs: expression}

and record_label_definition =
  | Kept of Types.type_expr
  | Overridden of Longident.t loc * expression

(* Value expressions for the module language *)
and module_expr = {
  mod_desc: module_expr_desc;
  mod_loc: Location.t;
  mod_type: Types.module_type;
  mod_env: Env.t;
  mod_attributes: attributes;
}

(** Annotations for [Tmod_constraint]. *)
and module_type_constraint =
  | Tmodtype_implicit
      (** The module type constraint has been synthesized during typechecking. *)
  | Tmodtype_explicit of module_type
      (** The module type was in the source file. *)

and module_expr_desc =
  | Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * string loc * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
      (** ME          (constraint = Tmodtype_implicit)
        (ME : MT)   (constraint = Tmodtype_explicit MT)
     *)
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items: structure_item list;
  str_type: Types.signature;
  str_final_env: Env.t;
}

and structure_item = {
  str_desc: structure_item_desc;
  str_loc: Location.t;
  str_env: Env.t;
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
      * string list (* runtime fields *)
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Path.t * module_coercion

and module_type = {
  mty_desc: module_type_desc;
  mty_type: Types.module_type;
  mty_env: Env.t;
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
  pc_type: type_expr;
  pc_env: Env.t;
  pc_loc: Location.t;
  pc_id: Ident.t;
}

and signature = {
  sig_items: signature_item list;
  sig_type: Types.signature;
  sig_final_env: Env.t;
}

and signature_item = {
  sig_desc: signature_item_desc;
  sig_env: Env.t; (* BINANNOT ADDED *)
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

and open_description = {
  open_path: Path.t;
  open_txt: Longident.t loc;
  open_override: override_flag;
  open_loc: Location.t;
  open_attributes: attribute list;
}

and 'a include_infos = {
  incl_mod: 'a;
  incl_type: Types.signature;
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
  ctyp_type: type_expr;
  ctyp_env: Env.t; (* BINANNOT ADDED *)
  ctyp_loc: Location.t;
  ctyp_attributes: attributes;
}

and arg = {attrs: attributes; lbl: Noloc.arg_label; typ: core_type}

and core_type_desc =
  | Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg * core_type * arity
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
  pack_type: Types.module_type;
  pack_txt: Longident.t loc;
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
  val_val: Types.value_description;
  val_prim: string list;
  val_loc: Location.t;
  val_attributes: attributes;
}

and type_declaration = {
  typ_id: Ident.t;
  typ_name: string loc;
  typ_params: (core_type * variance) list;
  typ_type: Types.type_declaration;
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
  ld_mutable: mutable_flag;
  ld_optional: bool;
  ld_type: core_type;
  ld_loc: Location.t;
  ld_attributes: attributes;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_name: string loc;
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
  tyext_txt: Longident.t loc;
  tyext_params: (core_type * variance) list;
  tyext_constructors: extension_constructor list;
  tyext_private: private_flag;
  tyext_attributes: attributes;
}

and extension_constructor = {
  ext_id: Ident.t;
  ext_name: string loc;
  ext_type: Types.extension_constructor;
  ext_kind: extension_constructor_kind;
  ext_loc: Location.t;
  ext_attributes: attributes;
}

and extension_constructor_kind =
  | Text_decl of constructor_arguments * core_type option
  | Text_rebind of Path.t * Longident.t loc

(* Auxiliary functions over the a.s.t. *)

val iter_pattern_desc : (pattern -> unit) -> pattern_desc -> unit
val map_pattern_desc : (pattern -> pattern) -> pattern_desc -> pattern_desc

val let_bound_idents : value_binding list -> Ident.t list
val rev_let_bound_idents : value_binding list -> Ident.t list

val alpha_pat : (Ident.t * Ident.t) list -> pattern -> pattern
(** Alpha conversion of patterns *)

val mknoloc : 'a -> 'a Asttypes.loc
val mkloc : 'a -> Location.t -> 'a Asttypes.loc

val pat_bound_idents : pattern -> Ident.t list
