(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                          Isaac "Izzy" Avram                            *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree

type iterator = {
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  env: iterator -> Env.t -> unit;
  expr: iterator -> expression -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_coercion: iterator -> module_coercion -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  package_type: iterator -> package_type -> unit;
  pat: iterator -> pattern -> unit;
  row_field: iterator -> row_field -> unit;
  object_field: iterator -> object_field -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_declarations: iterator -> rec_flag * type_declaration list -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_bindings: iterator -> rec_flag * value_binding list -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}

let structure sub {str_items; str_final_env; _} =
  List.iter (sub.structure_item sub) str_items;
  sub.env sub str_final_env

let module_type_declaration sub {mtd_type; _} =
  Option.iter (sub.module_type sub) mtd_type

let module_declaration sub {md_type; _} = sub.module_type sub md_type
let include_infos f {incl_mod; _} = f incl_mod

let structure_item sub {str_desc; str_env; _} =
  sub.env sub str_env;
  match str_desc with
  | Tstr_eval (exp, _) -> sub.expr sub exp
  | Tstr_value (rec_flag, list) -> sub.value_bindings sub (rec_flag, list)
  | Tstr_primitive v -> sub.value_description sub v
  | Tstr_type (rec_flag, list) -> sub.type_declarations sub (rec_flag, list)
  | Tstr_typext te -> sub.type_extension sub te
  | Tstr_exception ext -> sub.extension_constructor sub ext
  | Tstr_module mb -> sub.module_binding sub mb
  | Tstr_recmodule list -> List.iter (sub.module_binding sub) list
  | Tstr_modtype x -> sub.module_type_declaration sub x
  | Tstr_include incl -> include_infos (sub.module_expr sub) incl
  | Tstr_open _ -> ()
  | Tstr_attribute _ -> ()

let value_description sub x = sub.typ sub x.val_desc
let label_decl sub {ld_type; _} = sub.typ sub ld_type

let constructor_args sub = function
  | Cstr_tuple l -> List.iter (sub.typ sub) l
  | Cstr_record l -> List.iter (label_decl sub) l

let constructor_decl sub {cd_args; cd_res; _} =
  constructor_args sub cd_args;
  Option.iter (sub.typ sub) cd_res

let type_kind sub = function
  | Ttype_abstract -> ()
  | Ttype_variant list -> List.iter (constructor_decl sub) list
  | Ttype_record list -> List.iter (label_decl sub) list
  | Ttype_open -> ()

let type_declaration sub {typ_cstrs; typ_kind; typ_manifest; typ_params; _} =
  List.iter
    (fun (c1, c2, _) ->
      sub.typ sub c1;
      sub.typ sub c2)
    typ_cstrs;
  sub.type_kind sub typ_kind;
  Option.iter (sub.typ sub) typ_manifest;
  List.iter (fun (c, _) -> sub.typ sub c) typ_params

let type_declarations sub (_, list) = List.iter (sub.type_declaration sub) list

let type_extension sub {tyext_constructors; tyext_params; _} =
  List.iter (fun (c, _) -> sub.typ sub c) tyext_params;
  List.iter (sub.extension_constructor sub) tyext_constructors

let extension_constructor sub {ext_kind; _} =
  match ext_kind with
  | Text_decl (ctl, cto) ->
    constructor_args sub ctl;
    Option.iter (sub.typ sub) cto
  | Text_rebind _ -> ()

let pat sub {pat_extra; pat_desc; pat_env; _} =
  let extra = function
    | Tpat_type _ -> ()
    | Tpat_unpack -> ()
    | Tpat_open (_, _, env) -> sub.env sub env
    | Tpat_constraint ct -> sub.typ sub ct
  in
  sub.env sub pat_env;
  List.iter (fun (e, _, _) -> extra e) pat_extra;
  match pat_desc with
  | Tpat_any -> ()
  | Tpat_var _ -> ()
  | Tpat_constant _ -> ()
  | Tpat_tuple l -> List.iter (sub.pat sub) l
  | Tpat_construct (_, _, l) -> List.iter (sub.pat sub) l
  | Tpat_variant (_, po, _) -> Option.iter (sub.pat sub) po
  | Tpat_record (l, _) -> List.iter (fun (_, _, i, _) -> sub.pat sub i) l
  | Tpat_array l -> List.iter (sub.pat sub) l
  | Tpat_or (p1, p2, _) ->
    sub.pat sub p1;
    sub.pat sub p2
  | Tpat_alias (p, _, _) -> sub.pat sub p

let expr sub {exp_extra; exp_desc; exp_env; _} =
  let extra = function
    | Texp_constraint cty -> sub.typ sub cty
    | Texp_coerce cty2 -> sub.typ sub cty2
    | Texp_newtype _ -> ()
    | Texp_open (_, _, _, _) -> ()
  in
  List.iter (fun (e, _, _) -> extra e) exp_extra;
  sub.env sub exp_env;
  match exp_desc with
  | Texp_ident _ -> ()
  | Texp_constant _ -> ()
  | Texp_let (rec_flag, list, exp) ->
    sub.value_bindings sub (rec_flag, list);
    sub.expr sub exp
  | Texp_function {case; _} -> sub.case sub case
  | Texp_apply {funct = exp; args = list} ->
    sub.expr sub exp;
    List.iter (fun (_, o) -> Option.iter (sub.expr sub) o) list
  | Texp_match (exp, list1, list2, _) ->
    sub.expr sub exp;
    sub.cases sub list1;
    sub.cases sub list2
  | Texp_try (exp, cases) ->
    sub.expr sub exp;
    sub.cases sub cases
  | Texp_tuple list -> List.iter (sub.expr sub) list
  | Texp_construct (_, _, args) -> List.iter (sub.expr sub) args
  | Texp_variant (_, expo) -> Option.iter (sub.expr sub) expo
  | Texp_record {fields; extended_expression; _} ->
    Array.iter
      (function
        | _, Kept _, _ -> ()
        | _, Overridden (_, exp), _ -> sub.expr sub exp)
      fields;
    Option.iter (sub.expr sub) extended_expression
  | Texp_field (exp, _, _) -> sub.expr sub exp
  | Texp_setfield (exp1, _, _, exp2) ->
    sub.expr sub exp1;
    sub.expr sub exp2
  | Texp_array list -> List.iter (sub.expr sub) list
  | Texp_ifthenelse (exp1, exp2, expo) ->
    sub.expr sub exp1;
    sub.expr sub exp2;
    Option.iter (sub.expr sub) expo
  | Texp_sequence (exp1, exp2) ->
    sub.expr sub exp1;
    sub.expr sub exp2
  | Texp_while (exp1, exp2) ->
    sub.expr sub exp1;
    sub.expr sub exp2
  | Texp_for (_, _, exp1, exp2, _, exp3) ->
    sub.expr sub exp1;
    sub.expr sub exp2;
    sub.expr sub exp3
  | Texp_send (exp, _, expo) ->
    sub.expr sub exp;
    Option.iter (sub.expr sub) expo
  | Texp_letmodule (_, _, mexpr, exp) ->
    sub.module_expr sub mexpr;
    sub.expr sub exp
  | Texp_letexception (cd, exp) ->
    sub.extension_constructor sub cd;
    sub.expr sub exp
  | Texp_assert exp -> sub.expr sub exp
  | Texp_pack mexpr -> sub.module_expr sub mexpr
  | Texp_extension_constructor _ -> ()

let package_type sub {pack_fields; _} =
  List.iter (fun (_, p) -> sub.typ sub p) pack_fields

let signature sub {sig_items; sig_final_env; _} =
  sub.env sub sig_final_env;
  List.iter (sub.signature_item sub) sig_items

let signature_item sub {sig_desc; sig_env; _} =
  sub.env sub sig_env;
  match sig_desc with
  | Tsig_value v -> sub.value_description sub v
  | Tsig_type (rf, tdl) -> sub.type_declarations sub (rf, tdl)
  | Tsig_typext te -> sub.type_extension sub te
  | Tsig_exception ext -> sub.extension_constructor sub ext
  | Tsig_module x -> sub.module_declaration sub x
  | Tsig_recmodule list -> List.iter (sub.module_declaration sub) list
  | Tsig_modtype x -> sub.module_type_declaration sub x
  | Tsig_include incl -> include_infos (sub.module_type sub) incl
  | Tsig_open _od -> ()
  | Tsig_attribute _ -> ()

let module_type sub {mty_desc; mty_env; _} =
  sub.env sub mty_env;
  match mty_desc with
  | Tmty_ident _ -> ()
  | Tmty_alias _ -> ()
  | Tmty_signature sg -> sub.signature sub sg
  | Tmty_functor (_, _, mtype1, mtype2) ->
    Option.iter (sub.module_type sub) mtype1;
    sub.module_type sub mtype2
  | Tmty_with (mtype, list) ->
    sub.module_type sub mtype;
    List.iter (fun (_, _, e) -> sub.with_constraint sub e) list
  | Tmty_typeof mexpr -> sub.module_expr sub mexpr

let with_constraint sub = function
  | Twith_type decl -> sub.type_declaration sub decl
  | Twith_typesubst decl -> sub.type_declaration sub decl
  | Twith_module _ -> ()
  | Twith_modsubst _ -> ()

let module_coercion sub = function
  | Tcoerce_none -> ()
  | Tcoerce_functor (c1, c2) ->
    sub.module_coercion sub c1;
    sub.module_coercion sub c2
  | Tcoerce_alias (_, c1) -> sub.module_coercion sub c1
  | Tcoerce_structure (l1, l2, _) ->
    List.iter (fun (_, c) -> sub.module_coercion sub c) l1;
    List.iter (fun (_, _, c) -> sub.module_coercion sub c) l2
  | Tcoerce_primitive {pc_env; _} -> sub.env sub pc_env

let module_expr sub {mod_desc; mod_env; _} =
  sub.env sub mod_env;
  match mod_desc with
  | Tmod_ident _ -> ()
  | Tmod_structure st -> sub.structure sub st
  | Tmod_functor (_, _, mtype, mexpr) ->
    Option.iter (sub.module_type sub) mtype;
    sub.module_expr sub mexpr
  | Tmod_apply (mexp1, mexp2, c) ->
    sub.module_expr sub mexp1;
    sub.module_expr sub mexp2;
    sub.module_coercion sub c
  | Tmod_constraint (mexpr, _, Tmodtype_implicit, c) ->
    sub.module_expr sub mexpr;
    sub.module_coercion sub c
  | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, c) ->
    sub.module_expr sub mexpr;
    sub.module_type sub mtype;
    sub.module_coercion sub c
  | Tmod_unpack (exp, _) -> sub.expr sub exp

let module_binding sub {mb_expr; _} = sub.module_expr sub mb_expr

let typ sub {ctyp_desc; ctyp_env; _} =
  sub.env sub ctyp_env;
  match ctyp_desc with
  | Ttyp_any -> ()
  | Ttyp_var _ -> ()
  | Ttyp_arrow (arg, ret, _) ->
    sub.typ sub arg.typ;
    sub.typ sub ret
  | Ttyp_tuple list -> List.iter (sub.typ sub) list
  | Ttyp_constr (_, _, list) -> List.iter (sub.typ sub) list
  | Ttyp_object (list, _) -> List.iter (sub.object_field sub) list
  | Ttyp_alias (ct, _) -> sub.typ sub ct
  | Ttyp_variant (list, _, _) -> List.iter (sub.row_field sub) list
  | Ttyp_poly (_, ct) -> sub.typ sub ct
  | Ttyp_package pack -> sub.package_type sub pack

let row_field sub = function
  | Ttag (_label, _attrs, _bool, list) -> List.iter (sub.typ sub) list
  | Tinherit ct -> sub.typ sub ct

let object_field sub = function
  | OTtag (_, _, ct) | OTinherit ct -> sub.typ sub ct

let value_bindings sub (_, list) = List.iter (sub.value_binding sub) list
let cases sub l = List.iter (sub.case sub) l

let case sub {c_lhs; c_guard; c_rhs} =
  sub.pat sub c_lhs;
  Option.iter (sub.expr sub) c_guard;
  sub.expr sub c_rhs

let value_binding sub {vb_pat; vb_expr; _} =
  sub.pat sub vb_pat;
  sub.expr sub vb_expr

let env _sub _ = ()

let default_iterator =
  {
    case;
    cases;
    env;
    expr;
    extension_constructor;
    module_binding;
    module_coercion;
    module_declaration;
    module_expr;
    module_type;
    module_type_declaration;
    package_type;
    pat;
    object_field;
    row_field;
    signature;
    signature_item;
    structure;
    structure_item;
    typ;
    type_declaration;
    type_declarations;
    type_extension;
    type_kind;
    value_binding;
    value_bindings;
    value_description;
    with_constraint;
  }
