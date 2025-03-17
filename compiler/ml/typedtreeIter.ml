(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*
TODO:
 - 2012/05/10: Follow camlp4 way of building map and iter using classes
     and inheritance ?
*)

open Asttypes
open Typedtree

module type IteratorArgument = sig
  val enter_structure : structure -> unit
  val enter_value_description : value_description -> unit
  val enter_type_extension : type_extension -> unit
  val enter_extension_constructor : extension_constructor -> unit
  val enter_pattern : pattern -> unit
  val enter_expression : expression -> unit
  val enter_package_type : package_type -> unit
  val enter_signature : signature -> unit
  val enter_signature_item : signature_item -> unit
  val enter_module_type_declaration : module_type_declaration -> unit
  val enter_module_type : module_type -> unit
  val enter_module_expr : module_expr -> unit
  val enter_with_constraint : with_constraint -> unit

  val enter_core_type : core_type -> unit
  val enter_structure_item : structure_item -> unit

  val leave_structure : structure -> unit
  val leave_value_description : value_description -> unit
  val leave_type_extension : type_extension -> unit
  val leave_extension_constructor : extension_constructor -> unit
  val leave_pattern : pattern -> unit
  val leave_expression : expression -> unit
  val leave_package_type : package_type -> unit
  val leave_signature : signature -> unit
  val leave_signature_item : signature_item -> unit
  val leave_module_type_declaration : module_type_declaration -> unit
  val leave_module_type : module_type -> unit
  val leave_module_expr : module_expr -> unit
  val leave_with_constraint : with_constraint -> unit

  val leave_core_type : core_type -> unit
  val leave_structure_item : structure_item -> unit

  val enter_bindings : rec_flag -> unit
  val enter_binding : value_binding -> unit
  val leave_binding : value_binding -> unit
  val leave_bindings : rec_flag -> unit

  val enter_type_declarations : rec_flag -> unit
  val enter_type_declaration : type_declaration -> unit
  val leave_type_declaration : type_declaration -> unit
  val leave_type_declarations : rec_flag -> unit
end

module MakeIterator (Iter : IteratorArgument) : sig
  val iter_structure : structure -> unit
  val iter_signature : signature -> unit
  val iter_structure_item : structure_item -> unit
  val iter_signature_item : signature_item -> unit
  val iter_expression : expression -> unit
  val iter_module_type : module_type -> unit
  val iter_pattern : pattern -> unit
end = struct
  let may_iter f v =
    match v with
    | None -> ()
    | Some x -> f x

  let rec iter_structure str =
    Iter.enter_structure str;
    List.iter iter_structure_item str.str_items;
    Iter.leave_structure str

  and iter_binding vb =
    Iter.enter_binding vb;
    iter_pattern vb.vb_pat;
    iter_expression vb.vb_expr;
    Iter.leave_binding vb

  and iter_bindings rec_flag list =
    Iter.enter_bindings rec_flag;
    List.iter iter_binding list;
    Iter.leave_bindings rec_flag

  and iter_case {c_lhs; c_guard; c_rhs} =
    iter_pattern c_lhs;
    may_iter iter_expression c_guard;
    iter_expression c_rhs

  and iter_cases cases = List.iter iter_case cases

  and iter_structure_item item =
    Iter.enter_structure_item item;
    (match item.str_desc with
    | Tstr_eval (exp, _attrs) -> iter_expression exp
    | Tstr_value (rec_flag, list) -> iter_bindings rec_flag list
    | Tstr_primitive vd -> iter_value_description vd
    | Tstr_type (rf, list) -> iter_type_declarations rf list
    | Tstr_typext tyext -> iter_type_extension tyext
    | Tstr_exception ext -> iter_extension_constructor ext
    | Tstr_module x -> iter_module_binding x
    | Tstr_recmodule list -> List.iter iter_module_binding list
    | Tstr_modtype mtd -> iter_module_type_declaration mtd
    | Tstr_open _ -> ()
    | Tstr_include incl -> iter_module_expr incl.incl_mod
    | Tstr_attribute _ -> ());
    Iter.leave_structure_item item

  and iter_module_binding x = iter_module_expr x.mb_expr

  and iter_value_description v =
    Iter.enter_value_description v;
    iter_core_type v.val_desc;
    Iter.leave_value_description v

  and iter_constructor_arguments = function
    | Cstr_tuple l -> List.iter iter_core_type l
    | Cstr_record l -> List.iter (fun ld -> iter_core_type ld.ld_type) l

  and iter_constructor_declaration cd =
    iter_constructor_arguments cd.cd_args;
    option iter_core_type cd.cd_res

  and iter_type_parameter (ct, _v) = iter_core_type ct

  and iter_type_declaration decl =
    Iter.enter_type_declaration decl;
    List.iter iter_type_parameter decl.typ_params;
    List.iter
      (fun (ct1, ct2, _loc) ->
        iter_core_type ct1;
        iter_core_type ct2)
      decl.typ_cstrs;
    (match decl.typ_kind with
    | Ttype_abstract -> ()
    | Ttype_variant list -> List.iter iter_constructor_declaration list
    | Ttype_record list -> List.iter (fun ld -> iter_core_type ld.ld_type) list
    | Ttype_open -> ());
    option iter_core_type decl.typ_manifest;
    Iter.leave_type_declaration decl

  and iter_type_declarations rec_flag decls =
    Iter.enter_type_declarations rec_flag;
    List.iter iter_type_declaration decls;
    Iter.leave_type_declarations rec_flag

  and iter_extension_constructor ext =
    Iter.enter_extension_constructor ext;
    (match ext.ext_kind with
    | Text_decl (args, ret) ->
      iter_constructor_arguments args;
      option iter_core_type ret
    | Text_rebind _ -> ());
    Iter.leave_extension_constructor ext

  and iter_type_extension tyext =
    Iter.enter_type_extension tyext;
    List.iter iter_type_parameter tyext.tyext_params;
    List.iter iter_extension_constructor tyext.tyext_constructors;
    Iter.leave_type_extension tyext

  and iter_pattern pat =
    Iter.enter_pattern pat;
    List.iter
      (fun (cstr, _, _attrs) ->
        match cstr with
        | Tpat_type _ -> ()
        | Tpat_unpack -> ()
        | Tpat_open _ -> ()
        | Tpat_constraint ct -> iter_core_type ct)
      pat.pat_extra;
    (match pat.pat_desc with
    | Tpat_any -> ()
    | Tpat_var _ -> ()
    | Tpat_alias (pat1, _, _) -> iter_pattern pat1
    | Tpat_constant _ -> ()
    | Tpat_tuple list -> List.iter iter_pattern list
    | Tpat_construct (_, _, args) -> List.iter iter_pattern args
    | Tpat_variant (_, pato, _) -> (
      match pato with
      | None -> ()
      | Some pat -> iter_pattern pat)
    | Tpat_record (list, _closed) ->
      List.iter (fun (_, _, pat, _) -> iter_pattern pat) list
    | Tpat_array list -> List.iter iter_pattern list
    | Tpat_or (p1, p2, _) ->
      iter_pattern p1;
      iter_pattern p2
    | Tpat_lazy p -> iter_pattern p);
    Iter.leave_pattern pat

  and option f x =
    match x with
    | None -> ()
    | Some e -> f e

  and iter_expression exp =
    Iter.enter_expression exp;
    List.iter
      (function
        | cstr, _, _attrs -> (
          match cstr with
          | Texp_constraint ct -> iter_core_type ct
          | Texp_coerce cty2 -> iter_core_type cty2
          | Texp_open _ -> ()
          | Texp_newtype _ -> ()))
      exp.exp_extra;
    (match exp.exp_desc with
    | Texp_ident _ -> ()
    | Texp_constant _ -> ()
    | Texp_let (rec_flag, list, exp) ->
      iter_bindings rec_flag list;
      iter_expression exp
    | Texp_function {case; _} -> iter_case case
    | Texp_apply {funct = exp; args = list} ->
      iter_expression exp;
      List.iter
        (fun (_label, expo) ->
          match expo with
          | None -> ()
          | Some exp -> iter_expression exp)
        list
    | Texp_match (exp, list1, list2, _) ->
      iter_expression exp;
      iter_cases list1;
      iter_cases list2
    | Texp_try (exp, list) ->
      iter_expression exp;
      iter_cases list
    | Texp_tuple list -> List.iter iter_expression list
    | Texp_construct (_, _, args) -> List.iter iter_expression args
    | Texp_variant (_label, expo) -> (
      match expo with
      | None -> ()
      | Some exp -> iter_expression exp)
    | Texp_record {fields; extended_expression; _} -> (
      Array.iter
        (function
          | _, Kept _, _ -> ()
          | _, Overridden (_, exp), _ -> iter_expression exp)
        fields;
      match extended_expression with
      | None -> ()
      | Some exp -> iter_expression exp)
    | Texp_field (exp, _, _label) -> iter_expression exp
    | Texp_setfield (exp1, _, _label, exp2) ->
      iter_expression exp1;
      iter_expression exp2
    | Texp_array list -> List.iter iter_expression list
    | Texp_ifthenelse (exp1, exp2, expo) -> (
      iter_expression exp1;
      iter_expression exp2;
      match expo with
      | None -> ()
      | Some exp -> iter_expression exp)
    | Texp_sequence (exp1, exp2) ->
      iter_expression exp1;
      iter_expression exp2
    | Texp_while (exp1, exp2) ->
      iter_expression exp1;
      iter_expression exp2
    | Texp_for (_id, _, exp1, exp2, _dir, exp3) ->
      iter_expression exp1;
      iter_expression exp2;
      iter_expression exp3
    | Texp_send (exp, _meth, expo) -> (
      iter_expression exp;
      match expo with
      | None -> ()
      | Some exp -> iter_expression exp)
    | Texp_letmodule (_id, _, mexpr, exp) ->
      iter_module_expr mexpr;
      iter_expression exp
    | Texp_letexception (cd, exp) ->
      iter_extension_constructor cd;
      iter_expression exp
    | Texp_assert exp -> iter_expression exp
    | Texp_lazy exp -> iter_expression exp
    | Texp_pack mexpr -> iter_module_expr mexpr
    | Texp_extension_constructor _ -> ());
    Iter.leave_expression exp

  and iter_package_type pack =
    Iter.enter_package_type pack;
    List.iter (fun (_s, ct) -> iter_core_type ct) pack.pack_fields;
    Iter.leave_package_type pack

  and iter_signature sg =
    Iter.enter_signature sg;
    List.iter iter_signature_item sg.sig_items;
    Iter.leave_signature sg

  and iter_signature_item item =
    Iter.enter_signature_item item;
    (match item.sig_desc with
    | Tsig_value vd -> iter_value_description vd
    | Tsig_type (rf, list) -> iter_type_declarations rf list
    | Tsig_exception ext -> iter_extension_constructor ext
    | Tsig_typext tyext -> iter_type_extension tyext
    | Tsig_module md -> iter_module_type md.md_type
    | Tsig_recmodule list ->
      List.iter (fun md -> iter_module_type md.md_type) list
    | Tsig_modtype mtd -> iter_module_type_declaration mtd
    | Tsig_open _ -> ()
    | Tsig_include incl -> iter_module_type incl.incl_mod
    | Tsig_attribute _ -> ());
    Iter.leave_signature_item item

  and iter_module_type_declaration mtd =
    Iter.enter_module_type_declaration mtd;
    (match mtd.mtd_type with
    | None -> ()
    | Some mtype -> iter_module_type mtype);
    Iter.leave_module_type_declaration mtd

  and iter_module_type mty =
    Iter.enter_module_type mty;
    (match mty.mty_desc with
    | Tmty_ident _ -> ()
    | Tmty_alias _ -> ()
    | Tmty_signature sg -> iter_signature sg
    | Tmty_functor (_, _, mtype1, mtype2) ->
      Misc.may iter_module_type mtype1;
      iter_module_type mtype2
    | Tmty_with (mtype, list) ->
      iter_module_type mtype;
      List.iter (fun (_path, _, withc) -> iter_with_constraint withc) list
    | Tmty_typeof mexpr -> iter_module_expr mexpr);
    Iter.leave_module_type mty

  and iter_with_constraint cstr =
    Iter.enter_with_constraint cstr;
    (match cstr with
    | Twith_type decl -> iter_type_declaration decl
    | Twith_module _ -> ()
    | Twith_typesubst decl -> iter_type_declaration decl
    | Twith_modsubst _ -> ());
    Iter.leave_with_constraint cstr

  and iter_module_expr mexpr =
    Iter.enter_module_expr mexpr;
    (match mexpr.mod_desc with
    | Tmod_ident _ -> ()
    | Tmod_structure st -> iter_structure st
    | Tmod_functor (_, _, mtype, mexpr) ->
      Misc.may iter_module_type mtype;
      iter_module_expr mexpr
    | Tmod_apply (mexp1, mexp2, _) ->
      iter_module_expr mexp1;
      iter_module_expr mexp2
    | Tmod_constraint (mexpr, _, Tmodtype_implicit, _) -> iter_module_expr mexpr
    | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
      iter_module_expr mexpr;
      iter_module_type mtype
    | Tmod_unpack (exp, _mty) -> iter_expression exp)
    (*          iter_module_type mty *);
    Iter.leave_module_expr mexpr

  and iter_core_type ct =
    Iter.enter_core_type ct;
    (match ct.ctyp_desc with
    | Ttyp_any -> ()
    | Ttyp_var _ -> ()
    | Ttyp_arrow (_label, ct1, ct2, _) ->
      iter_core_type ct1;
      iter_core_type ct2
    | Ttyp_tuple list -> List.iter iter_core_type list
    | Ttyp_constr (_path, _, list) -> List.iter iter_core_type list
    | Ttyp_object (list, _o) -> List.iter iter_object_field list
    | Ttyp_alias (ct, _s) -> iter_core_type ct
    | Ttyp_variant (list, _bool, _labels) -> List.iter iter_row_field list
    | Ttyp_poly (_list, ct) -> iter_core_type ct
    | Ttyp_package pack -> iter_package_type pack);
    Iter.leave_core_type ct

  and iter_row_field rf =
    match rf with
    | Ttag (_label, _attrs, _bool, list) -> List.iter iter_core_type list
    | Tinherit ct -> iter_core_type ct

  and iter_object_field ofield =
    match ofield with
    | OTtag (_, _, ct) | OTinherit ct -> iter_core_type ct
end

module DefaultIteratorArgument = struct
  let enter_structure _ = ()
  let enter_value_description _ = ()
  let enter_type_extension _ = ()
  let enter_extension_constructor _ = ()
  let enter_pattern _ = ()
  let enter_expression _ = ()
  let enter_package_type _ = ()
  let enter_signature _ = ()
  let enter_signature_item _ = ()
  let enter_module_type_declaration _ = ()
  let enter_module_type _ = ()
  let enter_module_expr _ = ()
  let enter_with_constraint _ = ()

  let enter_core_type _ = ()
  let enter_structure_item _ = ()

  let leave_structure _ = ()
  let leave_value_description _ = ()
  let leave_type_extension _ = ()
  let leave_extension_constructor _ = ()
  let leave_pattern _ = ()
  let leave_expression _ = ()
  let leave_package_type _ = ()
  let leave_signature _ = ()
  let leave_signature_item _ = ()
  let leave_module_type_declaration _ = ()
  let leave_module_type _ = ()
  let leave_module_expr _ = ()
  let leave_with_constraint _ = ()

  let leave_core_type _ = ()
  let leave_structure_item _ = ()

  let enter_binding _ = ()
  let leave_binding _ = ()

  let enter_bindings _ = ()
  let leave_bindings _ = ()

  let enter_type_declaration _ = ()
  let leave_type_declaration _ = ()

  let enter_type_declarations _ = ()
  let leave_type_declarations _ = ()
end
