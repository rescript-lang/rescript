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

open Longident
open Asttypes
open Parsetree
open Ast_helper

module T = Typedtree

type mapper = {
  attribute: mapper -> T.attribute -> attribute;
  attributes: mapper -> T.attribute list -> attribute list;
  case: mapper -> T.case -> case;
  cases: mapper -> T.case list -> case list;
  constructor_declaration:
    mapper -> T.constructor_declaration -> constructor_declaration;
  expr: mapper -> T.expression -> expression;
  extension_constructor:
    mapper -> T.extension_constructor -> extension_constructor;
  include_declaration: mapper -> T.include_declaration -> include_declaration;
  include_description: mapper -> T.include_description -> include_description;
  label_declaration: mapper -> T.label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> T.module_binding -> module_binding;
  module_declaration: mapper -> T.module_declaration -> module_declaration;
  module_expr: mapper -> T.module_expr -> module_expr;
  module_type: mapper -> T.module_type -> module_type;
  module_type_declaration:
    mapper -> T.module_type_declaration -> module_type_declaration;
  package_type: mapper -> T.package_type -> package_type;
  open_description: mapper -> T.open_description -> open_description;
  pat: mapper -> T.pattern -> pattern;
  row_field: mapper -> T.row_field -> row_field;
  object_field: mapper -> T.object_field -> object_field;
  signature: mapper -> T.signature -> signature;
  signature_item: mapper -> T.signature_item -> signature_item;
  structure: mapper -> T.structure -> structure;
  structure_item: mapper -> T.structure_item -> structure_item;
  typ: mapper -> T.core_type -> core_type;
  type_declaration: mapper -> T.type_declaration -> type_declaration;
  type_extension: mapper -> T.type_extension -> type_extension;
  type_kind: mapper -> T.type_kind -> type_kind;
  value_binding: mapper -> T.value_binding -> value_binding;
  value_description: mapper -> T.value_description -> value_description;
  with_constraint:
    mapper ->
    Path.t * Longident.t Location.loc * T.with_constraint ->
    with_constraint;
}

open T

(*
Some notes:

   * For Pexp_function, we cannot go back to the exact original version
   when there is a default argument, because the default argument is
   translated in the typer. The code, if printed, will not be parsable because
   new generated identifiers are not correct.

   * For Pexp_apply, it is unclear whether arguments are reordered, especially
    when there are optional arguments.

*)

(** Utility functions. *)

let map_opt f = function
  | None -> None
  | Some e -> Some (f e)

let rec lident_of_path = function
  | Path.Pident id -> Longident.Lident (Ident.name id)
  | Path.Pdot (p, s, _) -> Longident.Ldot (lident_of_path p, s)
  | Path.Papply (p1, p2) ->
    Longident.Lapply (lident_of_path p1, lident_of_path p2)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

(** Try a name [$name$0], check if it's free, if not, increment and repeat. *)
let fresh_name s env =
  let rec aux i =
    let name = s ^ string_of_int i in
    try
      let _ = Env.lookup_value (Lident name) env in
      name
    with Not_found -> aux (i + 1)
  in
  aux 0

(** Mapping functions. *)

let constant = function
  | Const_char c -> Pconst_char c
  | Const_string (s, d) -> Pconst_string (s, d)
  | Const_int i -> Pconst_integer (string_of_int i, None)
  | Const_int32 i -> Pconst_integer (Int32.to_string i, Some 'l')
  | Const_int64 i -> Pconst_integer (Int64.to_string i, Some 'L')
  | Const_bigint (sign, i) ->
    Pconst_integer (Bigint_utils.to_string sign i, Some 'n')
  | Const_float f -> Pconst_float (f, None)

let attribute sub (s, p) = (map_loc sub s, p)
let attributes sub l = List.map (sub.attribute sub) l

let structure sub str = List.map (sub.structure_item sub) str.str_items

let open_description sub od =
  let loc = sub.location sub od.open_loc in
  let attrs = sub.attributes sub od.open_attributes in
  Opn.mk ~loc ~attrs ~override:od.open_override (map_loc sub od.open_txt)

let structure_item sub item =
  let loc = sub.location sub item.str_loc in
  let desc =
    match item.str_desc with
    | Tstr_eval (exp, attrs) -> Pstr_eval (sub.expr sub exp, attrs)
    | Tstr_value (rec_flag, list) ->
      Pstr_value (rec_flag, List.map (sub.value_binding sub) list)
    | Tstr_primitive vd -> Pstr_primitive (sub.value_description sub vd)
    | Tstr_type (rec_flag, list) ->
      Pstr_type (rec_flag, List.map (sub.type_declaration sub) list)
    | Tstr_typext tyext -> Pstr_typext (sub.type_extension sub tyext)
    | Tstr_exception ext -> Pstr_exception (sub.extension_constructor sub ext)
    | Tstr_module mb -> Pstr_module (sub.module_binding sub mb)
    | Tstr_recmodule list ->
      Pstr_recmodule (List.map (sub.module_binding sub) list)
    | Tstr_modtype mtd -> Pstr_modtype (sub.module_type_declaration sub mtd)
    | Tstr_open od -> Pstr_open (sub.open_description sub od)
    | Tstr_class () -> Pstr_class ()
    | Tstr_class_type () -> Pstr_class_type ()
    | Tstr_include incl -> Pstr_include (sub.include_declaration sub incl)
    | Tstr_attribute x -> Pstr_attribute x
  in
  Str.mk ~loc desc

let value_description sub v =
  let loc = sub.location sub v.val_loc in
  let attrs = sub.attributes sub v.val_attributes in
  Val.mk ~loc ~attrs ~prim:v.val_prim (map_loc sub v.val_name)
    (sub.typ sub v.val_desc)

let module_binding sub mb =
  let loc = sub.location sub mb.mb_loc in
  let attrs = sub.attributes sub mb.mb_attributes in
  Mb.mk ~loc ~attrs (map_loc sub mb.mb_name) (sub.module_expr sub mb.mb_expr)

let type_parameter sub (ct, v) = (sub.typ sub ct, v)

let type_declaration sub decl =
  let loc = sub.location sub decl.typ_loc in
  let attrs = sub.attributes sub decl.typ_attributes in
  Type.mk ~loc ~attrs
    ~params:(List.map (type_parameter sub) decl.typ_params)
    ~cstrs:
      (List.map
         (fun (ct1, ct2, loc) ->
           (sub.typ sub ct1, sub.typ sub ct2, sub.location sub loc))
         decl.typ_cstrs)
    ~kind:(sub.type_kind sub decl.typ_kind)
    ~priv:decl.typ_private
    ?manifest:(map_opt (sub.typ sub) decl.typ_manifest)
    (map_loc sub decl.typ_name)

let type_kind sub tk =
  match tk with
  | Ttype_abstract -> Ptype_abstract
  | Ttype_variant list ->
    Ptype_variant (List.map (sub.constructor_declaration sub) list)
  | Ttype_record list ->
    Ptype_record (List.map (sub.label_declaration sub) list)
  | Ttype_open -> Ptype_open

let constructor_arguments sub = function
  | Cstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
  | Cstr_record l -> Pcstr_record (List.map (sub.label_declaration sub) l)

let constructor_declaration sub cd =
  let loc = sub.location sub cd.cd_loc in
  let attrs = sub.attributes sub cd.cd_attributes in
  Type.constructor ~loc ~attrs
    ~args:(constructor_arguments sub cd.cd_args)
    ?res:(map_opt (sub.typ sub) cd.cd_res)
    (map_loc sub cd.cd_name)

let label_declaration sub ld =
  let loc = sub.location sub ld.ld_loc in
  let attrs = sub.attributes sub ld.ld_attributes in
  Type.field ~loc ~attrs ~mut:ld.ld_mutable (map_loc sub ld.ld_name)
    (sub.typ sub ld.ld_type)

let type_extension sub tyext =
  let attrs = sub.attributes sub tyext.tyext_attributes in
  Te.mk ~attrs
    ~params:(List.map (type_parameter sub) tyext.tyext_params)
    ~priv:tyext.tyext_private
    (map_loc sub tyext.tyext_txt)
    (List.map (sub.extension_constructor sub) tyext.tyext_constructors)

let extension_constructor sub ext =
  let loc = sub.location sub ext.ext_loc in
  let attrs = sub.attributes sub ext.ext_attributes in
  Te.constructor ~loc ~attrs (map_loc sub ext.ext_name)
    (match ext.ext_kind with
    | Text_decl (args, ret) ->
      Pext_decl (constructor_arguments sub args, map_opt (sub.typ sub) ret)
    | Text_rebind (_p, lid) -> Pext_rebind (map_loc sub lid))

let pattern sub pat =
  let loc = sub.location sub pat.pat_loc in
  (* todo: fix attributes on extras *)
  let attrs = sub.attributes sub pat.pat_attributes in
  let desc =
    match pat with
    | {pat_extra = [(Tpat_unpack, _, _attrs)]; pat_desc = Tpat_var (_, name); _}
      ->
      Ppat_unpack name
    | {pat_extra = [(Tpat_type (_path, lid), _, _attrs)]; _} ->
      Ppat_type (map_loc sub lid)
    | {pat_extra = (Tpat_constraint ct, _, _attrs) :: rem; _} ->
      Ppat_constraint (sub.pat sub {pat with pat_extra = rem}, sub.typ sub ct)
    | _ -> (
      match pat.pat_desc with
      | Tpat_any -> Ppat_any
      | Tpat_var (id, name) -> (
        match (Ident.name id).[0] with
        | 'A' .. 'Z' -> Ppat_unpack name
        | _ -> Ppat_var name)
      (* We transform (_ as x) in x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
      *)
      | Tpat_alias ({pat_desc = Tpat_any; pat_loc}, _id, name)
        when pat_loc = pat.pat_loc ->
        Ppat_var name
      | Tpat_alias (pat, _id, name) -> Ppat_alias (sub.pat sub pat, name)
      | Tpat_constant cst -> Ppat_constant (constant cst)
      | Tpat_tuple list -> Ppat_tuple (List.map (sub.pat sub) list)
      | Tpat_construct (lid, _, args) ->
        Ppat_construct
          ( map_loc sub lid,
            match args with
            | [] -> None
            | [arg] -> Some (sub.pat sub arg)
            | args -> Some (Pat.tuple ~loc (List.map (sub.pat sub) args)) )
      | Tpat_variant (label, pato, _) ->
        Ppat_variant (label, map_opt (sub.pat sub) pato)
      | Tpat_record (list, closed) ->
        Ppat_record
          ( List.map
              (fun (lid, _, pat) -> (map_loc sub lid, sub.pat sub pat))
              list,
            closed )
      | Tpat_array list -> Ppat_array (List.map (sub.pat sub) list)
      | Tpat_or (p1, p2, _) -> Ppat_or (sub.pat sub p1, sub.pat sub p2)
      | Tpat_lazy p -> Ppat_lazy (sub.pat sub p))
  in
  Pat.mk ~loc ~attrs desc

let exp_extra sub (extra, loc, attrs) sexp =
  let loc = sub.location sub loc in
  let attrs = sub.attributes sub attrs in
  let desc =
    match extra with
    | Texp_coerce ((), cty2) -> Pexp_coerce (sexp, (), sub.typ sub cty2)
    | Texp_constraint cty -> Pexp_constraint (sexp, sub.typ sub cty)
    | Texp_open (ovf, _path, lid, _) -> Pexp_open (ovf, map_loc sub lid, sexp)
    | Texp_poly cto -> Pexp_poly (sexp, map_opt (sub.typ sub) cto)
    | Texp_newtype s -> Pexp_newtype (mkloc s loc, sexp)
  in
  Exp.mk ~loc ~attrs desc

let cases sub l = List.map (sub.case sub) l

let case sub {c_lhs; c_guard; c_rhs} =
  {
    pc_lhs = sub.pat sub c_lhs;
    pc_guard = map_opt (sub.expr sub) c_guard;
    pc_rhs = sub.expr sub c_rhs;
  }

let value_binding sub vb =
  let loc = sub.location sub vb.vb_loc in
  let attrs = sub.attributes sub vb.vb_attributes in
  Vb.mk ~loc ~attrs (sub.pat sub vb.vb_pat) (sub.expr sub vb.vb_expr)

let expression sub exp =
  let loc = sub.location sub exp.exp_loc in
  let attrs = sub.attributes sub exp.exp_attributes in
  let desc =
    match exp.exp_desc with
    | Texp_ident (_path, lid, _) -> Pexp_ident (map_loc sub lid)
    | Texp_constant cst -> Pexp_constant (constant cst)
    | Texp_let (rec_flag, list, exp) ->
      Pexp_let
        (rec_flag, List.map (sub.value_binding sub) list, sub.expr sub exp)
    (* Pexp_function can't have a label, so we split in 3 cases. *)
    (* One case, no guard: It's a fun. *)
    | Texp_function
        {arg_label; cases = [{c_lhs = p; c_guard = None; c_rhs = e}]; _} ->
      Pexp_fun (arg_label, None, sub.pat sub p, sub.expr sub e)
    (* No label: it's a function. *)
    | Texp_function {arg_label = Nolabel; cases; _} ->
      Pexp_function (sub.cases sub cases)
    (* Mix of both, we generate `fun ~label:$name$ -> match $name$ with ...` *)
    | Texp_function {arg_label = (Labelled s | Optional s) as label; cases; _}
      ->
      let name = fresh_name s exp.exp_env in
      Pexp_fun
        ( label,
          None,
          Pat.var ~loc {loc; txt = name},
          Exp.match_ ~loc
            (Exp.ident ~loc {loc; txt = Lident name})
            (sub.cases sub cases) )
    | Texp_apply (exp, list) ->
      Pexp_apply
        ( sub.expr sub exp,
          List.fold_right
            (fun (label, expo) list ->
              match expo with
              | None -> list
              | Some exp -> (label, sub.expr sub exp) :: list)
            list [] )
    | Texp_match (exp, cases, exn_cases, _) ->
      let merged_cases =
        sub.cases sub cases
        @ List.map
            (fun c ->
              let uc = sub.case sub c in
              let pat = {uc.pc_lhs with ppat_desc = Ppat_exception uc.pc_lhs} in
              {uc with pc_lhs = pat})
            exn_cases
      in
      Pexp_match (sub.expr sub exp, merged_cases)
    | Texp_try (exp, cases) -> Pexp_try (sub.expr sub exp, sub.cases sub cases)
    | Texp_tuple list -> Pexp_tuple (List.map (sub.expr sub) list)
    | Texp_construct (lid, _, args) ->
      Pexp_construct
        ( map_loc sub lid,
          match args with
          | [] -> None
          | [arg] -> Some (sub.expr sub arg)
          | args -> Some (Exp.tuple ~loc (List.map (sub.expr sub) args)) )
    | Texp_variant (label, expo) ->
      Pexp_variant (label, map_opt (sub.expr sub) expo)
    | Texp_record {fields; extended_expression; _} ->
      let list =
        Array.fold_left
          (fun l -> function
            | _, Kept _ -> l
            | _, Overridden (lid, exp) -> (lid, sub.expr sub exp) :: l)
          [] fields
      in
      Pexp_record (list, map_opt (sub.expr sub) extended_expression)
    | Texp_field (exp, lid, _label) ->
      Pexp_field (sub.expr sub exp, map_loc sub lid)
    | Texp_setfield (exp1, lid, _label, exp2) ->
      Pexp_setfield (sub.expr sub exp1, map_loc sub lid, sub.expr sub exp2)
    | Texp_array list -> Pexp_array (List.map (sub.expr sub) list)
    | Texp_ifthenelse (exp1, exp2, expo) ->
      Pexp_ifthenelse
        (sub.expr sub exp1, sub.expr sub exp2, map_opt (sub.expr sub) expo)
    | Texp_sequence (exp1, exp2) ->
      Pexp_sequence (sub.expr sub exp1, sub.expr sub exp2)
    | Texp_while (exp1, exp2) ->
      Pexp_while (sub.expr sub exp1, sub.expr sub exp2)
    | Texp_for (_id, name, exp1, exp2, dir, exp3) ->
      Pexp_for
        (name, sub.expr sub exp1, sub.expr sub exp2, dir, sub.expr sub exp3)
    | Texp_send (exp, meth, _) ->
      Pexp_send
        ( sub.expr sub exp,
          match meth with
          | Tmeth_name name -> mkloc name loc )
    | Texp_new _ | Texp_instvar _ | Texp_setinstvar _ | Texp_override _ ->
      assert false
    | Texp_letmodule (_id, name, mexpr, exp) ->
      Pexp_letmodule (name, sub.module_expr sub mexpr, sub.expr sub exp)
    | Texp_letexception (ext, exp) ->
      Pexp_letexception (sub.extension_constructor sub ext, sub.expr sub exp)
    | Texp_assert exp -> Pexp_assert (sub.expr sub exp)
    | Texp_lazy exp -> Pexp_lazy (sub.expr sub exp)
    | Texp_object () -> assert false
    | Texp_pack mexpr -> Pexp_pack (sub.module_expr sub mexpr)
    | Texp_unreachable -> Pexp_unreachable
    | Texp_extension_constructor (lid, _) ->
      Pexp_extension
        ( {txt = "ocaml.extension_constructor"; loc},
          PStr [Str.eval ~loc (Exp.construct ~loc (map_loc sub lid) None)] )
  in
  List.fold_right (exp_extra sub) exp.exp_extra (Exp.mk ~loc ~attrs desc)

let package_type sub pack =
  ( map_loc sub pack.pack_txt,
    List.map (fun (s, ct) -> (s, sub.typ sub ct)) pack.pack_fields )

let module_type_declaration sub mtd =
  let loc = sub.location sub mtd.mtd_loc in
  let attrs = sub.attributes sub mtd.mtd_attributes in
  Mtd.mk ~loc ~attrs
    ?typ:(map_opt (sub.module_type sub) mtd.mtd_type)
    (map_loc sub mtd.mtd_name)

let signature sub sg = List.map (sub.signature_item sub) sg.sig_items

let signature_item sub item =
  let loc = sub.location sub item.sig_loc in
  let desc =
    match item.sig_desc with
    | Tsig_value v -> Psig_value (sub.value_description sub v)
    | Tsig_type (rec_flag, list) ->
      Psig_type (rec_flag, List.map (sub.type_declaration sub) list)
    | Tsig_typext tyext -> Psig_typext (sub.type_extension sub tyext)
    | Tsig_exception ext -> Psig_exception (sub.extension_constructor sub ext)
    | Tsig_module md -> Psig_module (sub.module_declaration sub md)
    | Tsig_recmodule list ->
      Psig_recmodule (List.map (sub.module_declaration sub) list)
    | Tsig_modtype mtd -> Psig_modtype (sub.module_type_declaration sub mtd)
    | Tsig_open od -> Psig_open (sub.open_description sub od)
    | Tsig_include incl -> Psig_include (sub.include_description sub incl)
    | Tsig_class () -> Psig_class ()
    | Tsig_class_type () -> Psig_class_type ()
    | Tsig_attribute x -> Psig_attribute x
  in
  Sig.mk ~loc desc

let module_declaration sub md =
  let loc = sub.location sub md.md_loc in
  let attrs = sub.attributes sub md.md_attributes in
  Md.mk ~loc ~attrs (map_loc sub md.md_name) (sub.module_type sub md.md_type)

let include_infos f sub incl =
  let loc = sub.location sub incl.incl_loc in
  let attrs = sub.attributes sub incl.incl_attributes in
  Incl.mk ~loc ~attrs (f sub incl.incl_mod)

let include_declaration sub = include_infos sub.module_expr sub
let include_description sub = include_infos sub.module_type sub

let module_type sub mty =
  let loc = sub.location sub mty.mty_loc in
  let attrs = sub.attributes sub mty.mty_attributes in
  let desc =
    match mty.mty_desc with
    | Tmty_ident (_path, lid) -> Pmty_ident (map_loc sub lid)
    | Tmty_alias (_path, lid) -> Pmty_alias (map_loc sub lid)
    | Tmty_signature sg -> Pmty_signature (sub.signature sub sg)
    | Tmty_functor (_id, name, mtype1, mtype2) ->
      Pmty_functor
        (name, map_opt (sub.module_type sub) mtype1, sub.module_type sub mtype2)
    | Tmty_with (mtype, list) ->
      Pmty_with
        (sub.module_type sub mtype, List.map (sub.with_constraint sub) list)
    | Tmty_typeof mexpr -> Pmty_typeof (sub.module_expr sub mexpr)
  in
  Mty.mk ~loc ~attrs desc

let with_constraint sub (_path, lid, cstr) =
  match cstr with
  | Twith_type decl ->
    Pwith_type (map_loc sub lid, sub.type_declaration sub decl)
  | Twith_module (_path, lid2) ->
    Pwith_module (map_loc sub lid, map_loc sub lid2)
  | Twith_typesubst decl ->
    Pwith_typesubst (map_loc sub lid, sub.type_declaration sub decl)
  | Twith_modsubst (_path, lid2) ->
    Pwith_modsubst (map_loc sub lid, map_loc sub lid2)

let module_expr sub mexpr =
  let loc = sub.location sub mexpr.mod_loc in
  let attrs = sub.attributes sub mexpr.mod_attributes in
  match mexpr.mod_desc with
  | Tmod_constraint (m, _, Tmodtype_implicit, _) -> sub.module_expr sub m
  | _ ->
    let desc =
      match mexpr.mod_desc with
      | Tmod_ident (_p, lid) -> Pmod_ident (map_loc sub lid)
      | Tmod_structure st -> Pmod_structure (sub.structure sub st)
      | Tmod_functor (_id, name, mtype, mexpr) ->
        Pmod_functor
          ( name,
            Misc.may_map (sub.module_type sub) mtype,
            sub.module_expr sub mexpr )
      | Tmod_apply (mexp1, mexp2, _) ->
        Pmod_apply (sub.module_expr sub mexp1, sub.module_expr sub mexp2)
      | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
        Pmod_constraint (sub.module_expr sub mexpr, sub.module_type sub mtype)
      | Tmod_constraint (_mexpr, _, Tmodtype_implicit, _) -> assert false
      | Tmod_unpack (exp, _pack) -> Pmod_unpack (sub.expr sub exp)
      (* TODO , sub.package_type sub pack) *)
    in
    Mod.mk ~loc ~attrs desc

let core_type sub ct =
  let loc = sub.location sub ct.ctyp_loc in
  let attrs = sub.attributes sub ct.ctyp_attributes in
  let desc =
    match ct.ctyp_desc with
    | Ttyp_any -> Ptyp_any
    | Ttyp_var s -> Ptyp_var s
    | Ttyp_arrow (label, ct1, ct2) ->
      Ptyp_arrow (label, sub.typ sub ct1, sub.typ sub ct2)
    | Ttyp_tuple list -> Ptyp_tuple (List.map (sub.typ sub) list)
    | Ttyp_constr (_path, lid, list) ->
      Ptyp_constr (map_loc sub lid, List.map (sub.typ sub) list)
    | Ttyp_object (list, o) ->
      Ptyp_object (List.map (sub.object_field sub) list, o)
    | Ttyp_alias (ct, s) -> Ptyp_alias (sub.typ sub ct, s)
    | Ttyp_variant (list, bool, labels) ->
      Ptyp_variant (List.map (sub.row_field sub) list, bool, labels)
    | Ttyp_poly (list, ct) ->
      let list = List.map (fun v -> mkloc v loc) list in
      Ptyp_poly (list, sub.typ sub ct)
    | Ttyp_package pack -> Ptyp_package (sub.package_type sub pack)
  in
  Typ.mk ~loc ~attrs desc

let row_field sub rf =
  match rf with
  | Ttag (label, attrs, bool, list) ->
    Rtag (label, sub.attributes sub attrs, bool, List.map (sub.typ sub) list)
  | Tinherit ct -> Rinherit (sub.typ sub ct)

let object_field sub ofield =
  match ofield with
  | OTtag (label, attrs, ct) ->
    Otag (label, sub.attributes sub attrs, sub.typ sub ct)
  | OTinherit ct -> Oinherit (sub.typ sub ct)

let location _sub l = l

let default_mapper =
  {
    attribute;
    attributes;
    structure;
    structure_item;
    module_expr;
    signature;
    signature_item;
    module_type;
    with_constraint;
    type_declaration;
    type_kind;
    typ = core_type;
    type_extension;
    extension_constructor;
    value_description;
    pat = pattern;
    expr = expression;
    module_declaration;
    module_type_declaration;
    module_binding;
    package_type;
    open_description;
    include_description;
    include_declaration;
    value_binding;
    constructor_declaration;
    label_declaration;
    cases;
    case;
    location;
    row_field;
    object_field;
  }

let untype_structure ?(mapper = default_mapper) structure =
  mapper.structure mapper structure

let untype_signature ?(mapper = default_mapper) signature =
  mapper.signature mapper signature
