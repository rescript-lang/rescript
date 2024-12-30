(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)

open Parsetree
open Location

type iterator = {
  attribute: iterator -> attribute -> unit;
  attributes: iterator -> attribute list -> unit;
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  constructor_declaration: iterator -> constructor_declaration -> unit;
  expr: iterator -> expression -> unit;
  extension: iterator -> extension -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  include_declaration: iterator -> include_declaration -> unit;
  include_description: iterator -> include_description -> unit;
  label_declaration: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  open_description: iterator -> open_description -> unit;
  pat: iterator -> pattern -> unit;
  payload: iterator -> payload -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

let iter_fst f (x, _) = f x
let iter_snd f (_, y) = f y
let iter_tuple f1 f2 (x, y) =
  f1 x;
  f2 y
let iter_tuple3 f1 f2 f3 (x, y, z) =
  f1 x;
  f2 y;
  f3 z
let iter_opt f = function
  | None -> ()
  | Some x -> f x

let iter_loc sub {loc; txt = _} = sub.location sub loc

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (_, attrs, _, tl) ->
      sub.attributes sub attrs;
      List.iter (sub.typ sub) tl
    | Rinherit t -> sub.typ sub t

  let object_field sub = function
    | Otag (_, attrs, t) ->
      sub.attributes sub attrs;
      sub.typ sub t
    | Oinherit t -> sub.typ sub t

  let iter sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ptyp_any | Ptyp_var _ -> ()
    | Ptyp_arrow (_lab, t1, t2, _) ->
      sub.typ sub t1;
      sub.typ sub t2
    | Ptyp_tuple tyl -> List.iter (sub.typ sub) tyl
    | Ptyp_constr (lid, tl) ->
      iter_loc sub lid;
      List.iter (sub.typ sub) tl
    | Ptyp_object (ol, _o) -> List.iter (object_field sub) ol
    | Ptyp_alias (t, _) -> sub.typ sub t
    | Ptyp_variant (rl, _b, _ll) -> List.iter (row_field sub) rl
    | Ptyp_poly (_, t) -> sub.typ sub t
    | Ptyp_package (lid, l) ->
      iter_loc sub lid;
      List.iter (iter_tuple (iter_loc sub) (sub.typ sub)) l
    | Ptyp_extension x -> sub.extension sub x

  let iter_type_declaration sub
      {
        ptype_name;
        ptype_params;
        ptype_cstrs;
        ptype_kind;
        ptype_private = _;
        ptype_manifest;
        ptype_attributes;
        ptype_loc;
      } =
    iter_loc sub ptype_name;
    List.iter (iter_fst (sub.typ sub)) ptype_params;
    List.iter
      (iter_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
      ptype_cstrs;
    sub.type_kind sub ptype_kind;
    iter_opt (sub.typ sub) ptype_manifest;
    sub.location sub ptype_loc;
    sub.attributes sub ptype_attributes

  let iter_type_kind sub = function
    | Ptype_abstract -> ()
    | Ptype_variant l -> List.iter (sub.constructor_declaration sub) l
    | Ptype_record l -> List.iter (sub.label_declaration sub) l
    | Ptype_open -> ()

  let iter_constructor_arguments sub = function
    | Pcstr_tuple l -> List.iter (sub.typ sub) l
    | Pcstr_record l -> List.iter (sub.label_declaration sub) l

  let iter_type_extension sub
      {
        ptyext_path;
        ptyext_params;
        ptyext_constructors;
        ptyext_private = _;
        ptyext_attributes;
      } =
    iter_loc sub ptyext_path;
    List.iter (sub.extension_constructor sub) ptyext_constructors;
    List.iter (iter_fst (sub.typ sub)) ptyext_params;
    sub.attributes sub ptyext_attributes

  let iter_extension_constructor_kind sub = function
    | Pext_decl (ctl, cto) ->
      iter_constructor_arguments sub ctl;
      iter_opt (sub.typ sub) cto
    | Pext_rebind li -> iter_loc sub li

  let iter_extension_constructor sub
      {pext_name; pext_kind; pext_loc; pext_attributes} =
    iter_loc sub pext_name;
    iter_extension_constructor_kind sub pext_kind;
    sub.location sub pext_loc;
    sub.attributes sub pext_attributes
end

module MT = struct
  (* Type expressions for the module language *)

  let iter sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmty_ident s -> iter_loc sub s
    | Pmty_alias s -> iter_loc sub s
    | Pmty_signature sg -> sub.signature sub sg
    | Pmty_functor (s, mt1, mt2) ->
      iter_loc sub s;
      iter_opt (sub.module_type sub) mt1;
      sub.module_type sub mt2
    | Pmty_with (mt, l) ->
      sub.module_type sub mt;
      List.iter (sub.with_constraint sub) l
    | Pmty_typeof me -> sub.module_expr sub me
    | Pmty_extension x -> sub.extension sub x

  let iter_with_constraint sub = function
    | Pwith_type (lid, d) ->
      iter_loc sub lid;
      sub.type_declaration sub d
    | Pwith_module (lid, lid2) ->
      iter_loc sub lid;
      iter_loc sub lid2
    | Pwith_typesubst (lid, d) ->
      iter_loc sub lid;
      sub.type_declaration sub d
    | Pwith_modsubst (s, lid) ->
      iter_loc sub s;
      iter_loc sub lid

  let iter_signature_item sub {psig_desc = desc; psig_loc = loc} =
    sub.location sub loc;
    match desc with
    | Psig_value vd -> sub.value_description sub vd
    | Psig_type (_rf, l) -> List.iter (sub.type_declaration sub) l
    | Psig_typext te -> sub.type_extension sub te
    | Psig_exception ed -> sub.extension_constructor sub ed
    | Psig_module x -> sub.module_declaration sub x
    | Psig_recmodule l -> List.iter (sub.module_declaration sub) l
    | Psig_modtype x -> sub.module_type_declaration sub x
    | Psig_open x -> sub.open_description sub x
    | Psig_include x -> sub.include_description sub x
    | Psig_class () -> ()
    | Psig_class_type () -> ()
    | Psig_extension (x, attrs) ->
      sub.extension sub x;
      sub.attributes sub attrs
    | Psig_attribute x -> sub.attribute sub x
end

module M = struct
  (* Value expressions for the module language *)

  let iter sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmod_ident x -> iter_loc sub x
    | Pmod_structure str -> sub.structure sub str
    | Pmod_functor (arg, arg_ty, body) ->
      iter_loc sub arg;
      iter_opt (sub.module_type sub) arg_ty;
      sub.module_expr sub body
    | Pmod_apply (m1, m2) ->
      sub.module_expr sub m1;
      sub.module_expr sub m2
    | Pmod_constraint (m, mty) ->
      sub.module_expr sub m;
      sub.module_type sub mty
    | Pmod_unpack e -> sub.expr sub e
    | Pmod_extension x -> sub.extension sub x

  let iter_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    sub.location sub loc;
    match desc with
    | Pstr_eval (x, attrs) ->
      sub.expr sub x;
      sub.attributes sub attrs
    | Pstr_value (_r, vbs) -> List.iter (sub.value_binding sub) vbs
    | Pstr_primitive vd -> sub.value_description sub vd
    | Pstr_type (_rf, l) -> List.iter (sub.type_declaration sub) l
    | Pstr_typext te -> sub.type_extension sub te
    | Pstr_exception ed -> sub.extension_constructor sub ed
    | Pstr_module x -> sub.module_binding sub x
    | Pstr_recmodule l -> List.iter (sub.module_binding sub) l
    | Pstr_modtype x -> sub.module_type_declaration sub x
    | Pstr_open x -> sub.open_description sub x
    | Pstr_class () -> ()
    | Pstr_class_type () -> ()
    | Pstr_include x -> sub.include_declaration sub x
    | Pstr_extension (x, attrs) ->
      sub.extension sub x;
      sub.attributes sub attrs
    | Pstr_attribute x -> sub.attribute sub x
end

module E = struct
  (* Value expressions for the core language *)

  let iter sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pexp_ident x -> iter_loc sub x
    | Pexp_constant _ -> ()
    | Pexp_let (_r, vbs, e) ->
      List.iter (sub.value_binding sub) vbs;
      sub.expr sub e
    | Pexp_fun {default = def; lhs = p; rhs = e} ->
      iter_opt (sub.expr sub) def;
      sub.pat sub p;
      sub.expr sub e
    | Pexp_apply (e, l) ->
      sub.expr sub e;
      List.iter (iter_snd (sub.expr sub)) l
    | Pexp_match (e, pel) ->
      sub.expr sub e;
      sub.cases sub pel
    | Pexp_try (e, pel) ->
      sub.expr sub e;
      sub.cases sub pel
    | Pexp_tuple el -> List.iter (sub.expr sub) el
    | Pexp_construct (lid, arg) ->
      iter_loc sub lid;
      iter_opt (sub.expr sub) arg
    | Pexp_variant (_lab, eo) -> iter_opt (sub.expr sub) eo
    | Pexp_record (l, eo) ->
      List.iter (iter_tuple3 (iter_loc sub) (sub.expr sub) (fun _ -> ())) l;
      iter_opt (sub.expr sub) eo
    | Pexp_field (e, lid) ->
      sub.expr sub e;
      iter_loc sub lid
    | Pexp_setfield (e1, lid, e2) ->
      sub.expr sub e1;
      iter_loc sub lid;
      sub.expr sub e2
    | Pexp_array el -> List.iter (sub.expr sub) el
    | Pexp_ifthenelse (e1, e2, e3) ->
      sub.expr sub e1;
      sub.expr sub e2;
      iter_opt (sub.expr sub) e3
    | Pexp_sequence (e1, e2) ->
      sub.expr sub e1;
      sub.expr sub e2
    | Pexp_while (e1, e2) ->
      sub.expr sub e1;
      sub.expr sub e2
    | Pexp_for (p, e1, e2, _d, e3) ->
      sub.pat sub p;
      sub.expr sub e1;
      sub.expr sub e2;
      sub.expr sub e3
    | Pexp_coerce (e, (), t2) ->
      sub.expr sub e;
      sub.typ sub t2
    | Pexp_constraint (e, t) ->
      sub.expr sub e;
      sub.typ sub t
    | Pexp_send (e, _s) -> sub.expr sub e
    | Pexp_new lid -> iter_loc sub lid
    | Pexp_setinstvar (s, e) ->
      iter_loc sub s;
      sub.expr sub e
    | Pexp_override sel ->
      List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) sel
    | Pexp_letmodule (s, me, e) ->
      iter_loc sub s;
      sub.module_expr sub me;
      sub.expr sub e
    | Pexp_letexception (cd, e) ->
      sub.extension_constructor sub cd;
      sub.expr sub e
    | Pexp_assert e -> sub.expr sub e
    | Pexp_lazy e -> sub.expr sub e
    | Pexp_poly (e, t) ->
      sub.expr sub e;
      iter_opt (sub.typ sub) t
    | Pexp_newtype (_s, e) -> sub.expr sub e
    | Pexp_pack me -> sub.module_expr sub me
    | Pexp_open (_ovf, lid, e) ->
      iter_loc sub lid;
      sub.expr sub e
    | Pexp_extension x -> sub.extension sub x
end

module P = struct
  (* Patterns *)

  let iter sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ppat_any -> ()
    | Ppat_var s -> iter_loc sub s
    | Ppat_alias (p, s) ->
      sub.pat sub p;
      iter_loc sub s
    | Ppat_constant _ -> ()
    | Ppat_interval _ -> ()
    | Ppat_tuple pl -> List.iter (sub.pat sub) pl
    | Ppat_construct (l, p) ->
      iter_loc sub l;
      iter_opt (sub.pat sub) p
    | Ppat_variant (_l, p) -> iter_opt (sub.pat sub) p
    | Ppat_record (lpl, _cf) ->
      List.iter (iter_tuple3 (iter_loc sub) (sub.pat sub) (fun _ -> ())) lpl
    | Ppat_array pl -> List.iter (sub.pat sub) pl
    | Ppat_or (p1, p2) ->
      sub.pat sub p1;
      sub.pat sub p2
    | Ppat_constraint (p, t) ->
      sub.pat sub p;
      sub.typ sub t
    | Ppat_type s -> iter_loc sub s
    | Ppat_lazy p -> sub.pat sub p
    | Ppat_unpack s -> iter_loc sub s
    | Ppat_exception p -> sub.pat sub p
    | Ppat_extension x -> sub.extension sub x
    | Ppat_open (lid, p) ->
      iter_loc sub lid;
      sub.pat sub p
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_iterator =
  {
    structure = (fun this l -> List.iter (this.structure_item this) l);
    structure_item = M.iter_structure_item;
    module_expr = M.iter;
    signature = (fun this l -> List.iter (this.signature_item this) l);
    signature_item = MT.iter_signature_item;
    module_type = MT.iter;
    with_constraint = MT.iter_with_constraint;
    type_declaration = T.iter_type_declaration;
    type_kind = T.iter_type_kind;
    typ = T.iter;
    type_extension = T.iter_type_extension;
    extension_constructor = T.iter_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim = _; pval_loc; pval_attributes} ->
        iter_loc this pval_name;
        this.typ this pval_type;
        this.attributes this pval_attributes;
        this.location this pval_loc);
    pat = P.iter;
    expr = E.iter;
    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
        iter_loc this pmd_name;
        this.module_type this pmd_type;
        this.attributes this pmd_attributes;
        this.location this pmd_loc);
    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
        iter_loc this pmtd_name;
        iter_opt (this.module_type this) pmtd_type;
        this.attributes this pmtd_attributes;
        this.location this pmtd_loc);
    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
        iter_loc this pmb_name;
        this.module_expr this pmb_expr;
        this.attributes this pmb_attributes;
        this.location this pmb_loc);
    open_description =
      (fun this {popen_lid; popen_override = _; popen_attributes; popen_loc} ->
        iter_loc this popen_lid;
        this.location this popen_loc;
        this.attributes this popen_attributes);
    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
        this.module_type this pincl_mod;
        this.location this pincl_loc;
        this.attributes this pincl_attributes);
    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
        this.module_expr this pincl_mod;
        this.location this pincl_loc;
        this.attributes this pincl_attributes);
    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
        this.pat this pvb_pat;
        this.expr this pvb_expr;
        this.location this pvb_loc;
        this.attributes this pvb_attributes);
    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
        iter_loc this pcd_name;
        T.iter_constructor_arguments this pcd_args;
        iter_opt (this.typ this) pcd_res;
        this.location this pcd_loc;
        this.attributes this pcd_attributes);
    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable = _; pld_attributes} ->
        iter_loc this pld_name;
        this.typ this pld_type;
        this.location this pld_loc;
        this.attributes this pld_attributes);
    cases = (fun this l -> List.iter (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
        this.pat this pc_lhs;
        iter_opt (this.expr this) pc_guard;
        this.expr this pc_rhs);
    location = (fun _this _l -> ());
    extension =
      (fun this (s, e) ->
        iter_loc this s;
        this.payload this e);
    attribute =
      (fun this (s, e) ->
        iter_loc this s;
        this.payload this e);
    attributes = (fun this l -> List.iter (this.attribute this) l);
    payload =
      (fun this -> function
        | PStr x -> this.structure this x
        | PSig x -> this.signature this x
        | PTyp x -> this.typ this x
        | PPat (x, g) ->
          this.pat this x;
          iter_opt (this.expr this) g);
  }
