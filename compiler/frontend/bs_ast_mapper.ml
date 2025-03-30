(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
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

open! Parsetree
open! Ast_helper
open Location

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  constructor_declaration:
    mapper -> constructor_declaration -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor:
    mapper -> extension_constructor -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration:
    mapper -> module_type_declaration -> module_type_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  (* #if true then    *)
  type_declaration_list:
    mapper -> type_declaration list -> type_declaration list;
  (* #end *)
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  (* #if true then    *)
  value_bindings_rec: mapper -> value_binding list -> value_binding list;
  value_bindings: mapper -> value_binding list -> value_binding list;
  (* #end *)
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

let id x = x
let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, attrs, b, tl) ->
      Rtag
        (map_loc sub l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
    | Rinherit t -> Rinherit (sub.typ sub t)

  let object_field sub = function
    | Otag (l, attrs, t) ->
      Otag (map_loc sub l, sub.attributes sub attrs, sub.typ sub t)
    | Oinherit t -> Oinherit (sub.typ sub t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow {lbl; arg; ret; arity} ->
      arrow ~loc ~attrs ~arity lbl (sub.typ sub arg) (sub.typ sub ret)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) ->
      constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_object (l, o) ->
      object_ ~loc ~attrs (List.map (object_field sub) l) o
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
    | Ptyp_variant (rl, b, ll) ->
      variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) ->
      poly ~loc ~attrs (List.map (map_loc sub) sl) (sub.typ sub t)
    | Ptyp_package (lid, l) ->
      package ~loc ~attrs (map_loc sub lid)
        (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_type_declaration sub
      {
        ptype_name;
        ptype_params;
        ptype_cstrs;
        ptype_kind;
        ptype_private;
        ptype_manifest;
        ptype_attributes;
        ptype_loc;
      } =
    Type.mk (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:ptype_private
      ~cstrs:
        (List.map
           (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
           ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ?manifest:(map_opt (sub.typ sub) ptype_manifest)
      ~loc:(sub.location sub ptype_loc)
      ~attrs:(sub.attributes sub ptype_attributes)

  (* #if true then *)
  let map_type_declaration_list sub l = List.map (sub.type_declaration sub) l

  (* #end *)
  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
      Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_constructor_arguments sub = function
    | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
    | Pcstr_record l -> Pcstr_record (List.map (sub.label_declaration sub) l)

  let map_type_extension sub
      {
        ptyext_path;
        ptyext_params;
        ptyext_constructors;
        ptyext_private;
        ptyext_attributes;
      } =
    Te.mk (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:ptyext_private
      ~attrs:(sub.attributes sub ptyext_attributes)

  let map_extension_constructor_kind sub = function
    | Pext_decl (ctl, cto) ->
      Pext_decl (map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
    | Pext_rebind li -> Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
      {pext_name; pext_kind; pext_loc; pext_attributes} =
    Te.constructor (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)
      ~loc:(sub.location sub pext_loc)
      ~attrs:(sub.attributes sub pext_attributes)
end

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
    | Pmty_functor (s, mt1, mt2) ->
      functor_ ~loc ~attrs (map_loc sub s)
        (Misc.may_map (sub.module_type sub) mt1)
        (sub.module_type sub mt2)
    | Pmty_with (mt, l) ->
      with_ ~loc ~attrs (sub.module_type sub mt)
        (List.map (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
      Pwith_type (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_module (lid, lid2) ->
      Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_typesubst (lid, d) ->
      Pwith_typesubst (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) -> Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub.location sub loc in
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type (rf, l) ->
      (* #if false then
            type_ ~loc rf (List.map (sub.type_declaration sub) l)
         #else *)
      type_ ~loc rf (sub.type_declaration_list sub l)
    (* #end *)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_recmodule l ->
      rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_extension (x, attrs) ->
      extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
end

module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
    | Pmod_functor (arg, arg_ty, body) ->
      functor_ ~loc ~attrs (map_loc sub arg)
        (Misc.may_map (sub.module_type sub) arg_ty)
        (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
      apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
    | Pmod_constraint (m, mty) ->
      constraint_ ~loc ~attrs (sub.module_expr sub m) (sub.module_type sub mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
    | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub.location sub loc in
    match desc with
    | Pstr_eval (x, attrs) ->
      eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
    | Pstr_value (r, vbs) ->
      (* #if false then
          value ~loc r (List.map (sub.value_binding sub) vbs)
         #else *)
      value ~loc r
        ((if r = Recursive then sub.value_bindings_rec else sub.value_bindings)
           sub vbs)
    (* #end *)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) ->
      (* #if false then
          type_ ~loc rf (List.map (sub.type_declaration sub) l)
         #else *)
      type_ ~loc rf (sub.type_declaration_list sub l)
    (* #end *)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_description sub x)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
      extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs x
    | Pexp_let (r, vbs, e) ->
      (* #if false then
              let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
                (sub.expr sub e)
         #else *)
      let_ ~loc ~attrs r
        ((if r = Recursive then sub.value_bindings_rec else sub.value_bindings)
           sub vbs)
        (sub.expr sub e)
    (* #end *)
    | Pexp_fun {arg_label = lab; default = def; lhs = p; rhs = e; arity; async}
      ->
      fun_ ~loc ~attrs ~arity ~async lab
        (map_opt (sub.expr sub) def)
        (sub.pat sub p) (sub.expr sub e)
    | Pexp_apply {funct = e; args = l; partial} ->
      apply ~loc ~attrs ~partial (sub.expr sub e)
        (List.map (map_snd (sub.expr sub)) l)
    | Pexp_match (e, pel) ->
      match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_construct (lid, arg) ->
      construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) ->
      variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
      record ~loc ~attrs
        (List.map (map_tuple3 (map_loc sub) (sub.expr sub) id) l)
        (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) ->
      field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
      setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid) (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
      ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
        (map_opt (sub.expr sub) e3)
    | Pexp_sequence (e1, e2) ->
      sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
      while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
      for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
        (sub.expr sub e3)
    | Pexp_coerce (e, (), t2) ->
      coerce ~loc ~attrs (sub.expr sub e) (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
      constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
    | Pexp_send (e, s) -> send ~loc ~attrs (sub.expr sub e) (map_loc sub s)
    | Pexp_letmodule (s, me, e) ->
      letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
        (sub.expr sub e)
    | Pexp_letexception (cd, e) ->
      letexception ~loc ~attrs
        (sub.extension_constructor sub cd)
        (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
    | Pexp_newtype (s, e) ->
      newtype ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
    | Pexp_open (ovf, lid, e) ->
      open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs c
    | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
      construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
      record ~loc ~attrs
        (List.map (map_tuple3 (map_loc sub) (sub.pat sub) id) lpl)
        cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
    | Ppat_constraint (p, t) ->
      constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_open (lid, p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    structure = (fun this l -> List.map (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> List.map (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    type_declaration = T.map_type_declaration;
    (* #if true then      *)
    type_declaration_list = T.map_type_declaration_list;
    (* #end *)
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim; pval_loc; pval_attributes} ->
        Val.mk (map_loc this pval_name) (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ~prim:pval_prim);
    pat = P.map;
    expr = E.map;
    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
        Md.mk (map_loc this pmd_name)
          (this.module_type this pmd_type)
          ~attrs:(this.attributes this pmd_attributes)
          ~loc:(this.location this pmd_loc));
    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
        Mtd.mk (map_loc this pmtd_name)
          ?typ:(map_opt (this.module_type this) pmtd_type)
          ~attrs:(this.attributes this pmtd_attributes)
          ~loc:(this.location this pmtd_loc));
    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
        Mb.mk (map_loc this pmb_name)
          (this.module_expr this pmb_expr)
          ~attrs:(this.attributes this pmb_attributes)
          ~loc:(this.location this pmb_loc));
    open_description =
      (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
        Opn.mk (map_loc this popen_lid) ~override:popen_override
          ~loc:(this.location this popen_loc)
          ~attrs:(this.attributes this popen_attributes));
    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
        Incl.mk
          (this.module_type this pincl_mod)
          ~loc:(this.location this pincl_loc)
          ~attrs:(this.attributes this pincl_attributes));
    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
        Incl.mk
          (this.module_expr this pincl_mod)
          ~loc:(this.location this pincl_loc)
          ~attrs:(this.attributes this pincl_attributes));
    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
        Vb.mk (this.pat this pvb_pat) (this.expr this pvb_expr)
          ~loc:(this.location this pvb_loc)
          ~attrs:(this.attributes this pvb_attributes));
    (* #if true then  *)
    value_bindings =
      (fun this vbs ->
        match vbs with
        | [vb] -> [this.value_binding this vb]
        | _ -> List.map (this.value_binding this) vbs);
    value_bindings_rec =
      (fun this vbs ->
        match vbs with
        | [vb] -> [this.value_binding this vb]
        | _ -> List.map (this.value_binding this) vbs);
    (* #end *)
    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
        Type.constructor (map_loc this pcd_name)
          ~args:(T.map_constructor_arguments this pcd_args)
          ?res:(map_opt (this.typ this) pcd_res)
          ~loc:(this.location this pcd_loc)
          ~attrs:(this.attributes this pcd_attributes));
    label_declaration =
      (fun this
        {pld_name; pld_type; pld_loc; pld_mutable; pld_optional; pld_attributes}
      ->
        Type.field (map_loc this pld_name) (this.typ this pld_type)
          ~mut:pld_mutable ~optional:pld_optional
          ~loc:(this.location this pld_loc)
          ~attrs:(this.attributes this pld_attributes));
    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
        {
          pc_lhs = this.pat this pc_lhs;
          pc_guard = map_opt (this.expr this) pc_guard;
          pc_rhs = this.expr this pc_rhs;
        });
    location = (fun _this l -> l);
    extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attributes = (fun this l -> List.map (this.attribute this) l);
    payload =
      (fun this -> function
        | PStr x -> PStr (this.structure this x)
        | PSig x -> PSig (this.signature this x)
        | PTyp x -> PTyp (this.typ this x)
        | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g));
  }
