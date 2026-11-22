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

open Parsetree
open Ast_helper
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
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

(* The default mapper returns a node as it is when all its parts come back
   physically unchanged, and rebuilds it otherwise, so a pass that rewrites
   little allocates little. Mappers can have effects, so the parts are mapped
   in the order in which they always were: the order in which OCaml evaluated
   the arguments of the constructor calls this mapper used to make, right to
   left in the order of the callee's parameters (or of a record's fields). *)

(* [List.map], keeping [l] when no element changes *)
let rec map_list f l =
  match l with
  | [] -> l
  | x :: xs ->
    let x' = f x in
    let xs' = map_list f xs in
    if x' == x && xs' == xs then l else x' :: xs'

let map_opt f o =
  match o with
  | None -> o
  | Some x ->
    let x' = f x in
    if x' == x then o else Some x'

let map_loc sub ({loc; txt} as l) =
  let loc' = sub.location sub loc in
  if loc' == loc then l else {txt; loc = loc'}

let map_raw_source sub (source : raw_source) : raw_source =
  match source with
  | Raw_string s ->
    let s' = map_loc sub s in
    if s' == s then source else Raw_string s'
  | Raw_template s ->
    let s' = map_loc sub s in
    if s' == s then source else Raw_template s'

module T = struct
  (* Type expressions for the core language *)

  let row_field sub field =
    match field with
    | Rtag (l, attrs, b, groups) ->
      let groups' =
        map_list
          (fun ({loc; txt} as group) ->
            let loc' = sub.location sub loc in
            let txt' = map_list (sub.typ sub) txt in
            if loc' == loc && txt' == txt then group
            else {loc = loc'; txt = txt'})
          groups
      in
      let attrs' = sub.attributes sub attrs in
      let l' = map_loc sub l in
      if l' == l && attrs' == attrs && groups' == groups then field
      else Rtag (l', attrs', b, groups')
    | Rinherit t ->
      let t' = sub.typ sub t in
      if t' == t then field else Rinherit t'

  let object_field sub field =
    match field with
    | Otag (l, attrs, t) ->
      let t' = sub.typ sub t in
      let attrs' = sub.attributes sub attrs in
      let l' = map_loc sub l in
      if l' == l && attrs' == attrs && t' == t then field
      else Otag (l', attrs', t')
    | Oinherit t ->
      let t' = sub.typ sub t in
      if t' == t then field else Oinherit t'

  let map_arg sub (arg : arg) : arg =
    match arg with
    | Parg_type a ->
      let typ = sub.typ sub a.typ in
      if typ == a.typ then arg else Parg_type {a with typ}
    | Parg_fixed a ->
      let txt : fixed_value =
        match a.value.txt with
        | Pfixed_int source ->
          let source' = map_loc sub source in
          if source' == source then a.value.txt else Pfixed_int source'
        | Pfixed_string _ -> a.value.txt
        | Pfixed_template source ->
          let source' = map_loc sub source in
          if source' == source then a.value.txt else Pfixed_template source'
        | Pfixed_json source ->
          let source' = map_loc sub source in
          if source' == source then a.value.txt else Pfixed_json source'
      in
      let loc = sub.location sub a.loc in
      let value_loc = sub.location sub a.value.loc in
      if txt == a.value.txt && value_loc == a.value.loc && loc == a.loc then arg
      else Parg_fixed {a with value = {txt; loc = value_loc}; loc}

  let map_desc sub desc =
    match desc with
    | Ptyp_any | Ptyp_var _ -> desc
    | Ptyp_arrow {params; ret} ->
      let ret' = sub.typ sub ret in
      let params' = map_list (map_arg sub) params in
      if params' == params && ret' == ret then desc
      else Ptyp_arrow {params = params'; ret = ret'}
    | Ptyp_tuple tyl ->
      let tyl' = map_list (sub.typ sub) tyl in
      if tyl' == tyl then desc else Ptyp_tuple tyl'
    | Ptyp_constr (lid, tl) ->
      let tl' = map_list (sub.typ sub) tl in
      let lid' = map_loc sub lid in
      if lid' == lid && tl' == tl then desc else Ptyp_constr (lid', tl')
    | Ptyp_object (l, o) ->
      let l' = map_list (object_field sub) l in
      if l' == l then desc else Ptyp_object (l', o)
    | Ptyp_alias (t, s) ->
      let t' = sub.typ sub t in
      if t' == t then desc else Ptyp_alias (t', s)
    | Ptyp_variant (rl, b, ll) ->
      let rl' = map_list (row_field sub) rl in
      if rl' == rl then desc else Ptyp_variant (rl', b, ll)
    | Ptyp_poly (sl, t) ->
      let t' = sub.typ sub t in
      let sl' = map_list (map_loc sub) sl in
      if sl' == sl && t' == t then desc else Ptyp_poly (sl', t')
    | Ptyp_package (lid, l) ->
      let l' =
        map_list
          (fun ((x, y) as pair) ->
            let y' = sub.typ sub y in
            let x' = map_loc sub x in
            if x' == x && y' == y then pair else (x', y'))
          l
      in
      let lid' = map_loc sub lid in
      if lid' == lid && l' == l then desc else Ptyp_package (lid', l')
    | Ptyp_extension x ->
      let x' = sub.extension sub x in
      if x' == x then desc else Ptyp_extension x'

  let map sub ({ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} as t)
      =
    let loc' = sub.location sub loc in
    let attrs' = sub.attributes sub attrs in
    let desc' = map_desc sub desc in
    if desc' == desc && loc' == loc && attrs' == attrs then t
    else Typ.mk ~loc:loc' ~attrs:attrs' desc'

  let map_type_declaration sub
      ({
         ptype_name;
         ptype_params;
         ptype_cstrs;
         ptype_kind;
         ptype_private;
         ptype_manifest;
         ptype_origin;
         ptype_attributes;
         ptype_loc;
       } as decl) =
    let name = map_loc sub ptype_name in
    let manifest = map_opt (sub.typ sub) ptype_manifest in
    let kind = sub.type_kind sub ptype_kind in
    let cstrs =
      map_list
        (fun ((x, y, z) as cstr) ->
          let z' = sub.location sub z in
          let y' = sub.typ sub y in
          let x' = sub.typ sub x in
          if x' == x && y' == y && z' == z then cstr else (x', y', z'))
        ptype_cstrs
    in
    let params =
      map_list
        (fun ((x, y) as param) ->
          let x' = sub.typ sub x in
          if x' == x then param else (x', y))
        ptype_params
    in
    let attrs = sub.attributes sub ptype_attributes in
    let loc = sub.location sub ptype_loc in
    if
      name == ptype_name && manifest == ptype_manifest && kind == ptype_kind
      && cstrs == ptype_cstrs && params == ptype_params
      && attrs == ptype_attributes && loc == ptype_loc
    then decl
    else
      Type.mk name ~params ~priv:ptype_private ~cstrs ~kind ?manifest
        ~origin:ptype_origin ~loc ~attrs

  let map_type_kind sub kind =
    match kind with
    | Ptype_abstract | Ptype_open -> kind
    | Ptype_variant l ->
      let l' = map_list (sub.constructor_declaration sub) l in
      if l' == l then kind else Ptype_variant l'
    | Ptype_record l ->
      let l' = map_list (sub.label_declaration sub) l in
      if l' == l then kind else Ptype_record l'

  let map_constructor_arguments sub args =
    match args with
    | Pcstr_tuple l ->
      let l' = map_list (sub.typ sub) l in
      if l' == l then args else Pcstr_tuple l'
    | Pcstr_record l ->
      let l' = map_list (sub.label_declaration sub) l in
      if l' == l then args else Pcstr_record l'

  let map_type_extension sub
      ({
         ptyext_path;
         ptyext_params;
         ptyext_constructors;
         ptyext_private;
         ptyext_attributes;
       } as ext) =
    let constructors =
      map_list (sub.extension_constructor sub) ptyext_constructors
    in
    let path = map_loc sub ptyext_path in
    let params =
      map_list
        (fun ((x, y) as param) ->
          let x' = sub.typ sub x in
          if x' == x then param else (x', y))
        ptyext_params
    in
    let attrs = sub.attributes sub ptyext_attributes in
    if
      constructors == ptyext_constructors
      && path == ptyext_path && params == ptyext_params
      && attrs == ptyext_attributes
    then ext
    else Te.mk path constructors ~params ~priv:ptyext_private ~attrs

  let map_extension_constructor_kind sub kind =
    match kind with
    | Pext_decl (ctl, cto) ->
      let cto' = map_opt (sub.typ sub) cto in
      let ctl' = map_constructor_arguments sub ctl in
      if ctl' == ctl && cto' == cto then kind else Pext_decl (ctl', cto')
    | Pext_rebind li ->
      let li' = map_loc sub li in
      if li' == li then kind else Pext_rebind li'

  let map_extension_constructor sub
      ({pext_name; pext_kind; pext_loc; pext_attributes} as ext) =
    let kind = map_extension_constructor_kind sub pext_kind in
    let name = map_loc sub pext_name in
    let attrs = sub.attributes sub pext_attributes in
    let loc = sub.location sub pext_loc in
    if
      kind == pext_kind && name == pext_name && attrs == pext_attributes
      && loc == pext_loc
    then ext
    else Te.constructor name kind ~loc ~attrs
end

module MT = struct
  (* Type expressions for the module language *)

  let map_desc sub desc =
    match desc with
    | Pmty_ident s ->
      let s' = map_loc sub s in
      if s' == s then desc else Pmty_ident s'
    | Pmty_alias s ->
      let s' = map_loc sub s in
      if s' == s then desc else Pmty_alias s'
    | Pmty_signature sg ->
      let sg' = sub.signature sub sg in
      if sg' == sg then desc else Pmty_signature sg'
    | Pmty_functor (s, mt1, mt2) ->
      let mt2' = sub.module_type sub mt2 in
      let mt1' = map_opt (sub.module_type sub) mt1 in
      let s' = map_loc sub s in
      if s' == s && mt1' == mt1 && mt2' == mt2 then desc
      else Pmty_functor (s', mt1', mt2')
    | Pmty_with (mt, l) ->
      let l' = map_list (sub.with_constraint sub) l in
      let mt' = sub.module_type sub mt in
      if mt' == mt && l' == l then desc else Pmty_with (mt', l')
    | Pmty_typeof me ->
      let me' = sub.module_expr sub me in
      if me' == me then desc else Pmty_typeof me'
    | Pmty_extension x ->
      let x' = sub.extension sub x in
      if x' == x then desc else Pmty_extension x'

  let map sub
      ({pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} as mt) =
    let loc' = sub.location sub loc in
    let attrs' = sub.attributes sub attrs in
    let desc' = map_desc sub desc in
    if desc' == desc && loc' == loc && attrs' == attrs then mt
    else Mty.mk ~loc:loc' ~attrs:attrs' desc'

  let map_with_constraint sub c =
    match c with
    | Pwith_type (lid, d) ->
      let d' = sub.type_declaration sub d in
      let lid' = map_loc sub lid in
      if lid' == lid && d' == d then c else Pwith_type (lid', d')
    | Pwith_module (lid, lid2) ->
      let lid2' = map_loc sub lid2 in
      let lid' = map_loc sub lid in
      if lid' == lid && lid2' == lid2 then c else Pwith_module (lid', lid2')
    | Pwith_typesubst (lid, d) ->
      let d' = sub.type_declaration sub d in
      let lid' = map_loc sub lid in
      if lid' == lid && d' == d then c else Pwith_typesubst (lid', d')
    | Pwith_modsubst (s, lid) ->
      let lid' = map_loc sub lid in
      let s' = map_loc sub s in
      if s' == s && lid' == lid then c else Pwith_modsubst (s', lid')

  let map_signature_item_desc sub desc =
    match desc with
    | Psig_value vd ->
      let vd' = sub.value_description sub vd in
      if vd' == vd then desc else Psig_value vd'
    | Psig_type (rf, l) ->
      let l' = map_list (sub.type_declaration sub) l in
      if l' == l then desc else Psig_type (rf, l')
    | Psig_typext te ->
      let te' = sub.type_extension sub te in
      if te' == te then desc else Psig_typext te'
    | Psig_exception ed ->
      let ed' = sub.extension_constructor sub ed in
      if ed' == ed then desc else Psig_exception ed'
    | Psig_module x ->
      let x' = sub.module_declaration sub x in
      if x' == x then desc else Psig_module x'
    | Psig_recmodule l ->
      let l' = map_list (sub.module_declaration sub) l in
      if l' == l then desc else Psig_recmodule l'
    | Psig_modtype x ->
      let x' = sub.module_type_declaration sub x in
      if x' == x then desc else Psig_modtype x'
    | Psig_open x ->
      let x' = sub.open_description sub x in
      if x' == x then desc else Psig_open x'
    | Psig_include x ->
      let x' = sub.include_description sub x in
      if x' == x then desc else Psig_include x'
    | Psig_extension (x, attrs) ->
      let x' = sub.extension sub x in
      let attrs' = sub.attributes sub attrs in
      if x' == x && attrs' == attrs then desc else Psig_extension (x', attrs')
    | Psig_attribute x ->
      let x' = sub.attribute sub x in
      if x' == x then desc else Psig_attribute x'

  let map_signature_item sub ({psig_desc = desc; psig_loc = loc} as item) =
    let loc' = sub.location sub loc in
    let desc' = map_signature_item_desc sub desc in
    if desc' == desc && loc' == loc then item else Sig.mk ~loc:loc' desc'
end

module M = struct
  (* Value expressions for the module language *)

  let map_desc sub desc =
    match desc with
    | Pmod_ident x ->
      let x' = map_loc sub x in
      if x' == x then desc else Pmod_ident x'
    | Pmod_structure str ->
      let str' = sub.structure sub str in
      if str' == str then desc else Pmod_structure str'
    | Pmod_functor (arg, arg_ty, body) ->
      let body' = sub.module_expr sub body in
      let arg_ty' = map_opt (sub.module_type sub) arg_ty in
      let arg' = map_loc sub arg in
      if arg' == arg && arg_ty' == arg_ty && body' == body then desc
      else Pmod_functor (arg', arg_ty', body')
    | Pmod_apply (m1, m2) ->
      let m2' = sub.module_expr sub m2 in
      let m1' = sub.module_expr sub m1 in
      if m1' == m1 && m2' == m2 then desc else Pmod_apply (m1', m2')
    | Pmod_constraint (m, mty) ->
      let mty' = sub.module_type sub mty in
      let m' = sub.module_expr sub m in
      if m' == m && mty' == mty then desc else Pmod_constraint (m', mty')
    | Pmod_unpack e ->
      let e' = sub.expr sub e in
      if e' == e then desc else Pmod_unpack e'
    | Pmod_extension x ->
      let x' = sub.extension sub x in
      if x' == x then desc else Pmod_extension x'
    | Pmod_await m ->
      let m' = sub.module_expr sub m in
      if m' == m then desc else Pmod_await m'

  let map sub
      ({pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} as me) =
    let loc' = sub.location sub loc in
    let attrs' = sub.attributes sub attrs in
    let desc' = map_desc sub desc in
    if desc' == desc && loc' == loc && attrs' == attrs then me
    else Mod.mk ~loc:loc' ~attrs:attrs' desc'

  let map_structure_item_desc sub desc =
    match desc with
    | Pstr_eval (x, attrs) ->
      let x' = sub.expr sub x in
      let attrs' = sub.attributes sub attrs in
      if x' == x && attrs' == attrs then desc else Pstr_eval (x', attrs')
    | Pstr_value (r, vbs) ->
      let vbs' = map_list (sub.value_binding sub) vbs in
      if vbs' == vbs then desc else Pstr_value (r, vbs')
    | Pstr_primitive vd ->
      let vd' = sub.value_description sub vd in
      if vd' == vd then desc else Pstr_primitive vd'
    | Pstr_type (rf, l) ->
      let l' = map_list (sub.type_declaration sub) l in
      if l' == l then desc else Pstr_type (rf, l')
    | Pstr_typext te ->
      let te' = sub.type_extension sub te in
      if te' == te then desc else Pstr_typext te'
    | Pstr_exception ed ->
      let ed' = sub.extension_constructor sub ed in
      if ed' == ed then desc else Pstr_exception ed'
    | Pstr_module x ->
      let x' = sub.module_binding sub x in
      if x' == x then desc else Pstr_module x'
    | Pstr_recmodule l ->
      let l' = map_list (sub.module_binding sub) l in
      if l' == l then desc else Pstr_recmodule l'
    | Pstr_modtype x ->
      let x' = sub.module_type_declaration sub x in
      if x' == x then desc else Pstr_modtype x'
    | Pstr_open x ->
      let x' = sub.open_description sub x in
      if x' == x then desc else Pstr_open x'
    | Pstr_include x ->
      let x' = sub.include_declaration sub x in
      if x' == x then desc else Pstr_include x'
    | Pstr_extension (x, attrs) ->
      let x' = sub.extension sub x in
      let attrs' = sub.attributes sub attrs in
      if x' == x && attrs' == attrs then desc else Pstr_extension (x', attrs')
    | Pstr_raw {name_loc; source; attrs} ->
      let source' = map_raw_source sub source in
      let name_loc' = sub.location sub name_loc in
      let attrs' = sub.attributes sub attrs in
      if source' == source && name_loc' == name_loc && attrs' == attrs then desc
      else Pstr_raw {name_loc = name_loc'; source = source'; attrs = attrs'}
    | Pstr_attribute x ->
      let x' = sub.attribute sub x in
      if x' == x then desc else Pstr_attribute x'

  let map_structure_item sub ({pstr_loc = loc; pstr_desc = desc} as item) =
    let loc' = sub.location sub loc in
    let desc' = map_structure_item_desc sub desc in
    if desc' == desc && loc' == loc then item else Str.mk ~loc:loc' desc'
end

module E = struct
  let map_jsx_children sub xs = map_list (sub.expr sub) xs

  let map_jsx_prop sub prop =
    match prop with
    | JSXPropPunning (optional, name) ->
      let name' = map_loc sub name in
      if name' == name then prop else JSXPropPunning (optional, name')
    | JSXPropValue (name, optional, value) ->
      let value' = sub.expr sub value in
      let name' = map_loc sub name in
      if name' == name && value' == value then prop
      else JSXPropValue (name', optional, value')
    | JSXPropSpreading (loc, e) ->
      let e' = sub.expr sub e in
      let loc' = sub.location sub loc in
      if loc' == loc && e' == e then prop else JSXPropSpreading (loc', e')

  let map_jsx_props sub = map_list (map_jsx_prop sub)

  let map_record_element sub map_x ({lid; x} as element) =
    let x' = map_x sub x in
    let lid' = map_loc sub lid in
    if lid' == lid && x' == x then element
    else {element with lid = lid'; x = x'}

  let map_fun_param sub param =
    let p_pat = sub.pat sub param.p_pat in
    let p_default = map_opt (sub.expr sub) param.p_default in
    if p_pat == param.p_pat && p_default == param.p_default then param
    else {param with p_default; p_pat}

  (* Value expressions for the core language *)

  let map_desc sub desc =
    match desc with
    | Pexp_braces {expr; braces_loc} ->
      let expr' = sub.expr sub expr in
      let braces_loc' = sub.location sub braces_loc in
      if expr' == expr && braces_loc' == braces_loc then desc
      else Pexp_braces {expr = expr'; braces_loc = braces_loc'}
    | Pexp_ident x ->
      let x' = map_loc sub x in
      if x' == x then desc else Pexp_ident x'
    | Pexp_constant _ | Pexp_break | Pexp_continue | Pexp_regexp _ -> desc
    | Pexp_let (r, vbs, e) ->
      let e' = sub.expr sub e in
      let vbs' = map_list (sub.value_binding sub) vbs in
      if vbs' == vbs && e' == e then desc else Pexp_let (r, vbs', e')
    | Pexp_fun {newtypes; params; body; async} ->
      let body' = sub.expr sub body in
      let params' = map_list (map_fun_param sub) params in
      let newtypes' =
        map_list
          (fun ((name, attrs) as newtype) ->
            let attrs' = sub.attributes sub attrs in
            let name' = map_loc sub name in
            if name' == name && attrs' == attrs then newtype else (name', attrs'))
          newtypes
      in
      if newtypes' == newtypes && params' == params && body' == body then desc
      else
        Pexp_fun {newtypes = newtypes'; params = params'; body = body'; async}
    | Pexp_apply ({funct; args} as apply) ->
      let args' =
        map_list
          (fun ((label, arg) as pair) ->
            let arg' = sub.expr sub arg in
            if arg' == arg then pair else (label, arg'))
          args
      in
      let funct' = sub.expr sub funct in
      if funct' == funct && args' == args then desc
      else Pexp_apply {apply with funct = funct'; args = args'}
    | Pexp_match (e, pel) ->
      let pel' = sub.cases sub pel in
      let e' = sub.expr sub e in
      if e' == e && pel' == pel then desc else Pexp_match (e', pel')
    | Pexp_try (e, pel, finally) ->
      let pel' = sub.cases sub pel in
      let e' = sub.expr sub e in
      let finally' = map_opt (sub.expr sub) finally in
      if e' == e && pel' == pel && finally' == finally then desc
      else Pexp_try (e', pel', finally')
    | Pexp_tuple el ->
      let el' = map_list (sub.expr sub) el in
      if el' == el then desc else Pexp_tuple el'
    | Pexp_construct (lid, {txt = args; loc = args_loc}) ->
      let args_loc' = sub.location sub args_loc in
      let args' = map_list (sub.expr sub) args in
      let lid' = map_loc sub lid in
      if lid' == lid && args' == args && args_loc' == args_loc then desc
      else Pexp_construct (lid', {txt = args'; loc = args_loc'})
    | Pexp_variant (lab, {txt = args; loc = args_loc}) ->
      let args_loc' = sub.location sub args_loc in
      let args' = map_list (sub.expr sub) args in
      if args' == args && args_loc' == args_loc then desc
      else Pexp_variant (lab, {txt = args'; loc = args_loc'})
    | Pexp_record (l, eo) ->
      let eo' = map_opt (sub.expr sub) eo in
      let l' = map_list (map_record_element sub sub.expr) l in
      if l' == l && eo' == eo then desc else Pexp_record (l', eo')
    | Pexp_field (e, lid) ->
      let lid' = map_loc sub lid in
      let e' = sub.expr sub e in
      if e' == e && lid' == lid then desc else Pexp_field (e', lid')
    | Pexp_setfield (e1, lid, e2) ->
      let e2' = sub.expr sub e2 in
      let lid' = map_loc sub lid in
      let e1' = sub.expr sub e1 in
      if e1' == e1 && lid' == lid && e2' == e2 then desc
      else Pexp_setfield (e1', lid', e2')
    | Pexp_array el ->
      let el' = map_list (sub.expr sub) el in
      if el' == el then desc else Pexp_array el'
    | Pexp_dict entries ->
      let entries' =
        map_list
          (fun entry ->
            match entry with
            | Pdict_entry (key, e) ->
              let e' = sub.expr sub e in
              let key' = map_loc sub key in
              if key' == key && e' == e then entry else Pdict_entry (key', e')
            | Pdict_spread e ->
              let e' = sub.expr sub e in
              if e' == e then entry else Pdict_spread e')
          entries
      in
      if entries' == entries then desc else Pexp_dict entries'
    | Pexp_spread (kind, elements) ->
      let elements' =
        map_list
          (fun element ->
            match element with
            | Pspread_item e ->
              let e' = sub.expr sub e in
              if e' == e then element else Pspread_item e'
            | Pspread e ->
              let e' = sub.expr sub e in
              if e' == e then element else Pspread e')
          elements
      in
      if elements' == elements then desc else Pexp_spread (kind, elements')
    | Pexp_ifthenelse (e1, e2, e3) ->
      let e3' = map_opt (sub.expr sub) e3 in
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      if e1' == e1 && e2' == e2 && e3' == e3 then desc
      else Pexp_ifthenelse (e1', e2', e3')
    | Pexp_ternary (e1, e2, e3) ->
      let e3' = sub.expr sub e3 in
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      if e1' == e1 && e2' == e2 && e3' == e3 then desc
      else Pexp_ternary (e1', e2', e3')
    | Pexp_sequence (e1, e2) ->
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      if e1' == e1 && e2' == e2 then desc else Pexp_sequence (e1', e2')
    | Pexp_while (e1, e2) ->
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      if e1' == e1 && e2' == e2 then desc else Pexp_while (e1', e2')
    | Pexp_for (p, e1, e2, d, e3) ->
      let e3' = sub.expr sub e3 in
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      let p' = sub.pat sub p in
      if p' == p && e1' == e1 && e2' == e2 && e3' == e3 then desc
      else Pexp_for (p', e1', e2', d, e3')
    | Pexp_for_of (p, e1, e2) ->
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      let p' = sub.pat sub p in
      if p' == p && e1' == e1 && e2' == e2 then desc
      else Pexp_for_of (p', e1', e2')
    | Pexp_for_await_of (p, e1, e2) ->
      let e2' = sub.expr sub e2 in
      let e1' = sub.expr sub e1 in
      let p' = sub.pat sub p in
      if p' == p && e1' == e1 && e2' == e2 then desc
      else Pexp_for_await_of (p', e1', e2')
    | Pexp_template {source_segments; values} ->
      let values' = map_list (sub.expr sub) values in
      let source_segments' = map_list (map_loc sub) source_segments in
      if source_segments' == source_segments && values' == values then desc
      else Pexp_template {source_segments = source_segments'; values = values'}
    | Pexp_tagged_template {tag; raw_sources; values} ->
      let values' = map_list (sub.expr sub) values in
      let raw_sources' = map_list (map_loc sub) raw_sources in
      let tag' = sub.expr sub tag in
      if tag' == tag && raw_sources' == raw_sources && values' == values then
        desc
      else
        Pexp_tagged_template
          {tag = tag'; raw_sources = raw_sources'; values = values'}
    | Pexp_coerce (e, t2) ->
      let t2' = sub.typ sub t2 in
      let e' = sub.expr sub e in
      if e' == e && t2' == t2 then desc else Pexp_coerce (e', t2')
    | Pexp_constraint (e, t) ->
      let t' = sub.typ sub t in
      let e' = sub.expr sub e in
      if e' == e && t' == t then desc else Pexp_constraint (e', t')
    | Pexp_object_get (e, s) ->
      let s' = map_loc sub s in
      let e' = sub.expr sub e in
      if e' == e && s' == s then desc else Pexp_object_get (e', s')
    | Pexp_object_set (e, s, v) ->
      let v' = sub.expr sub v in
      let s' = map_loc sub s in
      let e' = sub.expr sub e in
      if e' == e && s' == s && v' == v then desc
      else Pexp_object_set (e', s', v')
    | Pexp_object_literal fields ->
      let fields' =
        map_list
          (fun ((s, e) as field) ->
            let e' = sub.expr sub e in
            let s' = map_loc sub s in
            if s' == s && e' == e then field else (s', e'))
          fields
      in
      if fields' == fields then desc else Pexp_object_literal fields'
    | Pexp_letmodule (s, me, e) ->
      let e' = sub.expr sub e in
      let me' = sub.module_expr sub me in
      let s' = map_loc sub s in
      if s' == s && me' == me && e' == e then desc
      else Pexp_letmodule (s', me', e')
    | Pexp_letexception (cd, e) ->
      let e' = sub.expr sub e in
      let cd' = sub.extension_constructor sub cd in
      if cd' == cd && e' == e then desc else Pexp_letexception (cd', e')
    | Pexp_assert e ->
      let e' = sub.expr sub e in
      if e' == e then desc else Pexp_assert e'
    | Pexp_pack me ->
      let me' = sub.module_expr sub me in
      if me' == me then desc else Pexp_pack me'
    | Pexp_open (ovf, lid, e) ->
      let e' = sub.expr sub e in
      let lid' = map_loc sub lid in
      if lid' == lid && e' == e then desc else Pexp_open (ovf, lid', e')
    | Pexp_extension x ->
      let x' = sub.extension sub x in
      if x' == x then desc else Pexp_extension x'
    | Pexp_raw {kind; name_loc; source} ->
      let source' = map_raw_source sub source in
      let name_loc' = sub.location sub name_loc in
      if source' == source && name_loc' == name_loc then desc
      else Pexp_raw {kind; name_loc = name_loc'; source = source'}
    | Pexp_todo {name_loc; message} ->
      let message' = map_opt (sub.expr sub) message in
      let name_loc' = sub.location sub name_loc in
      if message' == message && name_loc' == name_loc then desc
      else Pexp_todo {name_loc = name_loc'; message = message'}
    | Pexp_await e ->
      let e' = sub.expr sub e in
      if e' == e then desc else Pexp_await e'
    | Pexp_jsx_element (Jsx_fragment fragment) ->
      let children = fragment.jsx_fragment_children in
      let children' = map_jsx_children sub children in
      if children' == children then desc
      else
        Pexp_jsx_element
          (Jsx_fragment {fragment with jsx_fragment_children = children'})
    | Pexp_jsx_element (Jsx_unary_element element) ->
      let props = element.jsx_unary_element_props in
      let name = element.jsx_unary_element_tag_name in
      let props' = map_jsx_props sub props in
      let name' = map_loc sub name in
      if name' == name && props' == props then desc
      else
        Pexp_jsx_element
          (Jsx_unary_element
             {
               jsx_unary_element_tag_name = name';
               jsx_unary_element_props = props';
             })
    | Pexp_jsx_element (Jsx_container_element element) ->
      let children = element.jsx_container_element_children in
      let props = element.jsx_container_element_props in
      let name = element.jsx_container_element_tag_name_start in
      let children' = map_jsx_children sub children in
      let props' = map_jsx_props sub props in
      let name' = map_loc sub name in
      if name' == name && props' == props && children' == children then desc
      else
        Pexp_jsx_element
          (Jsx_container_element
             {
               element with
               jsx_container_element_tag_name_start = name';
               jsx_container_element_props = props';
               jsx_container_element_children = children';
             })

  let map sub ({pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} as e)
      =
    let loc' = sub.location sub loc in
    let attrs' = sub.attributes sub attrs in
    let desc' = map_desc sub desc in
    (* Like [Exp.braces], give braces the location of their body *)
    let loc' =
      match desc' with
      | Pexp_braces {expr} -> expr.pexp_loc
      | _ -> loc'
    in
    if desc' == desc && loc' == loc && attrs' == attrs then e
    else Exp.mk ~loc:loc' ~attrs:attrs' desc'
end

module P = struct
  (* Patterns *)

  let map_desc sub desc =
    match desc with
    | Ppat_any | Ppat_constant _ | Ppat_interval _ -> desc
    | Ppat_var s ->
      let s' = map_loc sub s in
      if s' == s then desc else Ppat_var s'
    | Ppat_alias (p, s) ->
      let s' = map_loc sub s in
      let p' = sub.pat sub p in
      if p' == p && s' == s then desc else Ppat_alias (p', s')
    | Ppat_tuple pl ->
      let pl' = map_list (sub.pat sub) pl in
      if pl' == pl then desc else Ppat_tuple pl'
    | Ppat_construct (l, {txt = args; loc = args_loc}) ->
      let args_loc' = sub.location sub args_loc in
      let args' = map_list (sub.pat sub) args in
      let l' = map_loc sub l in
      if l' == l && args' == args && args_loc' == args_loc then desc
      else Ppat_construct (l', {txt = args'; loc = args_loc'})
    | Ppat_variant (l, {txt = args; loc = args_loc}) ->
      let args_loc' = sub.location sub args_loc in
      let args' = map_list (sub.pat sub) args in
      if args' == args && args_loc' == args_loc then desc
      else Ppat_variant (l, {txt = args'; loc = args_loc'})
    | Ppat_dict entries ->
      let entries' =
        map_list
          (fun entry ->
            let pdp_pattern = sub.pat sub entry.pdp_pattern in
            let pdp_key = map_loc sub entry.pdp_key in
            if pdp_key == entry.pdp_key && pdp_pattern == entry.pdp_pattern then
              entry
            else {entry with pdp_key; pdp_pattern})
          entries
      in
      if entries' == entries then desc else Ppat_dict entries'
    | Ppat_record (lpl, cf, rest) ->
      let lpl' = map_list (E.map_record_element sub sub.pat) lpl in
      let rest' =
        map_opt
          (fun ({rest_loc; rest_name; rest_type} as r) ->
            let rest_type' = map_opt (sub.typ sub) rest_type in
            let rest_name' = map_loc sub rest_name in
            let rest_loc' = sub.location sub rest_loc in
            if
              rest_type' == rest_type && rest_name' == rest_name
              && rest_loc' == rest_loc
            then r
            else
              {
                rest_loc = rest_loc';
                rest_name = rest_name';
                rest_type = rest_type';
              })
          rest
      in
      if lpl' == lpl && rest' == rest then desc
      else Ppat_record (lpl', cf, rest')
    | Ppat_array pl ->
      let pl' = map_list (sub.pat sub) pl in
      if pl' == pl then desc else Ppat_array pl'
    | Ppat_or (p1, p2) ->
      let p2' = sub.pat sub p2 in
      let p1' = sub.pat sub p1 in
      if p1' == p1 && p2' == p2 then desc else Ppat_or (p1', p2')
    | Ppat_constraint (p, t) ->
      let t' = sub.typ sub t in
      let p' = sub.pat sub p in
      if p' == p && t' == t then desc else Ppat_constraint (p', t')
    | Ppat_type s ->
      let s' = map_loc sub s in
      if s' == s then desc else Ppat_type s'
    | Ppat_variant_spread s ->
      let s' = map_loc sub s in
      if s' == s then desc else Ppat_variant_spread s'
    | Ppat_unpack s ->
      let s' = map_loc sub s in
      if s' == s then desc else Ppat_unpack s'
    | Ppat_exception p ->
      let p' = sub.pat sub p in
      if p' == p then desc else Ppat_exception p'
    | Ppat_extension x ->
      let x' = sub.extension sub x in
      if x' == x then desc else Ppat_extension x'

  let map sub ({ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} as p)
      =
    let loc' = sub.location sub loc in
    let attrs' = sub.attributes sub attrs in
    let desc' = map_desc sub desc in
    if desc' == desc && loc' == loc && attrs' == attrs then p
    else Pat.mk ~loc:loc' ~attrs:attrs' desc'
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    structure = (fun this l -> map_list (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> map_list (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    type_declaration = T.map_type_declaration;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this ({pval_name; pval_type; pval_prim; pval_loc} as vd) ->
        let typ = this.typ this pval_type in
        let name = map_loc this pval_name in
        (* The component annotation goes back into the attributes so that a
           mapper sees [@react.component] where it has always been, and
           [Val.mk] takes it out again. *)
        let written_attrs = Val.description_attributes vd in
        let attrs = this.attributes this written_attrs in
        let loc = this.location this pval_loc in
        if
          typ == pval_type && name == pval_name && attrs == written_attrs
          && loc == pval_loc
        then vd
        else Val.mk name typ ~attrs ~loc ?prim:pval_prim);
    pat = P.map;
    expr = E.map;
    module_declaration =
      (fun this ({pmd_name; pmd_type; pmd_attributes; pmd_loc} as md) ->
        let typ = this.module_type this pmd_type in
        let name = map_loc this pmd_name in
        let attrs = this.attributes this pmd_attributes in
        let loc = this.location this pmd_loc in
        if
          typ == pmd_type && name == pmd_name && attrs == pmd_attributes
          && loc == pmd_loc
        then md
        else Md.mk name typ ~attrs ~loc);
    module_type_declaration =
      (fun this ({pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} as mtd) ->
        let name = map_loc this pmtd_name in
        let typ = map_opt (this.module_type this) pmtd_type in
        let attrs = this.attributes this pmtd_attributes in
        let loc = this.location this pmtd_loc in
        if
          name == pmtd_name && typ == pmtd_type && attrs == pmtd_attributes
          && loc == pmtd_loc
        then mtd
        else Mtd.mk name ?typ ~attrs ~loc);
    module_binding =
      (fun this ({pmb_name; pmb_expr; pmb_attributes; pmb_loc} as mb) ->
        let expr = this.module_expr this pmb_expr in
        let name = map_loc this pmb_name in
        let attrs = this.attributes this pmb_attributes in
        let loc = this.location this pmb_loc in
        if
          expr == pmb_expr && name == pmb_name && attrs == pmb_attributes
          && loc == pmb_loc
        then mb
        else Mb.mk name expr ~attrs ~loc);
    open_description =
      (fun this
        ({popen_lid; popen_override; popen_attributes; popen_loc} as od)
      ->
        let lid = map_loc this popen_lid in
        let attrs = this.attributes this popen_attributes in
        let loc = this.location this popen_loc in
        if lid == popen_lid && attrs == popen_attributes && loc == popen_loc
        then od
        else Opn.mk lid ~override:popen_override ~loc ~attrs);
    include_description =
      (fun this ({pincl_mod; pincl_attributes; pincl_loc} as incl) ->
        let mod_ = this.module_type this pincl_mod in
        let attrs = this.attributes this pincl_attributes in
        let loc = this.location this pincl_loc in
        if mod_ == pincl_mod && attrs == pincl_attributes && loc == pincl_loc
        then incl
        else Incl.mk mod_ ~loc ~attrs);
    include_declaration =
      (fun this ({pincl_mod; pincl_attributes; pincl_loc} as incl) ->
        let mod_ = this.module_expr this pincl_mod in
        let attrs = this.attributes this pincl_attributes in
        let loc = this.location this pincl_loc in
        if mod_ == pincl_mod && attrs == pincl_attributes && loc == pincl_loc
        then incl
        else Incl.mk mod_ ~loc ~attrs);
    value_binding =
      (fun this ({pvb_pat; pvb_expr; pvb_constraint; pvb_loc} as vb) ->
        let pat = this.pat this pvb_pat in
        let constraint_ =
          map_opt
            (fun ({pvc_newtypes; pvc_type} as c) ->
              let pvc_type' = this.typ this pvc_type in
              let pvc_newtypes' = map_list (map_loc this) pvc_newtypes in
              if pvc_newtypes' == pvc_newtypes && pvc_type' == pvc_type then c
              else {pvc_newtypes = pvc_newtypes'; pvc_type = pvc_type'})
            pvb_constraint
        in
        let expr = this.expr this pvb_expr in
        (* As for value descriptions, the component annotation goes back into
           the attributes and [Vb.mk] takes it out again. *)
        let written_attrs = Vb.binding_attributes vb in
        let attrs = this.attributes this written_attrs in
        let loc = this.location this pvb_loc in
        if
          pat == pvb_pat
          && constraint_ == pvb_constraint
          && expr == pvb_expr && attrs == written_attrs && loc == pvb_loc
        then vb
        else Vb.mk pat expr ?constraint_ ~loc ~attrs);
    constructor_declaration =
      (fun this ({pcd_name; pcd_args; pcd_res; pcd_loc} as cd) ->
        let name = map_loc this pcd_name in
        let res = map_opt (this.typ this) pcd_res in
        let args = T.map_constructor_arguments this pcd_args in
        let written_attrs = Type.constructor_attributes cd in
        let attrs = this.attributes this written_attrs in
        let loc = this.location this pcd_loc in
        if
          name == pcd_name && res == pcd_res && args == pcd_args
          && attrs == written_attrs && loc == pcd_loc
        then cd
        else Type.constructor name ~args ?res ~loc ~attrs);
    label_declaration =
      (fun this
        ({pld_name; pld_type; pld_loc; pld_mutable; pld_optional} as ld)
      ->
        let typ = this.typ this pld_type in
        let name = map_loc this pld_name in
        (* The runtime name goes back into the attributes so that a mapper sees
           [@as] where it has always been, and [field] takes it out again. *)
        let written_attrs = Type.field_attributes ld in
        let attrs = this.attributes this written_attrs in
        let loc = this.location this pld_loc in
        if
          typ == pld_type && name == pld_name && attrs == written_attrs
          && loc == pld_loc
        then ld
        else
          Type.field name typ ~mut:pld_mutable ~optional:pld_optional ~loc
            ~attrs);
    cases = (fun this l -> map_list (this.case this) l);
    case =
      (fun this ({pc_lhs; pc_guard; pc_rhs} as case) ->
        let rhs = this.expr this pc_rhs in
        let guard = map_opt (this.expr this) pc_guard in
        let lhs = this.pat this pc_lhs in
        if lhs == pc_lhs && guard == pc_guard && rhs == pc_rhs then case
        else {case with pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs});
    location = (fun _this l -> l);
    extension =
      (fun this ((s, e) as ext) ->
        let e' = this.payload this e in
        let s' = map_loc this s in
        if s' == s && e' == e then ext else (s', e'));
    attribute =
      (fun this ((s, e) as attr) ->
        let e' = this.payload this e in
        let s' = map_loc this s in
        if s' == s && e' == e then attr else (s', e'));
    attributes = (fun this l -> map_list (this.attribute this) l);
    payload =
      (fun this payload ->
        match payload with
        | PStr x ->
          let x' = this.structure this x in
          if x' == x then payload else PStr x'
        | PSig x ->
          let x' = this.signature this x in
          if x' == x then payload else PSig x'
        | PTyp x ->
          let x' = this.typ this x in
          if x' == x then payload else PTyp x'
        | PPat (x, g) ->
          let g' = map_opt (this.expr this) g in
          let x' = this.pat this x in
          if x' == x && g' == g then payload else PPat (x', g'));
  }
