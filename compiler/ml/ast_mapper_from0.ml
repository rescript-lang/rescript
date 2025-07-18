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

open Parsetree0
open Ast_helper
open Location
module Pt = Parsetree

type mapper = {
  attribute: mapper -> attribute -> Pt.attribute;
  attributes: mapper -> attribute list -> Pt.attribute list;
  case: mapper -> case -> Pt.case;
  cases: mapper -> case list -> Pt.case list;
  constructor_declaration:
    mapper -> constructor_declaration -> Pt.constructor_declaration;
  expr: mapper -> expression -> Pt.expression;
  extension: mapper -> extension -> Pt.extension;
  extension_constructor:
    mapper -> extension_constructor -> Pt.extension_constructor;
  include_declaration: mapper -> include_declaration -> Pt.include_declaration;
  include_description: mapper -> include_description -> Pt.include_description;
  label_declaration: mapper -> label_declaration -> Pt.label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> Pt.module_binding;
  module_declaration: mapper -> module_declaration -> Pt.module_declaration;
  module_expr: mapper -> module_expr -> Pt.module_expr;
  module_type: mapper -> module_type -> Pt.module_type;
  module_type_declaration:
    mapper -> module_type_declaration -> Pt.module_type_declaration;
  open_description: mapper -> open_description -> Pt.open_description;
  pat: mapper -> pattern -> Pt.pattern;
  payload: mapper -> payload -> Pt.payload;
  signature: mapper -> signature -> Pt.signature;
  signature_item: mapper -> signature_item -> Pt.signature_item;
  structure: mapper -> structure -> Pt.structure;
  structure_item: mapper -> structure_item -> Pt.structure_item;
  typ: mapper -> core_type -> Pt.core_type;
  type_declaration: mapper -> type_declaration -> Pt.type_declaration;
  type_extension: mapper -> type_extension -> Pt.type_extension;
  type_kind: mapper -> type_kind -> Pt.type_kind;
  value_binding: mapper -> value_binding -> Pt.value_binding;
  value_description: mapper -> value_description -> Pt.value_description;
  with_constraint: mapper -> with_constraint -> Pt.with_constraint;
}

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function
  | None -> None
  | Some x -> Some (f x)
let map_constant = function
  | Pconst_integer (s, suffix) -> Pt.Pconst_integer (s, suffix)
  | Pconst_char c -> Pconst_char c
  | Pconst_string (s, q) -> Pconst_string (s, q)
  | Pconst_float (s, suffix) -> Pconst_float (s, suffix)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, attrs, b, tl) ->
      Pt.Rtag
        (map_loc sub l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
    | Rinherit t -> Rinherit (sub.typ sub t)

  let object_field sub = function
    | Otag (l, attrs, t) ->
      Pt.Otag (map_loc sub l, sub.attributes sub attrs, sub.typ sub t)
    | Oinherit t -> Oinherit (sub.typ sub t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> Typ.any ~loc ~attrs ()
    | Ptyp_var s -> Typ.var ~loc ~attrs s
    | Ptyp_arrow (lbl, t1, t2) ->
      let lbl = Asttypes.to_arg_label lbl in
      Typ.arrow ~loc ~arity:None
        {attrs; lbl; typ = sub.typ sub t1}
        (sub.typ sub t2)
    | Ptyp_tuple tyl -> Typ.tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) -> (
      let typ0 =
        Typ.constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      in
      match typ0.ptyp_desc with
      | Ptyp_constr (lid, [({ptyp_desc = Ptyp_arrow arr} as fun_t); t_arity])
        when lid.txt = Lident "function$" ->
        let decode_arity_string arity_s =
          int_of_string
            ((String.sub [@doesNotRaise]) arity_s 9 (String.length arity_s - 9))
        in
        let arity_from_type (typ : Parsetree.core_type) =
          match typ.ptyp_desc with
          | Ptyp_variant ([Rtag ({txt}, _, _, _)], _, _) ->
            decode_arity_string txt
          | _ -> assert false
        in
        let arity = arity_from_type t_arity in
        {fun_t with ptyp_desc = Ptyp_arrow {arr with arity = Some arity}}
      | _ -> typ0)
    | Ptyp_object (l, o) ->
      Typ.object_ ~loc ~attrs (List.map (object_field sub) l) o
    | Ptyp_class () -> assert false
    | Ptyp_alias (t, s) -> Typ.alias ~loc ~attrs (sub.typ sub t) s
    | Ptyp_variant (rl, b, ll) ->
      Typ.variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) ->
      Typ.poly ~loc ~attrs (List.map (map_loc sub) sl) (sub.typ sub t)
    | Ptyp_package (lid, l) ->
      Typ.package ~loc ~attrs (map_loc sub lid)
        (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
    | Ptyp_extension x -> Typ.extension ~loc ~attrs (sub.extension sub x)

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

  let map_type_kind sub = function
    | Ptype_abstract -> Pt.Ptype_abstract
    | Ptype_variant l ->
      Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_constructor_arguments sub = function
    | Pcstr_tuple l -> Pt.Pcstr_tuple (List.map (sub.typ sub) l)
    | Pcstr_record l -> Pt.Pcstr_record (List.map (sub.label_declaration sub) l)

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
      Pt.Pext_decl (map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
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
      Pt.Pwith_type (map_loc sub lid, sub.type_declaration sub d)
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
    | Psig_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_recmodule l ->
      rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_class _ -> assert false
    | Psig_class_type _ -> assert false
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
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_description sub x)
    | Pstr_class () -> failwith "Pstr_class is no longer present in ReScript"
    | Pstr_class_type () ->
      failwith "Pstr_class_type is no longer present in ReScript"
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
      extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let has_await_attribute attrs =
    List.exists
      (function
        | {Location.txt = "res.await"}, _ -> true
        | _ -> false)
      attrs

  let remove_await_attribute attrs =
    List.filter
      (function
        | {Location.txt = "res.await"}, _ -> false
        | _ -> true)
      attrs

  let map_jsx_children sub (e : expression) : Pt.jsx_children =
    let rec visit (e : expression) : Pt.expression list =
      match e.pexp_desc with
      | Pexp_construct
          ({txt = Longident.Lident "::"}, Some {pexp_desc = Pexp_tuple [e1; e2]})
        ->
        sub.expr sub e1 :: visit e2
      | Pexp_construct ({txt = Longident.Lident "[]"}, ext_opt) -> (
        match ext_opt with
        | None -> []
        | Some e -> visit e)
      | _ -> [sub.expr sub e]
    in
    match e.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident "[]" | Longident.Lident "::"}, _)
      ->
      JSXChildrenItems (visit e)
    | _ -> JSXChildrenSpreading (sub.expr sub e)

  let try_map_jsx_prop (sub : mapper) (lbl : Asttypes.Noloc.arg_label)
      (e : expression) : Parsetree.jsx_prop option =
    match (lbl, e) with
    | Asttypes.Noloc.Labelled "_spreadProps", expr ->
      Some (Parsetree.JSXPropSpreading (Location.none, sub.expr sub expr))
    | ( Asttypes.Noloc.Labelled name,
        {pexp_desc = Pexp_ident {txt = Longident.Lident v}; pexp_loc = name_loc}
      )
      when name = v ->
      Some (Parsetree.JSXPropPunning (false, {txt = name; loc = name_loc}))
    | ( Asttypes.Noloc.Optional name,
        {pexp_desc = Pexp_ident {txt = Longident.Lident v}; pexp_loc = name_loc}
      )
      when name = v ->
      Some (Parsetree.JSXPropPunning (true, {txt = name; loc = name_loc}))
    | Asttypes.Noloc.Labelled name, exp ->
      Some
        (Parsetree.JSXPropValue
           ({txt = name; loc = Location.none}, false, sub.expr sub exp))
    | Asttypes.Noloc.Optional name, exp ->
      Some
        (Parsetree.JSXPropValue
           ({txt = name; loc = Location.none}, true, sub.expr sub exp))
    | _ -> None

  let extract_props_and_children (sub : mapper) items =
    let rec visit props items =
      match items with
      | [] | [_] -> (List.rev props, None)
      | [(Asttypes.Noloc.Labelled "children", children_expr); _] ->
        (List.rev props, Some (map_jsx_children sub children_expr))
      | (lbl, e) :: rest -> (
        match try_map_jsx_prop sub lbl e with
        | Some prop -> visit (prop :: props) rest
        | None -> visit props rest)
    in
    let props, children = visit [] items in
    (props, children)

  let map sub e =
    let {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} = e in
    let open Exp in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    let has_jsx_attribute () =
      attrs |> List.exists (fun ({txt}, _) -> txt = "JSX")
    in
    match desc with
    | _ when has_await_attribute attrs ->
      let attrs = remove_await_attribute e.pexp_attributes in
      let e = sub.expr sub {e with pexp_attributes = attrs} in
      await ~loc e
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs (map_constant x)
    | Pexp_let (r, vbs, e) ->
      let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs) (sub.expr sub e)
    | Pexp_fun (lab, def, p, e) ->
      let lab = Asttypes.to_arg_label lab in
      let async = Ext_list.exists attrs (fun ({txt}, _) -> txt = "res.async") in
      fun_ ~loc ~attrs ~async ~arity:None lab
        (map_opt (sub.expr sub) def)
        (sub.pat sub p) (sub.expr sub e)
    | Pexp_function _ -> assert false
    | Pexp_apply ({pexp_desc = Pexp_ident tag_name}, args)
      when has_jsx_attribute () -> (
      let attrs = attrs |> List.filter (fun ({txt}, _) -> txt <> "JSX") in
      let props, children = extract_props_and_children sub args in
      match children with
      | None -> jsx_unary_element ~loc ~attrs tag_name props
      | Some children ->
        jsx_container_element ~loc ~attrs tag_name props Lexing.dummy_pos
          children None)
    | Pexp_apply (e, l) ->
      let e =
        match (e.pexp_desc, l) with
        | ( Pexp_ident ({txt = Longident.Lident "|."} as lid),
            [(Nolabel, _); (Nolabel, _)] ) ->
          {e with pexp_desc = Pexp_ident {lid with txt = Longident.Lident "->"}}
        | ( Pexp_ident ({txt = Longident.Lident "^"} as lid),
            [(Nolabel, _); (Nolabel, _)] ) ->
          {e with pexp_desc = Pexp_ident {lid with txt = Longident.Lident "++"}}
        | ( Pexp_ident ({txt = Longident.Lident "<>"} as lid),
            [(Nolabel, _); (Nolabel, _)] ) ->
          {e with pexp_desc = Pexp_ident {lid with txt = Longident.Lident "!="}}
        | ( Pexp_ident ({txt = Longident.Lident "!="} as lid),
            [(Nolabel, _); (Nolabel, _)] ) ->
          {
            e with
            pexp_desc = Pexp_ident {lid with txt = Longident.Lident "!=="};
          }
        | ( Pexp_ident ({txt = Longident.Lident "="} as lid),
            [(Nolabel, _); (Nolabel, _)] ) ->
          {e with pexp_desc = Pexp_ident {lid with txt = Longident.Lident "=="}}
        | ( Pexp_ident ({txt = Longident.Lident "=="} as lid),
            [(Nolabel, _); (Nolabel, _)] ) ->
          {
            e with
            pexp_desc = Pexp_ident {lid with txt = Longident.Lident "==="};
          }
        | _ -> e
      in
      let process_partial_app_attribute attrs =
        let rec process partial_app acc attrs =
          match attrs with
          | [] -> (partial_app, List.rev acc)
          | ({Location.txt = "res.partial"}, _) :: rest -> process true acc rest
          | attr :: rest -> process partial_app (attr :: acc) rest
        in
        process false [] attrs
      in
      let partial, attrs = process_partial_app_attribute attrs in
      apply ~loc ~attrs ~partial (sub.expr sub e)
        (List.map
           (fun (lbl, e) -> (Asttypes.to_arg_label lbl, sub.expr sub e))
           l)
    | Pexp_match (e, pel) ->
      match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
    (* <></> *)
    | Pexp_construct ({txt = Longident.Lident "[]" | Longident.Lident "::"}, _)
      when has_jsx_attribute () ->
      let attrs = attrs |> List.filter (fun ({txt}, _) -> txt <> "JSX") in
      jsx_fragment ~loc ~attrs loc.loc_start (map_jsx_children sub e)
        loc.loc_end
    | Pexp_construct (lid, arg) -> (
      let lid1 = map_loc sub lid in
      let arg1 = map_opt (sub.expr sub) arg in
      let exp1 = construct ~loc ~attrs lid1 arg1 in
      match lid.txt with
      | Lident "Function$" -> (
        let rec attributes_to_arity (attrs : Parsetree.attributes) =
          match attrs with
          | ( {txt = "res.arity"},
              PStr
                [
                  {
                    pstr_desc =
                      Pstr_eval
                        ( {pexp_desc = Pexp_constant (Pconst_integer (arity, _))},
                          _ );
                  };
                ] )
            :: _ ->
            int_of_string arity
          | _ :: rest -> attributes_to_arity rest
          | [] -> assert false
        in
        match arg1 with
        | Some ({pexp_desc = Pexp_fun f} as e1) ->
          let arity = Some (attributes_to_arity attrs) in
          {e1 with pexp_desc = Pexp_fun {f with arity}}
        | _ -> exp1)
      | _ -> exp1)
    | Pexp_variant (lab, eo) ->
      variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
      record ~loc ~attrs
        (Ext_list.map l (fun (lid, e) ->
             let lid1 = map_loc sub lid in
             let e1 = sub.expr sub e in
             let optional, attrs =
               Parsetree0.get_optional_attr e1.pexp_attributes
             in
             {
               Pt.lid = lid1;
               x = {e1 with pexp_attributes = attrs};
               opt = optional;
             }))
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
    | Pexp_new _ -> failwith "Pexp_new is no longer present in ReScript"
    | Pexp_setinstvar _ ->
      failwith "Pexp_setinstvar is no longer present in ReScript"
    | Pexp_override _ ->
      failwith "Pexp_override is no longer present in ReScript"
    | Pexp_letmodule (s, me, e) ->
      letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
        (sub.expr sub e)
    | Pexp_letexception (cd, e) ->
      letexception ~loc ~attrs
        (sub.extension_constructor sub cd)
        (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy _ -> failwith "Pexp_lazy is no longer present in ReScript"
    | Pexp_poly _ -> failwith "Pexp_poly is no longer present in ReScript"
    | Pexp_object () -> assert false
    | Pexp_newtype (s, e) ->
      newtype ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
    | Pexp_open (ovf, lid, e) ->
      open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pexp_unreachable -> assert false
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
    | Ppat_constant c -> constant ~loc ~attrs (map_constant c)
    | Ppat_interval (c1, c2) ->
      interval ~loc ~attrs (map_constant c1) (map_constant c2)
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
      construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
      record ~loc ~attrs
        (Ext_list.map lpl (fun (lid, p) ->
             let lid1 = map_loc sub lid in
             let p1 = sub.pat sub p in
             let optional, attrs =
               Parsetree0.get_optional_attr p1.ppat_attributes
             in
             {
               Pt.lid = lid1;
               x = {p1 with ppat_attributes = attrs};
               opt = optional;
             }))
        cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
    | Ppat_constraint (p, t) ->
      constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy _ -> failwith "Ppat_lazy is no longer present in ReScript"
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
    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
        Type.constructor (map_loc this pcd_name)
          ~args:(T.map_constructor_arguments this pcd_args)
          ?res:(map_opt (this.typ this) pcd_res)
          ~loc:(this.location this pcd_loc)
          ~attrs:(this.attributes this pcd_attributes));
    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
        let optional, attrs =
          Parsetree0.get_optional_attr (this.attributes this pld_attributes)
        in
        Type.field (map_loc this pld_name) (this.typ this pld_type)
          ~mut:pld_mutable ~optional
          ~loc:(this.location this pld_loc)
          ~attrs);
    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
        {
          pc_bar = None;
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
