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

let jsx_prop_loc_attr = "res.jsxPropLoc"
let jsx_spread_loc_attr = "res.jsxSpreadLoc"

let extract_internal_loc_attr attr_name attrs =
  let rec loop rev_acc = function
    | [] -> (None, List.rev rev_acc)
    | (({txt; loc}, payload) as attr) :: rest ->
      if txt = attr_name && payload = PStr [] then
        (Some loc, List.rev_append rev_acc rest)
      else loop (attr :: rev_acc) rest
  in
  loop [] attrs

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
let has_template_attr attrs =
  Ext_list.exists attrs (fun ({txt}, _) -> txt = "res.template")

let remove_template_attr attrs =
  List.filter (fun ({Location.txt}, _) -> txt <> "res.template") attrs

let decode_js_string ~loc s =
  match String_literal.decode_js_escapes s with
  | Some s -> s
  | None -> Location.raise_errorf ~loc "Invalid string escape sequence"

let normalize_ppx_semantic_string semantic =
  let length = String.length semantic in
  let buffer = Buffer.create length in
  let rec loop index =
    if index < length then
      let decoded = String.get_utf_8_uchar semantic index in
      if Uchar.utf_decode_is_valid decoded then (
        let decoded_length = Uchar.utf_decode_length decoded in
        Buffer.add_substring buffer semantic index decoded_length;
        loop (index + decoded_length))
      else (
        (* Before semantic strings were required to be valid UTF-8, the JS
           dumper emitted an invalid byte as [\xHH]. Preserve that runtime
           value by interpreting each such byte as U+00HH. *)
        Buffer.add_string buffer
          (Ext_utf8.encode_codepoint
             (Char.code (String.unsafe_get semantic index)));
        loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

let semantic_string semantic =
  let semantic = normalize_ppx_semantic_string semantic in
  Pt.Pconst_string {source = String_literal.encode_js_string semantic; semantic}

let source_string ~loc source =
  Pt.Pconst_string {source; semantic = decode_js_string ~loc source}

let template_source_from0 = function
  | source, Some ("js" | "*j") -> source
  | semantic, _ ->
    String_literal.encode_js_template (normalize_ppx_semantic_string semantic)

let map_constant ~loc = function
  | Pconst_integer (s, suffix) -> Pt.Pconst_integer (s, suffix)
  | Pconst_char semantic ->
    (* Ast0 stores only the code point, so source spelling cannot survive the
       PPX bridge. Reconstruct a valid canonical spelling on the way back. *)
    Pconst_char {source = String_literal.encode_char_source semantic; semantic}
  | Pconst_string (s, Some ("js" | "*j")) -> source_string ~loc s
  | Pconst_string (s, None) -> semantic_string s
  | Pconst_string (s, Some "json") -> Pconst_json s
  (* Other v0 quotation delimiters are syntax, not part of the string value.
     Tagged ReScript templates are represented as applications before PPX. *)
  | Pconst_string (semantic, Some _) -> semantic_string semantic
  | Pconst_float (s, suffix) -> Pconst_float (s, suffix)

let map_pattern_constant ~loc = function
  | Pconst_string (_, Some tag) when tag <> "js" && tag <> "*j" ->
    Location.raise_errorf ~loc
      "Tagged template literals are not supported in patterns"
  | constant -> map_constant ~loc constant

let is_raw_source_extension = function
  | "raw" | "ffi" | "re" -> true
  | _ -> false

let map_raw_source_payload sub = function
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc = Pexp_constant (Pconst_string (s, _));
                  pexp_loc;
                  pexp_attributes;
                },
                eval_attributes );
          pstr_loc;
        };
      ] ->
    let expression =
      Ast_helper.Exp.constant
        ~loc:(sub.location sub pexp_loc)
        ~attrs:(sub.attributes sub pexp_attributes)
        (Pt.Pconst_raw_source s)
    in
    Some
      (Pt.PStr
         [
           Ast_helper.Str.eval
             ~loc:(sub.location sub pstr_loc)
             ~attrs:(sub.attributes sub eval_attributes)
             expression;
         ])
  | _ -> None

let for_of_attr_name = "_res.for_of"
let for_await_of_attr_name = "_res.for_await_of"

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

(* Internal Parsetree0 bridge metadata; public res.* attributes pass through. *)
let record_rest_attr_name = "_res.record_rest"

let record_rest_of_pattern (rest : Pt.pattern) =
  match rest.Pt.ppat_desc with
  | Pt.Ppat_constraint ({ppat_desc = Pt.Ppat_var rest_name; _}, rest_type) ->
    Some {Pt.rest_loc = rest.ppat_loc; rest_name; rest_type = Some rest_type}
  | Pt.Ppat_var rest_name ->
    Some {Pt.rest_loc = rest.ppat_loc; rest_name; rest_type = None}
  | _ -> None

let get_record_rest_attr attrs_ =
  let rec remove_record_rest_attr acc = function
    | ({Location.txt = attr_name; _}, payload) :: attrs
      when attr_name = record_rest_attr_name -> (
      match payload with
      | Pt.PPat (rest, None) -> (
        match record_rest_of_pattern rest with
        | Some rest -> (Some rest, List.rev_append acc attrs)
        | None -> failwith "Malformed internal _res.record_rest attribute")
      | _ -> failwith "Malformed internal _res.record_rest attribute")
    | attr :: attrs -> remove_record_rest_attr (attr :: acc) attrs
    | [] -> (None, List.rev acc)
  in
  remove_record_rest_attr [] attrs_

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
      (* [Ast_mapper_to0] flattens the current parsetree's node/argument
         attribute split into the v0 arrow's single attribute list, marking
         the boundary with [_res.arrow_node_attrs] when node attributes are
         present: node attributes come before the marker, argument attributes
         after it. Without a marker, everything is an argument attribute. *)
      let node_attrs, arg_attrs =
        let rec split acc = function
          | ({txt = "_res.arrow_node_attrs"}, _) :: rest ->
            Some (List.rev acc, rest)
          | a :: rest -> split (a :: acc) rest
          | [] -> None
        in
        match split [] attrs with
        | Some (node_attrs, arg_attrs) -> (node_attrs, arg_attrs)
        | None -> ([], attrs)
      in
      Typ.arrow ~loc ~attrs:node_attrs
        [{attrs = arg_attrs; lbl; typ = sub.typ sub t1}]
        (sub.typ sub t2)
    | Ptyp_tuple tyl -> Typ.tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) -> (
      let typ0 =
        Typ.constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      in
      match typ0.ptyp_desc with
      | Ptyp_constr (lid, [({ptyp_desc = Ptyp_arrow _} as fun_t); t_arity])
        when lid.txt = Lident "function$" -> (
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
        (* Gather [arity] parameters from the converted chain of unary
           arrows into one n-ary node. Nested first-class function types
           are left intact: gathering stops once [arity] parameters have
           been collected (or the chain runs out, for PPX-mangled input). *)
        let rec gather ~is_head n acc (t : Parsetree.core_type) =
          if n <= 0 then (List.rev acc, t)
          else
            match t.ptyp_desc with
            | Ptyp_arrow {params; ret}
              when List.length params <= n && (is_head || t.ptyp_attributes = [])
              ->
              gather ~is_head:false
                (n - List.length params)
                (List.rev_append params acc)
                ret
            | _ -> (List.rev acc, t)
        in
        match gather ~is_head:true arity [] fun_t with
        | [], _ -> fun_t
        | params, ret -> {fun_t with ptyp_desc = Ptyp_arrow {params; ret}})
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

  let extract_for_of_attribute attrs =
    List.find_map
      (function
        | {Location.txt}, Pt.PPat (_, Some expr) when txt = for_of_attr_name ->
          Some expr
        | _ -> None)
      attrs

  let extract_for_await_of_attribute attrs =
    List.find_map
      (function
        | {Location.txt}, Pt.PPat (_, Some expr)
          when txt = for_await_of_attr_name ->
          Some expr
        | _ -> None)
      attrs

  let remove_for_of_attribute attrs =
    List.filter
      (function
        | {Location.txt}, _ when txt = for_of_attr_name -> false
        | _ -> true)
      attrs

  let remove_for_await_of_attribute attrs =
    List.filter
      (function
        | {Location.txt}, _ when txt = for_await_of_attr_name -> false
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
      visit e
    | _ -> [sub.expr sub e]

  let try_map_jsx_prop (sub : mapper) (lbl : Asttypes.Noloc.arg_label)
      (e : expression) : Parsetree.jsx_prop option =
    let map_expr_with_loc_attr attr_name fallback make_prop =
      let loc, attrs = extract_internal_loc_attr attr_name e.pexp_attributes in
      let e = {e with pexp_attributes = attrs} in
      let expr = sub.expr sub e in
      make_prop
        (match loc with
        | Some loc -> loc
        | None -> fallback expr)
        expr
    in
    match (lbl, e) with
    | Asttypes.Noloc.Labelled "_spreadProps", _expr ->
      Some
        (map_expr_with_loc_attr jsx_spread_loc_attr
           (fun expr -> expr.pexp_loc)
           (fun loc expr -> Parsetree.JSXPropSpreading (loc, expr)))
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
    | Asttypes.Noloc.Labelled name, _exp ->
      Some
        (map_expr_with_loc_attr jsx_prop_loc_attr
           (fun expr -> expr.pexp_loc)
           (fun loc expr ->
             Parsetree.JSXPropValue ({txt = name; loc}, false, expr)))
    | Asttypes.Noloc.Optional name, _exp ->
      Some
        (map_expr_with_loc_attr jsx_prop_loc_attr
           (fun expr -> expr.pexp_loc)
           (fun loc expr ->
             Parsetree.JSXPropValue ({txt = name; loc}, true, expr)))
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
      (* [Ast_mapper_to0] merges the await node's attributes and the inner
         expression's attributes into the one v0 slot, with [res.await] as
         the boundary: await-node attributes before it, inner attributes
         after it. *)
      let await_attrs0, inner_attrs0 =
        let rec split acc = function
          | ({Location.txt = "res.await"}, _) :: rest -> (List.rev acc, rest)
          | a :: rest -> split (a :: acc) rest
          | [] -> (List.rev acc, [])
        in
        split [] e.pexp_attributes
      in
      let inner = sub.expr sub {e with pexp_attributes = inner_attrs0} in
      await ~loc ~attrs:(sub.attributes sub await_attrs0) inner
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant (Pconst_string (text, delimiter))
      when has_template_attr attrs && delimiter <> Some "json" ->
      let attrs = remove_template_attr attrs in
      let source = template_source_from0 (text, delimiter) in
      template ~loc ~attrs [{txt = source; loc}] []
    | Pexp_constant x ->
      let template = has_template_attr attrs in
      let attrs = if template then remove_template_attr attrs else attrs in
      constant ~loc ~attrs (map_constant ~loc x)
    | Pexp_let (r, vbs, e) ->
      let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs) (sub.expr sub e)
    | Pexp_fun (lab, def, p, e) ->
      (* A bare (non-Function$-wrapped) v0 fun becomes a one-parameter
         function; [Function$] decoding below gathers chains of these into
         one n-ary node.

         [Ast_mapper_to0] flattens the current parsetree's node/parameter
         attribute split into the v0 fun's single attribute list, marking the
         boundary with [_res.fun_node_attrs] when parameter attributes are
         present: node attributes come before the marker, parameter
         attributes after it. Without a marker, everything is a node
         attribute (that is where the old parser kept them, and where the
         built-in PPX looks for decorators like [@this]). *)
      let lab = Asttypes.to_arg_label lab in
      let async = Ext_list.exists attrs (fun ({txt}, _) -> txt = "res.async") in
      (* [res.async] is bridge metadata added by [Ast_mapper_to0]; it is
         decoded into the [async] flag and must not survive as a real
         attribute. *)
      let attrs = attrs |> List.filter (fun ({txt}, _) -> txt <> "res.async") in
      let node_attrs, param_attrs =
        let rec split acc = function
          | ({txt = "_res.fun_node_attrs"}, _) :: rest ->
            Some (List.rev acc, rest)
          | a :: rest -> split (a :: acc) rest
          | [] -> None
        in
        match split [] attrs with
        | Some (node_attrs, param_attrs) -> (node_attrs, param_attrs)
        | None -> (attrs, [])
      in
      fun_ ~loc ~async ~attrs:node_attrs
        [
          {
            p_attrs = param_attrs;
            p_lbl = lab;
            p_default = map_opt (sub.expr sub) def;
            p_pat = sub.pat sub p;
          };
        ]
        (sub.expr sub e)
    | Pexp_function cases ->
      (* The current parsetree has no [function] construct; it can only come
         from an external PPX emitting OCaml-style [function | p -> e].
         Desugar to [fun x -> match x with | p -> e] with an unshadowable
         parameter name, as the OCaml parser would. *)
      let param = "*function*" in
      let pat = Pat.var ~loc (Location.mkloc param loc) in
      let scrutinee =
        ident ~loc (Location.mkloc (Longident.Lident param) loc)
      in
      let body = match_ ~loc scrutinee (sub.cases sub cases) in
      fun_ ~loc ~attrs
        [{p_attrs = []; p_lbl = Nolabel; p_default = None; p_pat = pat}]
        body
    | Pexp_apply
        ( {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
          [
            (Asttypes.Noloc.Nolabel, {pexp_desc = Pexp_send (e, s)});
            (Asttypes.Noloc.Nolabel, v);
          ] ) ->
      (* Decode the v0 encoding of property assignment. *)
      object_set ~loc ~attrs (sub.expr sub e) (map_loc sub s) (sub.expr sub v)
    | Pexp_apply ({pexp_desc = Pexp_ident tag_name}, args)
      when has_jsx_attribute () -> (
      let attrs = attrs |> List.filter (fun ({txt}, _) -> txt <> "JSX") in
      let props, children = extract_props_and_children sub args in
      let jsx_tag : Pt.jsx_tag_name =
        match tag_name.txt with
        | Longident.Lident s
          when String.length s > 0 && Char.lowercase_ascii s.[0] = s.[0] ->
          Pt.JsxLowerTag s
        | Longident.Lident _ -> Pt.JsxUpperTag tag_name.txt
        | Longident.Ldot (path, last)
          when String.length last > 0
               && Char.lowercase_ascii last.[0] = last.[0] ->
          Pt.JsxQualifiedLowerTag {path; name = last}
        | _ -> Pt.JsxUpperTag tag_name.txt
      in
      let jsx_tag_name = {txt = jsx_tag; loc = tag_name.loc} in
      match children with
      | None -> jsx_unary_element ~loc ~attrs jsx_tag_name props
      | Some children ->
        (* The v0 encoding has no closing-tag information; synthesize one
           matching the opening tag, otherwise the printer emits an element
           that is never closed. *)
        let closing_tag =
          {
            Pt.jsx_closing_container_tag_start = Lexing.dummy_pos;
            jsx_closing_container_tag_name = jsx_tag_name;
            jsx_closing_container_tag_end = Lexing.dummy_pos;
          }
        in
        jsx_container_element ~loc ~attrs jsx_tag_name props Lexing.dummy_pos
          children (Some closing_tag))
    | Pexp_apply
        ( tag,
          [
            (Nolabel, {pexp_desc = Pexp_array segments});
            (Nolabel, {pexp_desc = Pexp_array values});
          ] )
      when List.exists
             (fun ({Location.txt}, _) -> txt = "res.taggedTemplate")
             attrs ->
      let raw_sources =
        List.map
          (fun (segment : Parsetree0.expression) ->
            match segment.pexp_desc with
            | Pexp_constant (Pconst_string (txt, Some ("js" | "*j"))) ->
              {Location.txt; loc = sub.location sub segment.pexp_loc}
            | Pexp_constant (Pconst_string (semantic, _)) ->
              {
                Location.txt =
                  String_literal.encode_js_template
                    (normalize_ppx_semantic_string semantic);
                loc = sub.location sub segment.pexp_loc;
              }
            | _ -> assert false)
          segments
      in
      let attrs =
        List.filter
          (fun ({Location.txt}, _) -> txt <> "res.taggedTemplate")
          attrs
      in
      tagged_template ~loc ~attrs (sub.expr sub tag) raw_sources
        (List.map (sub.expr sub) values)
    | Pexp_apply _ as application when has_template_attr attrs ->
      let rec flatten acc (expression : Parsetree0.expression) =
        match expression.pexp_desc with
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
              [(Nolabel, lhs); (Nolabel, rhs)] )
          when has_template_attr expression.pexp_attributes ->
          flatten (rhs :: acc) lhs
        | _ -> expression :: acc
      in
      let parts = flatten [] {e with pexp_desc = application} in
      let reject_json_interpolation () =
        Location.raise_errorf ~loc
          "`json` literals do not support interpolation"
      in
      let rec collect sources values = function
        | [{pexp_desc = Pexp_constant (Pconst_string (_, Some "json"))}] ->
          reject_json_interpolation ()
        | [
            {
              pexp_desc = Pexp_constant (Pconst_string (text, delimiter));
              pexp_loc;
            };
          ] ->
          let txt = template_source_from0 (text, delimiter) in
          Some
            ( List.rev
                ({Location.txt; loc = sub.location sub pexp_loc} :: sources),
              List.rev values )
        | {pexp_desc = Pexp_constant (Pconst_string (_, Some "json"))}
          :: _value :: _rest ->
          reject_json_interpolation ()
        | {
            pexp_desc = Pexp_constant (Pconst_string (text, delimiter));
            pexp_loc;
          }
          :: value :: rest ->
          let txt = template_source_from0 (text, delimiter) in
          collect
            ({Location.txt; loc = sub.location sub pexp_loc} :: sources)
            (value :: values) rest
        | _ -> None
      in
      begin match collect [] [] parts with
      | Some (sources, values) ->
        let attrs = remove_template_attr attrs in
        template ~loc ~attrs sources (List.map (sub.expr sub) values)
      | None ->
        let attrs = remove_template_attr attrs in
        begin match application with
        | Pexp_apply (e, l) ->
          let e =
            match (e.pexp_desc, l) with
            | ( Pexp_ident ({txt = Longident.Lident "^"} as lid),
                [(Nolabel, _); (Nolabel, _)] ) ->
              {
                e with
                pexp_desc = Pexp_ident {lid with txt = Longident.Lident "++"};
              }
            | _ -> e
          in
          apply ~loc ~attrs (sub.expr sub e)
            (List.map
               (fun (lbl, e) -> (Asttypes.to_arg_label lbl, sub.expr sub e))
               l)
        | _ -> assert false
        end
      end
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
        | Some ({pexp_desc = Pexp_fun f} as e1) -> (
          let arity = attributes_to_arity attrs in
          (* Gather [arity] parameters from the converted chain of unary
             functions into one n-ary node. Nested first-class functions are
             left intact: gathering stops once [arity] parameters have been
             collected (or the chain shape breaks, for PPX-mangled input). *)
          let rec gather ~is_head n acc (e : Parsetree.expression) =
            if n <= 0 then (List.rev acc, e)
            else
              match e.pexp_desc with
              | Pexp_fun {params; body; async = inner_async}
                when List.length params <= n
                     && (is_head || (e.pexp_attributes = [] && not inner_async))
                ->
                gather ~is_head:false
                  (n - List.length params)
                  (List.rev_append params acc)
                  body
              | _ -> (List.rev acc, e)
          in
          match gather ~is_head:true arity [] e1 with
          | [], _ -> e1
          | params, body ->
            (* The construct node's other attributes become the function
               node's attributes rather than being dropped. *)
            let node_attrs =
              attrs |> List.filter (fun ({txt}, _) -> txt <> "res.arity")
            in
            {
              e1 with
              pexp_desc =
                Pexp_fun {newtypes = []; params; body; async = f.async};
              pexp_attributes = e1.pexp_attributes @ node_attrs;
            })
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
    | Pexp_extension ({txt = "res.break"; _}, PStr []) -> break ~loc ~attrs ()
    | Pexp_extension ({txt = "res.continue"; _}, PStr []) ->
      continue ~loc ~attrs ()
    | Pexp_while (e1, e2) ->
      while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) -> (
      let async_iterable_expr = extract_for_await_of_attribute attrs in
      let array_expr = extract_for_of_attribute attrs in
      let attrs =
        remove_for_await_of_attribute (remove_for_of_attribute attrs)
      in
      match (async_iterable_expr, array_expr) with
      | Some iterable, _ ->
        for_await_of ~loc ~attrs (sub.pat sub p) iterable (sub.expr sub e3)
      | None, Some array ->
        (* This is actually a for...of loop, decode it *)
        for_of ~loc ~attrs (sub.pat sub p) array (sub.expr sub e3)
      | None, None ->
        (* Regular for loop *)
        for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
          (sub.expr sub e3))
    | Pexp_coerce (e, (), t2) ->
      coerce ~loc ~attrs (sub.expr sub e) (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
      constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
    | Pexp_send (e, s) ->
      object_get ~loc ~attrs (sub.expr sub e) (map_loc sub s)
    | Pexp_extension
        ( {txt = "obj"},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval ({pexp_desc = Pexp_record (rows, None)}, []);
              };
            ] )
      when List.for_all
             (fun ((lid : Longident.t Location.loc), _) ->
               match lid.txt with
               | Longident.Lident _ -> true
               | _ -> false)
             rows ->
      (* Decode the reserved v0 %obj encoding of object literals. *)
      object_literal ~loc ~attrs
        (List.map
           (fun ((lid : Longident.t Location.loc), e) ->
             let name =
               match lid.txt with
               | Longident.Lident name -> name
               | _ -> assert false
             in
             ({txt = name; loc = lid.loc}, sub.expr sub e))
           rows)
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
    | Pexp_newtype (s, e) -> (
      (* Fuse a chain of newtype wrappers over a Function$ node into the
         function's [newtypes] field. Each wrapper's attributes are its
         newtype's attributes, except on this outermost wrapper:
         attributes before the internal [_res.newtype_attrs] marker (or
         all of them, when there is no marker) are function-node
         attributes, and those after the marker belong to the first
         newtype. *)
      let node_attrs, first_nt_attrs =
        let rec split acc = function
          | ({txt = "_res.newtype_attrs"}, _) :: rest -> (List.rev acc, rest)
          | a :: rest -> split (a :: acc) rest
          | [] -> (List.rev acc, [])
        in
        split [] attrs
      in
      let rec gather acc (e0 : Parsetree0.expression) =
        match e0.pexp_desc with
        | Pexp_newtype (s1, body) ->
          gather
            ((map_loc sub s1, sub.attributes sub e0.pexp_attributes) :: acc)
            body
        | Pexp_construct ({txt = Longident.Lident "Function$"}, Some _) ->
          Some (List.rev acc, e0)
        | _ -> None
      in
      let unsupported () =
        extension ~loc ~attrs
          (Ast_mapper.extension_of_error
             (Location.errorf ~loc
                "A PPX returned a locally abstract type wrapper that does not \
                 enclose a ReScript function. This v0 AST form is not \
                 supported."))
      in
      match gather [(map_loc sub s, first_nt_attrs)] e with
      | Some (newtypes, base) -> (
        let base1 = sub.expr sub base in
        match base1.pexp_desc with
        | Pexp_fun ({newtypes = []} as f) ->
          {
            Pt.pexp_desc = Pexp_fun {f with newtypes};
            pexp_attributes = base1.pexp_attributes @ node_attrs;
            pexp_loc = loc;
          }
        | _ -> unsupported ())
      | None -> unsupported ())
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
    | Ppat_constant c ->
      let template = has_template_attr attrs in
      let attrs = if template then remove_template_attr attrs else attrs in
      constant ~loc ~attrs (map_pattern_constant ~loc c)
    | Ppat_interval (c1, c2) ->
      interval ~loc ~attrs
        (map_pattern_constant ~loc c1)
        (map_pattern_constant ~loc c2)
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
      construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
      let rest, attrs = get_record_rest_attr attrs in
      record ~loc ~attrs ?rest
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
        let prim =
          match pval_prim with
          | [] -> None
          | [s] -> Some (Parsetree.Prim_name s)
          | _ :: _ :: _ ->
            Location.raise_errorf ~loc:pval_loc
              "An external declaration can carry only a single primitive string"
        in
        Val.mk (map_loc this pval_name) (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ?prim);
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
        let decoded =
          match pvb_pat with
          | {
           ppat_desc =
             Ppat_constraint
               ( pat,
                 {
                   ptyp_desc = Ptyp_poly (poly_newtypes, poly_type);
                   ptyp_attributes = [];
                 } );
           ppat_attributes = [];
          }
            when poly_newtypes <> [] -> (
            let rec gather_newtypes acc (expr : Parsetree0.expression) =
              match expr with
              | {pexp_desc = Pexp_newtype (newtype, rest); pexp_attributes = []}
                ->
                gather_newtypes (newtype :: acc) rest
              | {pexp_desc = Pexp_constraint (expr, typ); pexp_attributes = []}
                ->
                Some (List.rev acc, expr, typ)
              | _ -> None
            in
            match gather_newtypes [] pvb_expr with
            | Some (newtypes, expr, typ)
              when List.map (fun {txt} -> txt) newtypes
                   = List.map (fun {txt} -> txt) poly_newtypes
                   &&
                     try
                       Ast_helper0.Typ.varify_constructors newtypes typ
                       = poly_type
                     with Syntaxerr.Error _ -> false ->
              Some (pat, expr, newtypes, typ)
            | _ -> None)
          | _ -> None
        in
        match decoded with
        | Some (pat, expr, newtypes, typ) ->
          let constraint_ =
            {
              Pt.pvc_newtypes = List.map (map_loc this) newtypes;
              pvc_type = this.typ this typ;
            }
          in
          Vb.mk (this.pat this pat) (this.expr this expr) ~constraint_
            ~loc:(this.location this pvb_loc)
            ~attrs:(this.attributes this pvb_attributes)
        | None ->
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
    extension =
      (fun this (s, payload) ->
        let payload =
          if is_raw_source_extension s.txt then
            match map_raw_source_payload this payload with
            | Some payload -> payload
            | None -> this.payload this payload
          else this.payload this payload
        in
        (map_loc this s, payload));
    attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attributes = (fun this l -> List.map (this.attribute this) l);
    payload =
      (fun this -> function
        | PStr x -> PStr (this.structure this x)
        | PSig x -> PSig (this.signature this x)
        | PTyp x -> PTyp (this.typ this x)
        | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g));
  }
