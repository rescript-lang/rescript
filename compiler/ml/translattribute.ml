(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Parsetree.attribute

let is_inline_attribute (attr : t) =
  match attr with
  | {txt = "inline"}, _ -> true
  | _ -> false

let is_inlined_attribute (attr : t) =
  match attr with
  | {txt = "inlined"}, _ -> true
  | _ -> false

let find_attribute p (attributes : t list) =
  let inline_attribute, other_attributes = List.partition p attributes in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | _ :: ({txt; loc}, _) :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      None
  in
  (attr, other_attributes)

let parse_inline_attribute (attr : t option) : Lambda.inline_attribute =
  match attr with
  | None -> Default_inline
  | Some ({txt; loc}, payload) -> (
    let open Parsetree in
    (* the 'inline' and 'inlined' attributes can be used as
       [@inline], [@inline never] or [@inline always].
       [@inline] is equivalent to [@inline always] *)
    let warning txt =
      Warnings.Attribute_payload
        (txt, "It must be either empty, 'always' or 'never'")
    in
    match payload with
    | PStr [] -> Always_inline
    | PStr [{pstr_desc = Pstr_eval ({pexp_desc}, [])}] -> (
      match pexp_desc with
      | Pexp_ident {txt = Longident.Lident "never"} -> Never_inline
      | Pexp_ident {txt = Longident.Lident "always"} -> Always_inline
      | _ ->
        Location.prerr_warning loc (warning txt);
        Default_inline)
    | _ ->
      Location.prerr_warning loc (warning txt);
      Default_inline)

let get_inline_attribute l =
  let attr, _ = find_attribute is_inline_attribute l in
  parse_inline_attribute attr

let rec add_inline_attribute (expr : Lambda.lambda) loc attributes =
  match (expr, get_inline_attribute attributes) with
  | expr, Default_inline -> expr
  | Lfunction ({attr} as funct), inline ->
    (match attr.inline with
    | Default_inline -> ()
    | Always_inline | Never_inline ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute "inline"));
    let attr = {attr with inline} in
    Lfunction {funct with attr}
  | Lprim (((Pjs_fn_make _ | Pjs_fn_make_unit) as p), [e], l), _ ->
    Lambda.Lprim (p, [add_inline_attribute e loc attributes], l)
  | expr, Always_inline ->
    Location.prerr_warning loc (Warnings.Misplaced_attribute "inline1");
    expr
  | expr, Never_inline ->
    Location.prerr_warning loc (Warnings.Misplaced_attribute "inline2");
    expr

(* Get the [@inlined] attribute payload (or default if not present).
   It also returns the expression without this attribute. This is
   used to ensure that this attribute is not misplaced: If it
   appears on any expression, it is an error, otherwise it would
   have been removed by this function *)
let get_and_remove_inlined_attribute (e : Typedtree.expression) =
  let attr, exp_attributes =
    find_attribute is_inlined_attribute e.exp_attributes
  in
  let inlined = parse_inline_attribute attr in
  (inlined, {e with exp_attributes})

let get_and_remove_inlined_attribute_on_module (e : Typedtree.module_expr) =
  let attr, mod_attributes =
    find_attribute is_inlined_attribute e.mod_attributes
  in
  let inlined = parse_inline_attribute attr in
  (inlined, {e with mod_attributes})

let check_attribute (e : Typedtree.expression) (({txt; loc}, _) : t) =
  match txt with
  | "inline" -> (
    match e.exp_desc with
    | Texp_function _ -> ()
    | _ -> Location.prerr_warning loc (Warnings.Misplaced_attribute txt))
  | "inlined" ->
    (* Removed by the Texp_apply cases *)
    Location.prerr_warning loc (Warnings.Misplaced_attribute txt)
  | _ -> ()

let check_attribute_on_module (e : Typedtree.module_expr) (({txt; loc}, _) : t)
    =
  match txt with
  | "inline" -> (
    match e.mod_desc with
    | Tmod_functor _ -> ()
    | _ -> Location.prerr_warning loc (Warnings.Misplaced_attribute txt))
  | "inlined" ->
    (* Removed by the Texp_apply cases *)
    Location.prerr_warning loc (Warnings.Misplaced_attribute txt)
  | _ -> ()
