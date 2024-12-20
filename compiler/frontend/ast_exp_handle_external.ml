(* Copyright (C) 2020 Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper

(**
   {[
     Js.undefinedToOption 
       (if Js.typeof x = "undefined" then undefined 
        else x  )

   ]}

   @deprecated
*)
let handle_external loc (x : string) : Parsetree.expression =
  let raw_exp : Ast_exp.t =
    let str_exp =
      Ast_compatible.const_exp_string ~loc x ~delimiter:Ext_string.empty
    in
    {
      str_exp with
      pexp_desc =
        Ast_external_mk.local_external_apply loc ~pval_prim:["#raw_expr"]
          ~pval_type:
            (Typ.arrow ~arity:(Some 1) Nolabel (Typ.any ()) (Typ.any ()))
          [str_exp];
    }
  in
  let empty =
    (* FIXME: the empty delimiter does not make sense*)
    Exp.ident ~loc {txt = Ldot (Ldot (Lident "Js", "Undefined"), "empty"); loc}
  in
  let undefined_typeof =
    Exp.ident {loc; txt = Ldot (Lident "Js", "undefinedToOption")}
  in
  let typeof = Exp.ident {loc; txt = Ldot (Lident "Js", "typeof")} in

  Ast_compatible.app1 ~loc undefined_typeof
    (Exp.ifthenelse ~loc
       (Ast_compatible.app2 ~loc
          (Exp.ident ~loc {loc; txt = Ldot (Lident "Pervasives", "=")})
          (Ast_compatible.app1 ~loc typeof raw_exp)
          (Ast_compatible.const_exp_string ~loc "undefined"))
       empty (Some raw_exp))

let handle_debugger loc (payload : Ast_payload.t) =
  match payload with
  | PStr [] ->
    Ast_external_mk.local_external_apply loc ~pval_prim:["%debugger"]
      ~pval_type:
        (Typ.arrow ~arity:(Some 1) Nolabel (Typ.any ())
           (Ast_literal.type_unit ()))
      [Ast_literal.val_unit ~loc ()]
  | _ ->
    Location.raise_errorf ~loc "%%debugger extension doesn't accept arguments"

let handle_raw ~kind loc payload =
  let is_function = ref None in
  match Ast_payload.raw_as_string_exp_exn ~kind ~is_function payload with
  | None -> (
    match kind with
    | Raw_re ->
      Location.raise_errorf ~loc
        "%%re extension can only be applied to a string"
    | Raw_exp ->
      Location.raise_errorf ~loc
        "%%raw extension can only be applied to a string"
    | Raw_program ->
      Location.raise_errorf ~loc
        "%%%%raw extension can only be applied to a string")
  | Some exp ->
    {
      exp with
      pexp_desc =
        Ast_external_mk.local_external_apply loc ~pval_prim:["#raw_expr"]
          ~pval_type:
            (Typ.arrow ~arity:(Some 1) Nolabel (Typ.any ()) (Typ.any ()))
          [exp];
      pexp_attributes =
        (match !is_function with
        | None -> exp.pexp_attributes
        | Some _ -> Ast_attributes.internal_expansive :: exp.pexp_attributes);
    }

let handle_ffi ~loc ~payload =
  let is_function = ref None in
  let err () =
    Location.raise_errorf ~loc
      "%%ffi extension can only be applied to a string containing a JavaScript \
       function such as \"(x) => ...\""
  in
  match
    Ast_payload.raw_as_string_exp_exn ~kind:Raw_exp ~is_function payload
  with
  | None -> err ()
  | Some exp ->
    (* Wrap a type constraint based on arity.
       E.g. for arity 2 constrain to type (_, _) => _ *)
    let wrap_type_constraint (e : Parsetree.expression) =
      let loc = e.pexp_loc in
      let any = Ast_helper.Typ.any ~loc:e.pexp_loc () in
      let unit = Ast_literal.type_unit ~loc () in
      let rec arrow ~arity =
        if arity = 0 then Ast_helper.Typ.arrow ~arity:None ~loc Nolabel unit any
        else if arity = 1 then
          Ast_helper.Typ.arrow ~arity:None ~loc Nolabel any any
        else
          Ast_helper.Typ.arrow ~loc ~arity:None Nolabel any
            (arrow ~arity:(arity - 1))
      in
      match !is_function with
      | Some arity ->
        let type_ =
          Ast_uncurried.uncurried_type ~loc
            ~arity:(if arity = 0 then 1 else arity)
            (arrow ~arity)
        in
        Ast_helper.Exp.constraint_ ~loc e type_
      | _ -> err ()
    in
    wrap_type_constraint
      {
        exp with
        pexp_desc =
          Ast_external_mk.local_external_apply loc ~pval_prim:["#raw_expr"]
            ~pval_type:
              (Typ.arrow ~arity:(Some 1) Nolabel (Typ.any ()) (Typ.any ()))
            [exp];
        pexp_attributes =
          (match !is_function with
          | None -> exp.pexp_attributes
          | Some _ -> Ast_attributes.internal_expansive :: exp.pexp_attributes);
      }

let handle_raw_structure loc payload =
  match Ast_payload.raw_as_string_exp_exn ~kind:Raw_program payload with
  | Some exp ->
    Ast_helper.Str.eval
      {
        exp with
        pexp_desc =
          Ast_external_mk.local_external_apply loc ~pval_prim:["#raw_stmt"]
            ~pval_type:
              (Typ.arrow ~arity:(Some 1) Nolabel (Typ.any ()) (Typ.any ()))
            [exp];
      }
  | None ->
    Location.raise_errorf ~loc
      "%%%%raw extension can only be applied to a string"
