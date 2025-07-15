(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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
module U = Ast_derive_util

type tdcls = Parsetree.type_declaration list

let js_field (o : Parsetree.expression) m =
  Ast_compatible.app2
    (Exp.ident {txt = Lident "##"; loc = o.pexp_loc})
    o (Exp.ident m)

let handle_config (config : Parsetree.expression option) =
  match config with
  | Some config -> (
    match config.pexp_desc with
    | Pexp_record
        ( [
            {
              lid = {txt = Lident "newType"};
              x =
                {
                  pexp_desc =
                    ( Pexp_construct
                        ({txt = Lident (("true" | "false") as x)}, None)
                    | Pexp_ident {txt = Lident ("newType" as x)} );
                };
            };
          ],
          None ) ->
      not (x = "false")
    | Pexp_ident {txt = Lident "newType"} -> true
    | _ -> U.invalid_config config)
  | None -> false

let noloc = Location.none

(* [eraseType] will be instrumented, be careful about the name conflict*)
let erase_type_lit = "_eraseType"

let erase_type_exp = Exp.ident {loc = noloc; txt = Lident erase_type_lit}

let erase_type x = Ast_compatible.app1 erase_type_exp x

let erase_type_str =
  let any = Typ.any () in
  Str.primitive
    (Val.mk ~prim:["%identity"]
       {loc = noloc; txt = erase_type_lit}
       (Ast_helper.Typ.arrow ~arity:(Some 1) {lbl = Nolabel; typ = any} any))

let unsafe_index = "_index"

let unsafe_index_get =
  let any = Typ.any () in
  Str.primitive
    (Val.mk ~prim:[""]
       {loc = noloc; txt = unsafe_index}
       ~attrs:[Ast_attributes.get_index]
       (Ast_helper.Typ.arrow ~arity:None {lbl = Nolabel; typ = any}
          (Ast_helper.Typ.arrow ~arity:None {lbl = Nolabel; typ = any} any)))

let unsafe_index_get_exp = Exp.ident {loc = noloc; txt = Lident unsafe_index}

(* JavaScript has allowed trailing commas in array literals since the beginning,
   and later added them to object literals (ECMAScript 5) and most recently (ECMAScript 2017)
   to function parameters. *)
let add_key_value buf key value last =
  Ext_buffer.add_char_string buf '"' key;
  Ext_buffer.add_string buf "\":\"";
  Ext_buffer.add_string buf value;
  if last then Ext_buffer.add_string buf "\""
  else Ext_buffer.add_string buf "\","

let build_map (row_fields : Parsetree.row_field list) =
  let has_bs_as = ref false in
  let data, rev_data =
    let buf = Ext_buffer.create 50 in
    let rev_buf = Ext_buffer.create 50 in
    Ext_buffer.add_string buf "{";
    Ext_buffer.add_string rev_buf "{";
    let rec aux (row_fields : Parsetree.row_field list) =
      match row_fields with
      | [] -> ()
      | tag :: rest ->
        (match tag with
        | Rtag ({txt}, attrs, _, []) ->
          let name : string =
            match Ast_attributes.iter_process_bs_string_as attrs with
            | Some name ->
              has_bs_as := true;
              name
            | None -> txt
          in
          let last = rest = [] in
          add_key_value buf txt name last;
          add_key_value rev_buf name txt last
        | _ -> assert false (* checked by [is_enum_polyvar] *));
        aux rest
    in
    aux row_fields;
    Ext_buffer.add_string buf "}";
    Ext_buffer.add_string rev_buf "}";
    (Ext_buffer.contents buf, Ext_buffer.contents rev_buf)
  in
  (data, rev_data, !has_bs_as)

let app1 = Ast_compatible.app1

let app2 = Ast_compatible.app2

let ( ->~ ) a b =
  Ast_helper.Typ.arrow ~arity:(Some 1) {lbl = Nolabel; typ = a} b

let raise_when_not_found_ident =
  Longident.Ldot (Lident Primitive_modules.util, "raiseWhenNotFound")

let raise_when_not_found x =
  app1 (Exp.ident {loc = noloc; txt = raise_when_not_found_ident}) x

let deriving_name = "jsConverter"

let init () =
  Ast_derive.register deriving_name (fun (x : Parsetree.expression option) ->
      let create_type = handle_config x in

      {
        structure_gen =
          (fun (tdcls : tdcls) _ ->
            let handle_tdcl (tdcl : Parsetree.type_declaration) =
              let core_type = U.core_type_of_type_declaration tdcl in
              let name = tdcl.ptype_name.txt in
              let to_js = name ^ "ToJs" in
              let from_js = name ^ "FromJs" in
              let loc = tdcl.ptype_loc in
              let pat_to_js = {Asttypes.loc; txt = to_js} in
              let pat_from_js = {Asttypes.loc; txt = from_js} in
              let param = "param" in

              let ident_param = {Asttypes.txt = Longident.Lident param; loc} in
              let pat_param = {Asttypes.loc; txt = param} in
              let exp_param = Exp.ident ident_param in
              let new_type, new_tdcl =
                U.new_type_of_type_declaration tdcl ("abs_" ^ name)
              in
              let new_type_str =
                (* Abstract type *)
                Ast_compatible.rec_type_str Nonrecursive [new_tdcl]
              in
              let to_js_body body =
                Ast_comb.single_non_rec_value pat_to_js
                  (Ast_uncurried.uncurried_fun ~arity:1
                     (Ast_compatible.fun_ ~arity:None
                        (Pat.constraint_ (Pat.var pat_param) core_type)
                        body))
              in
              let ( +> ) a ty = Exp.constraint_ (erase_type a) ty in
              let ( +: ) a ty = erase_type (Exp.constraint_ a ty) in
              let coerce_result_to_new_type e =
                if create_type then e +> new_type else e
              in
              match tdcl.ptype_kind with
              | Ptype_record label_declarations ->
                let exp =
                  coerce_result_to_new_type
                    (Exp.extension
                       ( {Asttypes.loc; txt = "obj"},
                         PStr
                           [
                             Str.eval
                               (Exp.record
                                  (Ext_list.map label_declarations
                                     (fun {pld_name = {loc; txt}} ->
                                       let label =
                                         {
                                           Asttypes.loc;
                                           txt = Longident.Lident txt;
                                         }
                                       in
                                       {
                                         Parsetree.lid = label;
                                         x = Exp.field exp_param label;
                                         opt = false;
                                       }))
                                  None);
                           ] ))
                in
                let to_js = to_js_body exp in
                let obj_exp =
                  Exp.record
                    (Ext_list.map label_declarations
                       (fun {pld_name = {loc; txt}} ->
                         let label =
                           {Asttypes.loc; txt = Longident.Lident txt}
                         in
                         {
                           Parsetree.lid = label;
                           x = js_field exp_param label;
                           opt = false;
                         }))
                    None
                in
                let from_js =
                  Ast_comb.single_non_rec_value pat_from_js
                    (Ast_uncurried.uncurried_fun ~arity:1
                       (Ast_compatible.fun_ ~arity:(Some 1) (Pat.var pat_param)
                          (if create_type then
                             Exp.let_ Nonrecursive
                               [
                                 Vb.mk (Pat.var pat_param)
                                   (exp_param +: new_type);
                               ]
                               (Exp.constraint_ obj_exp core_type)
                           else Exp.constraint_ obj_exp core_type)))
                in
                let rest = [to_js; from_js] in
                if create_type then erase_type_str :: new_type_str :: rest
                else rest
              | Ptype_abstract -> (
                match Ast_polyvar.is_enum_polyvar tdcl with
                | Some row_fields ->
                  let map, rev_map = ("_map", "_revMap") in
                  let exp_map = Exp.ident {loc; txt = Lident map} in
                  let rev_exp_map = Exp.ident {loc; txt = Lident rev_map} in
                  let data, rev_data, has_bs_as = build_map row_fields in

                  let v =
                    [
                      erase_type_str;
                      unsafe_index_get;
                      Ast_comb.single_non_rec_value {loc; txt = map}
                        (Exp.extension
                           ( {txt = "raw"; loc},
                             PStr [Str.eval (Exp.constant (Const.string data))]
                           ));
                      Ast_comb.single_non_rec_value {loc; txt = rev_map}
                        (if has_bs_as then
                           Exp.extension
                             ( {txt = "raw"; loc},
                               PStr
                                 [
                                   Str.eval
                                     (Exp.constant (Const.string rev_data));
                                 ] )
                         else exp_map);
                      to_js_body
                        (if has_bs_as then
                           app2 unsafe_index_get_exp exp_map exp_param
                         else app1 erase_type_exp exp_param);
                      Ast_comb.single_non_rec_value pat_from_js
                        (Ast_uncurried.uncurried_fun ~arity:1
                           (Ast_compatible.fun_ ~arity:(Some 1)
                              (Pat.var pat_param)
                              (let result =
                                 app2 unsafe_index_get_exp rev_exp_map exp_param
                               in
                               if create_type then raise_when_not_found result
                               else result)));
                    ]
                  in
                  if create_type then new_type_str :: v else v
                | None ->
                  U.not_applicable tdcl.Parsetree.ptype_loc deriving_name;
                  [])
              | Ptype_variant _ ->
                U.not_applicable tdcl.Parsetree.ptype_loc deriving_name;
                []
              | Ptype_open ->
                U.not_applicable tdcl.Parsetree.ptype_loc deriving_name;
                []
            in
            Ext_list.flat_map tdcls handle_tdcl);
        signature_gen =
          (fun (tdcls : tdcls) _ ->
            let handle_tdcl tdcl =
              let core_type = U.core_type_of_type_declaration tdcl in
              let name = tdcl.ptype_name.txt in
              let to_js = name ^ "ToJs" in
              let from_js = name ^ "FromJs" in
              let loc = tdcl.ptype_loc in
              let pat_to_js = {Asttypes.loc; txt = to_js} in
              let pat_from_js = {Asttypes.loc; txt = from_js} in
              let to_js_type result =
                Ast_comb.single_non_rec_val pat_to_js
                  (Ast_helper.Typ.arrow ~arity:(Some 1)
                     {lbl = Nolabel; typ = core_type}
                     result)
              in
              let new_type, new_tdcl =
                U.new_type_of_type_declaration tdcl ("abs_" ^ name)
              in
              let new_type_str =
                Ast_compatible.rec_type_sig Nonrecursive [new_tdcl]
              in
              let ( +? ) v rest = if create_type then v :: rest else rest in
              match tdcl.ptype_kind with
              | Ptype_record label_declarations ->
                let obj_type flag =
                  Typ.object_
                    (Ext_list.map label_declarations
                       (fun {pld_name; pld_type} ->
                         Parsetree.Otag (pld_name, [], pld_type)))
                    flag
                in
                new_type_str
                +? [
                     to_js_type
                       (if create_type then new_type else obj_type Closed);
                     Ast_comb.single_non_rec_val pat_from_js
                       ((if create_type then new_type else obj_type Open)
                       ->~ core_type);
                   ]
              | Ptype_abstract -> (
                match Ast_polyvar.is_enum_polyvar tdcl with
                | Some _ ->
                  let ty1 =
                    if create_type then new_type else Ast_literal.type_string ()
                  in
                  let ty2 =
                    if create_type then core_type
                    else Ast_core_type.lift_option_type core_type
                  in
                  new_type_str
                  +? [
                       to_js_type ty1;
                       Ast_comb.single_non_rec_val pat_from_js (ty1 ->~ ty2);
                     ]
                | None ->
                  U.not_applicable tdcl.Parsetree.ptype_loc deriving_name;
                  [])
              | Ptype_variant _ ->
                U.not_applicable tdcl.Parsetree.ptype_loc deriving_name;
                []
              | Ptype_open ->
                U.not_applicable tdcl.Parsetree.ptype_loc deriving_name;
                []
            in
            Ext_list.flat_map tdcls handle_tdcl);
        expression_gen = None;
      })
