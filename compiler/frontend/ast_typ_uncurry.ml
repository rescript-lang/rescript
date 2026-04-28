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

type typ = Parsetree.core_type

let to_method_callback_type loc (mapper : Bs_ast_mapper.mapper) ~arity
    (meth_type : Parsetree.core_type) =
  let meth_type = Bs_ast_mapper.default_mapper.typ mapper meth_type in
  match arity with
  | Some n ->
    Ast_helper.Typ.constr
      {
        txt = Ldot (Ast_literal.Lid.js_meth_callback, "arity" ^ string_of_int n);
        loc;
      }
      [meth_type]
  | None -> assert false
