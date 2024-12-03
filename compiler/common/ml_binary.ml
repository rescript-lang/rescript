(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type _ kind = Ml : Parsetree.structure kind | Mli : Parsetree.signature kind

type ast0 = Impl of Parsetree0.structure | Intf of Parsetree0.signature

let magic_of_ast0 : ast0 -> string = function
  | Impl _ -> Config.ast_impl_magic_number
  | Intf _ -> Config.ast_intf_magic_number

let to_ast0 : type a. a kind -> a -> ast0 =
 fun kind ast ->
  match kind with
  | Ml ->
    Impl
      (Ast_mapper_to0.default_mapper.structure Ast_mapper_to0.default_mapper ast)
  | Mli ->
    Intf
      (Ast_mapper_to0.default_mapper.signature Ast_mapper_to0.default_mapper ast)

let ast0_to_structure : ast0 -> Parsetree.structure = function
  | Impl str0 ->
    Ast_mapper_from0.default_mapper.structure Ast_mapper_from0.default_mapper
      str0
  | Intf _ -> assert false

let ast0_to_signature : ast0 -> Parsetree.signature = function
  | Impl _ -> assert false
  | Intf sig0 ->
    Ast_mapper_from0.default_mapper.signature Ast_mapper_from0.default_mapper
      sig0

let magic_of_kind : type a. a kind -> string = function
  | Ml -> Config.ast_impl_magic_number
  | Mli -> Config.ast_intf_magic_number
