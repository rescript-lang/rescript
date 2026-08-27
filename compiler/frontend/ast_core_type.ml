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

type t = Parsetree.core_type

let lift_option_type ({ptyp_loc} as ty : t) : t =
  {
    ptyp_desc =
      Ptyp_constr ({txt = Ast_literal.predef_option; loc = ptyp_loc}, [ty]);
    ptyp_loc;
    ptyp_attributes = [];
  }

open Ast_helper

(* let replace_result (ty : t) (result : t) : t =
   let rec aux (ty : Parsetree.core_type) =
    match ty with
    | { ptyp_desc =
          Ptyp_arrow (label,t1,t2)
      } -> { ty with ptyp_desc = Ptyp_arrow(label,t1, aux t2)}
    | {ptyp_desc = Ptyp_poly(fs,ty)}
      ->  {ty with ptyp_desc = Ptyp_poly(fs, aux ty)}
    | _ -> result in
   aux ty *)

let is_builtin_rank0_type txt =
  match txt with
  | "int" | "char" | "float" | "bool" | "unit" | "exn" | "string" -> true
  | _ -> false

let is_unit (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr ({txt = Lident "unit"}, []) -> true
  | _ -> false

(* let is_array (ty : t) =
   match ty.ptyp_desc with
   | Ptyp_constr({txt =Lident "array"}, [_]) -> true
   | _ -> false *)

let is_user_option (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr
      ({txt = Lident "option" | Ldot (Lident "*predef*", "option")}, [_]) ->
    true
  | _ -> false

(* let is_user_bool (ty : t) =
   match ty.ptyp_desc with
   | Ptyp_constr({txt = Lident "bool"},[]) -> true
   | _ -> false *)

(* let is_user_int (ty : t) =
   match ty.ptyp_desc with
   | Ptyp_constr({txt = Lident "int"},[]) -> true
   | _ -> false *)

(* Note that OCaml type checker will not allow arbitrary
   name as type variables, for example:
   {[
     '_x'_
   ]}
   will be recognized as a invalid program
*)

let make_obj ~loc xs = Typ.object_ ~loc xs Closed

(**

   {[ 'a . 'a -> 'b ]}
   OCaml does not support such syntax yet
   {[ 'a -> ('a. 'a -> 'b) ]}

*)
let is_arity_one (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_arrow {params = [_]} -> true
  | _ -> false

let list_of_arrow (ty : t) : t * Parsetree.arg list =
  match ty.ptyp_desc with
  | Ptyp_arrow {params; ret} -> (ret, params)
  | _ -> (ty, [])
