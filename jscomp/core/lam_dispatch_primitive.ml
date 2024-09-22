(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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

module E = Js_exp_make
(* module S = Js_stmt_make *)

(** 
   There are two things we need consider:
   1.  For some primitives we can replace caml-primitive with js primitives directly
   2.  For some standard library functions, we prefer to replace with javascript primitives
    For example [Pervasives["^"] -> ^]
    We can collect all mli files in OCaml and replace it with an efficient javascript runtime

   TODO: return type to be expression is ugly, 
   we should allow return block    
*)
let translate loc (prim_name : string) (args : J.expression list) : J.expression
    =
  let[@inline] call ?name m =
    let name =
      match name with
      | None ->
          if prim_name.[0] = '?' then
            String.sub prim_name 1 (String.length prim_name - 1)
          else prim_name
      | Some x -> x
    in
    E.runtime_call m name args
  in
  match prim_name with
  (******************************************************************************)
  (************************* customized primitives ******************************)
  (******************************************************************************)
  | "?hash_mix_string" | "?hash_mix_int" | "?hash_final_mix" ->
      call Js_runtime_modules.hash_primitive
  | "?hash" -> call Js_runtime_modules.hash
  | missing_impl ->
    let msg = Warnings.message (Bs_unimplemented_primitive missing_impl) in
    Location.raise_errorf ~loc "%s" msg
