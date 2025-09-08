(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

(* val happens_to_be_diff:
   (int * Lambda.lambda) list -> int option *)

val convert :
  Set_ident.t -> Lambda.lambda -> Lam.t * Lam_module_ident.Hash_set.t
(** 
   [convert exports lam]
   it also collect [exit_map] and a collection of potential depended modules [may_depends]
   In this pass we also synchronized aliases so that 
   {[
     let a1 = a0 in 
     let a2 = a1 in 
     let a3 = a2 in 
     let a4 = a3 in 
   ]}
    converted to 
   {[
     let a1 = a0 in 
     let a2 = a0 in 
     let a3 = a0 in 
     let a4 = a0 in 
   ]}
    we dont eliminate unused let bindings to leave it for {!Lam_pass_lets_dce}
    we should remove all those let aliases, otherwise, it will be
    pushed into alias table again
*)
