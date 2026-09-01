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

(** Global modules a unit depends on. Convert used to drop [Lglobal_module]
    references and have them added back by module analysis (see #3852); they
    are collected here instead, from the Lambda term directly. *)
let required_modules (lam : Lambda.t) : Lam_module_ident.Hash_set.t =
  let required = Lam_module_ident.Hash_set.create 0 in
  let rec collect (lam : Lambda.t) =
    (match lam with
    | Lglobal_module id ->
      Lam_module_ident.Hash_set.add required (Lam_module_ident.of_ml id)
    | _ -> ());
    Lambda.iter collect lam
  in
  collect lam;
  required
