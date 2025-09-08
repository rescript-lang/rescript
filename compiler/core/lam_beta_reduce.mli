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

(** Beta reduction of lambda IR *)

val no_names_beta_reduce : Ident.t list -> Lam.t -> Lam.t list -> Lam.t
(* Compile-time beta-reduction of functions immediately applied:
      Lapply(Lfunction(Curried, params, body), args, loc) ->
        let paramN = argN in ... let param1 = arg1 in body
      Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock(args))], loc) ->
        let paramN = argN in ... let param1 = arg1 in body
   Assumes |args| = |params|.

   This function is used while counting used vars, no new names
   are generated to make this more complex
*)

(*
   Refresh all the identifiers, 
   otherwise the identifier property can not be preserved, 
   the obvious example is parameter
 *)

val propagate_beta_reduce :
  Lam_stats.t -> Ident.t list -> Lam.t -> Lam.t list -> Lam.t

val propagate_beta_reduce_with_map :
  Lam_stats.t ->
  Lam_var_stats.stats Map_ident.t ->
  Ident.t list ->
  Lam.t ->
  Lam.t list ->
  Lam.t
(** 
   {[ Lam_beta_reduce.propogate_beta_reduce_with_map 
       meta param_map
       params body args]}

   [param_map] collect the usage of parameters, it's readonly
   it can be  produced by 

   {[!Lam_analysis.free_variables meta.export_idents 
       (Lam_analysis.param_map_of_list params) body]}

   TODO:
   replace [propogate_beta_reduce] with such implementation 
   {[
     let propogate_beta_reduce meta params body args = 
       let (_, param_map) = 
         Lam_analysis.is_closed_with_map Set_ident.empty params body in 
       propogate_beta_reduce_with_map meta param_map params body args  
   ]}
*)
