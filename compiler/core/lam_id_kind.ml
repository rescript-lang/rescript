(* Copyright (C) Hongbo Zhang, Authors of ReScript
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

(** Keep track of which identifiers are aliased
*)

type rec_flag = Lam_rec | Lam_non_rec | Lam_self_rec
(* only a
   single mutual
   recursive function
*)

type element = NA | SimpleForm of Lam.t

type boxed_nullable = Undefined | Null | Null_undefined

type t =
  | Normal_optional of Lam.t (* Some [x] *)
  | OptionalBlock of Lam.t * boxed_nullable
  | ImmutableBlock of element array
  | MutableBlock of element array
  | Constant of Lam_constant.t
  | Module of Ident.t  (** TODO: static module vs first class module *)
  | FunctionId of {
      mutable arity: Lam_arity.t;
      (* TODO: This may contain some closure environment,
         check how it will interact with dead code elimination
      *)
      lambda: (Lam.t * rec_flag) option;
    }
  | Exception
  | Parameter
      (** For this case, it can help us determine whether it should be inlined or not *)
  | NA
      (** Not such information is associated with an identifier, it is immutable, 
           if you only associate a property to an identifier 
           we should consider [Lassign]
       *)

let pp = Format.fprintf

let print fmt (kind : t) =
  match kind with
  | ImmutableBlock arr -> pp fmt "Imm(%d)" (Array.length arr)
  | Normal_optional _ -> pp fmt "Some"
  | OptionalBlock (_, Null) -> pp fmt "?Null"
  | OptionalBlock (_, Undefined) -> pp fmt "?Undefined"
  | OptionalBlock (_, Null_undefined) -> pp fmt "?Nullable"
  | MutableBlock arr -> pp fmt "Mutable(%d)" (Array.length arr)
  | Constant _ -> pp fmt "Constant"
  | Module id -> pp fmt "%s/%d" id.name id.stamp
  | FunctionId _ -> pp fmt "FunctionID"
  | Exception -> pp fmt "Exception"
  | Parameter -> pp fmt "Parameter"
  | NA -> pp fmt "NA"
