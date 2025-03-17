(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of primitive functions *)

open Misc
open Parsetree

type description = {
  prim_name: string; (* Name of primitive  or C function *)
  prim_arity: int; (* Number of arguments *)
  prim_alloc: bool; (* Does it allocates or raise? *)
  prim_native_name: string; (* Name of C function for the nat. code gen. *)
  prim_from_constructor: bool;
      (* Is it from a type constructor instead of a concrete function type? *)
}

let coerce : (description -> description -> bool) ref =
  ref (fun (p1 : description) (p2 : description) -> p1 = p2)

let parse_declaration valdecl ~arity ~from_constructor =
  let name, native_name =
    match valdecl.pval_prim with
    | name :: name2 :: _ -> (name, name2)
    | name :: _ -> (name, "")
    | [] -> fatal_error "Primitive.parse_declaration"
  in
  {
    prim_name = name;
    prim_arity = arity;
    prim_alloc = true;
    prim_native_name = native_name;
    prim_from_constructor = from_constructor;
  }

open Outcometree

let print p osig_val_decl =
  let prims =
    if p.prim_native_name <> "" then [p.prim_name; p.prim_native_name]
    else [p.prim_name]
  in
  {osig_val_decl with oval_prims = prims; oval_attributes = []}
