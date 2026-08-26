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
  prim_name: string; (* Name of the intrinsic or the external's JS name *)
  prim_arity: int; (* Number of arguments *)
  prim_alloc: bool; (* Does it allocates or raise? *)
  prim_ffi: External_ffi_types.t option;
      (* FFI spec of the external; [None] for compiler intrinsics *)
  prim_from_constructor: bool;
      (* Is it from a type constructor instead of a concrete function type? *)
  transformed_jsx: bool;
}

let set_transformed_jsx d ~transformed_jsx = {d with transformed_jsx}

let with_arity d ~arity ~from_constructor =
  {d with prim_arity = arity; prim_from_constructor = from_constructor}

(* Can an implementation's primitive satisfy an interface's during signature
   inclusion? The specs must be equal, up to the widening rule in
   [External_ffi_types.inclusion_compatible]. *)
let coercible (impl : description) (intf : description) =
  impl.prim_name = intf.prim_name
  && impl.prim_arity = intf.prim_arity
  && impl.prim_ffi = intf.prim_ffi
  ||
  match (impl.prim_ffi, intf.prim_ffi) with
  | Some impl_ffi, Some intf_ffi ->
    External_ffi_types.inclusion_compatible impl_ffi intf_ffi
  | _ -> false

let parse_declaration (valdecl : Parsetree.value_description) ~arity
    ~from_constructor =
  let name, ffi =
    match valdecl.pval_prim with
    | Some (Prim_name name) -> (name, None)
    | Some (Prim_ffi {name; spec}) -> (name, Some spec)
    | None -> fatal_error "Primitive.parse_declaration"
  in
  {
    prim_name = name;
    prim_arity = arity;
    prim_alloc = true;
    prim_ffi = ffi;
    prim_from_constructor = from_constructor;
    transformed_jsx = false;
  }

open Outcometree

let print p osig_val_decl =
  let repr : Parsetree.primitive_repr =
    match p.prim_ffi with
    | None -> Prim_name p.prim_name
    | Some spec -> Prim_ffi {name = p.prim_name; spec}
  in
  {osig_val_decl with oval_prim = Some repr; oval_attributes = []}
