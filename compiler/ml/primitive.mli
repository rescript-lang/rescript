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

type description = private {
  prim_name: string; (* Name of the intrinsic or the external's JS name *)
  prim_arity: int; (* Number of arguments *)
  prim_alloc: bool; (* Does it allocates or raise? *)
  prim_ffi: External_ffi_types.t option;
      (* FFI spec of the external; [None] for compiler intrinsics *)
  prim_from_constructor: bool;
      (* Is it from a type constructor instead of a concrete function type? *)
}

val with_arity :
  description -> arity:int -> from_constructor:bool -> description

(* Invariant [List.length d.prim_native_repr_args = d.prim_arity] *)

val parse_declaration :
  Parsetree.value_description ->
  arity:int ->
  from_constructor:bool ->
  description

val print : description -> Outcometree.out_val_decl -> Outcometree.out_val_decl

val coercible : description -> description -> bool
(** Can an implementation's primitive satisfy an interface's during signature
    inclusion? *)
