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

type prim_kind =
  | Kind_intrinsic
  | Kind_external of External_ffi_types.t
  | Kind_inline_const of External_ffi_types.inline_const

type description = private {
  prim_name: string;
      (* Name of the intrinsic or the external's JS name; "" for inline
         constants and object creation *)
  prim_arity: int; (* Number of arguments *)
  prim_alloc: bool; (* Does it allocates or raise? *)
  prim_kind: prim_kind;
  prim_from_constructor: bool;
      (* Is it from a type constructor instead of a concrete function type? *)
}

val with_arity :
  description -> arity:int -> from_constructor:bool -> description

(* Invariant [List.length d.prim_native_repr_args = d.prim_arity] *)

val make :
  name:string ->
  kind:prim_kind ->
  arity:int ->
  from_constructor:bool ->
  description

type resolved_external = {
  resolved_type: Parsetree.core_type;
      (** The declared type, rewritten where FFI attributes erase or reshape
          arguments ([@as] constants, [@ignore], [@obj] results). *)
  resolved_attributes: Parsetree.attributes;
      (** The declaration's attributes that resolution did not consume. *)
  resolved_name: string;
  resolved_kind: prim_kind;
}
(** An [external] declaration after its FFI attributes are applied. *)

val resolve_external :
  (Parsetree.value_description -> string -> resolved_external) ref
(** [!resolve_external value_desc prim] resolves an [external] declaration
    whose primitive string is [prim] during type checking. FFI resolution lives in the frontend, above this library, which
    registers it before any source is type checked. *)

val print : description -> Outcometree.out_val_decl -> Outcometree.out_val_decl

val coercible : description -> description -> bool
(** Can an implementation's primitive satisfy an interface's during signature
    inclusion? *)
