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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Types

val constructor_has_optional_shape : Types.constructor_description -> bool

val extension_descr : Path.t -> extension_constructor -> constructor_description

val labels_of_type :
  Path.t -> type_declaration -> (Ident.t * label_description) list
val constructors_of_type :
  Path.t -> type_declaration -> (Ident.t * constructor_description) list

exception Constr_not_found

val find_constr_by_tag :
  constructor_tag -> constructor_declaration list -> constructor_declaration

val constructor_existentials :
  constructor_arguments -> type_expr option -> type_expr list * type_expr list
(** Takes [cd_args] and [cd_res] from a [constructor_declaration] and
    returns:
    - the types of the constructor's arguments
    - the existential variables introduced by the constructor
 *)

(* Set the polymorphic variant row_name field *)
val set_row_name : type_declaration -> Path.t -> unit
