(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Module dependencies. *)

module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string

type map_tree = Node of String_set.t * bound_map
and bound_map = map_tree String_map.t
val make_leaf : string -> map_tree
val make_node : bound_map -> map_tree
val weaken_map : String_set.t -> map_tree -> map_tree

val free_structure_names : String_set.t ref

(* dependencies found by preprocessing tools (plugins) *)
val pp_deps : string list ref

val open_module : bound_map -> Longident.t -> bound_map

val add_signature : bound_map -> Parsetree.signature -> unit

val add_implementation : bound_map -> Parsetree.structure -> unit

val add_implementation_binding : bound_map -> Parsetree.structure -> bound_map
val add_signature_binding : bound_map -> Parsetree.signature -> bound_map
