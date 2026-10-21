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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

type flat_make_step = {pos: int; name: string}
(** A module field on the way to a nested [make]: its runtime position
    (counted as in [Env]) and its source name. *)

type flat_make_path = flat_make_step list

type implementation = {
  lambda: Lambda.t;
  exports: Ident.t list;
  flat_make_paths: flat_make_path list;
      (** The nested [make] values of the final signature, which are also
          exported under [flat_export_name] of their path. *)
}

val transl_implementation :
  string ->
  Env.t ->
  Types.signature ->
  Typedtree.structure * Typedtree.module_coercion ->
  implementation
(** [transl_implementation module_name final_env signature (str, cc)] *)

val flat_export_name : string list -> string

val flat_make_name : string list -> string option
(** The flat export a read of this source path from another module goes
    through: [Some "A$B$make"] for [A.B.make], [None] for other paths. *)

type error
(* exception Error of Location.t * error *)
