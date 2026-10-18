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

type implementation = {lambda: Lambda.t; exports: Ident.t list}

val transl_implementation :
  string -> Typedtree.structure * Typedtree.module_coercion -> implementation

val flat_make_paths : Env.t -> Types.signature -> (int * string) list list
(** The nested [make] values of a compilation unit's final signature, which
    are also exported under [flat_export_name] of their path. *)

val flat_export_name : string list -> string

type error
(* exception Error of Location.t * error *)
