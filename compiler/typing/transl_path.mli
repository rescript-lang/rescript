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

(* Translating a path to the term that reads it. *)

val transl_normal_path : Path.t -> Lambda.t
(** The path is already normalized. *)

val transl_module_path : ?loc:Location.t -> Env.t -> Path.t -> Lambda.t

val transl_value_path : ?loc:Location.t -> Env.t -> Path.t -> Lambda.t

val transl_extension_path : ?loc:Location.t -> Env.t -> Path.t -> Lambda.t
