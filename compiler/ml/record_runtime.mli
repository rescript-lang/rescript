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

(* How a record field is named at run time. *)

val as_name : Parsetree.attribute -> string option
(** The string an [@as("...")] attribute carries, if this is one. *)

val field_name : string -> Parsetree.attributes -> string
(** The runtime name of a field declared as [declared_name] with [attributes]:
    the [@as] payload when there is one, otherwise the declared name. *)

val label_name : Types.label_description -> string

val declaration_name : Types.label_declaration -> string
