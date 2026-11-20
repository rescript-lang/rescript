(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The [ocaml.ppx.context] attribute, which passes the compiler's settings to
    external PPXes. *)

val add_str : tool_name:string -> Parsetree.structure -> Parsetree.structure
(** Extract information from the current environment and encode it
    into an attribute which is prepended to the list of structure
    items in order to pass the information to an external
    processor. *)

val add_sig : tool_name:string -> Parsetree.signature -> Parsetree.signature
(** Same as [add_str], but for signatures. *)

val drop_str : Parsetree.structure -> Parsetree.structure
(** Drop the ocaml.ppx.context attribute from a structure. The compiler does
    not take back any of its data, such as cookies a PPX set. *)

val drop_sig : Parsetree.signature -> Parsetree.signature
(** Same as [drop_str], but for signatures. *)
