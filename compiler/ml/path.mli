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

(* Access paths *)

type t = Pident of Ident.t | Pdot of t * string * int | Papply of t * t

val same : t -> t -> bool
val compare : t -> t -> int
val isfree : Ident.t -> t -> bool
val binding_time : t -> int
val flatten : t -> [`Contains_apply | `Ok of Ident.t * string list]

val nopos : int

val name : ?paren:(string -> bool) -> t -> string
(* [paren] tells whether a path suffix needs parentheses *)

val head : t -> Ident.t

val heads : t -> Ident.t list

val last : t -> string

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

val constructor_typath : t -> typath
val is_constructor_typath : t -> bool
