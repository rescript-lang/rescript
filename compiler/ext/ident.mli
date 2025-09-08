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

(* Identifiers (unique names) *)

type t = {stamp: int; name: string; mutable flags: int}

include Identifiable.S with type t := t
(* Notes:
   - [equal] compares identifiers by name
   - [compare x y] is 0 if [same x y] is true.
   - [compare] compares identifiers by binding location
*)

val create : string -> t
val create_persistent : string -> t
val create_predef_exn : string -> t
val rename : t -> t
val name : t -> string
val unique_name : t -> string
val unique_toplevel_name : t -> string
val persistent : t -> bool
val same : t -> t -> bool
(* Compare identifiers by binding location.
   Two identifiers are the same either if they are both
   non-persistent and have been created by the same call to
   [new], or if they are both persistent and have the same
   name. *)

val compare : t -> t -> int
val hide : t -> t
(* Return an identifier with same name as the given identifier,
   but stamp different from any stamp returned by new.
   When put in a 'a tbl, this identifier can only be looked
   up by name. *)

val make_global : t -> unit
val global : t -> bool
val is_predef_exn : t -> bool

val binding_time : t -> int
val current_time : unit -> int
val set_current_time : int -> unit
val reinit : unit -> unit

type 'a tbl
(* Association tables from identifiers to type 'a. *)

val empty : 'a tbl
val add : t -> 'a -> 'a tbl -> 'a tbl
val find_same : t -> 'a tbl -> 'a
val find_name : string -> 'a tbl -> t * 'a
val find_all : string -> 'a tbl -> (t * 'a) list
val fold_name : (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
val fold_all : (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
val iter : (t -> 'a -> unit) -> 'a tbl -> unit

(* Idents for sharing keys *)

val make_key_generator : unit -> t -> t
