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

(* Static exits: the numbering, and the catch a handler is wrapped in. *)

val next_raise_count : unit -> int
(** A fresh exit number. *)

val next_negative_raise_count : unit -> int
(** A fresh negative exit number, reserved for [match ... with exception x],
    where simplifications that assume a static raise sits in tail position of
    its handler do not apply. *)

val make_exit : int -> Lambda.t

val as_simple_exit : Lambda.t -> int option
(** The exit a term jumps to, when it is nothing but that jump. *)

val make_catch_delayed : Lambda.t -> int * (Lambda.t -> Lambda.t)
(** Exit number to raise to, and a wrapper that puts the catch around a body.
    A body that turns out to be exactly that raise gets the handler itself. *)
