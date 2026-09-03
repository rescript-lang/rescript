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

(* Static exits: the numbering, and the catch a handler is wrapped in.

   An exit number names a jump; the counters hand out fresh ones. Negative
   numbers are reserved for the exception cases of pattern matching, where
   simplifications that assume a static raise sits in tail position of its
   handler do not apply. *)

open Lambda

let raise_count = ref 0

let next_raise_count () =
  incr raise_count;
  !raise_count

let negative_raise_count = ref 0

let next_negative_raise_count () =
  decr negative_raise_count;
  !negative_raise_count

let make_exit i = staticraise i []

let rec as_simple_exit = function
  | Lstaticraise (i, []) -> Some i
  | Llet (Alias, _, _, e) -> as_simple_exit e
  | _ -> None

(* Introduce a catch around [handler], if worth it. Returns the exit number to
   raise to, and a function wrapping a body in the catch - a body that turns
   out to be exactly that raise gets the handler itself instead. *)
let make_catch_delayed handler =
  match as_simple_exit handler with
  | Some i -> (i, fun act -> act)
  | None -> (
    let i = next_raise_count () in
    ( i,
      fun body ->
        match body with
        | Lstaticraise (j, _) -> if i = j then handler else body
        | _ -> staticcatch body (i, []) handler ))
