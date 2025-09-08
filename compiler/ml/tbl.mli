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

(* Association tables from any ordered type to any type.
   We use the generic ordering to compare keys. *)

type ('k, 'v) t

val empty : ('k, 'v) t
val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val find : 'k -> ('k, 'v) t -> 'v
val find_str : string -> (string, 'v) t -> 'v
val mem : 'k -> ('k, 'v) t -> bool
val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val map : ('k -> 'v1 -> 'v2) -> ('k, 'v1) t -> ('k, 'v2) t
val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

open Format

val print :
  (formatter -> 'k -> unit) ->
  (formatter -> 'v -> unit) ->
  formatter ->
  ('k, 'v) t ->
  unit
