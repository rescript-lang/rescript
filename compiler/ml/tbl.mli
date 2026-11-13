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

val of_source : keys:'k array -> find:('k -> 'v) -> ('k, 'v) t
(** [of_source ~keys ~find] maps each of [keys] to [find key], computed when
    the key is looked up or visited, so [find] should memoize. [keys] must be
    sorted by [compare] without duplicates, so that [iter] and [fold] visit
    them in the same order as for a table built with [add], and [find] must
    raise [Not_found] for any other key. [add] builds the whole table. *)

val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val find : 'k -> ('k, 'v) t -> 'v
val find_str : string -> (string, 'v) t -> 'v
val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
