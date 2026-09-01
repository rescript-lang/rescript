(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*
  This module transforms generic switches in combinations
  of if tests and switches.
*)

(* For detecting action sharing, object style *)

(* Store for actions in object style:
   act_store : store an action, returns index in table
               In case an action with equal key exists, returns index
               of the stored action. Otherwise add entry in table.
   act_store_shared : This stored action will always be shared.
   act_get   : retrieve table
   act_get_shared : retrieve table, with sharing explicit
*)

type 'a shared = Shared of 'a | Single of 'a

type 'a t_store = {
  act_get_shared: unit -> 'a shared array;
  act_store: 'a -> int;
  act_store_shared: 'a -> int;
}

module type Stored = sig
  type t
  type key
  val compare_key : key -> key -> int
  val make_key : t -> key option
end

module Store (A : Stored) : sig
  val mk_store : unit -> A.t t_store
end

(*
  zyva (low, high) arg cases actions where
    - arg is the argument of the switch.
    - low, high are the interval limits.
    - cases is a list of sub-interval and action indices
    - actions is an array of actions.

  All these arguments specify a switch construct and zyva
  returns an action that performs the switch.
*)
val zyva :
  int * int ->
  Lambda.lambda ->
  (int * int * int) array ->
  Lambda.lambda t_store ->
  Lambda.lambda

val test_sequence :
  Lambda.lambda ->
  (int * int * int) array ->
  Lambda.lambda t_store ->
  Lambda.lambda
