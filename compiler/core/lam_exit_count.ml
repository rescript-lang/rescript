(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type collection = int Hash_int.t

(* Count occurrences of (exit n ...) statements *)
let count_exit (exits : collection) i = Hash_int.find_default exits i 0

let incr_exit (exits : collection) i =
  Hash_int.add_or_update exits i 1 ~update:succ

(** 
   This funcition counts how each [exit] is used, it will affect how the following optimizations performed.

   Some smart cases (this requires the following optimizations follow it): 

   {[
     Lstaticcatch(l1, (i,_), l2) 
   ]}
   If [l1] does not contain [(exit i)],
   [l2] will be removed, so don't count it.

   About Switch default branch handling, it maybe backend-specific
   See https://github.com/ocaml/ocaml/commit/fcf3571123e2c914768e34f1bd17e4cbaaa7d212#diff-704f66c0fa0fc9339230b39ce7d90919 
   For Lstringswitch ^

   For Lswitch, if it is not exhuastive pattern match, default will be counted twice.
   Since for pattern match,  we will  test whether it is  an integer or block, both have default cases predicate: [sw_consts_full] vs nconsts
*)
let count_helper (lam : Lam.t) : collection =
  let exits : collection = Hash_int.create 17 in
  let rec count (lam : Lam.t) =
    match lam with
    | Lstaticraise (i, ls) ->
      incr_exit exits i;
      Ext_list.iter ls count
    | Lstaticcatch (l1, (i, _), l2) ->
      count l1;
      if count_exit exits i > 0 then count l2
    | Lstringswitch (l, sw, d) ->
      count l;
      Ext_list.iter_snd sw count;
      Ext_option.iter d count
    | Lglobal_module _ | Lvar _ | Lconst _ -> ()
    | Lapply {ap_func; ap_args; _} ->
      count ap_func;
      Ext_list.iter ap_args count
    | Lfunction {body} -> count body
    | Llet (_, _, l1, l2) ->
      count l2;
      count l1
    | Lletrec (bindings, body) ->
      Ext_list.iter_snd bindings count;
      count body
    | Lprim {args; _} -> List.iter count args
    | Lswitch (l, sw) ->
      count_default sw;
      count l;
      Ext_list.iter_snd sw.sw_consts count;
      Ext_list.iter_snd sw.sw_blocks count
    | Ltrywith (l1, _v, l2) ->
      count l1;
      count l2
    | Lifthenelse (l1, l2, l3) ->
      count l1;
      count l2;
      count l3
    | Lsequence (l1, l2) ->
      count l1;
      count l2
    | Lwhile (l1, l2) ->
      count l1;
      count l2
    | Lfor (_, l1, l2, _dir, l3) ->
      count l1;
      count l2;
      count l3
    | Lassign (_, l) -> count l
  and count_default sw =
    match sw.sw_failaction with
    | None -> ()
    | Some al ->
      if (not sw.sw_consts_full) && not sw.sw_blocks_full then (
        count al;
        count al)
      else count al
  in
  count lam;
  exits
