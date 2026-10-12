(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

let min_int (x : int) y = if x < y then x else y

(* Tarjan's algorithm. Every node is pushed on the stack exactly once, so a
   fixed array of [Array.length e] elements is enough for it. *)
let graph (e : int array array) : int array list =
  let node_count = Array.length e in
  let index = ref 0 in
  let stack = Array.make node_count 0 in
  let stack_len = ref 0 in
  (* components in the order they are found, newest first *)
  let output = ref [] in

  let on_stack_array = Array.make node_count false in
  let index_array = Array.make node_count (-1) in
  let lowlink_array = Array.make node_count (-1) in

  let rec scc v_data =
    let new_index = !index + 1 in
    index := new_index;
    Array.unsafe_set stack !stack_len v_data;
    incr stack_len;

    index_array.(v_data) <- new_index;
    lowlink_array.(v_data) <- new_index;
    on_stack_array.(v_data) <- true;
    Array.iter
      (fun w_data ->
        if Array.unsafe_get index_array w_data < 0 then (
          (* not processed *)
          scc w_data;
          Array.unsafe_set lowlink_array v_data
            (min_int
               (Array.unsafe_get lowlink_array v_data)
               (Array.unsafe_get lowlink_array w_data)))
        else if Array.unsafe_get on_stack_array w_data then
          (* successor is in stack and hence in current scc *)
          Array.unsafe_set lowlink_array v_data
            (min_int
               (Array.unsafe_get lowlink_array v_data)
               (Array.unsafe_get lowlink_array w_data)))
      e.(v_data);

    if
      Array.unsafe_get lowlink_array v_data
      = Array.unsafe_get index_array v_data
    then (
      (* start a new scc *)
      let s_len = !stack_len in
      let last_index = ref (s_len - 1) in
      let u = ref (Array.unsafe_get stack !last_index) in
      while !u <> v_data do
        Array.unsafe_set on_stack_array !u false;
        last_index := !last_index - 1;
        u := Array.unsafe_get stack !last_index
      done;
      on_stack_array.(v_data) <- false;
      (* necessary *)
      output := Array.sub stack !last_index (s_len - !last_index) :: !output;
      stack_len := !last_index)
  in
  for i = 0 to node_count - 1 do
    if Array.unsafe_get index_array i < 0 then scc i
  done;
  List.rev !output
