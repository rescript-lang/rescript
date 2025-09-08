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

external reraise : exn -> 'a = "%raise"

let finally v ~clean:action f =
  match f v with
  | exception e ->
    action v;
    reraise e
  | e ->
    action v;
    e

(* let try_it f  =
   try ignore (f ()) with _ -> () *)

let with_file_as_chan filename f =
  finally (open_out_bin filename) ~clean:close_out f

let max_int (x : int) y = if x >= y then x else y

let min_int (x : int) y = if x < y then x else y

let max_int_option (x : int option) y =
  match (x, y) with
  | None, _ -> y
  | Some _, None -> x
  | Some x0, Some y0 -> if x0 >= y0 then x else y

(* external id : 'a -> 'a = "%identity" *)

(*
   let hash_variant s =
     let accu = ref 0 in
     for i = 0 to String.length s - 1 do
       accu := 223 * !accu + Char.code s.[i]
     done;
     (* reduce to 31 bits *)
     accu := !accu land (1 lsl 31 - 1);
     (* make it signed for 64 bits architectures *)
     if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu *)

(* let todo loc =
   failwith (loc ^ " Not supported yet")
*)

let rec int_of_string_aux s acc off len =
  if off >= len then acc
  else
    let d = Char.code (String.unsafe_get s off) - 48 in
    if d >= 0 && d <= 9 then int_of_string_aux s ((10 * acc) + d) (off + 1) len
    else -1
(* error *)

let nat_of_string_exn (s : string) =
  let acc = int_of_string_aux s 0 0 (String.length s) in
  if acc < 0 then invalid_arg s else acc

(** return index *)
let parse_nat_of_string (s : string) (cursor : int ref) =
  let current = !cursor in
  assert (current >= 0);
  let acc = ref 0 in
  let s_len = String.length s in
  let todo = ref true in
  let cur = ref current in
  while !todo && !cursor < s_len do
    let d = Char.code (String.unsafe_get s !cur) - 48 in
    if d >= 0 && d <= 9 then (
      acc := (10 * !acc) + d;
      incr cur)
    else todo := false
  done;
  cursor := !cur;
  !acc
