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

external ( .!() ) : 'a array -> int -> 'a = "%array_unsafe_get"

external ( .!()<- ) : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

let reverse_range a i len =
  if len = 0 then ()
  else
    for k = 0 to (len - 1) / 2 do
      let t = a.!(i + k) in
      a.!(i + k) <- a.!(i + len - 1 - k);
      a.!(i + len - 1 - k) <- t
    done

let reverse_in_place a = reverse_range a 0 (Array.length a)

let reverse a =
  let b_len = Array.length a in
  if b_len = 0 then [||]
  else
    let b = Array.copy a in
    for i = 0 to b_len - 1 do
      Array.unsafe_set b i (Array.unsafe_get a (b_len - 1 - i))
    done;
    b

let reverse_of_list = function
  | [] -> [||]
  | hd :: tl ->
    let len = List.length tl in
    let a = Array.make (len + 1) hd in
    let rec fill i = function
      | [] -> a
      | hd :: tl ->
        Array.unsafe_set a i hd;
        fill (i - 1) tl
    in
    fill (len - 1) tl

let filter a f =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len then reverse_of_list acc
    else
      let v = Array.unsafe_get a i in
      if f v then aux (v :: acc) (i + 1) else aux acc (i + 1)
  in
  aux [] 0

let filter_map a (f : _ -> _ option) =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len then reverse_of_list acc
    else
      let v = Array.unsafe_get a i in
      match f v with
      | Some v -> aux (v :: acc) (i + 1)
      | None -> aux acc (i + 1)
  in
  aux [] 0

let filter_mapi a (f : _ -> _ -> _ option) =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len then reverse_of_list acc
    else
      let v = Array.unsafe_get a i in
      match f i v with
      | Some v -> aux (v :: acc) (i + 1)
      | None -> aux acc (i + 1)
  in
  aux [] 0

let range from to_ =
  if from > to_ then invalid_arg "Ext_array.range"
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b =
  let len = Array.length a in
  if len <> Array.length b then invalid_arg "Ext_array.map2i"
  else Array.mapi (fun i a -> f i a (Array.unsafe_get b i)) a

let rec tolist_f_aux a f i res =
  if i < 0 then res
  else
    let v = Array.unsafe_get a i in
    tolist_f_aux a f (i - 1) (f v :: res)

let to_list_f a f = tolist_f_aux a f (Array.length a - 1) []

let rec tolist_aux a f i res =
  if i < 0 then res
  else
    tolist_aux a f (i - 1)
      (match f a.!(i) with
      | Some v -> v :: res
      | None -> res)

let to_list_map a f = tolist_aux a f (Array.length a - 1) []

let to_list_map_acc a acc f = tolist_aux a f (Array.length a - 1) acc

let of_list_map a f =
  match a with
  | [] -> [||]
  | [a0] ->
    let b0 = f a0 in
    [|b0|]
  | [a0; a1] ->
    let b0 = f a0 in
    let b1 = f a1 in
    [|b0; b1|]
  | [a0; a1; a2] ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    [|b0; b1; b2|]
  | [a0; a1; a2; a3] ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    let b3 = f a3 in
    [|b0; b1; b2; b3|]
  | [a0; a1; a2; a3; a4] ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    let b3 = f a3 in
    let b4 = f a4 in
    [|b0; b1; b2; b3; b4|]
  | a0 :: a1 :: a2 :: a3 :: a4 :: tl ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    let b3 = f a3 in
    let b4 = f a4 in
    let len = List.length tl + 5 in
    let arr = Array.make len b0 in
    Array.unsafe_set arr 1 b1;
    Array.unsafe_set arr 2 b2;
    Array.unsafe_set arr 3 b3;
    Array.unsafe_set arr 4 b4;
    let rec fill i = function
      | [] -> arr
      | hd :: tl ->
        Array.unsafe_set arr i (f hd);
        fill (i + 1) tl
    in
    fill 5 tl

(**
   {[
     # rfind_with_index [|1;2;3|] (=) 2;;
     - : int = 1
               # rfind_with_index [|1;2;3|] (=) 1;;
     - : int = 0
               # rfind_with_index [|1;2;3|] (=) 3;;
     - : int = 2
               # rfind_with_index [|1;2;3|] (=) 4;;
     - : int = -1
   ]}
*)
let rfind_with_index arr cmp v =
  let len = Array.length arr in
  let rec aux i =
    if i < 0 then i
    else if cmp (Array.unsafe_get arr i) v then i
    else aux (i - 1)
  in
  aux (len - 1)

type 'a split = No_split | Split of 'a array * 'a array

let find_with_index arr cmp v =
  let len = Array.length arr in
  let rec aux i len =
    if i >= len then -1
    else if cmp (Array.unsafe_get arr i) v then i
    else aux (i + 1) len
  in
  aux 0 len

let find_and_split arr cmp v : _ split =
  let i = find_with_index arr cmp v in
  if i < 0 then No_split
  else
    Split (Array.sub arr 0 i, Array.sub arr (i + 1) (Array.length arr - i - 1))

(** TODO: available since 4.03, use {!Array.exists} *)

let exists a p =
  let n = Array.length a in
  let rec loop i =
    if i = n then false
    else if p (Array.unsafe_get a i) then true
    else loop (succ i)
  in
  loop 0

let is_empty arr = Array.length arr = 0

let rec unsafe_loop index len p xs ys =
  if index >= len then true
  else
    p (Array.unsafe_get xs index) (Array.unsafe_get ys index)
    && unsafe_loop (succ index) len p xs ys

let for_alli a p =
  let n = Array.length a in
  let rec loop i =
    if i = n then true
    else if p i (Array.unsafe_get a i) then loop (succ i)
    else false
  in
  loop 0

let for_all2_no_exn xs ys p =
  let len_xs = Array.length xs in
  let len_ys = Array.length ys in
  len_xs = len_ys && unsafe_loop 0 len_xs p xs ys

let map a f =
  let open Array in
  let l = length a in
  if l = 0 then [||]
  else
    let r = make l (f (unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f (unsafe_get a i))
    done;
    r

let iter a f =
  let open Array in
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done

let fold_left a x f =
  let open Array in
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let get_or arr i cb =
  if i >= 0 && i < Array.length arr then Array.unsafe_get arr i else cb ()
