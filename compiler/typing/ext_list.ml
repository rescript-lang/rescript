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

let map_to_array a f =
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

let rec map_combine l1 l2 f =
  match (l1, l2) with
  | [], [] -> []
  | a1 :: l1, a2 :: l2 -> (f a1, a2) :: map_combine l1 l2 f
  | _, _ -> invalid_arg "Ext_list.map_combine"

let rec arr_list_combine_unsafe arr l i j acc f =
  if i = j then acc
  else
    match l with
    | [] -> invalid_arg "Ext_list.arr_list_combine_unsafe"
    | h :: tl ->
      (f arr.!(i), h) :: arr_list_combine_unsafe arr tl (i + 1) j acc f

let combine_array arr l f =
  let len = Array.length arr in
  arr_list_combine_unsafe arr l 0 len [] f

let rec arr_list_filter_map_unsafe arr l i j acc f =
  if i = j then acc
  else
    match l with
    | [] -> invalid_arg "Ext_list.arr_list_filter_map_unsafe"
    | h :: tl -> (
      match f arr.!(i) h with
      | None -> arr_list_filter_map_unsafe arr tl (i + 1) j acc f
      | Some v -> v :: arr_list_filter_map_unsafe arr tl (i + 1) j acc f)

let array_list_filter_map arr l f =
  let len = Array.length arr in
  arr_list_filter_map_unsafe arr l 0 len [] f

let rec map_snd l f =
  match l with
  | [] -> []
  | [(v1, x1)] ->
    let y1 = f x1 in
    [(v1, y1)]
  | [(v1, x1); (v2, x2)] ->
    let y1 = f x1 in
    let y2 = f x2 in
    [(v1, y1); (v2, y2)]
  | [(v1, x1); (v2, x2); (v3, x3)] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    [(v1, y1); (v2, y2); (v3, y3)]
  | [(v1, x1); (v2, x2); (v3, x3); (v4, x4)] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    [(v1, y1); (v2, y2); (v3, y3); (v4, y4)]
  | (v1, x1) :: (v2, x2) :: (v3, x3) :: (v4, x4) :: (v5, x5) :: tail ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    (v1, y1) :: (v2, y2) :: (v3, y3) :: (v4, y4) :: (v5, y5) :: map_snd tail f

let rec map_sharing l f =
  match l with
  | [] -> l
  | x :: xs ->
    let x' = f x in
    let xs' = map_sharing xs f in
    if x' == x && xs' == xs then l else x' :: xs'

let rec map_snd_sharing l f =
  match l with
  | [] -> l
  | (k, x) :: xs ->
    let x' = f x in
    let xs' = map_snd_sharing xs f in
    if x' == x && xs' == xs then l else (k, x') :: xs'

(* [concat_map_sharing] recurses over the first [concat_map_sharing_depth]
   elements, which allocates nothing for an unchanged list, and continues with
   a loop in constant stack after that, so the long structures the JSX
   transform maps don't grow the stack by a frame per element. [map_sharing]
   keeps its plain recursion: the Lambda and JS passes call it all the time,
   and a depth counter costs measurable instructions there. *)
let concat_map_sharing_depth = 1000

(* The elements of [l] before its suffix [stop], reversed onto [acc] *)
let rec rev_prefix_onto l stop acc =
  if l == stop then acc
  else
    match l with
    | [] -> acc
    | x :: xs -> rev_prefix_onto xs stop (x :: acc)

(* [concat_map_sharing] in constant stack. [acc] is the result up to the last
   changed element, reversed, and [run] the input after that element, whose
   elements seen so far map to themselves and are shared at the end. *)
let rec concat_map_sharing_loop f changed acc run l =
  match l with
  | [] -> if changed then List.rev_append acc run else run
  | x :: xs -> (
    match f x with
    | [x'] when x' == x -> concat_map_sharing_loop f changed acc run xs
    | mapped ->
      concat_map_sharing_loop f true
        (List.rev_append mapped (rev_prefix_onto run l acc))
        xs xs)

let rec concat_map_sharing_bounded l f depth =
  match l with
  | [] -> l
  | _ when depth = 0 -> concat_map_sharing_loop f false [] l l
  | x :: xs -> (
    let mapped = f x in
    let xs' = concat_map_sharing_bounded xs f (depth - 1) in
    match mapped with
    | [x'] when x' == x && xs' == xs -> l
    | _ -> mapped @ xs')

let concat_map_sharing l f =
  concat_map_sharing_bounded l f concat_map_sharing_depth

let rec map_last l f =
  match l with
  | [] -> []
  | [x1] ->
    let y1 = f true x1 in
    [y1]
  | [x1; x2] ->
    let y1 = f false x1 in
    let y2 = f true x2 in
    [y1; y2]
  | [x1; x2; x3] ->
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f true x3 in
    [y1; y2; y3]
  | [x1; x2; x3; x4] ->
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f false x3 in
    let y4 = f true x4 in
    [y1; y2; y3; y4]
  | x1 :: x2 :: x3 :: x4 :: tail ->
    (* make sure that tail is not empty *)
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f false x3 in
    let y4 = f false x4 in
    y1 :: y2 :: y3 :: y4 :: map_last tail f

let rec mapi_aux lst i f tail =
  match lst with
  | [] -> tail
  | a :: l ->
    let r = f i a in
    r :: mapi_aux l (i + 1) f tail

let mapi_append lst f tail = mapi_aux lst 0 f tail

let rec last xs =
  match xs with
  | [x] -> x
  | _ :: tl -> last tl
  | [] -> invalid_arg "Ext_list.last"

let rec map_append l1 l2 f =
  match l1 with
  | [] -> l2
  | [a0] -> f a0 :: l2
  | [a0; a1] ->
    let b0 = f a0 in
    let b1 = f a1 in
    b0 :: b1 :: l2
  | [a0; a1; a2] ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    b0 :: b1 :: b2 :: l2
  | [a0; a1; a2; a3] ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    let b3 = f a3 in
    b0 :: b1 :: b2 :: b3 :: l2
  | [a0; a1; a2; a3; a4] ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    let b3 = f a3 in
    let b4 = f a4 in
    b0 :: b1 :: b2 :: b3 :: b4 :: l2
  | a0 :: a1 :: a2 :: a3 :: a4 :: rest ->
    let b0 = f a0 in
    let b1 = f a1 in
    let b2 = f a2 in
    let b3 = f a3 in
    let b4 = f a4 in
    b0 :: b1 :: b2 :: b3 :: b4 :: map_append rest l2 f

let rec fold_right3 l r last acc f =
  match (l, r, last) with
  | [], [], [] -> acc
  | [a0], [b0], [c0] -> f a0 b0 c0 acc
  | [a0; a1], [b0; b1], [c0; c1] -> f a0 b0 c0 (f a1 b1 c1 acc)
  | [a0; a1; a2], [b0; b1; b2], [c0; c1; c2] ->
    f a0 b0 c0 (f a1 b1 c1 (f a2 b2 c2 acc))
  | [a0; a1; a2; a3], [b0; b1; b2; b3], [c0; c1; c2; c3] ->
    f a0 b0 c0 (f a1 b1 c1 (f a2 b2 c2 (f a3 b3 c3 acc)))
  | [a0; a1; a2; a3; a4], [b0; b1; b2; b3; b4], [c0; c1; c2; c3; c4] ->
    f a0 b0 c0 (f a1 b1 c1 (f a2 b2 c2 (f a3 b3 c3 (f a4 b4 c4 acc))))
  | ( a0 :: a1 :: a2 :: a3 :: a4 :: arest,
      b0 :: b1 :: b2 :: b3 :: b4 :: brest,
      c0 :: c1 :: c2 :: c3 :: c4 :: crest ) ->
    f a0 b0 c0
      (f a1 b1 c1
         (f a2 b2 c2
            (f a3 b3 c3 (f a4 b4 c4 (fold_right3 arest brest crest acc f)))))
  | _, _, _ -> invalid_arg "Ext_list.fold_right2"

let rec fold_left_with_offset l accu i f =
  match l with
  | [] -> accu
  | a :: l -> fold_left_with_offset l (f a accu i) (i + 1) f

let rec small_split_at n acc l =
  if n <= 0 then (List.rev acc, l)
  else
    match l with
    | x :: xs -> small_split_at (n - 1) (x :: acc) xs
    | _ -> invalid_arg "Ext_list.split_at"

let split_at l n = small_split_at n [] l

let rec split_at_last_aux acc x =
  match x with
  | [] -> invalid_arg "Ext_list.split_at_last"
  | [x] -> (List.rev acc, x)
  | y0 :: ys -> split_at_last_aux (y0 :: acc) ys

let split_at_last (x : 'a list) =
  match x with
  | [] -> invalid_arg "Ext_list.split_at_last"
  | [a0] -> ([], a0)
  | [a0; a1] -> ([a0], a1)
  | [a0; a1; a2] -> ([a0; a1], a2)
  | [a0; a1; a2; a3] -> ([a0; a1; a2], a3)
  | [a0; a1; a2; a3; a4] -> ([a0; a1; a2; a3], a4)
  | a0 :: a1 :: a2 :: a3 :: a4 :: rest ->
    let rev, last = split_at_last_aux [] rest in
    (a0 :: a1 :: a2 :: a3 :: a4 :: rev, last)

(**
   can not do loop unroll due to state combination
*)
let filter_mapi xs f =
  let rec aux i xs =
    match xs with
    | [] -> []
    | y :: ys -> (
      match f y i with
      | None -> aux (i + 1) ys
      | Some z -> z :: aux (i + 1) ys)
  in
  aux 0 xs

let rec rev_map_append l1 l2 f =
  match l1 with
  | [] -> l2
  | a :: l -> rev_map_append l (f a :: l2) f

let rec group (eq : 'a -> 'a -> bool) lst =
  match lst with
  | [] -> []
  | x :: xs -> aux eq x (group eq xs)

and aux eq (x : 'a) (xss : 'a list list) : 'a list list =
  match xss with
  | [] -> [[x]]
  | (y0 :: _ as y) :: ys ->
    (* cannot be empty *)
    if eq x y0 then (x :: y) :: ys else y :: aux eq x ys
  | _ :: _ -> assert false

let stable_group lst eq = group eq lst |> List.rev

let rec find_first_not xs p =
  match xs with
  | [] -> None
  | a :: l -> if p a then find_first_not l p else Some a

let rec rev_iter l f =
  match l with
  | [] -> ()
  | [x1] -> f x1
  | [x1; x2] ->
    f x2;
    f x1
  | [x1; x2; x3] ->
    f x3;
    f x2;
    f x1
  | [x1; x2; x3; x4] ->
    f x4;
    f x3;
    f x2;
    f x1
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
    rev_iter tail f;
    f x5;
    f x4;
    f x3;
    f x2;
    f x1

let rec for_all_snd lst p =
  match lst with
  | [] -> true
  | (_, a) :: l -> p a && for_all_snd l p

let rec for_all2_no_exn l1 l2 p =
  match (l1, l2) with
  | [], [] -> true
  | a1 :: l1, a2 :: l2 -> p a1 a2 && for_all2_no_exn l1 l2 p
  | _, _ -> false

let rec split_map l f =
  match l with
  | [] -> ([], [])
  | [x1] ->
    let a0, b0 = f x1 in
    ([a0], [b0])
  | [x1; x2] ->
    let a1, b1 = f x1 in
    let a2, b2 = f x2 in
    ([a1; a2], [b1; b2])
  | [x1; x2; x3] ->
    let a1, b1 = f x1 in
    let a2, b2 = f x2 in
    let a3, b3 = f x3 in
    ([a1; a2; a3], [b1; b2; b3])
  | [x1; x2; x3; x4] ->
    let a1, b1 = f x1 in
    let a2, b2 = f x2 in
    let a3, b3 = f x3 in
    let a4, b4 = f x4 in
    ([a1; a2; a3; a4], [b1; b2; b3; b4])
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
    let a1, b1 = f x1 in
    let a2, b2 = f x2 in
    let a3, b3 = f x3 in
    let a4, b4 = f x4 in
    let a5, b5 = f x5 in
    let ass, bss = split_map tail f in
    (a1 :: a2 :: a3 :: a4 :: a5 :: ass, b1 :: b2 :: b3 :: b4 :: b5 :: bss)

let sort_via_array lst cmp =
  let arr = Array.of_list lst in
  Array.sort cmp arr;
  Array.to_list arr

let sort_via_arrayf lst cmp f =
  let arr = Array.of_list lst in
  Array.sort cmp arr;
  Array.fold_right (fun x acc -> f x :: acc) arr []

let rec assoc_by_string lst (k : string) def =
  match lst with
  | [] -> (
    match def with
    | None -> assert false
    | Some x -> x)
  | (k1, v1) :: rest -> if k1 = k then v1 else assoc_by_string rest k def

let rec nth_aux l n =
  match l with
  | [] -> None
  | a :: l -> if n = 0 then Some a else nth_aux l (n - 1)

let nth_opt l n = if n < 0 then None else nth_aux l n

let rec iter_snd lst f =
  match lst with
  | [] -> ()
  | (_, x) :: xs ->
    f x;
    iter_snd xs f

let rec iter_fst lst f =
  match lst with
  | [] -> ()
  | (x, _) :: xs ->
    f x;
    iter_fst xs f

let rec exists_fst l p =
  match l with
  | [] -> false
  | (a, _) :: l -> p a || exists_fst l p

let rec exists_snd l p =
  match l with
  | [] -> false
  | (_, a) :: l -> p a || exists_snd l p

let singleton_exn xs =
  match xs with
  | [x] -> x
  | _ -> assert false

let is_empty = function
  | [] -> true
  | _ :: _ -> false
