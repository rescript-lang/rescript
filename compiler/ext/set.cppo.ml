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


#if defined TYPE_STRING 
type elt = string
let compare_elt = Ext_string.compare 
let [@inline] eq_elt (x : elt) y = x = y
let print_elt = Format.pp_print_string
#elif defined TYPE_IDENT
type elt = Ident.t
let compare_elt (x : elt) (y : elt) = 
  let a =  Stdlib.compare (x.stamp : int) y.stamp in 
  if a <> 0 then a 
  else 
    let b = Stdlib.compare (x.name : string) y.name in 
    if b <> 0 then b 
    else Stdlib.compare (x.flags : int) y.flags     
let [@inline] eq_elt (x : elt) y = Ident.same x y
let print_elt = Ident.print
#elif defined TYPE_INT
type elt = int 
let compare_elt = Ext_int.compare 
let print_elt = Format.pp_print_int
let [@inline] eq_elt (x : elt) y = x = y
#else 
[%error "unknown type" ]
#endif


(* let (=) (a:int) b = a = b *)

type ('a ) t0 = 'a Set_gen.t 

type  t = elt t0

let empty = Set_gen.empty 
let is_empty = Set_gen.is_empty
let iter = Set_gen.iter
let fold = Set_gen.fold
let for_all = Set_gen.for_all 
let exists = Set_gen.exists 
let singleton = Set_gen.singleton 
let cardinal = Set_gen.cardinal
let elements = Set_gen.elements
let choose = Set_gen.choose 

let of_sorted_array = Set_gen.of_sorted_array

let rec mem (tree : t) (x : elt) =  match tree with 
  | Empty -> false
  | Leaf v -> eq_elt x  v 
  | Node{l; v; r} ->
    let c = compare_elt x v in
    c = 0 || mem (if c < 0 then l else r) x

type split = 
  | Yes of  {l : t ;  r :  t }
  | No of { l : t; r : t}  

let [@inline] split_l (x : split) = 
  match x with 
  | Yes {l} | No {l} -> l 

let [@inline] split_r (x : split) = 
  match x with 
  | Yes {r} | No {r} -> r       

let [@inline] split_pres (x : split) = match x with | Yes _ -> true | No _ -> false   

let rec split (tree : t) x : split =  match tree with 
  | Empty ->
     No {l = empty;  r = empty}
  | Leaf v ->   
    let c = compare_elt x v in
    if c = 0 then Yes {l = empty; r = empty}
    else if c < 0 then
      No {l = empty;  r = tree}
    else
      No {l = tree;  r = empty}
  | Node {l; v; r} ->
    let c = compare_elt x v in
    if c = 0 then Yes {l; r}
    else if c < 0 then
      match split l x with 
      | Yes result -> 
        Yes { result with r = Set_gen.internal_join result.r v r }
      | No result ->
        No { result with r= Set_gen.internal_join result.r v r }
    else
      match split r x with
      | Yes result -> 
        Yes {result with l = Set_gen.internal_join l v result.l}
      | No result ->   
        No {result with l = Set_gen.internal_join l v result.l}

let rec add (tree : t) x : t =  match tree with 
  | Empty -> singleton x
  | Leaf v -> 
    let c = compare_elt x v in
    if c = 0 then tree else     
    if c < 0 then 
      Set_gen.unsafe_two_elements x v
    else 
      Set_gen.unsafe_two_elements v x 
  | Node {l; v; r} as t ->
    let c = compare_elt x v in
    if c = 0 then t else
    if c < 0 then Set_gen.bal (add l x ) v r else Set_gen.bal l v (add r x )

let rec union (s1 : t) (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, t) 
  | (t, Empty) -> t
  | Node _, Leaf v2 ->
    add s1 v2 
  | Leaf v1, Node _ -> 
    add s2 v1 
  | Leaf x, Leaf v -> 
    let c = compare_elt x v in
    if c = 0 then s1 else     
    if c < 0 then 
      Set_gen.unsafe_two_elements x v
    else 
      Set_gen.unsafe_two_elements v x
  | Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2} ->
    if h1 >= h2 then    
      let split_result =  split s2 v1 in
      Set_gen.internal_join 
        (union l1 (split_l split_result)) v1 
        (union r1 (split_r split_result))  
    else    
      let split_result =  split s1 v2 in
      Set_gen.internal_join 
        (union (split_l split_result) l2) v2 
        (union (split_r split_result) r2)


let rec inter (s1 : t)  (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, _) 
  | (_, Empty) -> empty  
  | Leaf v, _ -> 
    if mem s2 v then s1 else empty
  | Node ({ v } as s1), _ ->
    let result = split s2 v in 
    if split_pres result then 
      Set_gen.internal_join 
        (inter s1.l (split_l result)) 
        v 
        (inter s1.r (split_r result))
    else
      Set_gen.internal_concat 
        (inter s1.l (split_l result)) 
        (inter s1.r (split_r result))


let rec diff (s1 : t) (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, _) -> empty
  | (t1, Empty) -> t1
  | Leaf v, _-> 
    if mem s2 v then empty else s1 
  | (Node({ v} as s1), _) ->
    let result =  split s2 v in
    if split_pres result then 
      Set_gen.internal_concat 
        (diff s1.l (split_l result)) 
        (diff s1.r (split_r result))    
    else
      Set_gen.internal_join 
        (diff s1.l (split_l result))
        v 
        (diff s1.r (split_r result))







let rec remove (tree : t)  (x : elt) : t = match tree with 
  | Empty -> empty (* This case actually would be never reached *)
  | Leaf v ->     
    if eq_elt x  v then empty else tree    
  | Node{l; v; r} ->
    let c = compare_elt x v in
    if c = 0 then Set_gen.internal_merge l r else
    if c < 0 then Set_gen.bal (remove l x) v r else Set_gen.bal l v (remove r x )

(* let compare s1 s2 = Set_gen.compare ~cmp:compare_elt s1 s2  *)



let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add (singleton x0) x1 
  | [x0; x1; x2] -> add (add (singleton x0)  x1) x2 
  | [x0; x1; x2; x3] -> add (add (add (singleton x0) x1 ) x2 ) x3 
  | [x0; x1; x2; x3; x4] -> add (add (add (add (singleton x0) x1) x2 ) x3 ) x4 
  | _ -> 
    let arrs = Array.of_list l in 
    Array.sort compare_elt arrs ; 
    of_sorted_array arrs



(* also check order *)
let invariant t =
  Set_gen.check t ;
  Set_gen.is_ordered ~cmp:compare_elt t          

let print fmt s = 
  Format.fprintf 
   fmt   "@[<v>{%a}@]@."
    (fun fmt s   -> 
       iter s
         (fun e -> Format.fprintf fmt "@[<v>%a@],@ " 
         print_elt e) 
    )
    s     





