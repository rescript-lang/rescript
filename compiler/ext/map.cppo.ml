
(* we don't create [map_poly], since some operations require raise an exception which carries [key] *)

#ifdef TYPE_STRING
type key = string 
let compare_key = Ext_string.compare
let [@inline] eq_key (x : key) y = x = y
#elif defined TYPE_INT
type key = int
let compare_key = Ext_int.compare
let [@inline] eq_key (x : key) y = x = y
#elif defined TYPE_IDENT
type key = Ident.t
let compare_key = Ext_ident.compare
let [@inline] eq_key (x : key) y = Ident.same x y
#else
    [%error "unknown type"]
#endif
    (* let [@inline] (=) (a : int) b = a = b *)
type + 'a t = (key,'a) Map_gen.t

let empty = Map_gen.empty 
let is_empty = Map_gen.is_empty
let iter = Map_gen.iter
let fold = Map_gen.fold
let for_all = Map_gen.for_all 
let exists = Map_gen.exists 
let singleton = Map_gen.singleton 
let cardinal = Map_gen.cardinal
let bindings = Map_gen.bindings
let to_sorted_array = Map_gen.to_sorted_array
let to_sorted_array_with_f = Map_gen.to_sorted_array_with_f
let keys = Map_gen.keys



let map = Map_gen.map 
let mapi = Map_gen.mapi
let bal = Map_gen.bal 
let height = Map_gen.height 


let rec add (tree : _ Map_gen.t as 'a) x data  : 'a = match tree with 
  | Empty ->
    singleton x data
  | Leaf {k;v} ->
    let c = compare_key x k in 
    if c = 0 then singleton x data else
    if c < 0 then 
      Map_gen.unsafe_two_elements x data k v 
    else 
      Map_gen.unsafe_two_elements k v x data  
  | Node {l; k ; v ; r; h} ->
    let c = compare_key x k in
    if c = 0 then
      Map_gen.unsafe_node x data l r h (* at least need update data *)
    else if c < 0 then
      bal (add l x data ) k v r
    else
      bal l k v (add r x data )


let rec adjust (tree : _ Map_gen.t as 'a) x replace  : 'a = 
  match tree with 
  | Empty ->
    singleton x (replace None)
  | Leaf {k ; v} -> 
    let c = compare_key x k in 
    if c = 0 then singleton x (replace (Some v)) else 
    if c < 0 then 
      Map_gen.unsafe_two_elements x (replace None) k v   
    else
      Map_gen.unsafe_two_elements k v x (replace None)   
  | Node ({l; k ; r} as tree) ->
    let c = compare_key x k in
    if c = 0 then
      Map_gen.unsafe_node x (replace  (Some tree.v)) l r tree.h
    else if c < 0 then
      bal (adjust l x  replace ) k tree.v r
    else
      bal l k tree.v (adjust r x  replace )


let rec find_exn (tree : _ Map_gen.t ) x = match tree with 
  | Empty ->
    raise Not_found
  | Leaf leaf -> 
    if eq_key x leaf.k then leaf.v else raise Not_found  
  | Node tree ->
    let c = compare_key x tree.k in
    if c = 0 then tree.v
    else find_exn (if c < 0 then tree.l else tree.r) x

let rec find_opt (tree : _ Map_gen.t ) x = match tree with 
  | Empty -> None 
  | Leaf leaf -> 
    if eq_key x leaf.k then Some leaf.v else None
  | Node tree ->
    let c = compare_key x tree.k in
    if c = 0 then Some tree.v
    else find_opt (if c < 0 then tree.l else tree.r) x

let rec find_default (tree : _ Map_gen.t ) x  default     = match tree with 
  | Empty -> default  
  | Leaf leaf -> 
    if eq_key x leaf.k then  leaf.v else default
  | Node tree ->
    let c = compare_key x tree.k in
    if c = 0 then tree.v
    else find_default (if c < 0 then tree.l else tree.r) x default

let rec mem (tree : _ Map_gen.t )  x= match tree with 
  | Empty ->
    false
  | Leaf leaf -> eq_key x leaf.k 
  | Node{l; k ;  r} ->
    let c = compare_key x k in
    c = 0 || mem (if c < 0 then l else r) x 

let rec remove (tree : _ Map_gen.t as 'a) x : 'a = match tree with 
  | Empty -> empty
  | Leaf leaf -> 
    if eq_key x leaf.k then empty 
    else tree
  | Node{l; k ; v; r} ->
    let c = compare_key x k in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove l x) k v r
    else
      bal l k v (remove r x )

type 'a split = 
  | Yes of {l : (key,'a) Map_gen.t; r : (key,'a)Map_gen.t ; v : 'a}
  | No of {l : (key,'a) Map_gen.t; r : (key,'a)Map_gen.t }


let rec split  (tree : (key,'a) Map_gen.t) x : 'a split  = 
  match tree with 
  | Empty ->
    No {l = empty; r = empty}
  | Leaf leaf -> 
    let c = compare_key x leaf.k in 
    if c = 0 then Yes {l = empty; v= leaf.v; r = empty} 
    else if c < 0 then No { l = empty; r = tree }
    else  No { l = tree; r = empty}
  | Node {l; k ; v ; r} ->
    let c = compare_key x k in
    if c = 0 then Yes {l; v; r}
    else if c < 0 then      
      match  split l x with 
      | Yes result -> Yes {result with r = Map_gen.join result.r k v r }
      | No result -> No {result with r = Map_gen.join result.r k v r } 
    else
      match split r x with 
      | Yes result -> 
        Yes {result with l = Map_gen.join l k v result.l}
      | No result -> 
        No {result with l = Map_gen.join l k v result.l}


let rec disjoint_merge_exn  
    (s1 : _ Map_gen.t) 
    (s2  : _ Map_gen.t) 
    fail : _ Map_gen.t =
  match s1 with
  | Empty -> s2  
  | Leaf ({k } as l1)  -> 
    begin match s2 with 
      | Empty -> s1 
      | Leaf l2 -> 
        let c = compare_key k l2.k in 
        if c = 0 then raise_notrace (fail k l1.v l2.v)
        else if c < 0 then Map_gen.unsafe_two_elements l1.k l1.v l2.k l2.v
        else Map_gen.unsafe_two_elements l2.k l2.v k l1.v
      | Node _ -> 
        adjust s2 k (fun data -> 
            match data with 
            |  None -> l1.v
            | Some s2v  -> raise_notrace (fail k l1.v s2v)
          )        
    end
  | Node ({k} as xs1) -> 
    if  xs1.h >= height s2 then
      begin match split s2 k with 
        | No {l; r} -> 
          Map_gen.join 
            (disjoint_merge_exn  xs1.l l fail)
            k 
            xs1.v 
            (disjoint_merge_exn xs1.r r fail)
        | Yes { v =  s2v} ->
          raise_notrace (fail k xs1.v s2v)
      end        
    else let [@warning "-8"] (Node ({k} as s2) : _ Map_gen.t)  = s2 in 
      begin match  split s1 k with 
        | No {l;  r} -> 
          Map_gen.join 
            (disjoint_merge_exn  l s2.l fail) k s2.v 
            (disjoint_merge_exn  r s2.r fail)
        | Yes { v = s1v} -> 
          raise_notrace (fail k s1v s2.v)
      end






let add_list (xs : _ list ) init = 
  Ext_list.fold_left xs init (fun  acc (k,v) -> add acc k v )

let of_list xs = add_list xs empty

let of_array xs = 
  Ext_array.fold_left xs empty (fun acc (k,v) -> add acc k v ) 
