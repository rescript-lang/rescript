#if defined TYPE_FUNCTOR
module Make(H: Hashtbl.HashedType): (S with type key = H.t) =
struct
  type key = H.t
  type   'value t = (key,'value) Ordered_hash_map_gen.t
  let key_index (h : _ t) key =
    (H.hash  key) land (Array.length h.data - 1)
  let equal_key = H.equal
#elif defined TYPE_LOCAL_IDENT
  type key = Ident.t
  type   'value t = (key,'value) Ordered_hash_map_gen.t
  let key_index (h : _ t) (key : key) =
    (Bs_hash_stubs.hash_int  key.stamp) land (Array.length h.data - 1)
  let equal_key = Ext_ident.equal

#else
      [%error "unknown type"]
#endif 

  open Ordered_hash_map_gen

  let create = create
  let clear = clear
  let reset = reset

  let iter = iter
  let fold = fold
  let length = length

  let elements = elements
  let choose = choose
  let to_sorted_array = to_sorted_array



  let rec small_bucket_mem key lst =
    match lst with 
    | Empty -> false 
    | Cons rhs -> 
      equal_key key rhs.key ||
      match rhs.next with 
      | Empty -> false 
      | Cons rhs -> 
        equal_key key rhs.key ||
        match rhs.next with 
        | Empty -> false 
        | Cons rhs -> 
          equal_key key rhs.key ||
          small_bucket_mem key rhs.next

  let rec small_bucket_rank key lst =
    match lst with 
    | Empty -> -1
    | Cons rhs -> 
      if equal_key key rhs.key then rhs.ord 
      else match rhs.next with 
        | Empty -> -1 
        | Cons rhs -> 
          if equal_key key rhs.key then rhs.ord else
            match rhs.next with 
            | Empty -> -1 
            | Cons rhs -> 
              if equal_key key rhs.key then rhs.ord else
                small_bucket_rank key rhs.next

  let rec small_bucket_find_value  key (lst : (_,_) bucket)   =
    match lst with 
    | Empty -> raise Not_found
    | Cons rhs -> 
      if equal_key key rhs.key then rhs.data
      else match rhs.next with 
        | Empty -> raise Not_found 
        | Cons rhs -> 
          if equal_key key  rhs.key then rhs.data else
            match rhs.next with 
            | Empty -> raise Not_found 
            | Cons rhs -> 
              if equal_key key rhs.key then rhs.data else
                small_bucket_find_value key rhs.next 

  let add h key value =
    let i = key_index h key  in 
    if not (small_bucket_mem key  h.data.(i)) then 
      begin 
        h.data.(i) <- Cons {key; ord = h.size; data = value; next =  h.data.(i)};
        h.size <- h.size + 1 ;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      end

  let mem h key =
    small_bucket_mem key (Array.unsafe_get h.data (key_index h key)) 
  let rank h key = 
    small_bucket_rank key(Array.unsafe_get h.data (key_index h key))  

  let find_value h key =
    small_bucket_find_value key (Array.unsafe_get h.data (key_index h key))


#if defined TYPE_FUNCTOR
end
#endif










