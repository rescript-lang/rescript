(** Zero-allocation (steady-state) open-addressing hash maps and sets.

    Vendored from Hachis (François Pottier, Inria Paris).
    Uses linear probing with void/tomb sentinels, power-of-2 capacity,
    and Obj for type erasure.  After tables reach steady-state capacity,
    [clear] + [replace] cycles perform zero heap allocation. *)

(* ---- Internal open-addressing table ---- *)

(* Sentinels: physically unique values that can never be == to any user key. *)
let void = Obj.repr (ref ())
let tomb = Obj.repr (ref ())

let[@inline] is_sentinel c = c == void || c == tomb
let[@inline] is_not_sentinel c = not (is_sentinel c)

let log_alloc kind before_cap after_cap =
  let _ = before_cap in
  let _ = after_cap in
  ReactiveAllocTrace.emit_alloc_kind kind

type table = {
  mutable population: int; (* number of live keys *)
  mutable occupation: int; (* number of live keys + tombstones *)
  mutable mask: int; (* capacity - 1 *)
  mutable keys: Obj.t array;
  mutable vals: Obj.t array;
}

let initial_capacity = 8

(* Max occupancy: 105/128 ≈ 0.82 *)
let max_occupancy = 105

let[@inline] capacity t = Array.length t.keys
let[@inline] start t x = Hashtbl.hash x land t.mask
let[@inline] next t j = (j + 1) land t.mask
let[@inline] prev t j = (j - 1) land t.mask

let[@inline] crowded_or_full occ cap =
  128 * occ > max_occupancy * cap || occ = cap

let create_table () =
  let cap = initial_capacity in
  log_alloc ReactiveAllocTrace.Map_create 0 cap;
  {
    population = 0;
    occupation = 0;
    mask = cap - 1;
    keys = Array.make cap void;
    vals = [||];
  }

let[@inline] ensure_vals t dummy =
  if Array.length t.vals = 0 then (
    log_alloc ReactiveAllocTrace.Map_vals_init 0 (capacity t);
    t.vals <- Array.make (capacity t) dummy)

(* Zap slot j: replace with void or tomb, maintaining the invariant
   that tomb is never followed by void. *)
let zap t j =
  if Array.unsafe_get t.keys (next t j) == void then (
    Array.unsafe_set t.keys j void;
    let k = ref (prev t j) in
    let count = ref 1 in
    while Array.unsafe_get t.keys !k == tomb do
      Array.unsafe_set t.keys !k void;
      k := prev t !k;
      count := !count + 1
    done;
    t.occupation <- t.occupation - !count)
  else Array.unsafe_set t.keys j tomb

(* Insert a key known to be absent, with no tombstones present.
   Does NOT update population/occupation. Used by resize. *)
let rec add_absent t x v j =
  let c = Array.unsafe_get t.keys j in
  if c == void then (
    Array.unsafe_set t.keys j x;
    Array.unsafe_set t.vals j v)
  else add_absent t x v (next t j)

let resize t new_cap =
  log_alloc ReactiveAllocTrace.Table_resize (capacity t) new_cap;
  let old_keys = t.keys in
  let old_vals = t.vals in
  let old_cap = capacity t in
  t.mask <- new_cap - 1;
  t.keys <- Array.make new_cap void;
  (if Array.length old_vals > 0 then
     let dummy = Array.unsafe_get old_vals 0 in
     t.vals <- Array.make new_cap dummy);
  for k = 0 to old_cap - 1 do
    let c = Array.unsafe_get old_keys k in
    if is_not_sentinel c then
      add_absent t c (Array.unsafe_get old_vals k) (start t c)
  done;
  t.occupation <- t.population

let[@inline] possibly_grow t =
  let o = t.occupation and c = capacity t in
  if crowded_or_full o c then resize t (2 * c)

(* ---- mem ---- *)

let rec mem_probe t x j =
  let c = Array.unsafe_get t.keys j in
  if c == void then false
  else if c == tomb then mem_probe t x (next t j)
  else if c = x then true
  else mem_probe t x (next t j)

let[@inline] table_mem t x = mem_probe t x (start t x)

(* ---- find_value (raises Not_found) ---- *)

let rec find_probe t x j =
  let c = Array.unsafe_get t.keys j in
  if c == void then raise Not_found
  else if c == tomb then find_probe t x (next t j)
  else if c = x then Array.unsafe_get t.vals j
  else find_probe t x (next t j)

let[@inline] table_find t x = find_probe t x (start t x)

(* ---- find_maybe (zero-allocation) ---- *)

let maybe_none_obj : Obj.t = Obj.repr Maybe.none

let rec find_maybe_probe t x j =
  let c = Array.unsafe_get t.keys j in
  if c == void then maybe_none_obj
  else if c == tomb then find_maybe_probe t x (next t j)
  else if c = x then Array.unsafe_get t.vals j
  else find_maybe_probe t x (next t j)

let[@inline] table_find_maybe t x = find_maybe_probe t x (start t x)

(* ---- replace ---- *)

let rec replace_probe t x v j =
  let c = Array.unsafe_get t.keys j in
  if c == void then (
    t.occupation <- t.occupation + 1;
    ensure_vals t v;
    t.population <- t.population + 1;
    Array.unsafe_set t.keys j x;
    Array.unsafe_set t.vals j v;
    true)
  else if c == tomb then replace_aux t x v j (next t j)
  else if c = x then (
    Array.unsafe_set t.keys j x;
    Array.unsafe_set t.vals j v;
    false)
  else replace_probe t x v (next t j)

and replace_aux t x v tomb_j j =
  let c = Array.unsafe_get t.keys j in
  if c == void then (
    (* not found; insert at tombstone slot *)
    let j = tomb_j in
    t.population <- t.population + 1;
    Array.unsafe_set t.keys j x;
    Array.unsafe_set t.vals j v;
    true)
  else if c == tomb then replace_aux t x v tomb_j (next t j)
  else if c = x then (
    (* found beyond tombstone; move it back *)
    Array.unsafe_set t.keys tomb_j c;
    Array.unsafe_set t.vals tomb_j (Array.unsafe_get t.vals j);
    zap t j;
    let j = tomb_j in
    Array.unsafe_set t.keys j x;
    Array.unsafe_set t.vals j v;
    false)
  else replace_aux t x v tomb_j (next t j)

let table_replace t x v =
  let was_added = replace_probe t x v (start t x) in
  if was_added then possibly_grow t

(* ---- remove ---- *)

let rec remove_probe t x j =
  let c = Array.unsafe_get t.keys j in
  if c == void then ()
  else if c == tomb then remove_probe t x (next t j)
  else if c = x then (
    t.population <- t.population - 1;
    zap t j)
  else remove_probe t x (next t j)

let[@inline] table_remove t x = remove_probe t x (start t x)

(* ---- find_value_and_remove ---- *)

let rec find_value_and_remove_probe t x j =
  let c = Array.unsafe_get t.keys j in
  if c == void then raise Not_found
  else if c == tomb then find_value_and_remove_probe t x (next t j)
  else if c = x then (
    let v = Array.unsafe_get t.vals j in
    t.population <- t.population - 1;
    zap t j;
    v)
  else find_value_and_remove_probe t x (next t j)

let[@inline] table_find_value_and_remove t x =
  find_value_and_remove_probe t x (start t x)

(* ---- tighten ---- *)

let rec possibly_shrink t new_cap =
  if new_cap = initial_capacity || crowded_or_full t.population (new_cap / 2)
  then (if new_cap < capacity t then resize t new_cap)
  else possibly_shrink t (new_cap / 2)

let table_tighten t = possibly_shrink t (capacity t)

(* ---- clear ---- *)

let table_clear t =
  t.population <- 0;
  t.occupation <- 0;
  Array.fill t.keys 0 (capacity t) void

(* ---- iter ---- *)

let table_iter_kv f t =
  if t.population > 0 then
    for i = 0 to Array.length t.keys - 1 do
      let c = Array.unsafe_get t.keys i in
      if is_not_sentinel c then f c (Array.unsafe_get t.vals i)
    done

let table_iter_kv_with f arg t =
  if t.population > 0 then
    for i = 0 to Array.length t.keys - 1 do
      let c = Array.unsafe_get t.keys i in
      if is_not_sentinel c then f arg c (Array.unsafe_get t.vals i)
    done

let table_iter_k f t =
  if t.population > 0 then
    for i = 0 to Array.length t.keys - 1 do
      let c = Array.unsafe_get t.keys i in
      if is_not_sentinel c then f c
    done

let table_iter_k_with f arg t =
  if t.population > 0 then
    for i = 0 to Array.length t.keys - 1 do
      let c = Array.unsafe_get t.keys i in
      if is_not_sentinel c then f arg c
    done

exception Found

(* ---- exists (early-exit scans) ---- *)

let table_exists_k p t =
  if t.population = 0 then false
  else
    try
      for i = 0 to Array.length t.keys - 1 do
        let c = Array.unsafe_get t.keys i in
        if is_not_sentinel c && p c then raise Found
      done;
      false
    with Found -> true

let table_exists_k_with p arg t =
  if t.population = 0 then false
  else
    try
      for i = 0 to Array.length t.keys - 1 do
        let c = Array.unsafe_get t.keys i in
        if is_not_sentinel c && p arg c then raise Found
      done;
      false
    with Found -> true

(* ---- has_common_key ---- *)

let table_has_common_key a b =
  if a.population = 0 then false
  else
    try
      for i = 0 to Array.length a.keys - 1 do
        let c = Array.unsafe_get a.keys i in
        if is_not_sentinel c && table_mem b c then raise Found
      done;
      false
    with Found -> true

(* ---- Set (keys only, no values) ---- *)

(* For Set we reuse the same table but skip the value array.
   We use replace_set which never touches vals. *)

let rec set_replace_probe t x j =
  let c = Array.unsafe_get t.keys j in
  if c == void then (
    t.occupation <- t.occupation + 1;
    t.population <- t.population + 1;
    Array.unsafe_set t.keys j x;
    true)
  else if c == tomb then set_replace_aux t x j (next t j)
  else if c = x then (
    Array.unsafe_set t.keys j x;
    false)
  else set_replace_probe t x (next t j)

and set_replace_aux t x tomb_j j =
  let c = Array.unsafe_get t.keys j in
  if c == void then (
    let j = tomb_j in
    t.population <- t.population + 1;
    Array.unsafe_set t.keys j x;
    true)
  else if c == tomb then set_replace_aux t x tomb_j (next t j)
  else if c = x then (
    Array.unsafe_set t.keys tomb_j c;
    zap t j;
    false)
  else set_replace_aux t x tomb_j (next t j)

let set_replace t x =
  let was_added = set_replace_probe t x (start t x) in
  if was_added then
    let o = t.occupation and c = capacity t in
    if crowded_or_full o c then (
      (* resize without value array *)
      let old_keys = t.keys in
      let old_cap = capacity t in
      let new_cap = 2 * c in
      log_alloc ReactiveAllocTrace.Set_resize c new_cap;
      t.mask <- new_cap - 1;
      t.keys <- Array.make new_cap void;
      for k = 0 to old_cap - 1 do
        let c = Array.unsafe_get old_keys k in
        if is_not_sentinel c then (
          (* inline add_absent for keys only *)
          let j = ref (start t c) in
          while Array.unsafe_get t.keys !j != void do
            j := next t !j
          done;
          Array.unsafe_set t.keys !j c)
      done;
      t.occupation <- t.population)

let create_set () =
  let cap = initial_capacity in
  log_alloc ReactiveAllocTrace.Set_create 0 cap;
  {
    population = 0;
    occupation = 0;
    mask = cap - 1;
    keys = Array.make cap void;
    vals = [||];
  }

(* ==== Public typed API ==== *)

module Map = struct
  type ('k, 'v) t = table

  let create () = create_table ()
  let clear t = table_clear t

  let replace (type k v) (t : (k, v) t) (k : k) (v : v) =
    table_replace t (Obj.repr k) (Obj.repr v)

  let find_opt (type k v) (t : (k, v) t) (k : k) : v option =
    match table_find t (Obj.repr k) with
    | v -> Some (Obj.obj v : v)
    | exception Not_found -> None

  let find (type k v) (t : (k, v) t) (k : k) : v =
    (Obj.obj (table_find t (Obj.repr k)) : v)

  let find_maybe (type k v) (t : (k, v) t) (k : k) : v Maybe.t =
    Obj.obj (table_find_maybe t (Obj.repr k))

  let mem (type k v) (t : (k, v) t) (k : k) = table_mem t (Obj.repr k)

  let remove (type k v) (t : (k, v) t) (k : k) = table_remove t (Obj.repr k)

  let find_value_and_remove (type k v) (t : (k, v) t) (k : k) : v =
    (Obj.obj (table_find_value_and_remove t (Obj.repr k)) : v)

  let tighten t = table_tighten t

  let iter (type k v) (f : k -> v -> unit) (t : (k, v) t) =
    table_iter_kv (Obj.magic f : Obj.t -> Obj.t -> unit) t

  let iter_with (type a k v) (f : a -> k -> v -> unit) (arg : a) (t : (k, v) t)
      =
    table_iter_kv_with
      (Obj.magic f : Obj.t -> Obj.t -> Obj.t -> unit)
      (Obj.repr arg) t

  let has_common_key (type k v1 v2) (a : (k, v1) t) (b : (k, v2) t) : bool =
    table_has_common_key a b

  let cardinal t = t.population
end

module Set = struct
  type 'k t = table

  let create () = create_set ()
  let clear t = table_clear t

  let add (type k) (t : k t) (k : k) = set_replace t (Obj.repr k)

  let remove (type k) (t : k t) (k : k) = table_remove t (Obj.repr k)
  let mem (type k) (t : k t) (k : k) = table_mem t (Obj.repr k)
  let tighten t = table_tighten t

  let iter (type k) (f : k -> unit) (t : k t) =
    table_iter_k (Obj.magic f : Obj.t -> unit) t

  let iter_with (type a k) (f : a -> k -> unit) (arg : a) (t : k t) =
    table_iter_k_with (Obj.magic f : Obj.t -> Obj.t -> unit) (Obj.repr arg) t

  let exists (type k) (p : k -> bool) (t : k t) =
    table_exists_k (Obj.magic p : Obj.t -> bool) t

  let exists_with (type a k) (p : a -> k -> bool) (arg : a) (t : k t) =
    table_exists_k_with (Obj.magic p : Obj.t -> Obj.t -> bool) (Obj.repr arg) t

  let cardinal t = t.population
end
