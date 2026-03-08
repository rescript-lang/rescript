type ('k, 'v) t = {
  keys: ('k, int, int) Allocator.Block2.t;
  vals: 'v Allocator.Block.t;
}

let initial_capacity = 8
let max_load_percent = 82

let empty_sentinel : Obj.t = Obj.repr (Array.make 257 0)
let tomb_sentinel : Obj.t = Obj.repr (Array.make 257 0)

let[@inline] empty_slot () : 'a Offheap.t = Obj.magic empty_sentinel
let[@inline] tomb_slot () : 'a Offheap.t = Obj.magic tomb_sentinel

let key_capacity t = Allocator.Block2.capacity t.keys
let population t = Allocator.Block2.get0 t.keys
let set_population t n = Allocator.Block2.set0 t.keys n
let occupation t = Allocator.Block2.get1 t.keys
let set_occupation t n = Allocator.Block2.set1 t.keys n
let[@inline] mask t = key_capacity t - 1

let[@inline] start t x = Hashtbl.hash (Offheap.unsafe_to_value x) land mask t

let[@inline] next t j = (j + 1) land mask t
let[@inline] crowded_or_full occ cap = 100 * occ > max_load_percent * cap

let clear_keys t =
  for i = 0 to key_capacity t - 1 do
    Allocator.Block2.set t.keys i (empty_slot ())
  done

let create () =
  let keys = Allocator.Block2.create ~capacity:initial_capacity ~x0:0 ~y0:0 in
  let vals = Allocator.Block.create ~capacity:initial_capacity in
  let t = {keys; vals} in
  clear_keys t;
  t

let destroy t =
  Allocator.Block2.destroy t.keys;
  Allocator.Block.destroy t.vals

let clear t =
  set_population t 0;
  set_occupation t 0;
  clear_keys t

let insert_absent t k v =
  let empty : 'k Offheap.t = empty_slot () in
  let j = ref (start t k) in
  while Allocator.Block2.get t.keys !j != empty do
    j := next t !j
  done;
  Allocator.Block2.set t.keys !j k;
  Allocator.Block.set t.vals !j v

let resize t new_cap =
  let old_cap = key_capacity t in
  let old_keys = Allocator.Block2.create ~capacity:old_cap ~x0:0 ~y0:0 in
  let old_vals = Allocator.Block.create ~capacity:old_cap in
  Allocator.Block2.blit ~src:t.keys ~src_pos:0 ~dst:old_keys ~dst_pos:0
    ~len:old_cap;
  Allocator.Block.blit ~src:t.vals ~src_pos:0 ~dst:old_vals ~dst_pos:0
    ~len:old_cap;
  Allocator.Block2.resize t.keys ~capacity:new_cap;
  Allocator.Block.resize t.vals ~capacity:new_cap;
  set_population t 0;
  set_occupation t 0;
  clear_keys t;
  for i = 0 to old_cap - 1 do
    let k = Allocator.Block2.get old_keys i in
    if k != empty_slot () && k != tomb_slot () then (
      insert_absent t k (Allocator.Block.get old_vals i);
      set_population t (population t + 1);
      set_occupation t (occupation t + 1))
  done;
  Allocator.Block2.destroy old_keys;
  Allocator.Block.destroy old_vals

let maybe_grow_before_insert t =
  let cap = key_capacity t in
  let next_occupation = occupation t + 1 in
  if crowded_or_full next_occupation cap then resize t (2 * cap)

let replace t k v =
  maybe_grow_before_insert t;
  let empty : 'k Offheap.t = empty_slot () in
  let tomb : 'k Offheap.t = tomb_slot () in
  let j = ref (start t k) in
  let first_tomb = ref (-1) in
  let done_ = ref false in
  while not !done_ do
    let current = Allocator.Block2.get t.keys !j in
    if current == empty then (
      let dst = if !first_tomb >= 0 then !first_tomb else !j in
      if !first_tomb < 0 then set_occupation t (occupation t + 1);
      set_population t (population t + 1);
      Allocator.Block2.set t.keys dst k;
      Allocator.Block.set t.vals dst v;
      done_ := true)
    else if current == tomb then (
      if !first_tomb < 0 then first_tomb := !j;
      j := next t !j)
    else if current = k then (
      Allocator.Block.set t.vals !j v;
      done_ := true)
    else j := next t !j
  done

let remove t k =
  let empty : 'k Offheap.t = empty_slot () in
  let tomb : 'k Offheap.t = tomb_slot () in
  let j = ref (start t k) in
  let done_ = ref false in
  while not !done_ do
    let current = Allocator.Block2.get t.keys !j in
    if current == empty then done_ := true
    else if current == tomb then j := next t !j
    else if current = k then (
      Allocator.Block2.set t.keys !j tomb;
      set_population t (population t - 1);
      done_ := true)
    else j := next t !j
  done

let mem t k =
  let empty : 'k Offheap.t = empty_slot () in
  let tomb : 'k Offheap.t = tomb_slot () in
  let j = ref (start t k) in
  let found = ref false in
  let done_ = ref false in
  while not !done_ do
    let current = Allocator.Block2.get t.keys !j in
    if current == empty then done_ := true
    else if current == tomb then j := next t !j
    else if current = k then (
      found := true;
      done_ := true)
    else j := next t !j
  done;
  !found

let find_maybe t k =
  let empty : 'k Offheap.t = empty_slot () in
  let tomb : 'k Offheap.t = tomb_slot () in
  let j = ref (start t k) in
  let found = ref Maybe.none in
  let done_ = ref false in
  while not !done_ do
    let current = Allocator.Block2.get t.keys !j in
    if current == empty then done_ := true
    else if current == tomb then j := next t !j
    else if current = k then (
      found := Maybe.some (Allocator.Block.get t.vals !j);
      done_ := true)
    else j := next t !j
  done;
  !found

let iter_with f arg t =
  let empty : 'k Offheap.t = empty_slot () in
  let tomb : 'k Offheap.t = tomb_slot () in
  if population t > 0 then
    for i = 0 to key_capacity t - 1 do
      let k = Allocator.Block2.get t.keys i in
      if k != empty && k != tomb then f arg k (Allocator.Block.get t.vals i)
    done

let iter f t = iter_with (fun f k v -> f k v) f t
let cardinal = population
